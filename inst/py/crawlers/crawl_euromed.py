#!/usr/bin/env python3
"""Crawl the live Euro+Med PlantBase from the EDIT CDM REST API.

The germansl.infinitenature.org flat file this backbone used to build from is
frozen at Euro+Med 2020 v1.2 and cannot refresh (issue #7). The maintained
data lives only behind the CyberTaxonomy CDM server that backs europlusmed.org,
which exposes no working bulk export (the /dwca and /checklist/export endpoints
time out through the public proxy). This crawler harvests the full checklist
through the per-taxon portal API instead and freezes it as an NDJSON snapshot,
exactly like the ecoflora / floraweb / italic scrape snapshots; taxifydb
read_euromed() builds the .vtr from the snapshot on the build machine.

Two 403 regimes on the per-taxon portal endpoints, handled differently:
  * per-taxon: ~20% of taxa are not published in the portal and always 403
    (deterministic, independent of rate/IP/UA). These are skipped.
  * global throttle: high concurrency trips a sticky rolling-window penalty that
    403s everything. Avoided by running slow; a canary (a known-published taxon)
    tells the two apart -- if the canary is 403 too, the whole pool waits.

Run slow to stay under the throttle, e.g. from a spare host over days:
    EUROMED_WORKERS=1 EUROMED_MIN_INTERVAL=5 python3 crawl_euromed.py

Three phases, each resume-safe:
  1. enumerate   page /taxon -> taxa_listing.tsv (uuid, class, titleCache)
  2. detail      per accepted taxon: /portal/taxon/{uuid} (name, authorship,
                 rank, genus) + /synonymy -> euromed.jsonl; not-published taxa
                 to skipped.tsv. done-set = detail.done
  3. hierarchy   per genus/suprageneric taxon: /taxonNodes -> nodes.tsv
                 (uuid, rank, name, treeIndex) so read_euromed() can map every
                 genus to its family from the materialized tree path.

Stdlib only. Output feeds taxifydb read_euromed() on the build machine.
"""
import json, os, ssl, sys, threading, time
import urllib.error, urllib.parse, urllib.request
from concurrent.futures import ThreadPoolExecutor, as_completed

BASE = "https://api.cybertaxonomy.org/euromed"
CLASSIFICATION = "314a68f9-8449-495a-91c2-92fde8bcf344"  # "Euro+Med 2018"
OUTDIR = os.path.expanduser("~/dev/taxify-crawls/euromed")

LISTING = os.path.join(OUTDIR, "taxa_listing.tsv")
JSONL = os.path.join(OUTDIR, "euromed.jsonl")
SKIPPED = os.path.join(OUTDIR, "skipped.tsv")
DETAIL_DONE = os.path.join(OUTDIR, "detail.done")
NODES = os.path.join(OUTDIR, "nodes.tsv")
NODES_DONE = os.path.join(OUTDIR, "nodes.done")
DONE = os.path.join(OUTDIR, "DONE")

PAGE_SIZE = 1000
# Rate is env-tunable so the same crawler can run very slowly (from a spare host
# over days) to stay under the global throttle. Defaults are gentle.
WORKERS = int(os.environ.get("EUROMED_WORKERS", "2"))
MIN_INTERVAL = float(os.environ.get("EUROMED_MIN_INTERVAL", "0.22"))
RETRY_403_DELAY = float(os.environ.get("EUROMED_RETRY_403", "2"))
COOLDOWN_SECONDS = int(os.environ.get("EUROMED_COOLDOWN", "600"))
UA = "taxifydb euromed harvest (research; offline backbone build; gilles.colling051@gmail.com)"

# Ranks whose treeIndex we harvest so genus->family can be resolved. Species and
# infraspecific taxa get their genus from genusOrUninomial and inherit family.
HIER_RANKS = {
    "Kingdom", "Subkingdom", "Division", "Subdivision", "Class", "Subclass",
    "Superorder", "Order", "Suborder", "Family", "Subfamily", "Tribe",
    "Subtribe", "Genus", "Subgenus",
}

# A known-published taxon (Euphorbia helioscopia L.) used as a liveness canary.
CANARY = "/portal/taxon/1231b21a-5ae6-4247-a8de-74e15546e0b3"

_ctx = ssl.create_default_context()
_ctx.check_hostname = False
_ctx.verify_mode = ssl.CERT_NONE

_rate_lock = threading.Lock()
_next_slot = [0.0]
_cooldown_until = [0.0]


class Forbidden(Exception):
    """A 403 that persisted through retries."""


def _rate_gate():
    # Assign each caller a time slot MIN_INTERVAL apart (and past any active
    # global cooldown), sleeping outside the lock so the global request rate is
    # capped no matter how many workers run.
    with _rate_lock:
        now = time.monotonic()
        base = max(now, _next_slot[0], _cooldown_until[0])
        _next_slot[0] = base + MIN_INTERVAL
        wait = base - now
    if wait > 0:
        time.sleep(wait)


def get_json(path, retries=3):
    # Transient errors retry with backoff; a 403 is retried a few times (the host
    # emits occasional spurious 403s) then raised as Forbidden for the caller to
    # disambiguate (per-taxon not-published vs global throttle) via the canary.
    # Published taxa never 403, so a few retries suffice to flag not-published
    # ones fast without burning many gated requests each.
    url = BASE + path
    err = None
    for attempt in range(retries):
        _rate_gate()
        try:
            req = urllib.request.Request(
                url, headers={"User-Agent": UA, "Accept": "application/json"})
            raw = urllib.request.urlopen(req, timeout=60, context=_ctx).read()
            return json.loads(raw.decode("utf-8", "replace"))
        except urllib.error.HTTPError as exc:
            err = exc
            if exc.code == 403:
                time.sleep(RETRY_403_DELAY)
                continue
            if attempt == retries - 1:
                raise
            time.sleep(2 * (attempt + 1))
        except Exception as exc:
            err = exc
            if attempt == retries - 1:
                raise
            time.sleep(2 * (attempt + 1))
    if isinstance(err, urllib.error.HTTPError) and err.code == 403:
        raise Forbidden(path)
    raise err


def _api_alive():
    try:
        get_json(CANARY, retries=3)
        return True
    except Exception:
        return False


def _wait_out_throttle():
    # The canary itself is 403 -> global throttle. Pause the whole pool and block
    # until the API recovers.
    with _rate_lock:
        _cooldown_until[0] = max(_cooldown_until[0],
                                 time.monotonic() + COOLDOWN_SECONDS)
    sys.stderr.write("global throttle -> cooling down until canary recovers\n")
    sys.stderr.flush()
    while not _api_alive():
        time.sleep(COOLDOWN_SECONDS)
    sys.stderr.write("canary recovered -> resuming\n")
    sys.stderr.flush()


def load_done(path):
    if not os.path.exists(path):
        return set()
    with open(path, encoding="utf-8") as fh:
        return {ln.strip() for ln in fh if ln.strip()}


# ---- phase 1: enumerate every taxon + synonym in the classification ---------

def enumerate_taxa():
    if os.path.exists(LISTING):
        sys.stderr.write("phase 1: taxa_listing.tsv exists, skipping\n")
        return
    total = None
    with open(LISTING + ".tmp", "w", encoding="utf-8") as out:
        idx = 0
        while True:
            pg = get_json("/taxon?pageSize=%d&pageIndex=%d" % (PAGE_SIZE, idx))
            recs = pg.get("records", [])
            if total is None:
                total = pg.get("count")
            if not recs:
                break
            for r in recs:
                out.write("%s\t%s\t%s\n" % (
                    r.get("uuid", ""), r.get("class", ""),
                    (r.get("titleCache", "") or "").replace("\t", " ").replace("\n", " ")))
            out.flush()
            idx += 1
            sys.stderr.write("phase 1: page %d (%d records)\n" % (idx, len(recs)))
            sys.stderr.flush()
    os.replace(LISTING + ".tmp", LISTING)
    n = sum(1 for _ in open(LISTING, encoding="utf-8"))
    sys.stderr.write("phase 1: wrote %d rows (API count=%s)\n" % (n, total))
    if total is not None and abs(n - int(total)) > int(total) * 0.02:
        sys.stderr.write("phase 1 WARNING: row count %d far from API count %s\n"
                         % (n, total))


def read_listing():
    acc, syn = [], 0
    with open(LISTING, encoding="utf-8") as fh:
        for ln in fh:
            parts = ln.rstrip("\n").split("\t")
            if len(parts) < 2:
                continue
            uuid, cls = parts[0], parts[1]
            if cls == "Taxon":
                acc.append(uuid)
            elif cls == "Synonym":
                syn += 1
    return acc, syn


# ---- phase 2: per accepted taxon, name + rank + genus + synonyms ------------

def _syn_records(synonymy):
    out = []
    groups = list(synonymy.get("homotypicSynonymsByHomotypicGroup") or [])
    for grp in (synonymy.get("heterotypicSynonymyGroups") or []):
        groups.extend(grp or [])
    for s in groups:
        if not isinstance(s, dict):
            continue
        nm = s.get("name") or {}
        rank = (nm.get("rank") or {}).get("representation_L10n")
        out.append({
            "uuid": s.get("uuid", ""),
            "name": nm.get("nameCache") or "",
            "fullname": nm.get("titleCache") or "",
            "rank": rank or "",
        })
    return out


def detail_one(uuid):
    # Returns the taxon record, or None if the portal does not publish this taxon
    # (a per-taxon 403 while the API is otherwise live). Blocks through a global
    # throttle. The full name string (name.titleCache) carries the authorship;
    # read_euromed() derives it there.
    while True:
        try:
            tax = get_json("/portal/taxon/%s" % uuid)
            break
        except Forbidden:
            if _api_alive():
                return None            # taxon not published -> skip
            _wait_out_throttle()       # global throttle -> wait, then retry
    try:
        syns = _syn_records(get_json("/portal/taxon/%s/synonymy" % uuid))
    except Forbidden:
        syns = []                      # taxon served but synonymy forbidden
    nm = tax.get("name") or {}
    rank = (nm.get("rank") or {}).get("representation_L10n")
    return {
        "uuid": uuid,
        "name": nm.get("nameCache") or "",
        "fullname": nm.get("titleCache") or "",
        "rank": rank or "",
        "genus": nm.get("genusOrUninomial") or "",
        "doubtful": bool(tax.get("doubtful", False)),
        "synonyms": syns,
    }


def detail_all(accepted):
    done = load_done(DETAIL_DONE)
    todo = [u for u in accepted if u not in done]
    sys.stderr.write("phase 2: %d accepted, %d done, %d to fetch\n"
                     % (len(accepted), len(done), len(todo)))
    if not todo:
        return
    n = n_skip = 0
    with open(JSONL, "a", encoding="utf-8") as out, \
         open(DETAIL_DONE, "a", encoding="utf-8") as dn, \
         open(SKIPPED, "a", encoding="utf-8") as sk, \
         ThreadPoolExecutor(max_workers=WORKERS) as pool:
        futs = {pool.submit(detail_one, u): u for u in todo}
        for fut in as_completed(futs):
            u = futs[fut]
            try:
                rec = fut.result()
            except Exception as exc:
                sys.stderr.write("phase 2: %s ERROR: %s\n" % (u, exc))
                continue
            if rec is None:
                sk.write(u + "\n")
                n_skip += 1
            else:
                out.write(json.dumps(rec, ensure_ascii=False) + "\n")
                n += 1
            # Flush every record so a kill mid-run loses nothing and resume from
            # detail.done is exact (the crawl runs unattended for days).
            dn.write(u + "\n")
            out.flush(); sk.flush(); dn.flush()
            if (n + n_skip) % 200 == 0:
                sys.stderr.write("phase 2: %d done, %d skipped / %d\n"
                                 % (n, n_skip, len(todo)))
                sys.stderr.flush()
    sys.stderr.write("phase 2: fetched %d, skipped %d (not published)\n"
                     % (n, n_skip))


# ---- phase 3: treeIndex for genus/suprageneric taxa (genus->family) ---------

def hierarchy():
    want = {}
    with open(JSONL, encoding="utf-8") as fh:
        for ln in fh:
            if not ln.strip():
                continue
            r = json.loads(ln)
            if r.get("rank") in HIER_RANKS:
                want[r["uuid"]] = (r.get("rank", ""), r.get("name", ""))
    done = load_done(NODES_DONE)
    todo = [u for u in want if u not in done]
    sys.stderr.write("phase 3: %d hier taxa, %d done, %d to fetch\n"
                     % (len(want), len(done), len(todo)))
    if not todo:
        return

    def node_one(u):
        while True:
            try:
                nodes = get_json("/portal/taxon/%s/taxonNodes" % u)
                break
            except Forbidden:
                if _api_alive():
                    return u, ""       # not published -> no treeIndex
                _wait_out_throttle()
        return u, (nodes[0].get("treeIndex", "") if nodes else "")

    n = 0
    with open(NODES, "a", encoding="utf-8") as out, \
         open(NODES_DONE, "a", encoding="utf-8") as dn, \
         ThreadPoolExecutor(max_workers=WORKERS) as pool:
        futs = {pool.submit(node_one, u): u for u in todo}
        for fut in as_completed(futs):
            u = futs[fut]
            try:
                _, ti = fut.result()
            except Exception as exc:
                sys.stderr.write("phase 3: %s ERROR: %s\n" % (u, exc))
                continue
            rank, name = want[u]
            out.write("%s\t%s\t%s\t%s\n" % (u, rank, name.replace("\t", " "), ti))
            dn.write(u + "\n")
            out.flush(); dn.flush()
            n += 1
            if n % 200 == 0:
                sys.stderr.write("phase 3: %d/%d\n" % (n, len(todo)))
                sys.stderr.flush()
    sys.stderr.write("phase 3: fetched %d\n" % n)


def main():
    os.makedirs(OUTDIR, exist_ok=True)
    t0 = time.time()
    enumerate_taxa()
    accepted, n_syn = read_listing()
    sys.stderr.write("listing: %d accepted, %d synonym records\n"
                     % (len(accepted), n_syn))
    detail_all(accepted)
    hierarchy()
    with open(DONE, "w", encoding="utf-8") as fh:
        fh.write("done accepted=%d elapsed=%.0fs\n"
                 % (len(accepted), time.time() - t0))
    sys.stderr.write("EUROMED CRAWL DONE (%.0f min)\n" % ((time.time() - t0) / 60))


if __name__ == "__main__":
    main()
