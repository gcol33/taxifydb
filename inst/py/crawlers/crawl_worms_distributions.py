#!/usr/bin/env python3
"""Crawl WoRMS species distributions per taxon and freeze them as a snapshot.

Issue #21 needs a marine species -> region table so taxify's region=/coords=
constraint can generalise beyond WCVP (plants). WoRMS is the authoritative
marine source, but its distributions are NOT in the ChecklistBank/GBIF export
(the per-taxon distribution endpoint there returns [], and the dataset says
"obtain a full copy via usersrequest.php"). The maintained distribution records
live only behind the WoRMS REST API, per taxon, keyed on Marine Regions
localities (MRGID). So this crawler harvests them the same way the euromed
crawler harvests Euro+Med: enumerate the checklist, fetch per taxon, freeze an
NDJSON snapshot; taxifydb parse_marine_distribution() builds the .vtr from the
snapshot (after crosswalk_mrgid_meow.py maps each MRGID to a MEOW ecoregion).

Run slow from a spare host over days (571k accepted species):
    WORMS_MIN_INTERVAL=0.3 python3 crawl_worms_distributions.py

Two phases, each resume-safe:
  1. enumerate  walk ChecklistBank dataset 2011 (WoRMS) by higher taxon,
                recursing where a taxon's accepted-species count exceeds CLB's
                100000 offset cap, and page each partition -> species.tsv
                (aphia_id, canonical_name, fossil). Uses ChecklistBank because it
                gives the AphiaID + clean binomial; the distributions themselves
                come from WoRMS REST in phase 2.
  2. distribute per aphia_id: WoRMS AphiaDistributionsByAphiaID -> one NDJSON
                line per (species, MRGID) record. done-set = distribute.done.
                Fossil (dagger-marked) taxa have no modern range and are skipped
                unless WORMS_INCLUDE_FOSSIL=1.

Stdlib only. Output feeds taxifydb parse_marine_distribution() on the build host.
"""
import json, os, sys, time
import urllib.error, urllib.parse, urllib.request

CLB = "https://api.checklistbank.org"
WORMS_DATASET = 2011  # World Register of Marine Species on ChecklistBank
REST = "https://www.marinespecies.org/rest"
OUTDIR = os.path.expanduser(os.environ.get("WORMS_OUTDIR", "~/dev/taxify-crawls/worms_dist"))

MIN_INTERVAL = float(os.environ.get("WORMS_MIN_INTERVAL", "0.3"))
INCLUDE_FOSSIL = os.environ.get("WORMS_INCLUDE_FOSSIL", "0") == "1"
UA = "taxifydb-worms-distribution-crawler (https://github.com/gcol33/taxifydb)"

SPECIES_TSV = os.path.join(OUTDIR, "species.tsv")
ENUM_DONE = os.path.join(OUTDIR, "enumerate.done")
DIST_JSONL = os.path.join(OUTDIR, "worms_distributions.jsonl")
DIST_DONE = os.path.join(OUTDIR, "distribute.done")

BIOTA = "urn:lsid:marinespecies.org:taxname:1"  # WoRMS root
PAGE_CAP = 100000  # ChecklistBank hard-caps search/list offset at 100000

_last = [0.0]


def throttle():
    dt = time.time() - _last[0]
    if dt < MIN_INTERVAL:
        time.sleep(MIN_INTERVAL - dt)
    _last[0] = time.time()


def get(url, tries=6):
    """GET with backoff. Returns (status, parsed-json-or-None)."""
    for attempt in range(tries):
        throttle()
        req = urllib.request.Request(url, headers={"User-Agent": UA,
                                                   "Accept": "application/json"})
        try:
            with urllib.request.urlopen(req, timeout=60) as r:
                if r.status == 204:
                    return 204, None
                body = r.read()
                return r.status, (json.loads(body) if body else None)
        except urllib.error.HTTPError as e:
            if e.code == 204:
                return 204, None
            if e.code in (429, 500, 502, 503, 504):
                time.sleep(min(60, 2 ** attempt))
                continue
            return e.code, None
        except (urllib.error.URLError, TimeoutError, ConnectionError):
            time.sleep(min(60, 2 ** attempt))
    return 0, None


def aphia_from_lsid(lsid):
    return lsid.rsplit(":", 1)[-1] if lsid else None


def mrgid_from_url(u):
    # locationID is a Marine Regions URL, e.g. http://marineregions.org/mrgid/2350
    return u.rstrip("/").rsplit("/", 1)[-1] if u else None


# ---------------------------------------------------------------- phase 1
#
# ChecklistBank hard-caps the search/list offset at 100000, so the 571k accepted
# species cannot be paged linearly. Instead the WoRMS tree is walked and every
# higher taxon whose accepted-species count exceeds the window is recursed into
# (via /tree/{id}/children) until each leaf partition fits under the cap, then
# each partition is paged with the TAXON_ID classification filter. Resume-safe:
# leaf partitions fully paged are recorded in enumerate.done and skipped, and
# rows are de-duplicated against the AphiaIDs already in species.tsv, so an
# interrupted run picks up where it left off. Internal (recursed) nodes are not
# marked done -- re-expanding them on resume is a couple of cheap calls and keeps
# the walk from ever dropping a subtree.


def species_count(taxon_id):
    q = urllib.parse.urlencode({
        "rank": "species", "status": "accepted", "limit": 0,
        "TAXON_ID": taxon_id,
    })
    status, d = get(f"{CLB}/dataset/{WORMS_DATASET}/nameusage/search?{q}")
    return d.get("total") if (status == 200 and d) else None


def taxon_children(taxon_id):
    out, offset = [], 0
    while True:
        q = urllib.parse.urlencode({"limit": 1000, "offset": offset})
        tid = urllib.parse.quote(taxon_id, safe="")
        status, d = get(f"{CLB}/dataset/{WORMS_DATASET}/tree/{tid}/children?{q}")
        if status != 200 or not d:
            break
        res = d.get("result", [])
        out.extend(c.get("id") for c in res if c.get("id"))
        offset += 1000
        if d.get("last") or not res:
            break
    return out


def page_partition(taxon_id, total, out, seen):
    offset = 0
    while offset <= total and offset <= PAGE_CAP:
        q = urllib.parse.urlencode({
            "rank": "species", "status": "accepted",
            "TAXON_ID": taxon_id, "offset": offset, "limit": 1000,
        })
        status, d = get(f"{CLB}/dataset/{WORMS_DATASET}/nameusage/search?{q}")
        if status != 200 or not d:
            time.sleep(10)
            continue
        res = d.get("result", [])
        if not res:
            break
        for rec in res:
            aid = aphia_from_lsid(rec.get("id"))
            usage = rec.get("usage") or {}
            name = (usage.get("name") or {}).get("scientificName")
            label = usage.get("label") or ""
            if not aid or not name or aid in seen:
                continue
            seen.add(aid)
            fossil = "1" if label.strip().startswith("†") else "0"
            out.write(f"{aid}\t{name}\t{fossil}\n")
        out.flush()
        offset += 1000
        if d.get("last"):
            break


def enumerate_species():
    os.makedirs(OUTDIR, exist_ok=True)

    seen = set()
    if os.path.exists(SPECIES_TSV):
        with open(SPECIES_TSV, encoding="utf-8") as f:
            next(f, None)
            for line in f:
                aid = line.split("\t", 1)[0].strip()
                if aid:
                    seen.add(aid)
    done = set()
    if os.path.exists(ENUM_DONE):
        done = set(l.strip() for l in open(ENUM_DONE) if l.strip())

    mode = "a" if os.path.exists(SPECIES_TSV) else "w"
    with open(SPECIES_TSV, mode, encoding="utf-8") as out, \
         open(ENUM_DONE, "a", encoding="utf-8") as donef:
        if mode == "w":
            out.write("aphia_id\tcanonical_name\tfossil\n")
        stack = [BIOTA]
        while stack:
            tid = stack.pop()
            if tid in done:
                continue
            n = species_count(tid)
            if n is None:
                print(f"  count failed for {tid}; will retry on resume",
                      file=sys.stderr)
                continue
            if n == 0:
                donef.write(tid + "\n"); donef.flush()
                continue
            if n > PAGE_CAP:
                kids = taxon_children(tid)
                if not kids:
                    # No children to split an over-cap leaf (should not happen in
                    # WoRMS); page what the window allows rather than drop it.
                    page_partition(tid, n, out, seen)
                    donef.write(tid + "\n"); donef.flush()
                else:
                    stack.extend(kids)
                continue
            page_partition(tid, n, out, seen)
            donef.write(tid + "\n"); donef.flush()
            print(f"  enumerated {len(seen)} species (partition {n})",
                  file=sys.stderr)
    print(f"phase 1 done: {len(seen)} species at {SPECIES_TSV}", file=sys.stderr)


# ---------------------------------------------------------------- phase 2

def load_species():
    rows = []
    with open(SPECIES_TSV, encoding="utf-8") as f:
        next(f, None)
        for line in f:
            parts = line.rstrip("\n").split("\t")
            if len(parts) >= 3:
                rows.append((parts[0], parts[1], parts[2]))
    return rows


def load_done():
    if not os.path.exists(DIST_DONE):
        return set()
    return set(l.strip() for l in open(DIST_DONE) if l.strip())


def distribute():
    species = load_species()
    done = load_done()
    total = len(species)
    with open(DIST_JSONL, "a", encoding="utf-8") as out, \
         open(DIST_DONE, "a", encoding="utf-8") as donef:
        for i, (aid, name, fossil) in enumerate(species):
            if aid in done:
                continue
            if fossil == "1" and not INCLUDE_FOSSIL:
                donef.write(aid + "\n")
                continue
            status, recs = get(f"{REST}/AphiaDistributionsByAphiaID/{aid}")
            if status == 204 or not recs:
                donef.write(aid + "\n")
                donef.flush()
                continue
            if status != 200:
                # transient failure already retried in get(); leave undone so a
                # later pass retries it rather than freezing a gap.
                continue
            for r in recs:
                out.write(json.dumps({
                    "aphia_id": aid,
                    "canonical_name": name,
                    "mrgid": mrgid_from_url(r.get("locationID")),
                    "locality": r.get("locality"),
                    "establishment_means": r.get("establishmentMeans"),
                    "invasiveness": r.get("invasiveness"),
                    "occurrence": r.get("occurrence"),
                    "record_status": r.get("recordStatus"),
                }, ensure_ascii=False) + "\n")
            out.flush()
            donef.write(aid + "\n")
            donef.flush()
            if i % 500 == 0:
                print(f"  distribute {i} / {total} ({len(done) + i} seen)",
                      file=sys.stderr)
    print(f"phase 2 done: {DIST_JSONL}", file=sys.stderr)


def main():
    phase = sys.argv[1] if len(sys.argv) > 1 else "all"
    if phase in ("all", "enumerate"):
        enumerate_species()
    if phase in ("all", "distribute"):
        distribute()


if __name__ == "__main__":
    main()
