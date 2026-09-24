#!/usr/bin/env python3
"""Crawl per-area distribution status from the Euro+Med PlantBase CDM API.

The euromed backbone (crawl_euromed.py) carries names and synonymy only. The
same CDM server states, for every published accepted taxon, its status in each
Euro+Med area (native, introduced, casual, ...) as Distribution elements of the
taxon's factual description, with the citing references. There is no bulk export,
so this crawler reads /portal/taxon/{uuid}/descriptions once per taxon and
freezes the Distribution elements as an NDJSON snapshot,
euromed_distribution.jsonl, which taxifydb parse_euromed_distribution() reads.

Reuses crawl_euromed's request layer (rate gate, 403 retry, canary and global
throttle cooldown) and its taxon list, so the taxon UUIDs are exactly the
`uuid` of euromed.jsonl and the `taxon_id` of the euromed backbone. Taxa the
portal does not publish (skipped.tsv from the name crawl) are not requested.

Only the non-aggregated descriptions are read: the AGGREGATED_DISTRIBUTION
description is a computed roll-up that carries no references.

Resume-safe (done-set = distribution.done). Run slow, e.g.
    EUROMED_WORKERS=1 EUROMED_MIN_INTERVAL=3 python3 crawl_euromed_distribution.py
Stdlib only.
"""
import json, os, sys, time
from concurrent.futures import ThreadPoolExecutor, as_completed

import crawl_euromed as ce

OUT = os.path.join(ce.OUTDIR, "euromed_distribution.jsonl")
DONE_FILE = os.path.join(ce.OUTDIR, "distribution.done")
NO_DESC = os.path.join(ce.OUTDIR, "distribution.skipped.tsv")
LIMIT = int(os.environ.get("EUROMED_DIST_LIMIT", "0"))
NAMES = {}
# Restrict the crawl to these canonical names (comma-separated), for sampling.
ONLY = ({n.strip() for n in os.environ["EUROMED_DIST_NAMES"].split(",")}
        if os.environ.get("EUROMED_DIST_NAMES") else None)


def taxa():
    # uuid -> (canonical name, full name) of every published accepted taxon.
    skipped = ce.load_done(ce.SKIPPED)
    out = {}
    with open(ce.JSONL, encoding="utf-8") as fh:
        for ln in fh:
            if not ln.strip():
                continue
            r = json.loads(ln)
            u = r.get("uuid", "")
            if u and u not in skipped and u not in out and                     (ONLY is None or r.get("name", "") in ONLY):
                out[u] = (r.get("name", ""), r.get("fullname", ""))
    return out


def _ref(src):
    cit = (src or {}).get("citation") or {}
    return {
        "uuid": cit.get("uuid", ""),
        "citation": cit.get("titleCache") or cit.get("title") or "",
        "id_in_source": (src or {}).get("idInSource") or "",
    }


def _element(el, description):
    area = el.get("area") or {}
    status = el.get("status") or {}
    level = area.get("level") or {}
    return {
        "area": area.get("idInVocabulary") or "",
        "area_name": area.get("representation_L10n") or "",
        "area_level": level.get("representation_L10n") or "",
        "status": status.get("representation_L10n") or "",
        "status_code": status.get("idInVocabulary") or "",
        "absent": bool(status.get("absenceTerm", False)),
        "refs": [_ref(s) for s in (el.get("sources") or [])],
        "description": description,
    }


def descriptions(uuid):
    # Blocks through a global throttle; None when the portal does not serve the
    # taxon's descriptions (a per-taxon 403 while the API is otherwise live).
    recs, idx = [], 0
    while True:
        while True:
            try:
                page = ce.get_json("/portal/taxon/%s/descriptions?pageIndex=%d"
                                   % (uuid, idx))
                break
            except ce.Forbidden:
                if ce._api_alive():
                    return None
                ce._wait_out_throttle()
        recs.extend(page.get("records") or [])
        nxt = page.get("nextIndex")
        if nxt is None or nxt == idx:
            return recs
        idx = nxt


def detail_one(uuid):
    recs = descriptions(uuid)
    if recs is None:
        return None
    rows = []
    for r in recs:
        if "AGGREGATED_DISTRIBUTION" in (r.get("types") or []):
            continue
        title = r.get("titleCache") or ""
        for el in (r.get("elements") or []):
            if el.get("class") == "Distribution":
                rows.append(_element(el, title))
    return {"uuid": uuid, "name": NAMES[uuid][0], "fullname": NAMES[uuid][1],
            "distribution": rows}


def main():
    NAMES.update(taxa())
    todo_all = list(NAMES)
    done = ce.load_done(DONE_FILE)
    todo = [u for u in todo_all if u not in done]
    if LIMIT:
        todo = todo[:LIMIT]
    sys.stderr.write("distribution: %d taxa, %d done, %d to fetch\n"
                     % (len(todo_all), len(done), len(todo)))
    n = n_skip = 0
    t0 = time.time()
    with open(OUT, "a", encoding="utf-8") as out, \
         open(DONE_FILE, "a", encoding="utf-8") as dn, \
         open(NO_DESC, "a", encoding="utf-8") as sk, \
         ce.ThreadPoolExecutor(max_workers=ce.WORKERS) as pool:
        futs = {pool.submit(detail_one, u): u for u in todo}
        for fut in as_completed(futs):
            u = futs[fut]
            try:
                rec = fut.result()
            except Exception as exc:
                sys.stderr.write("distribution: %s ERROR: %s\n" % (u, exc))
                continue
            if rec is None:
                sk.write(u + "\n")
                n_skip += 1
            else:
                out.write(json.dumps(rec, ensure_ascii=False) + "\n")
                n += 1
            dn.write(u + "\n")
            out.flush(); dn.flush(); sk.flush()
            if (n + n_skip) % 100 == 0:
                sys.stderr.write("distribution: %d done, %d skipped / %d (%.0f min)\n"
                                 % (n, n_skip, len(todo), (time.time() - t0) / 60))
                sys.stderr.flush()
    sys.stderr.write("DISTRIBUTION CRAWL DONE: %d fetched, %d skipped\n" % (n, n_skip))


if __name__ == "__main__":
    main()
