#!/usr/bin/env python3
"""Crawl the open BacDive /fetch API (credential-free as of the v2 open release).

Iterates BacDive-IDs 1..MAXID (sparse; empty IDs skipped) and stores the full
strain JSON, one record per line, gzip-compressed. Resume-safe: block
checkpointing (highest contiguous block completed), so a kill loses at most one
in-flight block. Writes a DONE marker at the ceiling.

Full JSON is stored deliberately: the ~185k requests are the expensive part, so
never trim at crawl time. taxifydb parse_bacdive() selects fields on the build
machine.

Stdlib only.
"""
import gzip, json, os, ssl, sys, time, urllib.error, urllib.request
from concurrent.futures import ThreadPoolExecutor

BASE = "https://api.bacdive.dsmz.de/fetch/{}"
MAXID = 190000          # ceiling confirmed empty by 190000; real strains ~99k
BLOCK = 200             # ids per checkpoint block
WORKERS = 6             # polite concurrency against DSMZ
OUTDIR = os.path.expanduser("~/dev/taxify-crawls/bacdive")
OUT = os.path.join(OUTDIR, "bacdive.jsonl.gz")
CKPT = os.path.join(OUTDIR, "bacdive.ckpt")
DONE = os.path.join(OUTDIR, "DONE")

_ctx = ssl.create_default_context()
_ctx.check_hostname = False
_ctx.verify_mode = ssl.CERT_NONE


def fetch(i, retries=4):
    url = BASE.format(i)
    for attempt in range(retries):
        try:
            req = urllib.request.Request(
                url,
                headers={"User-Agent": "taxify-crawl (research; offline enrichment build)",
                         "Accept": "application/json"},
            )
            raw = urllib.request.urlopen(req, timeout=45, context=_ctx).read()
            d = json.loads(raw)
            if d.get("count", 0) >= 1:
                return list(d.get("results", {}).values())
            return []
        except urllib.error.HTTPError as exc:
            if exc.code == 404:
                return []
            if attempt == retries - 1:
                raise
            time.sleep(2 * (attempt + 1))
        except Exception:
            if attempt == retries - 1:
                raise
            time.sleep(2 * (attempt + 1))
    return []


def worker(i):
    try:
        return i, fetch(i)
    except Exception as exc:
        sys.stderr.write("id %d FAILED: %s\n" % (i, exc))
        return i, []


def main():
    os.makedirs(OUTDIR, exist_ok=True)
    start = 1
    if os.path.exists(CKPT):
        start = int(open(CKPT).read().strip()) + 1
    if start > MAXID:
        open(DONE, "w").write("done\n")
        sys.stderr.write("BACDIVE already complete\n")
        return
    total = 0
    out = gzip.open(OUT, "at", encoding="utf-8")
    with ThreadPoolExecutor(max_workers=WORKERS) as ex:
        b = start
        while b <= MAXID:
            ids = list(range(b, min(b + BLOCK, MAXID + 1)))
            n = 0
            for _i, recs in ex.map(worker, ids):
                for r in recs:
                    out.write(json.dumps(r, ensure_ascii=False) + "\n")
                    n += 1
            out.flush()
            last = ids[-1]
            open(CKPT, "w").write(str(last))
            total += n
            sys.stderr.write("block %d-%d +%d strains (total %d)\n" % (ids[0], last, n, total))
            sys.stderr.flush()
            b = last + 1
    out.close()
    open(DONE, "w").write("done total=%d\n" % total)
    sys.stderr.write("BACDIVE DONE total=%d\n" % total)


if __name__ == "__main__":
    main()
