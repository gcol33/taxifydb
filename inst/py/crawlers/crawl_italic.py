#!/usr/bin/env python3
"""Crawl ITALIC (Information System on Italian Lichens) taxon pages.

Enumerates taxonpage num=1..MAXNUM, extracts species name + structured
lichen traits (growth form, substrata, photobiont, reproductive strategy).
Resume-safe: appends NDJSON to italic.jsonl, checkpoint = highest num done,
writes a DONE marker when the full range is covered.

Stdlib only. Output feeds taxifydb parse_italic() on the build machine.
"""
import json, os, re, ssl, sys, time, urllib.error, urllib.request

BASE = "https://italic.units.it/index.php?procedure=taxonpage&num={}"
MAXNUM = 3624
OUTDIR = os.path.expanduser("~/dev/taxify-crawls/italic")
OUT = os.path.join(OUTDIR, "italic.jsonl")
CKPT = os.path.join(OUTDIR, "italic.ckpt")
DONE = os.path.join(OUTDIR, "DONE")
FIELDS = ["Growth form", "Substrata", "Photobiont", "Reproductive strategy"]
SLEEP = 0.20  # ~5 req/s, polite for a small academic host

_ctx = ssl.create_default_context()
_ctx.check_hostname = False
_ctx.verify_mode = ssl.CERT_NONE


def fetch(num, retries=4):
    url = BASE.format(num)
    for attempt in range(retries):
        try:
            req = urllib.request.Request(
                url, headers={"User-Agent": "Mozilla/5.0 taxify-crawl (research; offline enrichment build)"}
            )
            return urllib.request.urlopen(req, timeout=45, context=_ctx).read().decode("utf-8", "replace")
        except Exception as exc:
            if attempt == retries - 1:
                raise
            time.sleep(2 * (attempt + 1))


def parse(html, num):
    t = re.search(r"<title>([^<]*)</title>", html)
    name = t.group(1).strip() if t else ""
    # Empty/placeholder pages carry the bare "ITALIC:" masthead title; real
    # taxon pages carry the species name directly.
    if not name or name.upper().startswith("ITALIC"):
        return None
    rec = {"num": num, "name": name}
    for lab in FIELDS:
        m = re.search(
            re.escape("<b>" + lab) + r"[^<]*</b>(.*?)(?=<b>|</td>|</div>|<br|<p>)", html, re.S
        )
        if m:
            val = re.sub("<[^>]+>", " ", m.group(1))
            val = re.sub(r"\s+", " ", val).strip()
            if val:
                rec[lab] = val
    return rec


def main():
    os.makedirs(OUTDIR, exist_ok=True)
    start = 1
    if os.path.exists(CKPT):
        start = int(open(CKPT).read().strip()) + 1
    if start > MAXNUM:
        open(DONE, "w").write("done %d\n" % MAXNUM)
        sys.stderr.write("ITALIC already complete\n")
        return
    with open(OUT, "a", encoding="utf-8") as out:
        for num in range(start, MAXNUM + 1):
            try:
                rec = parse(fetch(num), num)
                if rec:
                    out.write(json.dumps(rec, ensure_ascii=False) + "\n")
                    out.flush()
            except Exception as exc:
                sys.stderr.write("num %d FAILED: %s\n" % (num, exc))
            open(CKPT, "w").write(str(num))
            if num % 100 == 0:
                sys.stderr.write("...%d/%d\n" % (num, MAXNUM))
                sys.stderr.flush()
            time.sleep(SLEEP)
    open(DONE, "w").write("done %d\n" % MAXNUM)
    sys.stderr.write("ITALIC DONE\n")


if __name__ == "__main__":
    main()
