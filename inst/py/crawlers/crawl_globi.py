#!/usr/bin/env python3
"""Fetch the GloBI interaction snapshot and roll it up to distinct edges.

Two phases, each resume-safe:
  1. Download interactions.tsv.gz (~2.8 GB) with `curl -C -` (resumable; the
     depot host is Cloudflare-fronted and curl's default TLS/UA passes).
  2. Stream-decompress, detect the three relevant columns by header name, and
     write DISTINCT (source_name, interaction_type, target_name) edges, gzip.

Distinct edges (not per-name counts) are the shipped artifact on purpose:
interaction degree is a distinct-count, so it must be aggregated AFTER names are
resolved to accepted grain on the build machine (same lesson as the host-breadth
enrichments). parse_globi() resolves both endpoints and counts distinct accepted
partners. A per-name rollup is written alongside as a sanity artifact only.

Memory-bounded dedup via a set of 64-bit edge hashes. Stdlib + system curl.
"""
import gzip, os, shutil, ssl, subprocess, sys, zlib

URL = "https://depot.globalbioticinteractions.org/snapshot/target/data/tsv/interactions.tsv.gz"
OUTDIR = os.path.expanduser("~/dev/taxify-crawls/globi")
RAW = os.path.join(OUTDIR, "interactions.tsv.gz")
EDGES = os.path.join(OUTDIR, "globi_edges.tsv.gz")
ROLLUP = os.path.join(OUTDIR, "globi_degree.tsv")
DL_DONE = os.path.join(OUTDIR, "DOWNLOAD_DONE")
DONE = os.path.join(OUTDIR, "DONE")

SRC_COL, TYPE_COL, TGT_COL = "sourceTaxonName", "interactionTypeName", "targetTaxonName"


def content_length():
    try:
        out = subprocess.run(
            ["curl", "-sI", URL], capture_output=True, text=True, timeout=60
        ).stdout
        for line in out.splitlines():
            if line.lower().startswith("content-length:"):
                return int(line.split(":", 1)[1].strip())
    except Exception:
        pass
    return None


def download():
    if os.path.exists(DL_DONE):
        return
    expected = content_length()
    # curl -C - resumes a partial file; loop until size matches Content-Length.
    for _ in range(50):
        subprocess.run(["curl", "-L", "-C", "-", "-o", RAW, URL], check=False)
        have = os.path.getsize(RAW) if os.path.exists(RAW) else 0
        sys.stderr.write("downloaded %d / %s bytes\n" % (have, expected))
        sys.stderr.flush()
        if expected and have >= expected:
            break
        if not expected:
            break
    open(DL_DONE, "w").write("%d\n" % (os.path.getsize(RAW) if os.path.exists(RAW) else 0))


def aggregate():
    seen = set()          # 64-bit edge hashes
    deg = {}              # name -> set of partner-name hashes (for the sanity rollup)
    nrec = {}
    kept = 0
    read = 0
    with gzip.open(RAW, "rt", encoding="utf-8", errors="replace") as f, \
            gzip.open(EDGES, "wt", encoding="utf-8") as out:
        hdr = f.readline().rstrip("\n").split("\t")
        idx = {c: i for i, c in enumerate(hdr)}
        for c in (SRC_COL, TYPE_COL, TGT_COL):
            if c not in idx:
                sys.stderr.write("FATAL: column %s not in header (%d cols)\n" % (c, len(hdr)))
                sys.exit(2)
        si, ti, gi = idx[SRC_COL], idx[TYPE_COL], idx[TGT_COL]
        out.write("source_name\tinteraction_type\ttarget_name\n")
        for line in f:
            read += 1
            p = line.rstrip("\n").split("\t")
            if len(p) <= gi:
                continue
            src, itype, tgt = p[si].strip(), p[ti].strip(), p[gi].strip()
            if not src or not tgt or src == "no name" or tgt == "no name":
                continue
            h = hash((src, itype, tgt)) & 0xFFFFFFFFFFFFFFFF
            if h in seen:
                continue
            seen.add(h)
            out.write("%s\t%s\t%s\n" % (src, itype, tgt))
            kept += 1
            # undirected degree sanity rollup
            for a, b in ((src, tgt), (tgt, src)):
                bh = hash(b) & 0xFFFFFFFFFFFFFFFF
                s = deg.get(a)
                if s is None:
                    deg[a] = {bh}
                    nrec[a] = 1
                else:
                    s.add(bh)
                    nrec[a] += 1
            if read % 2_000_000 == 0:
                sys.stderr.write("...read %d, distinct edges %d\n" % (read, kept))
                sys.stderr.flush()
    with open(ROLLUP, "w", encoding="utf-8") as r:
        r.write("name\tinteraction_degree\tn_records\n")
        for name, partners in deg.items():
            r.write("%s\t%d\t%d\n" % (name, len(partners), nrec[name]))
    sys.stderr.write("GLOBI aggregate: read %d rows, %d distinct edges, %d names\n"
                     % (read, kept, len(deg)))
    open(DONE, "w").write("edges=%d names=%d\n" % (kept, len(deg)))


def main():
    os.makedirs(OUTDIR, exist_ok=True)
    download()
    aggregate()
    sys.stderr.write("GLOBI DONE\n")


if __name__ == "__main__":
    main()
