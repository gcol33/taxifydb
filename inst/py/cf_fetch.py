"""Cloudflare-aware fetcher for the taxifydb enrichment build pipeline.

R's libcurl (and the `curl` R package) send a static TLS ClientHello whose
JA3/JA4 fingerprint every anti-bot has catalogued, so a plain download of a
Cloudflare-fronted source returns the "Just a moment" HTML challenge instead of
the data. `curl_cffi` wraps `curl-impersonate` (patched libcurl + BoringSSL) and
reproduces a real Chrome fingerprint, clearing the TLS-only tier that gates the
NHM Data Portal and similar research portals.

Invoked from R via `download_cf_file()` / `harvest_ckan_datastore()` in
R/enrichment-helpers.R. Kept dependency-light (curl_cffi only) and build-time
only; the taxify runtime never calls this.

Usage:
    python cf_fetch.py get  <url> <out_path>
    python cf_fetch.py ckan <api_base> <resource_id> <out.jsonl>

`api_base` is the CKAN action root, e.g. https://data.nhm.ac.uk/api/3/action
"""
import json
import sys

# Pin the newest concrete browser identity, NOT the bare "chrome" alias: the
# alias maps to an older default that Cloudflare still blocked on NHM, while a
# specific recent version passes. Bump this as curl_cffi ships newer tags.
IMPERSONATE = "chrome131"


def _session():
    from curl_cffi import requests as creq
    return creq.Session(impersonate=IMPERSONATE)


def cmd_get(url, out_path):
    sess = _session()
    r = sess.get(url, timeout=900, stream=True)
    if r.status_code != 200:
        sys.exit(f"cf_fetch get: HTTP {r.status_code} for {url}")
    n = 0
    with open(out_path, "wb") as fh:
        for chunk in r.iter_content(chunk_size=1 << 20):
            fh.write(chunk)
            n += len(chunk)
    r.close()
    if n < 100:
        sys.exit(f"cf_fetch get: suspiciously small response ({n} bytes)")
    print(f"cf_fetch get: wrote {n} bytes to {out_path}", flush=True)


def cmd_ckan(api_base, resource_id, out_path):
    """Harvest an entire CKAN datastore resource via search_after cursor.

    Plain offset paging caps at max_result_window (10000); datastore_search
    returns an `after` cursor that pages the full table with no window limit.
    """
    sess = _session()
    url = api_base.rstrip("/") + "/datastore_search"
    page = 1000
    after = None
    total = None
    seen = 0
    with open(out_path, "w", encoding="utf-8") as fh:
        while True:
            params = {"resource_id": resource_id, "limit": page, "sort": "_id"}
            if after is not None:
                params["after"] = json.dumps(after)
            res = None
            for attempt in range(5):
                try:
                    j = sess.get(url, params=params, timeout=120).json()
                    if j.get("success"):
                        res = j["result"]
                        break
                    sys.stderr.write(f"  retry {attempt}: {str(j.get('error'))[:120]}\n")
                except Exception as e:  # noqa: BLE001 - transient CF/ES hiccup
                    sys.stderr.write(f"  retry {attempt} EXC: {type(e).__name__}: {e}\n")
                import time
                time.sleep(2 * (attempt + 1))
            if res is None:
                sys.exit("cf_fetch ckan: page fetch failed after retries")
            if total is None:
                total = res.get("total")
                print(f"cf_fetch ckan: total {total} records", flush=True)
            recs = res.get("records", [])
            if not recs:
                break
            for rec in recs:
                fh.write(json.dumps(rec, ensure_ascii=False) + "\n")
            seen += len(recs)
            after = res.get("after")
            if seen % 20000 < page:
                print(f"  {seen}/{total}", flush=True)
            if after is None or (total is not None and seen >= total):
                break
    print(f"cf_fetch ckan: wrote {seen} records to {out_path}", flush=True)


def main(argv):
    if len(argv) < 2:
        sys.exit(__doc__)
    mode = argv[1]
    if mode == "get" and len(argv) == 4:
        cmd_get(argv[2], argv[3])
    elif mode == "ckan" and len(argv) == 5:
        cmd_ckan(argv[2], argv[3], argv[4])
    else:
        sys.exit(__doc__)


if __name__ == "__main__":
    main(sys.argv)
