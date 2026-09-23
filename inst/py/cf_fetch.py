"""Cloudflare-aware fetcher for the taxifydb enrichment build pipeline.

R's libcurl (and the `curl` R package) send a static TLS ClientHello whose
JA3/JA4 fingerprint every anti-bot has catalogued, so a plain download of a
Cloudflare-fronted source returns the "Just a moment" HTML challenge instead of
the data. `curl_cffi` wraps `curl-impersonate` (patched libcurl + BoringSSL) and
reproduces a real Chrome fingerprint, clearing the TLS-only tier that gates the
NHM Data Portal and similar research portals.

A fingerprint is not enough for every wall. A source that answers
`cf-mitigated: challenge` is running a JavaScript challenge, which no
impersonation identity can satisfy because curl_cffi executes no JS; USGS
ScienceBase serves that tier. Those are cleared once in a visible Chrome whose
cookies and User-Agent the curl_cffi session then reuses, cached per host for
12h under ~/.cache/taxifydb/clearance.

Invoked from R via `download_cf_file()` / `harvest_ckan_datastore()` in
R/enrichment-helpers.R. curl_cffi is the only hard requirement; nodriver is
needed just by the browser rung, and only for a source that runs a JS
challenge. Build-time only; the taxify runtime never calls this.

Usage:
    python cf_fetch.py get   <url> <out_path>
    python cf_fetch.py ckan  <api_base> <resource_id> <out.jsonl>
    python cf_fetch.py wiley <article_url> <doi> <sup_file> <out_path>

`api_base` is the CKAN action root, e.g. https://data.nhm.ac.uk/api/3/action
"""
import json
import os
import re
import sys
import tempfile
import time
import typing
from pathlib import Path
from urllib.parse import urlsplit

CACHE_DIR = Path(os.environ.get(
    "TAXIFYDB_CLEARANCE_CACHE",
    Path.home() / ".cache" / "taxifydb" / "clearance"))
CLEARANCE_TTL = 12 * 3600
BROWSER_TIMEOUT = 120
MAX_TRIES = 5

# Per host: whether the cached clearance was tried, whether a browser has
# already been opened, and the clearance currently applied.
_TRIED_CACHE = set()
_BROWSER_CLEARED = set()
_CLEARANCE = {}


def _impersonate():
    """The newest concrete Chrome identity the installed curl_cffi ships.

    Never the bare "chrome" alias: it maps to an older pinned default that
    Cloudflare already blocks. Reading the identity from the installed
    catalogue keeps it current as curl_cffi is upgraded, where a hard-coded
    tag silently goes stale (the previous pin, chrome131, was eight releases
    behind and is no longer a current fingerprint).
    """
    from curl_cffi.requests.impersonate import BrowserTypeLiteral
    tags = [b for b in typing.get_args(BrowserTypeLiteral)
            if re.fullmatch(r"chrome\d+", b)]
    return max(tags, key=lambda b: int(b[len("chrome"):]))


def _session(url=None):
    """A curl_cffi session carrying any clearance already held for `url`'s host."""
    from curl_cffi import requests as creq
    sess = creq.Session(impersonate=_impersonate())
    state = _CLEARANCE.get(urlsplit(url).netloc) if url else None
    if state:
        if state.get("ua"):
            sess.headers["User-Agent"] = state["ua"]
        for c in state.get("cookies", []):
            sess.cookies.set(c["name"], c["value"], domain=c.get("domain"),
                             path=c.get("path") or "/")
    return sess


def _challenged(r):
    """Whether a non-200 response is a bot wall rather than a real refusal.

    Cloudflare states it outright in `cf-mitigated: challenge`; other walls
    only show as a small HTML body where the data was asked for, so an HTML
    403/503 counts too. A genuine "you may not have this file" is a refusal no
    fingerprint or cookie changes, and escalating on it would open a browser
    for nothing -- but it also costs one clear per host at most, which is
    cheaper than failing a build on a wall we could have walked through.
    """
    headers = getattr(r, "headers", None) or {}
    if headers.get("cf-mitigated") == "challenge":
        return True
    ct = (headers.get("content-type") or "").lower()
    return r.status_code in (403, 503) and ct.startswith("text/html")


def _clearance_path(host):
    return CACHE_DIR / f"{host}.json"


def _load_clearance(host):
    p = _clearance_path(host)
    if not p.exists():
        return None
    try:
        state = json.loads(p.read_text(encoding="utf-8"))
    except (ValueError, OSError):
        return None
    return state if time.time() - state.get("ts", 0) <= CLEARANCE_TTL else None


def _clear_wall(url):
    """Obtain clearance for `url`'s host, cheapest route first.

    A cached clearance is tried once per host per run; if the wall is still
    there after it was applied, the cache is stale, so the next call falls
    through to a real browser clear that overwrites it. The browser itself
    opens at most once per host, so a response that merely looks like a wall
    -- a plain HTML 403 the server means -- costs one window, not one per
    retry. Returns False when no clearance could be obtained, leaving the
    caller to report the wall.
    """
    host = urlsplit(url).netloc
    if host not in _TRIED_CACHE:
        _TRIED_CACHE.add(host)
        cached = _load_clearance(host)
        if cached:
            _CLEARANCE[host] = cached
            sys.stderr.write(f"cf_fetch: reusing cached clearance for {host}\n")
            return True
    if host in _BROWSER_CLEARED:
        return False
    _BROWSER_CLEARED.add(host)
    try:
        state = _browser_clear(url)
    except RuntimeError as e:
        sys.stderr.write(f"cf_fetch: {e}\n")
        return False
    _CLEARANCE[host] = state
    try:
        CACHE_DIR.mkdir(parents=True, exist_ok=True)
        _clearance_path(host).write_text(json.dumps(state), encoding="utf-8")
    except OSError:  # a non-writable cache costs a browser next run, not the build
        pass
    return True


def _browser_clear(url, wall_timeout=BROWSER_TIMEOUT):
    """Run the JS challenge on `url` in a visible Chrome, return {ua, cookies}.

    curl_cffi reproduces a browser's TLS fingerprint but runs no JavaScript,
    so a wall that answers `cf-mitigated: challenge` cannot be cleared by any
    impersonation identity -- the challenge has to execute. The window is
    visible on purpose: Cloudflare serves headless Chrome a manual-check page
    that never clears. The cookies it writes are then handed to curl_cffi for
    the actual transfer, so the browser is paid for once per host per 12h.
    """
    try:
        import nodriver as uc
    except ImportError as e:
        raise RuntimeError(
            "a JavaScript bot-wall challenge needs a browser to clear; "
            "install it with `pip install nodriver`") from e

    import asyncio

    def _raw_cookies():
        resp = yield {"method": "Storage.getCookies", "params": {}}
        return resp["cookies"]

    def _walled(title):
        return any(m in title for m in
                   ("Just a moment", "Checking", "Verifying", "DDoS-Guard"))

    async def _run():
        browser = await uc.start(headless=False,
                                 user_data_dir=tempfile.mkdtemp(prefix="cf_fetch-"))
        try:
            tab = await browser.get(url)
            for _ in range(wall_timeout):
                await asyncio.sleep(1)
                if not _walled(await tab.evaluate("document.title") or ""):
                    break
            await asyncio.sleep(3)
            # The title clears before the challenge has finished writing its
            # cookies, so reload and let the cookie count settle before
            # harvesting; a partial set is refused by the file endpoint.
            await browser.get(url)
            prev, cookies = -1, []
            for _ in range(8):
                await asyncio.sleep(2)
                cookies = await asyncio.wait_for(tab.send(_raw_cookies()), 20)
                if len(cookies) == prev:
                    break
                prev = len(cookies)
            return {"ua": await tab.evaluate("navigator.userAgent"),
                    "cookies": cookies, "ts": time.time()}
        finally:
            browser.stop()

    sys.stderr.write(f"cf_fetch: clearing a browser wall on {urlsplit(url).netloc} "
                     f"(a Chrome window will open briefly)\n")
    return uc.loop().run_until_complete(_run())


def _fetch_with_retries(request, out_path, label, wall_url=None):
    """Stream the response of `request()` to `out_path`, retrying failures.

    Wiley's supplement endpoint answers the same request with 200 or 403 from
    one attempt to the next, so a single try fails a build by chance. Each
    attempt calls `request` afresh, which builds a new session; a network
    error, a non-200 status and a near-empty body all count as a failed try.

    A response that is a bot wall rather than a refusal escalates instead of
    just retrying: `wall_url` is cleared and the attempt reissued at once,
    since the identical request will keep failing until the session holds
    clearance.
    """
    last, walled = None, False
    for attempt in range(1, MAX_TRIES + 1):
        try:
            r = request()
            if r.status_code == 200:
                n = _stream_to_file(r, out_path)
                if n >= 100:
                    print(f"cf_fetch {label}: wrote {n} bytes to {out_path}", flush=True)
                    return
                last = f"suspiciously small response ({n} bytes)"
            else:
                challenge = bool(wall_url) and _challenged(r)
                status = r.status_code
                r.close()
                last = f"HTTP {status}"
                if challenge:
                    walled = True
                    last += " (bot-wall challenge)"
                    if _clear_wall(wall_url):
                        sys.stderr.write(
                            f"cf_fetch {label}: attempt {attempt}/{MAX_TRIES} hit a "
                            f"bot wall; retrying with clearance\n")
                        continue
        except Exception as e:  # noqa: BLE001 - transient network or TLS failure
            last = f"{type(e).__name__}: {e}"
        sys.stderr.write(f"cf_fetch {label}: attempt {attempt}/{MAX_TRIES} failed ({last})\n")
        if attempt < MAX_TRIES:
            time.sleep(3 * 2 ** (attempt - 1))
    hint = ("; the wall needs a browser to clear -- `pip install nodriver`"
            if walled else "")
    sys.exit(f"cf_fetch {label}: {last} after {MAX_TRIES} tries{hint}")


def cmd_get(url, out_path):
    _fetch_with_retries(lambda: _session(url).get(url, timeout=900, stream=True),
                        out_path, "get", wall_url=url)


def cmd_ckan(api_base, resource_id, out_path):
    """Harvest an entire CKAN datastore resource via search_after cursor.

    Plain offset paging caps at max_result_window (10000); datastore_search
    returns an `after` cursor that pages the full table with no window limit.
    """
    url = api_base.rstrip("/") + "/datastore_search"
    sess = _session(url)
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


def _stream_to_file(r, out_path):
    """Write a 200 response body to `out_path`; return the byte count."""
    n = 0
    with open(out_path, "wb") as fh:
        for chunk in r.iter_content(chunk_size=1 << 20):
            fh.write(chunk)
            n += len(chunk)
    r.close()
    return n


def cmd_wiley(article_url, doi, sup_file, out_path):
    """Download a Wiley/Atypon supporting-information file.

    The supplement is served from /action/downloadSupplement with the doi and
    file as query parameters and the article as Referer. Wiley has at times
    required a session that loaded the article page first, so each attempt
    still requests the article; that page is now behind a Cloudflare challenge
    and the supplement request succeeds without it, so its status is ignored.
    """
    p = urlsplit(article_url)
    action = f"{p.scheme}://{p.netloc}/action/downloadSupplement"

    def request():
        sess = _session(article_url)
        try:
            sess.get(article_url, timeout=300)
        except Exception:  # noqa: BLE001 - priming is best effort
            pass
        return sess.get(action, params={"doi": doi, "file": sup_file},
                        headers={"Referer": article_url}, timeout=900, stream=True)

    _fetch_with_retries(request, out_path, "wiley", wall_url=article_url)


def main(argv):
    if len(argv) < 2:
        sys.exit(__doc__)
    mode = argv[1]
    if mode == "get" and len(argv) == 4:
        cmd_get(argv[2], argv[3])
    elif mode == "ckan" and len(argv) == 5:
        cmd_ckan(argv[2], argv[3], argv[4])
    elif mode == "wiley" and len(argv) == 6:
        cmd_wiley(argv[2], argv[3], argv[4], argv[5])
    else:
        sys.exit(__doc__)


if __name__ == "__main__":
    main(sys.argv)
