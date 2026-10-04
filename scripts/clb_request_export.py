"""Request a ChecklistBank export for a dataset and wait until it is served.

ChecklistBank serves `/dataset/{key}/export.zip` only once an export job for
that dataset has run, and only a logged-in GBIF account can start one. The
credentials are read from GBIF_USER / GBIF_PWD in the environment, else from
~/.Renviron.

    python scripts/clb_request_export.py 316441 --format DwCA
"""

import argparse
import base64
import json
import os
import pathlib
import sys
import time
import urllib.error
import urllib.request

API = "https://api.checklistbank.org"


def credentials():
    user, pwd = os.environ.get("GBIF_USER"), os.environ.get("GBIF_PWD")
    renviron = pathlib.Path.home() / ".Renviron"
    if (not user or not pwd) and renviron.exists():
        for line in renviron.read_text(encoding="utf-8").splitlines():
            key, sep, val = line.partition("=")
            if not sep:
                continue
            val = val.strip().strip('"').strip("'")
            if key.strip() == "GBIF_USER" and not user:
                user = val
            elif key.strip() == "GBIF_PWD" and not pwd:
                pwd = val
    if not user or not pwd:
        sys.exit("GBIF_USER / GBIF_PWD not found in the environment or ~/.Renviron")
    return user, pwd


def call(method, path, auth=None, body=None):
    data = None if body is None else json.dumps(body).encode()
    req = urllib.request.Request(API + path, data=data, method=method)
    req.add_header("Accept", "application/json")
    if data is not None:
        req.add_header("Content-Type", "application/json")
    if auth is not None:
        token = base64.b64encode(f"{auth[0]}:{auth[1]}".encode()).decode()
        req.add_header("Authorization", "Basic " + token)
    with urllib.request.urlopen(req, timeout=120) as resp:
        raw = resp.read().decode()
    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        return raw.strip().strip('"')


def served(key, fmt):
    req = urllib.request.Request(f"{API}/dataset/{key}/export.zip?format={fmt}",
                                 method="HEAD")
    opener = urllib.request.build_opener(NoRedirect)
    try:
        opener.open(req, timeout=60)
        return True
    except urllib.error.HTTPError as e:
        return e.code in (301, 302, 303, 307, 308)


class NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, *args, **kwargs):
        return None


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("dataset_key", type=int)
    ap.add_argument("--format", default="DwCA")
    ap.add_argument("--poll", type=int, default=60, help="seconds between checks")
    ap.add_argument("--timeout", type=int, default=6 * 3600)
    args = ap.parse_args()

    key, fmt = args.dataset_key, args.format
    if served(key, fmt):
        print(f"{fmt} export of dataset {key} is already served.")
        return

    auth = credentials()
    job = call("POST", f"/dataset/{key}/export", auth=auth,
               body={"format": fmt, "synonyms": True})
    job = job["key"] if isinstance(job, dict) else job
    print(f"Requested {fmt} export of dataset {key}: job {job}", flush=True)

    deadline = time.time() + args.timeout
    while time.time() < deadline:
        time.sleep(args.poll)
        info = call("GET", f"/export/{job}")
        status = info.get("status") if isinstance(info, dict) else None
        print(f"{time.strftime('%H:%M:%S')} status={status}", flush=True)
        if status in ("failed", "canceled"):
            sys.exit(f"Export job {job} {status}: {info.get('error')}")
        if status == "finished" and served(key, fmt):
            print(f"{fmt} export of dataset {key} is served.")
            return
    sys.exit(f"Export of dataset {key} not served after {args.timeout} s")


if __name__ == "__main__":
    main()
