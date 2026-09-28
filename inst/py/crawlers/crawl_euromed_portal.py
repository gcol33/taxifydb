#!/usr/bin/env python3
"""Crawl per-area distribution status from the Euro+Med PlantBase web portal.

crawl_euromed_distribution.py reads the same statements from the CDM REST API
(api.cybertaxonomy.org). This crawler reads them from the public taxon pages of
europlusmed.org instead, which render a taxon's distribution, synonymy and
bibliography on the server. It writes euromed_distribution_portal.jsonl in the
record layout of the API crawler, so taxifydb parse_euromed_distribution() reads
either file unchanged, and archives every fetched page gzipped so names,
synonymy or a corrected area map can be re-extracted without another request.

The portal sits behind an Anubis proof-of-work check, solved in code by
bot_wall.WallSession (curl_cffi session, cookies reused for the whole crawl).

Differences from the API records, by construction of the page:
  * area codes and levels are not printed (the page's nesting depth is not the
    area level); both come from euromed_areas.json (area label -> level, code).
    A label missing from the table is logged to
    portal.unknown_areas.tsv and written with an empty code, and the archived
    page keeps the data for --reparse once the table is extended.
  * a reference is the bibliography entry the footnote letter names. Its `uuid`
    is the first 12 hex digits of the md5 of the citation text (the page carries
    no CDM identifier), so the same citation gets the same id in every taxon.
  * only literature references are shown; the API's import-source references
    (`EuroPlusMed_00_Edit`, `pandora import`) are not on the page.

    EUROMED_PORTAL_INTERVAL=3 python3 crawl_euromed_portal.py
    python3 crawl_euromed_portal.py --reparse

Resume-safe: a taxon is written to portal.done only after its page is archived
and its record written. A 429/5xx, a connection failure or a non-200 while the
canary page is also failing waits and retries; it never marks a taxon done.
"""
import gzip
import hashlib
import json
import os
import re
import sys
import time
from html.parser import HTMLParser

sys.path.insert(0, os.path.expanduser("~/.python"))

BASE = "https://europlusmed.org"
OUTDIR = os.environ.get("EUROMED_OUTDIR",
                        os.path.expanduser("~/dev/taxify-crawls/euromed"))
HERE = os.path.dirname(os.path.abspath(__file__))
AREAS = os.path.join(HERE, "euromed_areas.json")
JSONL_IN = os.path.join(OUTDIR, "euromed.jsonl")
SKIPPED_IN = os.path.join(OUTDIR, "skipped.tsv")
PAGES = os.path.join(OUTDIR, "portal_pages")
OUT = os.path.join(OUTDIR, "euromed_distribution_portal.jsonl")
DONE_FILE = os.path.join(OUTDIR, "portal.done")
SKIP_FILE = os.path.join(OUTDIR, "portal.skipped.tsv")
UNKNOWN = os.path.join(OUTDIR, "portal.unknown_areas.tsv")
INTERVAL = float(os.environ.get("EUROMED_PORTAL_INTERVAL", "3"))
LIMIT = int(os.environ.get("EUROMED_PORTAL_LIMIT", "0"))
ONLY = ({n.strip() for n in os.environ["EUROMED_DIST_NAMES"].split(",")}
        if os.environ.get("EUROMED_DIST_NAMES") else None)
CANARY = "/cdm_dataportal/taxon/1231b21a-5ae6-4247-a8de-74e15546e0b3"
CANARY_WAIT = 600

ABSENT = ("reported in error", "formerly native")

# A taxon-wide endemism note ("Europe: endemic" / "Europe: not endemic")
# shares the descriptionElement/area_label/distributionStatus markup with the
# genuine per-country Distribution entries but is a different CDM feature
# (endemism, not presence/absence); "Europe" carries no Euro+Med area code
# (the checklist's top area is "Euro+Med", not "Europe") and every other
# unmapped area label's status set held real presence/absence terms
# (native/introduced/naturalised/...), never bare endemism ones.
ENDEMISM = ("endemic", "not endemic", "unknown endemism")


def _classes(attrs):
    return (dict(attrs).get("class") or "").split()


class _Page(HTMLParser):
    """Distribution elements and bibliography of one taxon page."""

    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.spans = []       # one frame per open <span>: (kind, payload)
        self.elems = []       # open distribution elements, innermost last
        self.out = []         # finished elements, document order
        self.refs = {}        # footnote letter -> citation text
        self.foot = None      # letter of the bibliography entry being read
        self.buf = None

    def handle_starttag(self, tag, attrs):
        a = dict(attrs)
        cls = _classes(attrs)
        if tag == "div" and "footnote" in cls:
            m = next((re.match(r"footnote-(\w+)$", c) for c in cls
                      if re.match(r"footnote-(\w+)$", c)), None)
            self.foot = m.group(1) if m else None
            return
        if tag == "a" and self.elems:
            m = re.match(r"#footnote-(\w+)$", a.get("href") or "")
            if m:
                self.elems[-1]["foot"].append(m.group(1))
            return
        if tag != "span":
            return
        kind = None
        lvl = next((int(m.group(1)) for c in cls
                    for m in [re.match(r"level_index_(\d+)$", c)] if m), None)
        if "descriptionElement" in cls and lvl is not None:
            el = {"level": lvl, "area_name": None, "status": [], "foot": []}
            self.elems.append(el)
            kind = ("elem", el)
        elif "area_label" in cls and self.elems:
            self.buf = []
            kind = ("area", self.elems[-1])
        elif self.elems and any(c.startswith("distributionStatus-") for c in cls):
            code = next(c for c in cls if c.startswith("distributionStatus-"))
            self.buf = []
            kind = ("status", (self.elems[-1], code[len("distributionStatus-"):]))
        elif "reference" in cls and self.foot:
            self.buf = []
            kind = ("ref", self.foot)
        self.spans.append(kind)

    def handle_endtag(self, tag):
        if tag != "span" or not self.spans:
            return
        kind = self.spans.pop()
        if kind is None:
            return
        what, payload = kind
        text = "".join(self.buf).strip() if self.buf is not None else ""
        if what == "elem":
            self.elems.pop()
            self.out.append(payload)
        elif what == "area":
            if payload["area_name"] is None:
                payload["area_name"] = text
            self.buf = None
        elif what == "status":
            el, code = payload
            el["status"].append((code, text))
            self.buf = None
        elif what == "ref":
            self.refs.setdefault(payload, text)
            self.buf = None

    def handle_data(self, data):
        if self.buf is not None:
            self.buf.append(data)


def _ref_id(citation):
    return hashlib.md5(citation.encode("utf-8")).hexdigest()[:12]


def load_areas():
    with open(AREAS, encoding="utf-8") as fh:
        return {n: (l, c) for n, l, c in json.load(fh)}


def parse_page(html, areas):
    """Distribution records of one taxon page, and the area labels the area
    table does not know."""
    p = _Page()
    p.feed(html)
    rows, unknown = [], set()
    for el in p.out:
        status = [(c, t) for c, t in el["status"] if t.strip().lower() not in ENDEMISM]
        if not status or not el["area_name"]:
            continue
        level, code = areas.get(el["area_name"], ("", ""))
        if not code:
            unknown.add(el["area_name"])
        refs = []
        for f in el["foot"]:
            cit = p.refs.get(f, "")
            refs.append({"uuid": _ref_id(cit), "citation": cit,
                         "id_in_source": ""})
        for scode, stext in status:
            rows.append({
                "area": code, "area_name": el["area_name"], "area_level": level,
                "status": stext, "status_code": scode,
                "absent": any(t in stext for t in ABSENT),
                "refs": refs, "description": "",
            })
    return rows, unknown


def _page_path(uuid):
    return os.path.join(PAGES, uuid[:2], uuid + ".html.gz")


def load_done(path):
    if not os.path.exists(path):
        return set()
    with open(path, encoding="utf-8") as fh:
        return {ln.strip() for ln in fh if ln.strip()}


def taxa():
    skipped = load_done(SKIPPED_IN)
    out = {}
    with open(JSONL_IN, encoding="utf-8") as fh:
        for ln in fh:
            if not ln.strip():
                continue
            r = json.loads(ln)
            u = r.get("uuid", "")
            if u and u not in skipped and u not in out and \
                    (ONLY is None or r.get("name", "") in ONLY):
                out[u] = (r.get("name", ""), r.get("fullname", ""))
    return out


def _log(msg):
    sys.stderr.write(msg + "\n")
    sys.stderr.flush()


class Portal:
    def __init__(self):
        from bot_wall import WallSession
        self.s = WallSession()
        self.last = 0.0

    def get(self, path):
        wait = self.last + INTERVAL - time.time()
        if wait > 0:
            time.sleep(wait)
        try:
            r = self.s.get(BASE + path)
            return r.status_code, r.text
        except Exception as exc:
            return 0, repr(exc)
        finally:
            self.last = time.time()

    def alive(self):
        code, text = self.get(CANARY)
        return code == 200 and "distribution_hierarchy" in text

    def wait_out(self):
        while True:
            _log("portal: canary failing, waiting %d s" % CANARY_WAIT)
            time.sleep(CANARY_WAIT)
            if self.alive():
                _log("portal: canary answers again")
                return


def _is_taxon_page(html):
    # A taxon page carries the portal title; a higher-rank or placeholder taxon
    # has no distribution block but is still a page to archive. An interstitial
    # or the 404 template has another title.
    m = re.search(r"<title>(.*?)</title>", html, re.S)
    title = m.group(1) if m else ""
    return "| Euro+Med-Plantbase" in title and \
        not title.startswith("Page not found")


def fetch_taxon(portal, uuid):
    """(status, html): status 'ok', 'skip' (portal does not publish the taxon)."""
    tries = 0
    while True:
        code, text = portal.get("/cdm_dataportal/taxon/" + uuid)
        if code == 200 and _is_taxon_page(text):
            return "ok", text
        if code in (404, 403, 410) and portal.alive():
            return "skip", "HTTP %d" % code
        if not portal.alive():
            portal.wait_out()
        else:
            tries += 1
            if tries >= 4:
                return "skip", "HTTP %s" % code
            time.sleep(30 * tries)


def write_record(out, uuid, name, fullname, rows):
    out.write(json.dumps({"uuid": uuid, "name": name, "fullname": fullname,
                          "distribution": rows}, ensure_ascii=False) + "\n")


def reparse():
    areas = load_areas()
    names = taxa()
    n = 0
    with open(OUT, "w", encoding="utf-8") as out, \
            open(UNKNOWN, "w", encoding="utf-8") as unk:
        for u in sorted(load_done(DONE_FILE)):
            path = _page_path(u)
            if not os.path.exists(path) or u not in names:
                continue
            with gzip.open(path, "rt", encoding="utf-8") as fh:
                rows, unknown = parse_page(fh.read(), areas)
            write_record(out, u, names[u][0], names[u][1], rows)
            for name in sorted(unknown):
                unk.write("%s\t%s\n" % (u, name))
            n += 1
    _log("reparse: %d taxa" % n)


def main():
    if "--reparse" in sys.argv:
        return reparse()
    os.makedirs(PAGES, exist_ok=True)
    areas = load_areas()
    names = taxa()
    done = load_done(DONE_FILE)
    todo = [u for u in names if u not in done]
    if LIMIT:
        todo = todo[:LIMIT]
    _log("portal: %d taxa, %d done, %d to fetch" % (len(names), len(done),
                                                   len(todo)))
    portal = Portal()
    t0 = time.time()
    n = n_skip = 0
    with open(OUT, "a", encoding="utf-8") as out, \
            open(DONE_FILE, "a", encoding="utf-8") as dn, \
            open(SKIP_FILE, "a", encoding="utf-8") as sk, \
            open(UNKNOWN, "a", encoding="utf-8") as unk:
        for u in todo:
            status, html = fetch_taxon(portal, u)
            if status == "skip":
                sk.write("%s\t%s\n" % (u, html))
                n_skip += 1
            else:
                path = _page_path(u)
                os.makedirs(os.path.dirname(path), exist_ok=True)
                with gzip.open(path, "wt", encoding="utf-8") as fh:
                    fh.write(html)
                rows, unknown = parse_page(html, areas)
                write_record(out, u, names[u][0], names[u][1], rows)
                for name in sorted(unknown):
                    unk.write("%s\t%s\n" % (u, name))
                n += 1
            dn.write(u + "\n")
            for f in (out, dn, sk, unk):
                f.flush()
            if (n + n_skip) % 100 == 0:
                _log("portal: %d done, %d skipped / %d (%.0f min)"
                     % (n, n_skip, len(todo), (time.time() - t0) / 60))
    _log("PORTAL CRAWL DONE: %d fetched, %d skipped" % (n, n_skip))


if __name__ == "__main__":
    main()
