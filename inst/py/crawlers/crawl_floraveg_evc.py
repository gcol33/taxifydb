#!/usr/bin/env python3
"""Crawl the EuroVegChecklist synonymy from the FloraVeg.EU Vegetation module.

FloraVeg.EU publishes the current EuroVegChecklist (Mucina et al. 2016 with the
changes the European Vegetation Classification Committee has approved since) as
a spreadsheet of accepted classes, orders and alliances, but the synonyms are
only on the per-unit Nomenclature pages. This reads the accepted list from the
spreadsheet and fetches one Nomenclature page per unit.

Each output line (NDJSON) is one accepted unit:
  code       EVC code, e.g. "CM01A (MOL-01A)"
  name       accepted name as the spreadsheet writes it (page key)
  accepted   "Accepted name" as the page renders it (name + author)
  evc_name   "EuroVegChecklist name (Mucina et al. 2016)" (name + author)
  synonyms   [{"text": name + author, "status": "(...)" content, "remark": ...}]
  truncated  true when the server failed mid-render (an "error 500" page body
             inside the synonym list), so the list may miss its tail

Resume-safe: units already in the output are skipped. Writes DONE when every
unit of the spreadsheet is present. Stdlib only.
"""
import html, json, os, re, sys, time, urllib.parse, urllib.request, zipfile
import xml.etree.ElementTree as ET

LIST_URL = ("https://files.ibot.cas.cz/cevs/downloads/floraveg/"
            "List_of_European_vegetation_units_version_4.xlsx")
PAGE_URL = "https://floraveg.eu/vegetation/nomenclature/{}"
OUTDIR = os.path.expanduser(os.environ.get("EVC_CRAWL_DIR",
                                           "~/dev/taxify-crawls/evc"))
OUT = os.path.join(OUTDIR, "evc_synonyms.jsonl")
XLSX = os.path.join(OUTDIR, os.path.basename(LIST_URL))
DONE = os.path.join(OUTDIR, "DONE")
SLEEP = 0.4
UA = "Mozilla/5.0 taxify-crawl (research; offline backbone build)"
NS = {"m": "http://schemas.openxmlformats.org/spreadsheetml/2006/main"}


def fetch(url, retries=4):
    for attempt in range(retries):
        try:
            req = urllib.request.Request(url, headers={"User-Agent": UA})
            return urllib.request.urlopen(req, timeout=60).read()
        except Exception:
            if attempt == retries - 1:
                raise
            time.sleep(3 * (attempt + 1))


def read_xlsx_rows(path):
    """Rows of the first sheet as lists of strings (stdlib xlsx reader)."""
    with zipfile.ZipFile(path) as z:
        shared = []
        if "xl/sharedStrings.xml" in z.namelist():
            root = ET.fromstring(z.read("xl/sharedStrings.xml"))
            for si in root.findall("m:si", NS):
                shared.append("".join(t.text or "" for t in si.iter(
                    "{%s}t" % NS["m"])))
        sheet = ET.fromstring(z.read("xl/worksheets/sheet1.xml"))
    rows = []
    for r in sheet.iter("{%s}row" % NS["m"]):
        cells = {}
        for c in r.findall("m:c", NS):
            ref = re.match(r"([A-Z]+)", c.get("r")).group(1)
            col = 0
            for ch in ref:
                col = col * 26 + (ord(ch) - 64)
            v = c.find("m:v", NS)
            is_ = c.find("m:is", NS)
            if c.get("t") == "s" and v is not None:
                val = shared[int(v.text)]
            elif is_ is not None:
                val = "".join(t.text or "" for t in is_.iter("{%s}t" % NS["m"]))
            else:
                val = v.text if v is not None else ""
            cells[col - 1] = val
        if cells:
            rows.append([cells.get(i, "") for i in range(max(cells) + 1)])
    return rows


def text_of(fragment):
    return html.unescape(re.sub(r"<[^>]+>", " ", fragment))


def section_p(page, heading):
    m = re.search(r"<h3>\s*" + re.escape(heading) + r"\s*</h3>\s*<p>(.*?)</p>",
                  page, re.S)
    return re.sub(r"\s+", " ", text_of(m.group(1))).strip() if m else None


def parse_synonyms(page):
    i = page.find("<h3>Synonyms</h3>")
    if i < 0:
        return []
    body = page[i:]
    out = []
    for li in re.findall(r"<li>(.*?)(?=</li>|<li>|<!DOCTYPE)", body, re.S):
        lines = [l.strip() for l in text_of(li).split("\n")]
        lines = [l for l in lines if l]
        if not lines:
            continue
        rec = {"text": re.sub(r"\s+", " ", lines[0]), "status": None,
               "remark": None}
        rest = " ".join(lines[1:])
        m = re.match(r"\(([^)]*)\)\s*(.*)$", rest)
        if m:
            rec["status"] = m.group(1).strip()
            rest = m.group(2)
        rest = re.sub(r"^[–\-]\s*", "", rest.strip())
        if rest:
            rec["remark"] = re.sub(r"\s+", " ", rest)
        out.append(rec)
    return out


def main():
    os.makedirs(OUTDIR, exist_ok=True)
    if not os.path.exists(XLSX):
        with open(XLSX, "wb") as fh:
            fh.write(fetch(LIST_URL))
    rows = read_xlsx_rows(XLSX)
    header, rows = rows[0], rows[1:]
    i_code = 0
    i_name = next(k for k, h in enumerate(header)
                  if h.startswith("Syntaxon_name (EVC, version"))
    units = [(r[i_code].strip(), r[i_name].strip()) for r in rows
             if len(r) > i_name and r[i_name].strip()]

    done = set()
    if os.path.exists(OUT):
        with open(OUT, encoding="utf-8") as fh:
            for line in fh:
                if line.strip():
                    done.add(json.loads(line)["code"])

    todo = [u for u in units if u[0] not in done]
    sys.stderr.write("%d units, %d to fetch\n" % (len(units), len(todo)))
    with open(OUT, "a", encoding="utf-8") as out:
        for k, (code, name) in enumerate(todo, 1):
            url = PAGE_URL.format(urllib.parse.quote(name))
            try:
                page = fetch(url).decode("utf-8", "replace")
            except Exception as exc:
                sys.stderr.write("%s FAILED: %s\n" % (code, exc))
                continue
            syn_part = page[page.find("<h3>Synonyms</h3>"):] \
                if "<h3>Synonyms</h3>" in page else ""
            rec = {
                "code": code,
                "name": name,
                "accepted": section_p(page, "Accepted name"),
                "evc_name": section_p(
                    page, "EuroVegChecklist name (Mucina et al. 2016)"),
                "synonyms": parse_synonyms(page),
                "truncated": "error 500" in syn_part or
                             ("Server Error" in page and
                              section_p(page, "Accepted name") is None),
            }
            out.write(json.dumps(rec, ensure_ascii=False) + "\n")
            out.flush()
            if k % 100 == 0:
                sys.stderr.write("...%d/%d\n" % (k, len(todo)))
                sys.stderr.flush()
            time.sleep(SLEEP)

    with open(OUT, encoding="utf-8") as fh:
        have = {json.loads(l)["code"] for l in fh if l.strip()}
    if all(c in have for c, _ in units):
        with open(DONE, "w") as fh:
            fh.write("done %d\n" % len(units))
        sys.stderr.write("EVC DONE\n")


if __name__ == "__main__":
    main()
