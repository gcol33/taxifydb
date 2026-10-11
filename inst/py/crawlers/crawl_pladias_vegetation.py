#!/usr/bin/env python3
"""Crawl the synonyms of the Czech vegetation units from Pladias.

Pladias (Chytry et al. 2021, Preslia 93: 1-87) publishes the national
vegetation classification of the Czech Republic (Chytry 2007-2013: 39 classes,
138 alliances, 496 associations) as a spreadsheet of accepted units, and the
synonyms of each unit on its Nomenclature page, as a "Syn.:" paragraph in which
every synonym is an italic name followed by its author citation and any
annotation ("Arrhenatheretum elatioris Braun 1915 (par. 36, nomen ambiguum)").

Each output line (NDJSON) is one accepted unit:
  code       national code, e.g. "TDA01"
  name       accepted name (page key)
  synonyms   [{"name": italic part, "rest": the text up to the next name}]
  original   the "Orig.:" paragraph (the protologue form), or null

Resume-safe; writes DONE when every unit of the spreadsheet is present.
Stdlib only (the spreadsheet is read with the reader of crawl_floraveg_evc).
"""
import html, json, os, re, sys, time, urllib.parse, urllib.request

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from crawl_floraveg_evc import fetch, read_xlsx_rows  # noqa: E402

LIST_URL = ("https://files.ibot.cas.cz/cevs/downloads/"
            "vegetation_units_of_the_czech_republic.xlsx")
PAGE_URL = "https://pladias.cz/en/vegetation/nomenclature/{}"
OUTDIR = os.path.expanduser(os.environ.get("PLADIAS_CRAWL_DIR",
                                           "~/dev/taxify-crawls/vegcz"))
OUT = os.path.join(OUTDIR, "vegcz_synonyms.jsonl")
XLSX = os.path.join(OUTDIR, os.path.basename(LIST_URL))
DONE = os.path.join(OUTDIR, "DONE")
SLEEP = 0.4


def clean(fragment):
    t = html.unescape(re.sub(r"<[^>]+>", " ", fragment))
    return re.sub(r"\s+", " ", t).strip()


def paragraph(page, label):
    m = re.search(r"<p>\s*" + re.escape(label) + r"\s*(.*?)</p>", page, re.S)
    return m.group(1) if m else None


def split_synonyms(par):
    """Every <i>name</i> outside parentheses opens a synonym; the text up to
    the next one is its citation and annotation. An italic name inside an
    open parenthesis belongs to that annotation ("(par. 31, younger homonym:
    non <i>X</i> Braun 1915)") and is kept in it as text."""
    out = []
    parts = re.split(r"<i>(.*?)</i>", par, flags=re.S)
    # parts: [lead, name1, rest1, name2, rest2, ...]
    for k in range(1, len(parts), 2):
        name = clean(parts[k])
        rest = clean(parts[k + 1]) if k + 1 < len(parts) else ""
        if out:
            prev = out[-1]["rest"]
            if prev.count("(") > prev.count(")"):
                out[-1]["rest"] = (prev + " " + name + " " + rest).strip()
                continue
        if name:
            out.append({"name": name, "rest": rest})
    for rec in out:
        rec["rest"] = re.sub(r"^[,;]\s*|[,;]\s*$", "", rec["rest"]).strip()
    return out


def main():
    os.makedirs(OUTDIR, exist_ok=True)
    if not os.path.exists(XLSX):
        with open(XLSX, "wb") as fh:
            fh.write(fetch(LIST_URL))
    rows = read_xlsx_rows(XLSX)
    header, rows = rows[0], rows[1:]
    i_code, i_name = header.index("Code"), header.index("Name")
    units = [(r[i_code].strip(), r[i_name].strip()) for r in rows
             if len(r) > i_name and r[i_name].strip()]

    done = set()
    if os.path.exists(OUT):
        with open(OUT, encoding="utf-8") as fh:
            done = {json.loads(l)["code"] for l in fh if l.strip()}
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
            syn = paragraph(page, "Syn.:")
            orig = paragraph(page, "Orig.")
            rec = {"code": code, "name": name,
                   "synonyms": split_synonyms(syn) if syn else [],
                   "original": clean(orig) if orig else None}
            out.write(json.dumps(rec, ensure_ascii=False) + "\n")
            out.flush()
            if k % 100 == 0:
                sys.stderr.write("...%d/%d\n" % (k, len(todo)))
            time.sleep(SLEEP)

    with open(OUT, encoding="utf-8") as fh:
        have = {json.loads(l)["code"] for l in fh if l.strip()}
    if all(c in have for c, _ in units):
        with open(DONE, "w") as fh:
            fh.write("done %d\n" % len(units))
        sys.stderr.write("VEGCZ DONE\n")


if __name__ == "__main__":
    main()
