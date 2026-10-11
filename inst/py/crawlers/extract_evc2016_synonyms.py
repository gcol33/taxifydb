#!/usr/bin/env python3
"""Extract the synonym lists of the original EuroVegChecklist from its paper.

Mucina et al. (2016) Vegetation of Europe: hierarchical floristic classification
system of vascular plant, bryophyte, lichen, and algal communities. Applied
Vegetation Science 19 (Suppl. 1): 3-264. doi:10.1111/avsc.12257. CC BY 4.0.

The paper prints every class, order and alliance as a header line
("MOL-01A Arrhenatherion elatioris Luquet 1926"), its diagnosis, then one
bulleted line per synonym ("* Arrhenatherion Br.-Bl. 1931 (2b)"), with numbered
remarks ("mol06 A proposal to ...") in between. This writes one TSV row per
synonym: the unit's mnemonic code, the synonym as printed (name + author), and
its parenthesized nomenclatural annotation.

The FloraVeg.EU Nomenclature pages carry the current synonymy and are the
primary source; taxifydb reads these rows only for units whose page failed
mid-render, to restore the synonyms the page lost.

Usage: python extract_evc2016_synonyms.py <paper.pdf> <out.tsv>
Requires PyMuPDF (fitz).
"""
import re, sys
import fitz

HEADER = re.compile(r"^([A-Z]{3}(?:-[0-9]{2}[A-Z]?)?)\s+([A-Z].*)$")
REMARK = re.compile(r"^[a-z]{3}[0-9]{2}\s")
BULLET = "•"
NOISE = re.compile(r"^(Applied Vegetation Science|Doi:|Vegetation of Europe$|"
                   r"L\. Mucina et al\.$|[0-9]{1,3}$)")
STATUS = re.compile(r"\s*\(([^()]*)\)\s*$")


def lines_of(pdf):
    for page in fitz.open(pdf):
        for line in page.get_text().split("\n"):
            line = line.strip()
            if line and not NOISE.match(line):
                yield line


def main(pdf, out):
    rows = []
    unit = None
    cur = None

    def flush():
        nonlocal cur
        if cur is not None and unit is not None:
            text = re.sub(r"\s+", " ", cur).strip()
            status = None
            m = STATUS.search(text)
            # The annotation is the last parenthesis; a citation like
            # "(Oberd. 1957) Passarge 1964" ends in a year, not in one.
            if m and not re.search(r"[0-9]{4}\)?$", m.group(1)):
                status = re.sub(r"(?<=[a-z])-(?=[a-z])", "", m.group(1))
                text = text[:m.start()].rstrip()
                m2 = STATUS.search(text)
                if m2 and re.fullmatch(r"[A-Za-z.\- ]+", m2.group(1)) and \
                        not re.search(r"[0-9]", m2.group(1)):
                    status = re.sub(r"(?<=[a-z])-(?=[a-z])", "",
                                    m2.group(1)) + "; " + status
                    text = text[:m2.start()].rstrip()
            rows.append((unit, text, status or ""))
        cur = None

    for line in lines_of(pdf):
        h = HEADER.match(line)
        if line.startswith(BULLET):
            flush()
            cur = line[1:].strip()
            continue
        if h and len(h.group(1)) in (3, 6, 7) and not line.isupper():
            flush()
            unit = h.group(1)
            continue
        if REMARK.match(line) or line.isupper():
            flush()
            continue
        if cur is not None:
            # A wrapped synonym continues until its annotation closes.
            if cur.endswith("-"):
                cur += line
            else:
                cur += " " + line
            if cur.rstrip().endswith(")") and \
                    cur.count("(") == cur.count(")"):
                flush()

    flush()
    with open(out, "w", encoding="utf-8", newline="\n") as fh:
        fh.write("mnemonic\ttext\tstatus\n")
        for u, t, s in rows:
            fh.write("%s\t%s\t%s\n" % (u, t.replace("\t", " "), s))
    sys.stderr.write("%d synonyms over %d units\n" %
                     (len(rows), len({r[0] for r in rows})))


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
