#!/usr/bin/env python3
"""Build the MRGID -> MEOW ecoregion crosswalk for the marine distribution asset.

WoRMS records distributions against Marine Regions localities (MRGID), from
ocean basins down to individual bays (crawl_worms_distributions.py). Issue #21
rolls those up to Marine Ecoregions of the World (MEOW; Spalding et al. 2007) so
a species -> ecoregion range table can back taxify's region=/coords= filter.
This script maps every distinct MRGID seen in the WoRMS snapshot to the MEOW
ecoregion(s) it falls in, and freezes the result as mrgid_meow.tsv, which
taxifydb parse_marine_distribution() joins against.

Assignment (deliberately approximate -- MRGIDs are not MEOW-coded, so this is a
geometric best effort, imperfect at region margins as accepted for #21):
  * the MRGID centroid's containing ecoregion (point-in-polygon), plus
  * for coarse MRGIDs (ocean basins, large seas) every ecoregion whose own
    centroid falls inside the MRGID bounding box, so a basin maps to all its
    ecoregions rather than the single one its centre happens to sit in.
  * globe-spanning entries (gazetteer placeType "World") are skipped: they roll
    up to all 232 ecoregions, which places a species everywhere and so
    constrains nothing.

Inputs:
  worms_distributions.jsonl  (the WoRMS snapshot; only its MRGIDs are read)
  meow_ecos.geojson          (the frozen MEOW ecoregion polygons; the same
                              snapshot backend-meow.R builds meow.vtr from)

Two phases. `fetch` is the only network pass: it caches each MRGID's gazetteer
record to mrgid_gazetteer.jsonl. `assign` recomputes mrgid_meow.tsv from that
cache, so revising how a locality maps to an ecoregion costs seconds instead of
another pass over the gazetteer.

Run (bounded: a few thousand distinct MRGIDs, not the 571k species):
    python3 crosswalk_mrgid_meow.py path/to/meow_ecos.geojson          # both
    python3 crosswalk_mrgid_meow.py path/to/meow_ecos.geojson assign   # recompute

Stdlib only. Resume-safe (done set), throttled. Output feeds the build host.
"""
import json, os, sys, time
import urllib.error, urllib.request

OUTDIR = os.path.expanduser(os.environ.get("WORMS_OUTDIR", "~/dev/taxify-crawls/worms_dist"))
DIST_JSONL = os.path.join(OUTDIR, "worms_distributions.jsonl")
XW_TSV = os.path.join(OUTDIR, "mrgid_meow.tsv")
XW_DONE = os.path.join(OUTDIR, "crosswalk.done")
GAZ_JSONL = os.path.join(OUTDIR, "mrgid_gazetteer.jsonl")

REST = "https://www.marineregions.org/rest"
MIN_INTERVAL = float(os.environ.get("MRGID_MIN_INTERVAL", "0.3"))
UA = "taxifydb-mrgid-meow-crosswalk (https://github.com/gcol33/taxifydb)"

_last = [0.0]


def throttle():
    dt = time.time() - _last[0]
    if dt < MIN_INTERVAL:
        time.sleep(MIN_INTERVAL - dt)
    _last[0] = time.time()


def gazetteer(mrgid, tries=6):
    # The trailing slash is required by the Marine Regions REST router.
    url = f"{REST}/getGazetteerRecordByMRGID.json/{mrgid}/"
    for attempt in range(tries):
        throttle()
        req = urllib.request.Request(url, headers={"User-Agent": UA,
                                                   "Accept": "application/json"})
        try:
            with urllib.request.urlopen(req, timeout=60) as r:
                body = r.read()
                return json.loads(body) if body else None
        except urllib.error.HTTPError as e:
            if e.code == 404:
                return None
            if e.code in (429, 500, 502, 503, 504):
                time.sleep(min(60, 2 ** attempt))
                continue
            return None
        except (urllib.error.URLError, TimeoutError, ConnectionError):
            time.sleep(min(60, 2 ** attempt))
    return None


# ----------------------------------------------------- MEOW geometry (stdlib)

def _prop(props, name):
    for k in props:
        if k.upper() == name:
            return props[k]
    return None


def _wraps(xs):
    """True when a 0..360 longitude framing describes the shape more tightly.

    13 MEOW ecoregions cross the antimeridian (the Aleutians, Kamchatka, the
    Bering and Chukchi Seas, Hawaii, Fiji, New Zealand, the Ross Sea). Their
    vertices run to both -180 and +180, so in the raw framing they look 360
    degrees wide and their centre falls at longitude 0 -- the Aleutians would
    centre on the English Channel and be picked up by every coarse European
    bounding box. Re-framing to 0..360 makes such a region contiguous again:
    they narrow from 360 degrees to between 7 and 64.

    The comparison needs a tolerance because taking the modulo of a longitude
    that is already positive returns a value a few ulps away, which would
    otherwise report a region wholly inside one hemisphere as re-framed.
    """
    w180 = max(xs) - min(xs)
    xs360 = [x % 360.0 for x in xs]
    return (max(xs360) - min(xs360)) < w180 - 1e-6


def _wrap180(x):
    return ((x + 180.0) % 360.0) - 180.0


def load_meow(geojson_path):
    gj = json.load(open(geojson_path, encoding="utf-8"))
    ecos = []
    for ft in gj["features"]:
        props = ft.get("properties") or {}
        geom = ft.get("geometry") or {}
        code = _prop(props, "ECO_CODE")
        if code is None or geom.get("type") not in ("Polygon", "MultiPolygon"):
            continue
        polys = ([geom["coordinates"]] if geom["type"] == "Polygon"
                 else geom["coordinates"])
        xs, ys = [], []
        for poly in polys:
            for x, y in poly[0]:
                xs.append(x); ys.append(y)

        # Antimeridian-crossing regions are held in 0..360 space; a query point
        # is shifted the same way before any test against them.
        shift = _wraps(xs)
        if shift:
            polys = [[[[x % 360.0, y] for x, y in ring] for ring in poly]
                     for poly in polys]
            xs = [x % 360.0 for x in xs]

        ecos.append({
            "eco_code": str(int(code)) if float(code).is_integer() else str(code),
            "ecoregion": _prop(props, "ECOREGION"),
            "province": _prop(props, "PROVINCE"),
            "realm": _prop(props, "REALM"),
            "polys": polys,
            "shift": shift,
            "bbox": (min(xs), min(ys), max(xs), max(ys)),
            # Reported back in -180..180 so it can be compared with an MRGID
            # bounding box, which is always given in that framing.
            "cx": _wrap180((min(xs) + max(xs)) / 2.0),
            "cy": (min(ys) + max(ys)) / 2.0,
        })
    return ecos


def _point_in_ring(x, y, ring):
    inside = False
    n = len(ring)
    j = n - 1
    for i in range(n):
        xi, yi = ring[i][0], ring[i][1]
        xj, yj = ring[j][0], ring[j][1]
        if ((yi > y) != (yj > y)) and \
           (x < (xj - xi) * (y - yi) / (yj - yi + 1e-15) + xi):
            inside = not inside
        j = i
    return inside


def point_in_eco(x, y, eco):
    if eco["shift"]:
        x = x % 360.0
    bx0, by0, bx1, by1 = eco["bbox"]
    if x < bx0 or x > bx1 or y < by0 or y > by1:
        return False
    for poly in eco["polys"]:
        if _point_in_ring(x, y, poly[0]):
            # outer ring hit; reject if the point sits in a hole
            if not any(_point_in_ring(x, y, hole) for hole in poly[1:]):
                return True
    return False


# Gazetteer place types that span the whole globe. "World" / "World Oceans"
# roll up to every one of the 232 ecoregions, which states that a species is
# everywhere -- no constraint at all, so the rows only inflate the asset.
GLOBAL_PLACE_TYPES = {"world"}


def assign(rec, ecos):
    """Return list of ecoregion dicts an MRGID gazetteer record maps to."""
    if (rec.get("placeType") or "").strip().lower() in GLOBAL_PLACE_TYPES:
        return []
    lat, lon = rec.get("latitude"), rec.get("longitude")
    hits = {}
    if lat is not None and lon is not None:
        for e in ecos:
            if point_in_eco(lon, lat, e):
                hits[e["eco_code"]] = e
    # Coarse regions: add every ecoregion whose centre lies in the MRGID bbox.
    x0, y0 = rec.get("minLongitude"), rec.get("minLatitude")
    x1, y1 = rec.get("maxLongitude"), rec.get("maxLatitude")
    if None not in (x0, y0, x1, y1):
        # An MRGID box that crosses the antimeridian is given with its west edge
        # east of its east edge, so the longitude test is the union of the two
        # halves rather than a single interval.
        wraps = x0 > x1
        for e in ecos:
            cx = e["cx"]
            in_lon = (cx >= x0 or cx <= x1) if wraps else (x0 <= cx <= x1)
            if in_lon and y0 <= e["cy"] <= y1:
                hits[e["eco_code"]] = e
    return list(hits.values())


# ----------------------------------------------------------------- driver

def distinct_mrgids():
    seen = set()
    with open(DIST_JSONL, encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            m = json.loads(line).get("mrgid")
            if m:
                seen.add(str(m))
    return sorted(seen, key=lambda s: int(s) if s.isdigit() else 0)


def load_done():
    if not os.path.exists(XW_DONE):
        return set()
    return set(l.strip() for l in open(XW_DONE) if l.strip())


def load_cache():
    """Cached gazetteer records, keyed on MRGID."""
    if not os.path.exists(GAZ_JSONL):
        return {}
    cache = {}
    with open(GAZ_JSONL, encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            rec = json.loads(line)
            mrgid = str(rec.get("MRGID") or rec.get("mrgid") or "")
            if mrgid:
                cache[mrgid] = rec
    return cache


def fetch_records(mrgids):
    """Ensure every MRGID has a cached gazetteer record. The only network phase.

    Kept separate from assignment so that changing how a locality is mapped to
    an ecoregion is a free recomputation rather than another pass over the
    gazetteer.
    """
    cache = load_cache()
    done = load_done()
    todo = [m for m in mrgids if m not in cache and m not in done]
    print(f"{len(cache)} records cached, {len(todo)} to fetch", file=sys.stderr)

    with open(GAZ_JSONL, "a", encoding="utf-8") as out, \
         open(XW_DONE, "a", encoding="utf-8") as donef:
        for i, mrgid in enumerate(todo):
            rec = gazetteer(mrgid)
            if rec:
                rec.setdefault("MRGID", mrgid)
                out.write(json.dumps(rec, ensure_ascii=False) + "\n")
                out.flush()
                cache[mrgid] = rec
            donef.write(mrgid + "\n")
            donef.flush()
            if i % 200 == 0:
                print(f"  fetch {i} / {len(todo)}", file=sys.stderr)
    return cache


def write_crosswalk(mrgids, cache, ecos):
    """Recompute the crosswalk from cached records. No network."""
    def tsv(v):
        return "" if v is None else str(v).replace("\t", " ").replace("\n", " ")

    rows = 0
    with open(XW_TSV, "w", encoding="utf-8") as out:
        out.write("mrgid\teco_code\tecoregion\tprovince\trealm\n")
        for mrgid in mrgids:
            rec = cache.get(mrgid)
            if not rec:
                continue
            for e in assign(rec, ecos):
                out.write("\t".join([mrgid, e["eco_code"], tsv(e["ecoregion"]),
                                     tsv(e["province"]), tsv(e["realm"])]) + "\n")
                rows += 1
    print(f"done: {XW_TSV} ({rows} rows)", file=sys.stderr)


def main():
    if len(sys.argv) < 2:
        sys.exit("usage: crosswalk_mrgid_meow.py <meow_ecos.geojson> [phase]")
    phase = sys.argv[2] if len(sys.argv) > 2 else "all"

    ecos = load_meow(sys.argv[1])
    print(f"loaded {len(ecos)} MEOW ecoregions", file=sys.stderr)

    mrgids = distinct_mrgids()
    print(f"{len(mrgids)} distinct MRGIDs", file=sys.stderr)

    cache = fetch_records(mrgids) if phase in ("all", "fetch") else load_cache()
    if phase in ("all", "assign"):
        write_crosswalk(mrgids, cache, ecos)


if __name__ == "__main__":
    main()
