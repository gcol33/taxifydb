#!/usr/bin/env python3
"""Assemble the MEOW ecoregion GeoJSON snapshot from the Marine Regions WFS.

The marine distribution asset (issue #21) needs the Marine Ecoregions of the
World geometry both to build meow.vtr (backend-meow.R) and to roll WoRMS MRGID
localities up to ecoregions (crosswalk_mrgid_meow.py). The canonical MEOW
download (Spalding et al. 2007) is a form-gated shapefile with no stable URL,
but Marine Regions serves the same data through its public WFS (CC BY):

  Ecoregions:ecoregions  232 ecoregions (eco_code, ecoregion, mrgid, centroid)

MEOW nests each ecoregion in exactly one province and each province in exactly
one realm. The WFS ecoregion layer carries neither, but it does carry the
ecoregion's Marine Regions gazetteer id, and the gazetteer publishes that
nesting as "part of" relations. So the hierarchy is read from the gazetteer
(ecoregion -> Marine Province -> Realm) rather than inferred from geometry:
232 ecoregion lookups plus one per distinct province, all cached.

Reading it is what keeps the hierarchy self-consistent. Deriving province and
realm independently by point-in-polygon on an ecoregion's centroid produces
pairs that cannot co-occur in MEOW (a Cold Temperate Northwest Pacific province
under a Central Indo-Pacific realm), silently mis-scopes any ecoregion whose
centroid falls outside its own polygon or across the antimeridian (the Aleutian
Islands land in the Atlantic), and there is no reason to guess a relation the
source states.

Writes a single meow_ecos.geojson with properties eco_code / ecoregion /
province / realm / mrgid. That frozen file is the `marine-snapshots-*` release
asset both consumers read.

Run:
    python3 build_meow_geojson.py meow_ecos.geojson

Stdlib only.
"""
import json, sys, time
import urllib.error, urllib.request

WFS = ("https://geo.vliz.be/geoserver/Ecoregions/wfs?service=WFS&version=2.0.0"
       "&request=GetFeature&typeName=Ecoregions:{layer}"
       "&outputFormat=application/json")
REST = "https://www.marineregions.org/rest"
UA = "taxifydb-meow-builder (https://github.com/gcol33/taxifydb)"

MIN_INTERVAL = 0.2
_last = [0.0]


def throttle():
    dt = time.time() - _last[0]
    if dt < MIN_INTERVAL:
        time.sleep(MIN_INTERVAL - dt)
    _last[0] = time.time()


def fetch(layer):
    req = urllib.request.Request(WFS.format(layer=layer),
                                 headers={"User-Agent": UA})
    with urllib.request.urlopen(req, timeout=120) as r:
        return json.load(r)


def get_json(url, tries=6):
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


def parent_of(mrgid, place_type):
    """The 'part of' parent of a gazetteer entry, by its place type.

    A few provinces are filed under the ecoregion place type rather than
    "Marine Province" (Sahul Shelf, parent of the four Arafura/Sahul
    ecoregions). MEOW gives an ecoregion exactly one parent, so a lone parent
    is that parent whatever it is labelled; the place type only has to break a
    tie when there are several.
    """
    res = get_json("%s/getGazetteerRelationsByMRGID.json/%s/?direction=upper"
                   "&type=partof" % (REST, mrgid))
    if not res:
        return None, None
    for r in res:
        if (r.get("placeType") or "").strip().lower() == place_type:
            return r.get("preferredGazetteerName"), r.get("MRGID")
    if len(res) == 1:
        return res[0].get("preferredGazetteerName"), res[0].get("MRGID")
    return None, None


def main():
    out_path = sys.argv[1] if len(sys.argv) > 1 else "meow_ecos.geojson"

    eco = fetch("ecoregions")
    n_eco = len(eco["features"])
    print("ecoregions=%d" % n_eco, file=sys.stderr)

    realm_of_province = {}
    features = []
    n_prov = n_realm = 0

    for i, f in enumerate(eco["features"]):
        p = f["properties"]
        mrgid = p.get("mrgid")
        province = realm = None
        if mrgid is not None:
            province, prov_mrgid = parent_of(mrgid, "marine province")
            if prov_mrgid is not None:
                if prov_mrgid not in realm_of_province:
                    realm_of_province[prov_mrgid] = parent_of(prov_mrgid, "realm")[0]
                realm = realm_of_province[prov_mrgid]
        n_prov += province is not None
        n_realm += realm is not None
        features.append({
            "type": "Feature",
            "properties": {
                "eco_code": p.get("eco_code"),
                "ecoregion": p.get("ecoregion"),
                "province": province,
                "realm": realm,
                "mrgid": mrgid,
            },
            "geometry": f["geometry"],
        })
        if i % 25 == 0:
            print("  %d / %d" % (i, n_eco), file=sys.stderr)

    if n_prov < n_eco or n_realm < n_eco:
        print("WARNING: %d of %d ecoregions have no province, %d no realm"
              % (n_eco - n_prov, n_eco, n_eco - n_realm), file=sys.stderr)

    fc = {"type": "FeatureCollection", "features": features}
    with open(out_path, "w", encoding="utf-8") as fh:
        json.dump(fc, fh)
    print("wrote %s: %d ecoregions, %d provinces, %d realms"
          % (out_path, len(features), len(set(
              f["properties"]["province"] for f in features)),
             len(set(f["properties"]["realm"] for f in features))),
          file=sys.stderr)


if __name__ == "__main__":
    main()
