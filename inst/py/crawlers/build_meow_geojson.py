#!/usr/bin/env python3
"""Assemble the MEOW ecoregion GeoJSON snapshot from the Marine Regions WFS.

The marine distribution asset (issue #21) needs the Marine Ecoregions of the
World geometry both to build meow.vtr (backend-meow.R) and to roll WoRMS MRGID
localities up to ecoregions (crosswalk_mrgid_meow.py). The canonical MEOW
download (Spalding et al. 2007) is a form-gated shapefile with no stable URL,
but Marine Regions serves the same data through its public WFS (CC BY):

  Ecoregions:ecoregions  232 ecoregions (eco_code, ecoregion, centroid, geom)
  Ecoregions:provinces    62 provinces  (province, geom)
  Ecoregions:realm        12 realms     (realm, geom)

The ecoregions layer carries eco_code and name but not its province/realm, so
this script stamps each ecoregion with the province and realm its centroid
falls in (point-in-polygon; nearest-centroid fallback for a centroid that lands
just outside its own polygon at a complex coastline) and writes a single
meow_ecos.geojson with properties eco_code / ecoregion / province / realm. That
frozen file is the `marine-snapshots-*` release asset both consumers read.

Run:
    python3 build_meow_geojson.py meow_ecos.geojson

Stdlib only.
"""
import json, math, sys, urllib.request

WFS = ("https://geo.vliz.be/geoserver/Ecoregions/wfs?service=WFS&version=2.0.0"
       "&request=GetFeature&typeName=Ecoregions:{layer}"
       "&outputFormat=application/json")
UA = "taxifydb-meow-builder (https://github.com/gcol33/taxifydb)"


def fetch(layer):
    req = urllib.request.Request(WFS.format(layer=layer),
                                 headers={"User-Agent": UA})
    with urllib.request.urlopen(req, timeout=120) as r:
        return json.load(r)


def polys_of(feature):
    g = feature["geometry"]
    if g["type"] == "Polygon":
        return [g["coordinates"]]
    if g["type"] == "MultiPolygon":
        return g["coordinates"]
    return []


def bbox_of(polys):
    xs, ys = [], []
    for poly in polys:
        for x, y in poly[0]:
            xs.append(x); ys.append(y)
    return min(xs), min(ys), max(xs), max(ys)


def point_in_ring(x, y, ring):
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


def point_in_feature(x, y, feat):
    bx0, by0, bx1, by1 = feat["bbox"]
    if x < bx0 or x > bx1 or y < by0 or y > by1:
        return False
    for poly in feat["polys"]:
        if point_in_ring(x, y, poly[0]):
            if not any(point_in_ring(x, y, hole) for hole in poly[1:]):
                return True
    return False


def prep(fc, name_key):
    out = []
    for f in fc["features"]:
        polys = polys_of(f)
        if not polys:
            continue
        p = f["properties"]
        out.append({
            "name": p.get(name_key),
            "cx": p.get("long", p.get("lon")),
            "cy": p.get("lat"),
            "polys": polys,
            "bbox": bbox_of(polys),
        })
    return out


def assign(cx, cy, feats):
    for ft in feats:
        if point_in_feature(cx, cy, ft):
            return ft["name"]
    # Fallback: nearest feature centroid (a coastline centroid can land just
    # outside its own polygon). MEOW nests cleanly, so nearest is unambiguous.
    best, bestd = None, None
    for ft in feats:
        if ft["cx"] is None or ft["cy"] is None:
            continue
        d = (cx - ft["cx"]) ** 2 + (cy - ft["cy"]) ** 2
        if bestd is None or d < bestd:
            best, bestd = ft["name"], d
    return best


def main():
    out_path = sys.argv[1] if len(sys.argv) > 1 else "meow_ecos.geojson"

    eco = fetch("ecoregions")
    provinces = prep(fetch("provinces"), "province")
    realms = prep(fetch("realm"), "realm")
    print(f"ecoregions={len(eco['features'])} provinces={len(provinces)} "
          f"realms={len(realms)}", file=sys.stderr)

    features = []
    n_pip_prov = n_pip_realm = 0
    for f in eco["features"]:
        p = f["properties"]
        cx, cy = p.get("long"), p.get("lat")
        province = assign(cx, cy, provinces) if cx is not None else None
        realm = assign(cx, cy, realms) if cx is not None else None
        n_pip_prov += province is not None
        n_pip_realm += realm is not None
        features.append({
            "type": "Feature",
            "properties": {
                "eco_code": p.get("eco_code"),
                "ecoregion": p.get("ecoregion"),
                "province": province,
                "realm": realm,
                "mrgid": p.get("mrgid"),
            },
            "geometry": f["geometry"],
        })

    fc = {"type": "FeatureCollection", "features": features}
    with open(out_path, "w", encoding="utf-8") as fh:
        json.dump(fc, fh)
    print(f"wrote {out_path}: {len(features)} ecoregions "
          f"({n_pip_prov} with province, {n_pip_realm} with realm)",
          file=sys.stderr)


if __name__ == "__main__":
    main()
