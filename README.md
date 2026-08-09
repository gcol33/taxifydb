# taxifydb

![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)
![R >= 4.1](https://img.shields.io/badge/R-%3E%3D%204.1-blue)

Build pipeline for [**taxify**](https://github.com/gcol33/taxify). taxifydb
downloads raw taxonomic and trait data from official providers, normalizes it to
a unified Darwin Core-like schema, and writes the pre-compiled `.vtr` files that
the taxify runtime consumes. All network access, parsing, and schema
normalization live here, so taxify itself ships without heavy build
dependencies.

## When you need it

taxify works on its own. It downloads pre-built `.vtr` backbones and enrichments
from GitHub Releases through a manifest, so ordinary name matching never touches
taxifydb.

Install taxifydb when you want to:

* build a backbone or enrichment from source (offline, reproducible, or a newer
  snapshot than the published release);
* add a new backbone or trait dataset to the taxify ecosystem;
* maintain or republish the hosted `.vtr` data.

## Installation

```r
# pak resolves the taxify dependency (declared in Remotes) automatically
pak::pak("gcol33/taxifydb")

# or
remotes::install_github("gcol33/taxifydb")
```

taxifydb is a build companion distributed from GitHub; it is not on CRAN.

## What it builds

### Backbones

Nineteen taxonomic backbones, each built through one entry point,
`build_backend(name)`, and normalized to the same schema (`canonical_name`,
`taxon_id`, `taxonomic_status`, resolved `family` / `genus`, accepted-name
links).

| Backbone | Scope |
|---|---|
| WFO | World Flora Online (plants) |
| COL | Catalogue of Life (all kingdoms) |
| COL XR | Catalogue of Life Extended Release (all kingdoms) |
| GBIF | GBIF backbone (all kingdoms) |
| ITIS | Integrated Taxonomic Information System |
| NCBI | NCBI Taxonomy |
| OTT | Open Tree of Life synthesis |
| WoRMS | World Register of Marine Species |
| Euro+Med | Euro+Med PlantBase (European/Mediterranean plants) |
| Index Fungorum | Fungi |
| AlgaeBase | Algae |
| FishBase | Fishes |
| SeaLifeBase | Non-fish aquatic species |
| Reptile Database | Reptiles |
| WCVP | World Checklist of Vascular Plants (Kew) |
| LCVP | Leipzig Catalogue of Vascular Plants |
| MDD | Mammal Diversity Database |
| AviList | Birds |
| LPSN | Prokaryotes (Bacteria/Archaea) |

### Reference geometry

Boundary polygons taxify reads for its `region=` / `coords=` constraint. Built,
versioned and published like a backbone, but they carry polygon vertices rather
than names, so they are listed by `list_geometry()` rather than
`list_backends()` and take no part in enrichment name resolution.

| Artifact | Scope |
|---|---|
| WGSRPD | TDWG Level 3 botanical regions |
| MEOW | Marine Ecoregions of the World |

### Enrichments

Ninety-plus trait, ecology, conservation, and invasion datasets, each parsed to
per-species (or per-genus) rows and resolved to accepted names so they join any
backbone's output. Run `list_enrichments()` for the full, current set.

## Usage

```r
library(taxifydb)

list_backends()                    # the nineteen backbones
list_geometry()                    # the reference-geometry artifacts
list_enrichments()                 # every registered enrichment

# Build a backbone .vtr from source
build_backend("gbif")              # or the direct builder, build_gbif()

# Build an enrichment .vtr from source
build_enrichment("clopla")

# Cross-backbone name-lookup tables, used to resolve enrichment names
build_all_name_lookups()
```

Each builder returns the path to the `.vtr` file it wrote. taxify picks these up
from its data directory, and falls back to building through them when a
pre-built download is unavailable.

## How it fits with taxify

```
providers  --download-->  taxifydb   --.vtr-->  GitHub Releases
(WFO, GBIF,               parse,                 hosted assets +
 GRIIS, GIFT,             normalize,             manifest.json
 ...)                     index                        |
                                                       | download
                                                       v
                                                 taxify (runtime)
                                                 match / enrich, offline
```

* **taxify** (runtime): matching engine, S3 dispatch, enrichment joins. No build
  dependencies; runs fully against pre-built `.vtr` downloads.
* **taxifydb** (this package): every download, parse, normalize, and index step.
  Needed only to build `.vtr` files yourself.

When taxify has to build from source, it delegates to `taxifydb::build_<name>()`
/ `taxifydb::build_enrichment()`. Without taxifydb installed, taxify still runs
on the hosted downloads.

## Data hosting

Built `.vtr` files are published as GitHub Release assets: one tag per backbone
(e.g. `gbif-2026.06`), and a single rolling `enrichment-<version>` tag for all
enrichments. `manifest/manifest.json` records each asset's version, URL, and
content id, and taxify reads it to resolve downloads. `publish_release()`,
`publish_enrichment_release()`, `update_manifest()`, and
`update_enrichment_manifest()` keep the build-side and runtime manifests in
step.

## Requirements

* R >= 4.1
* Imports: curl, digest, jsonlite, vectra, taxify
* Some backbones and enrichments need an optional package at build time (RSQLite
  for ITIS, openxlsx2 for XLSX sources, rfishbase for FishBase / SeaLifeBase,
  GIFT, and others). A builder that needs one errors with an install message.
* A few sources sit behind Cloudflare or publisher supplement walls and are
  fetched with a bundled Python helper (`curl_cffi`). taxifydb discovers a
  suitable interpreter automatically (pyenv versions, the Windows `py` launcher,
  then PATH); set `TAXIFYDB_PYTHON` to force a specific one.

## Adding a backbone or enrichment

A backbone needs a `build_<name>()` in `R/backend-<name>.R`, registered in
`build_backend()`. An enrichment needs a `parse_<name>()` and a registry entry
in `.enrichment_build_registry`. `CLAUDE.md` documents the full checklist, the
schema contract, and the cross-backbone name-resolution step.

## License

MIT for this package's code. Each source dataset keeps its own license.
taxifydb redistributes only openly-licensed derived data through taxify's
releases; citation-only or unlicensed sources are built locally and never
redistributed. Cite the original dataset when you use its data.
