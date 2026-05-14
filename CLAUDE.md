# taxify-backbones — Build Pipeline (`taxifydb` package)

## What This Is

R package `taxifydb`: the build pipeline for the `taxify` runtime. Downloads
raw source data, normalizes to the unified Darwin Core-like schema, and writes
the pre-compiled `.vtr` files that `taxify` consumes at runtime.

Two-repo split:

- `taxify` (runtime) — matching engine, S3 dispatch, enrichment join glue.
  Lean. No build deps. Does NOT need `taxifydb` to function with pre-built
  `.vtr` downloads.
- `taxifydb` (this repo) — every download/parse/normalize/index step. Has
  heavy build deps (curl, openxlsx2, RSQLite, rfishbase, ...). Only required
  when a user wants to build `.vtr` files themselves.

## Architecture

```
R/normalize.R              — normalize_backbone(), resolve_hierarchy()
R/precompute.R             — precompute_keys(), embed_accepted(),
                              precompute_backbone()
R/build_vtr.R              — build_vtr(), sha256()
R/build_enrichment_vtr.R   — build_enrichment_vtr()
R/diff.R                   — has_xdelta3(), create_delta(), apply_delta()
R/resolve_names.R          — resolve_enrichment_names() (cross-backbone)
R/build_name_lookup.R      — build_name_lookup(),
                              build_all_name_lookups()
R/check_versions.R         — upstream version check helpers
R/publish.R                — publish_release(), update_manifest(),
                              update_enrichment_manifest()

R/backend-<name>.R         — per-backend download / read / build_<name>()
R/build_backend.R          — build_backend(name, ...) dispatcher,
                              list_backends()

R/enrichment-helpers.R     — shared download helpers
R/enrichment-parsers*.R    — 24 parse_<name>() functions
R/enrichment-registry.R    — .enrichment_build_registry
R/build_enrichment.R       — build_enrichment(name, ...) dispatcher,
                              list_enrichments(),
                              enrichment_emergency_fallback()
```

## Backends

10 backends. All built via the same `build_backend(name)` entrypoint.

| Backend | Format | Notes |
|---------|--------|-------|
| wfo | Zenodo ZIP / classification.txt | WFO 2024-12 snapshot |
| col | DwC-A TSV | Catalogue of Life |
| gbif | simple.txt.gz | GBIF backbone, denormalized hierarchy |
| itis | SQLite | parent_tsn walk, needs RSQLite |
| ncbi | pipe-delimited .dmp | aggressive noise filter |
| ott | TSV (Open Tree) | NCBI+GBIF+WoRMS+IRMNG synthesis |
| worms | DwC-A (ChecklistBank) | marine taxa, denormalized |
| euromed | semicolon CSV | Euro+Med PlantBase 2020 snapshot |
| fungorum | (depends) | Index Fungorum |
| algaebase | ChecklistBank /nameusage/search | paginated API; /archive disabled (CC BY-NC) |

## Enrichments

24 enrichments registered in `.enrichment_build_registry`. Every enrichment
goes through cross-backbone name resolution before its `.vtr` is written:

1. `parse_<name>()` cleans the source to `canonical_name` + trait columns
2. `resolve_enrichment_names()` expands each name across the 7 backbones
3. `build_enrichment_vtr()` writes the indexed `.vtr` + `meta.json` sidecar

Group-based enrichments (GRIIS, WCVP, common_names) pass `group_cols` so
deduplication respects the grouping column.

## Building locally

```bash
# Install the package
Rscript -e "devtools::install_local('.', upgrade = 'never')"

# Build one backend
Rscript build_all.R itis output/itis

# Build all backends
Rscript build_all.R all output

# Build one enrichment
Rscript build_enrichments.R woodiness

# Build all enrichments
Rscript build_enrichments.R all

# Publish (after build)
Rscript build_all.R publish itis 2026.05
```

Direct package API (equivalent):

```r
library(taxifydb)
build_backend("itis", output_dir = "output/itis")
build_enrichment("woodiness", output_dir = "output/enrichment/woodiness")
update_manifest("manifest/manifest.json", "itis", "2026.05",
                "output/itis/itis.vtr")
```

## CI

Three workflows under `.github/workflows/`:

- `build-light.yml` — Ubuntu, the 4 light backends (ITIS, NCBI, OTT, WoRMS),
  twice a year + manual dispatch
- `build-heavy.yml` — Windows self-hosted, the 6 heavy backends (WFO, COL,
  GBIF, Euro+Med, Fungorum, AlgaeBase), same cadence
- `check-enrichment-versions.yml` — weekly cron, opens/updates a GitHub
  issue labeled `enrichment-outdated` when upstream versions advance

All workflows install `taxifydb` from the repo's source (`devtools::install_local(".")`)
and call the package API directly.

## Dependencies

- R >= 4.1
- Imports: vectra, curl, digest, jsonlite, utils
- Suggests: DBI, RSQLite (ITIS only); openxlsx2 (xlsx enrichments);
  rfishbase (fishbase only); taxify (cross-backbone name resolution)
- System: xdelta3 (for binary diffs); gh CLI (publishing)
