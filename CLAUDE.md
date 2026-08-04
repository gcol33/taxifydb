# taxifydb — Build Pipeline (`taxifydb` package)

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
R/backend-wgsrpd.R         — reference geometry (not a backbone): WGSRPD Level 3
                              botanical regions -> wgsrpd.vtr (plant range polygons)
R/backend-meow.R           — reference geometry (not a backbone): MEOW marine
                              ecoregions -> meow.vtr (marine range polygons, #21)
R/register.R               — genus extractors, kingdom normalization,
                              resolve_kingdom_via_gbif(), build_genus_register(),
                              build_backend_coverage(), build_register() (#23)
R/life-form.R              — family -> taxon_group / kingdom_group lookup table,
                              assign_life_form()

R/enrichment-helpers.R     — shared download helpers
R/enrichment-parsers*.R    — 24 parse_<name>() functions
R/enrichment-registry.R    — .enrichment_build_registry
R/build_enrichment.R       — build_enrichment(name, ...) dispatcher,
                              list_enrichments(),
                              enrichment_emergency_fallback()
```

## Backends

15 backends. All built via the same `build_backend(name)` entrypoint.

| Backend | Format | Notes |
|---------|--------|-------|
| wfo | Zenodo ZIP / classification.txt | WFO 2024-12 snapshot |
| col | DwC-A TSV | Catalogue of Life |
| gbif | simple.txt.gz | GBIF backbone, denormalized hierarchy |
| itis | SQLite | parent_tsn walk, needs RSQLite |
| ncbi | pipe-delimited .dmp | aggressive noise filter |
| ott | TSV (Open Tree) | NCBI+GBIF+WoRMS+IRMNG synthesis |
| worms | DwC-A (ChecklistBank) | marine taxa, denormalized; ChecklistBank double-quotes TSV fields, so `read_worms` reads with `read.delim(quote = "\"")` not `quote = ""` (see WoRMS quote note below) |
| euromed | CDM REST snapshot (NDJSON) | Euro+Med PlantBase, harvested live from the EDIT CyberTaxonomy CDM API (`api.cybertaxonomy.org/euromed`) by `inst/py/crawlers/crawl_euromed.py` into a frozen `euromed.jsonl` + `nodes.tsv` snapshot (accepted taxa with nested synonyms; genus->family from node `treeIndex`). Replaces the frozen 2020 v1.2 flat file that could not refresh (#7). No working bulk export exists (the CDM `/dwca` and `/checklist/export` endpoints time out through the public proxy), so the per-taxon portal API is crawled |
| fungorum | (depends) | Index Fungorum |
| algaebase | ChecklistBank /nameusage/search | paginated API; /archive disabled (CC BY-NC) |
| fishbase | rfishbase `load_taxa()` + `synonyms()` | fishes; shared reader `.read_rfishbase_backbone()`; needs rfishbase |
| sealifebase | rfishbase (server = sealifebase) | non-fish aquatic; same shared reader |
| reptiledb | taxa.csv + synonyms.xlsx + checklist.xlsx | reptiles; CC-BY; synonyms from 2023-04 snapshot, order via family->order map; needs openxlsx2 |
| wcvp | pipe-delimited `wcvp_names.csv` (Kew) | vascular plants; CC BY; `taxon_name` is the rendered canonical (hybrids + infraspecific markers); acceptance derived from `accepted_plant_name_id` (self=accepted, other=synonym, empty=unplaced), not the nine `taxon_status` spellings. Kew does NOT field-wrap, and 0.06% of names carry genuine embedded `"` (e.g. `f. "A"`), so `read_wcvp` reads with `quote = ""` (opposite of WoRMS); optional data.table fread |
| lcvp | `tab_lcvp.rda` (idiv-biodiversity/LCVP) | vascular plants; MIT; loaded via base `load()` (no LCVP pkg dep); canonical assembled from `Input.Genus`/`Input.Epitheton`/`Rank`/`Input.Subspecies.Epitheton` (`nil` = species; `forma`->`f.`); synonym->accepted via `globalId.of.Output.Taxon`; `unresolved` kept as own accepted concept; no hybrids in input columns |

**WoRMS quote note (#2, fixed in `worms-2026.07`).** ChecklistBank double-quotes its `Taxon.tsv` / `SpeciesProfile.tsv` string fields. `read_worms` (and the SpeciesProfile reader) must use `read.delim(quote = "\"")`, NOT `quote = ""` — with `quote = ""` the field-wrapping quotes are kept as literal characters, so 90.2% of `canonical_name` / `key_ci` / `key_normalized` / `authorship` came out wrapped in `"` (e.g. `"Aglaophamus malmgreni"`) and some quoted-field rows were mis-split (bibliographic text leaking into `taxon_id`). Runtime effect: exact match fails on the quotes, falls to fuzzy, and the quoted `accepted_name` then breaks every marine enrichment join. Using `quote = "\""` parses the double-quotes as quotes and strips them; only `"` is a quote (not `'`), so apostrophes in authorship (`d'Orbigny`, `O'Brien`) stay intact. After the fix: `canonical_name` quotes 1,406,915 -> 11 (the 11 genuine embedded quotes, e.g. `Gyrodactylus barbatuli f. "A"`), rows 1,559,455 -> 1,557,860. This was WoRMS-only: every other backbone and all enrichments have <=0.03% quoted, all genuine embedded quotes in informal/provisional names, so none needs the change. When rebuilding another delimited backbone/enrichment whose source wraps fields, prefer `quote = "\""` over `quote = ""`.

## Enrichments

92 enrichments registered in `.enrichment_build_registry` (includes `fishbase`,
`sealifebase`, `groot`, and `marine_distribution`). Ecoflora and FloraWeb are built into `.vtr` files
from frozen per-species scrape snapshots. Only 1 on-demand source remains
(Pignatti, copyrighted), catalogued in `.enrichment_scrape_only`
(`list_scrape_only_enrichments()`) and accessed by taxify's `add_pignatti()` via
the TR8 package. Licence-blocked candidate sources whose live terms permit
citation/scientific use only, not third-party redistribution, are recorded in
`.enrichment_build_only` (`list_build_only_enrichments()`) with the verified
licence, so taxifydb builds no `.vtr`, writes no manifest entry, and adds
nothing to the cross-source trait registry for them: freshwaterecology.info
(#33; non-commercial, registration-gated, (c) BOKU), NEMAPLEX (#31; UC Davis,
no redistribution grant) and BETSI (#31; CNRS, citation-only, by-request). A
source leaves that catalog for `.enrichment_build_registry` only if its live
licence is relicensed to permit redistribution. Every built enrichment goes
through cross-backbone name resolution before its `.vtr` is written:

1. `parse_<name>()` cleans the source to `canonical_name` + trait columns
2. `resolve_enrichment_names()` expands each name across the 7 backbones
3. `build_enrichment_vtr()` writes the indexed `.vtr` + `meta.json` sidecar

Group-based enrichments (GRIIS, WCVP, common_names, marine_distribution) pass
`group_cols` so deduplication respects the grouping column.

`marine_distribution` is the marine analogue of the WCVP range table (#21): a
`canonical_name` + `region_code` + `native_status` asset so taxify's
`region=`/`coords=` filter can constrain animal/marine matches, not just plants.
WoRMS distributions are not in the ChecklistBank export, so they are harvested
per taxon from the WoRMS REST API (`inst/py/crawlers/crawl_worms_distributions.py`,
a multi-day throttled crawl like euromed) keyed on Marine Regions localities
(MRGID). `inst/py/crawlers/crosswalk_mrgid_meow.py` rolls each MRGID up to the
MEOW ecoregion(s) it falls in (point-in-polygon against the frozen MEOW
GeoJSON), and `parse_marine_distribution()` joins the two frozen snapshots. Its
`region_code` is the MEOW ECO_CODE, the same key `backend-meow.R`'s `meow.vtr`
geometry is indexed on, so the runtime coords->region path is a drop-in beside
`wgsrpd.vtr`. Both the WoRMS snapshot and the MEOW GeoJSON are frozen as
`marine-snapshots-*` release assets (the WoRMS full copy is request-gated and
the MEOW download is a form-gated shapefile with no stable URL).

`gidias` carries two grains in one `.vtr`, keyed on `affected_taxon`: `"Any"`
(every record for the species) plus one row per affected native taxon (`Plant`,
`Invertebrate`, `Vertebrate`, `Microbe`, `Fungi`). The `"Any"` row is not the
union of the others and cannot be dropped — it is the only one carrying SEICAT
and the negative records with no affected taxon recorded, so a door reading
gidias without a group must select `affected_taxon == "Any"`.

## Genus Register

`genus_register.vtr` (one row per genus: classification + `kingdom_group` /
`taxon_group` / `life_form`) and `backend_coverage.vtr` (long format, one row
per genus x backend) are the cross-backbone index `taxify()`'s
`kingdom_group`/`taxon_group`/`life_form` output and `inspect()` read (#23).
Both are built here, not on the taxify runtime side, so every user downloads
the same published register regardless of which backbones they have
installed locally — taxify's own `build_genus_register()` used to assemble it
from whatever the caller happened to have on disk, so two users running
identical code could get different `kingdom_group`/`taxon_group` output.

`build_register()` (or `build_genus_register()` / `build_backend_coverage()`
individually) unions the fixed 13-backbone set `register_backbones()` returns
(every backbone except Fungorum and AlgaeBase) via the extractor registry in
`R/register.R`. Each backbone's `.vtr` resolves, in order: an explicit
`backbone_paths` override, a local `output/<name>/<name>.vtr` build, or the
version published in `manifest/manifest.json` (downloaded into
`<output_dir>/_cache/`). Classification conflicts resolve by backbone
priority (WoRMS > COL > WCVP > Reptile DB > GBIF > Euro+Med > LCVP > ITIS >
NCBI > OTT > WFO > FishBase > SeaLifeBase); `kingdom_group`/`taxon_group`/
`life_form` come from `R/life-form.R`'s family lookup table, with a GBIF
parent-key hierarchy walk (`resolve_kingdom_via_gbif()`) as a second pass for
genera the family table cannot place. Each `.vtr` publishes under its own
release tag like a backbone (`genus_register-<version>`,
`backend_coverage-<version>`), and both are recorded in `manifest.json`'s
`backends` block (alongside `wgsrpd`/`meow`, the other non-taxonomic
reference assets built there).

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
