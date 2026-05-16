# Build-pipeline divergence — taxify-backbones (2026-05-16)

## Status: resolved in code (2026-05-16) — pending republish

Path **B** chosen: keep `normalize_backbone()` in the build, fix the
runtime to read the unified snake_case schema, republish WFO + COL +
GBIF. Code changes landed across taxify-backbones and taxify; deployed
Zenodo `.vtr` files still reflect the old (source-native) schema and
must be rebuilt + republished before the new taxify HEAD will work
against them.

See **Republish runbook** at the bottom of this file for the commands.

## Observation

The published WFO `.vtr` (Zenodo 14538251, downloaded 2026-05-02, 749 MB,
1,638,552 rows) has **camelCase main column names**:

```
taxonID, scientificName, taxonRank, taxonomicStatus, acceptedNameUsageID,
scientificNameAuthorship, infraspecificEpithet, scientificNameID,
parentNameUsageID, namePublishedIn, nomenclaturalStatus, taxonRemarks,
subfamily, tribe, subtribe, subgenus
```

plus the precomputed columns from `taxify::embed_accepted()` (which are
hardcoded snake_case): `accepted_name`, `accepted_family`,
`accepted_genus`, `accepted_taxon_id`, `is_synonym`.

But the current `R/backend-wfo.R::read_wfo()` calls
`normalize_backbone(df, col_map, extra_cols)` where `col_map` maps:

```r
list(
  taxon_id               = "taxonID",
  canonical_name         = "scientificName",
  taxon_rank             = "taxonRank",
  taxonomic_status       = "taxonomicStatus",
  accepted_name_usage_id = "acceptedNameUsageID",
  ...
)
```

`normalize_backbone()` returns a data.frame whose column **names** are
the LHS keys of that map (`taxon_id`, `canonical_name`,
`taxonomic_status`, ...) — i.e. snake_case unified-schema names. If we
run `build_wfo()` today, it produces a `.vtr` with `taxon_id` /
`canonical_name`, not `taxonID` / `scientificName`.

This is incompatible with `taxify::wfo_backend()`'s `col_map`, which
expects the camelCase names that the published `.vtr` has:

```r
.wfo_col_map <- list(
  name   = "scientificName",
  status = "taxonomicStatus",
  id     = "taxonID",
  ...
)
```

So either:

1. The published `.vtr` was built with an **older** `read_wfo` that
   skipped `normalize_backbone()`, before that helper landed; or
2. The `normalize_backbone()` call was added to `read_wfo()` but the
   downstream side (`build_vtr`, `taxify`'s col_map) was never updated.

Running `build_wfo()` today will produce a `.vtr` that:

- Has the **right data** (including `nomenclaturalStatus`), and
- Has the **wrong column names** for taxify's runtime to recognise.

## Reproduction

```r
devtools::load_all("C:/GillesC/Documents/dev/taxify-backbones")
df <- read_wfo(
  "C:/GillesC/Documents/dev/eunisesy/data-raw/crosswalk/WFO_Backbone/classification.csv",
  verbose = FALSE
)
names(df)  # snake_case unified-schema, not camelCase
```

vs.

```r
# What the deployed .vtr has:
names(vectra::tbl(file.path(
  taxify_data_dir(), "wfo.vtr"
)) |> vectra::slice_head(n = 1L) |> vectra::collect())
# camelCase + snake_case precomputed
```

## Recommended fix

One of:

- **A. Match the published artefact**: drop the `normalize_backbone()`
  call in `read_wfo()` (and other `read_*()` functions) so build output
  uses camelCase, and update `taxify-backbones` README + helper API to
  match.
- **B. Match the runtime expectations**: keep `normalize_backbone()` in
  the build, and update `taxify::*_backend()` `col_map` entries to use
  the unified-schema column names. Republish all backbones (WFO, COL,
  GBIF, ITIS, NCBI, OTT, WORMS, Fungorum, Algaebase, Euro+Med). Bigger
  change but cleaner long-term: every backend ends up with the same
  schema.

For now: **don't run `build_wfo()` against a deployed taxify
installation** — it will produce a `.vtr` that taxify won't read
correctly. The published Zenodo artefact (and the runtime's downloader)
work fine.

## Severity

Low (no current user impact — Zenodo download path works fine), but
this is a **silent reproducibility hole**: a downstream user who tries
to build their own WFO backbone from this repo will produce a broken
`.vtr` with no error, just nothing matching at query time.

## Action items

- [x] Decide A vs B for the standardised schema → **B**.
- [x] Update taxify col_maps for WFO/COL/GBIF to unified snake_case.
- [x] Update `add_wfo_info()` / `add_col_info()` / `add_gbif_info()` to
      join on `taxon_id` and read `infraspecific_epithet` from main
      schema (user-facing output column names left unchanged for
      backwards compatibility of the enrichment API).
- [x] Update `register.R::extract_{wfo,col,gbif}_genera()` and
      `resolve_kingdom_via_gbif()` to use unified-schema names.
- [x] Preserve `parent_key` in the GBIF build (added to
      `.gbif_extra_cols`) — needed by `resolve_kingdom_via_gbif()`.
- [x] Update mock backbones in `tests/testthat/helper-mock-*` to use
      unified schema so the runtime test suite exercises the new
      contract.
- [x] Add build-side regression test
      `tests/testthat/test-unified-schema.R` asserting `read_wfo()`,
      `read_col()`, `read_gbif()` each emit the unified-schema columns.
- [ ] Run `build_wfo()`, `build_col()`, `build_gbif()` and publish new
      releases — see runbook below.
- [ ] After republish: update `manifest/manifest.json` so taxify's
      runtime downloader picks up the new release.

## Republish runbook

The deployed Zenodo `.vtr` for WFO/COL/GBIF still has the old
source-native column names. Until they are rebuilt with the unified
schema and republished, taxify HEAD will fail to query them.

Heavy step (network + disk + hours). Run from
`C:/Users/Gilles Colling/Documents/dev/taxify-backbones`.

```r
# 1. Build the three divergent backbones locally.
#    GBIF is ~1.5 GB download, COL ~600 MB, WFO ~120 MB.
#    Use a fresh R session for each to keep memory pressure down.
library(taxifydb)
build_wfo (output_dir = "output/wfo")    # ~10 min
build_col (output_dir = "output/col")    # ~15 min
build_gbif(output_dir = "output/gbif")   # ~40 min on RTX-5080 box

# 2. Sanity-check schema before publishing — same shape as the 7 already-
#    unified backbones.
required <- c("taxon_id", "canonical_name", "taxon_rank",
              "taxonomic_status", "accepted_name_usage_id",
              "family", "genus", "specific_epithet",
              "authorship", "infraspecific_epithet")
for (bb in c("wfo", "col", "gbif")) {
  cols <- names(vectra::tbl(file.path("output", bb, paste0(bb, ".vtr"))) |>
                  utils::head(1L) |> vectra::collect())
  missing <- setdiff(required, cols)
  if (length(missing) > 0L) stop(bb, " missing: ",
                                 paste(missing, collapse = ", "))
}

# 3. Publish to GitHub releases + update manifest. Pick a version tag —
#    suggestion: append a build-revision suffix to the upstream source
#    tag, since the source data is unchanged but the build schema is.
ver_wfo  <- "2024-12-r1"
ver_col  <- "2025-r1"
ver_gbif <- "2026.05"   # GBIF has no source-side version; use build date

publish_release("wfo",  ver_wfo,  "output/wfo/wfo.vtr")
publish_release("col",  ver_col,  "output/col/col.vtr")
publish_release("gbif", ver_gbif, "output/gbif/gbif.vtr")

update_manifest("manifest/manifest.json", "wfo",  ver_wfo,
                "output/wfo/wfo.vtr",
                source_url = "https://zenodo.org/records/14538251/files/_DwC_backbone_R.zip")
update_manifest("manifest/manifest.json", "col",  ver_col,
                "output/col/col.vtr",
                source_url = "https://download.checklistbank.org/col/annual/2025_dwca.zip")
update_manifest("manifest/manifest.json", "gbif", ver_gbif,
                "output/gbif/gbif.vtr",
                source_url = "https://hosted-datasets.gbif.org/datasets/backbone/current/simple.txt.gz")
```

After publish, bump `.wfo_version` / `.col_version` / `.gbif_version` in
the taxify repo to match (`R/backend-wfo.R` line 7 etc.) so a fresh
taxify install resolves the new release.
