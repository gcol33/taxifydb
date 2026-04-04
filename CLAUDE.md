# taxify-backbones — Build Pipeline

## What This Is

Build pipeline for taxify backbone .vtr files. **Separate repo from the taxify
R package.** The taxify package contains backend R code (S3 methods, matching
logic); this repo builds the pre-compiled .vtr files that taxify downloads
at runtime.

## Architecture

```
backends/<name>/download.R  — fetch raw data
backends/<name>/convert.R   — raw → normalized df → .vtr (CLI entrypoint)
backends/<name>/config.yml  — metadata + schema docs (optional)
shared/normalize.R          — unified DwC-like schema normalization
shared/precompute.R         — key_ci, key_normalized, embed_accepted()
shared/build.R              — sort + write_vtr + indexes + metadata
shared/build_enrichment.R   — enrichment df → .vtr + meta.json
shared/resolve_names.R      — cross-backbone name resolution for enrichments
shared/diff.R               — xdelta3 binary diff wrapper
shared/publish.R            — GitHub Release upload + manifest update
```

## Backends

| Backend | Format | Hierarchy Walk | Size | Notes |
|---------|--------|---------------|------|-------|
| itis | SQLite | Yes (parent_tsn) | ~212 MB | RSQLite required |
| ncbi | .dmp (pipe-delimited) | Yes (parent_id) | ~141 MB | Aggressive noise filtering |
| otl | TSV (pipe-delimited) | Yes (parent_uid) | ~106 MB | Synthetic taxonomy (NCBI+GBIF+WoRMS+IRMNG) |
| worms | DwC-A (TSV) | No (denormalized) | ~200 MB | Marine taxa, via GBIF ChecklistBank |

WFO, COL, GBIF backbones are built from the taxify package's own `taxify_download()` methods (not in this repo).

## Building

```bash
# Build one backend
Rscript build_all.R itis output/itis
Rscript build_all.R worms output/worms

# Build all
Rscript build_all.R all output

# Publish (after build)
Rscript build_all.R publish itis 2025.04
```

## Enrichments

12 enrichment datasets, each in `enrichment/<name>/convert.R`. Every enrichment
runs through **cross-backbone name resolution** before building the .vtr:

1. Source data is cleaned to `canonical_name` + trait columns
2. `resolve_enrichment_names()` runs each unique name through `taxify()` against
   all 7 backends (WFO, COL, GBIF, ITIS, NCBI, OTT, WoRMS) separately
3. The union of all accepted names is collected, expanding the data.frame
4. `build_enrichment_vtr()` writes the .vtr with an index on `canonical_name`

This ensures enrichment joins work regardless of which backbone produced the
user's `taxify()` result. Realistic size increase: ~1.1–1.5x (backends agree
on >90% of names).

Group-based enrichments (GRIIS, WCVP, common_names) pass `group_cols` to
`resolve_enrichment_names()` so deduplication respects the grouping column.

## Dependencies

- R 4.1+
- vectra (write_vtr, create_index)
- taxify (cross-backbone name resolution for enrichment builds)
- digest (SHA-256 checksums)
- jsonlite (manifest.json)
- RSQLite + DBI (ITIS only)
- xdelta3 CLI (optional, for binary diffs)
- gh CLI (publishing to GitHub Releases)
