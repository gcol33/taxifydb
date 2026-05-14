#' taxifydb: Build Backbones and Enrichments for the taxify Package
#'
#' Build pipeline for the [taxify](https://github.com/gcol33/taxify) package.
#' Downloads raw source data from official providers (WFO, COL, GBIF, ITIS,
#' NCBI Taxonomy, Open Tree of Life, WoRMS, Euro+Med PlantBase, Index Fungorum,
#' AlgaeBase) and a wide set of trait and conservation datasets, normalizes
#' to a unified Darwin Core-like schema, and writes pre-compiled `.vtr`
#' files that the taxify runtime consumes.
#'
#' Separates build-time concerns (network access, parsing, schema
#' normalization) from runtime concerns so that taxify itself stays lean.
#'
#' @section Backbone build pipeline:
#' Each backend has a `build_<name>()` function that:
#' \enumerate{
#'   \item Downloads the raw source archive.
#'   \item Parses it into a data.frame.
#'   \item Calls [normalize_backbone()] to map to the unified schema.
#'   \item Calls [precompute_backbone()] to add matching keys and embed
#'     synonym resolution.
#'   \item Calls [build_vtr()] to write the final `.vtr` with indexes and
#'     metadata sidecar.
#' }
#'
#' @section Enrichment build pipeline:
#' Each enrichment is registered with a download URL, parse function, and
#' optional group column. The build pipeline runs:
#' \enumerate{
#'   \item Download + parse to a data.frame with `canonical_name` + trait
#'     columns.
#'   \item Pass through [resolve_enrichment_names()] to expand names across
#'     all 7 backbones.
#'   \item Call [build_enrichment_vtr()] to write the indexed `.vtr` plus
#'     `meta.json` sidecar.
#' }
#'
#' @keywords internal
"_PACKAGE"
