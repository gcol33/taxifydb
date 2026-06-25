# Shared build pipeline: enrichment data.frame -> .vtr.
#
# Simpler than backbone build: no hierarchy walk, no precompute keys.
# Just canonical_name sort, write .vtr, create indexes, write meta.json.

#' Write an enrichment .vtr file
#'
#' Sorts by `canonical_name`, writes the .vtr, creates hash indexes on
#' `canonical_name` (and optionally a group column), and writes a
#' `meta.json` sidecar.
#'
#' @param df A data.frame with at least a `canonical_name` column.
#' @param vtr_path Character. Output path for the .vtr file.
#' @param name Character. Enrichment identifier (e.g., "woodiness").
#' @param version Character. Version string (e.g., "2026.04").
#' @param source_url Character. URL the source data was downloaded from.
#' @param source_doi Character or NULL. DOI of the source dataset.
#' @param license Character. License string (e.g., "CC0", "CC BY 4.0").
#' @param attribution Character. Human-readable attribution string.
#' @param group_col Character or NULL. Column to index for group-based
#'   enrichments (e.g., "country_code", "tdwg_code", "lang").
#' @param batch_size Integer. Row group size for vectra (default 50000).
#' @return The path to the .vtr file (invisibly).
#' @export
build_enrichment_vtr <- function(df, vtr_path, name, version, source_url,
                                 source_doi = NULL, license = "unknown",
                                 attribution = NULL, group_col = NULL,
                                 batch_size = 50000L) {
  if (!"canonical_name" %in% names(df)) {
    stop("Enrichment data.frame must have a 'canonical_name' column.")
  }

  # Fold any aggregate marker spelling on the join key to one canonical form, so
  # the enrichment join lines up with aggregate accepted names from any backbone.
  df$canonical_name <- taxify::normalize_aggregate_name(df$canonical_name)

  df <- df[order(df$canonical_name, na.last = TRUE), ]
  rownames(df) <- NULL
  df <- df[!is.na(df$canonical_name), ]

  dir.create(dirname(vtr_path), recursive = TRUE, showWarnings = FALSE)
  vectra::write_vtr(df, vtr_path, batch_size = batch_size)

  vectra::create_index(vtr_path, "canonical_name")
  if (!is.null(group_col) && group_col %in% names(df)) {
    vectra::create_index(vtr_path, group_col)
  }

  available_groups <- NULL
  if (!is.null(group_col) && group_col %in% names(df)) {
    available_groups <- sort(unique(df[[group_col]]))
    available_groups <- available_groups[!is.na(available_groups)]
    message(sprintf(
      "[enrichment/%s] %d distinct %s values",
      name, length(available_groups), group_col
    ))
  }

  meta <- list(
    type             = "enrichment",
    name             = name,
    version          = version,
    source_url       = source_url,
    source_doi       = source_doi,
    license          = license,
    attribution      = attribution,
    group_col        = group_col,
    available_groups = available_groups,
    built            = format(Sys.Date(), "%Y-%m-%d"),
    nrow             = nrow(df),
    schema_version   = 2L
  )
  meta_path <- file.path(dirname(vtr_path), "meta.json")
  jsonlite::write_json(meta, meta_path, pretty = TRUE, auto_unbox = TRUE,
                       null = "null")

  message(sprintf(
    "[enrichment/%s] Built %s: %s rows, %.1f MB",
    name, basename(vtr_path), format(nrow(df), big.mark = ","),
    file.size(vtr_path) / 1048576
  ))

  invisible(vtr_path)
}
