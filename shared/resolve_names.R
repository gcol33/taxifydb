# ---- Cross-backbone name resolution for enrichments ----
#
# Every enrichment .vtr must be joinable regardless of which backbone produced
# the user's taxify() result. This function resolves source names against all
# 7 backends and expands the data.frame so each source row maps to every
# distinct accepted name found across backends.
#
# Requires: taxify package installed with all 7 backbone .vtr files available.

#' Resolve enrichment names against all taxify backends
#'
#' Takes an enrichment data.frame with `canonical_name` + trait columns and
#' expands it so that each source name maps to all unique `accepted_name`
#' values across all 7 backends (WFO, COL, GBIF, ITIS, NCBI, OTT, WoRMS).
#'
#' @param df A data.frame with at least a `canonical_name` column.
#' @param group_cols Character vector of grouping columns (e.g.,
#'   `"country_code"`, `"tdwg_code"`, `"lang"`). Deduplication uses
#'   `canonical_name` + `group_cols` as the key. Default `NULL`.
#' @param backends Character vector of backend names. Default: all 7.
#' @param verbose Logical.
#' @return The expanded data.frame with `canonical_name` replaced by resolved
#'   accepted names. Rows are deduplicated by `canonical_name` (+ `group_cols`).
resolve_enrichment_names <- function(df,
                                     group_cols = NULL,
                                     backends = c("wfo", "col", "gbif",
                                                  "itis", "ncbi", "ott",
                                                  "worms"),
                                     verbose = TRUE) {
  if (!"canonical_name" %in% names(df)) {
    stop("df must have a 'canonical_name' column")
  }

  if (!requireNamespace("taxify", quietly = TRUE)) {
    stop("taxify package required for cross-backbone name resolution. ",
         "Install with: remotes::install_github('gcol33/taxify')")
  }

  unique_names <- unique(df$canonical_name)
  n_source <- length(unique_names)

  if (verbose) {
    message(sprintf(
      "  Resolving %s source names against %d backends...",
      format(n_source, big.mark = ","), length(backends)
    ))
  }

  # Run each backend separately — taxify() with multiple backends is a

  # fallback chain (first match wins), but we need ALL accepted names
  all_mappings <- vector("list", length(backends))

  for (i in seq_along(backends)) {
    b <- backends[i]
    if (verbose) message(sprintf("    [%d/%d] %s...", i, length(backends), b))

    res <- tryCatch(
      taxify::taxify(unique_names, backend = b, verbose = FALSE),
      error = function(e) {
        warning(sprintf("Backend '%s' failed: %s", b, conditionMessage(e)),
                call. = FALSE)
        NULL
      }
    )

    if (!is.null(res)) {
      matched <- res[!is.na(res$accepted_name),
                     c("input_name", "accepted_name"), drop = FALSE]
      if (nrow(matched) > 0L) {
        all_mappings[[i]] <- unique(matched)
      }
    }
  }

  mapping <- do.call(rbind, all_mappings)

  if (is.null(mapping) || nrow(mapping) == 0L) {
    warning("No names resolved against any backend. Returning original df.")
    return(df)
  }

  mapping <- unique(mapping)

  # Also keep original names as self-mapping for unresolved species
  unresolved <- setdiff(unique_names, mapping$input_name)
  if (length(unresolved) > 0L) {
    self_map <- data.frame(
      input_name    = unresolved,
      accepted_name = unresolved,
      stringsAsFactors = FALSE
    )
    mapping <- rbind(mapping, self_map)
  }

  n_accepted <- length(unique(mapping$accepted_name))
  ratio <- n_accepted / n_source

  if (verbose) {
    message(sprintf(
      "  %s source names -> %s unique accepted names (%.2fx)",
      format(n_source, big.mark = ","),
      format(n_accepted, big.mark = ","),
      ratio
    ))
  }

  # Expand df: join on canonical_name == input_name
  # This creates one row per (source_row × accepted_name)
  expanded <- merge(
    df, mapping,
    by.x = "canonical_name", by.y = "input_name",
    all.x = TRUE
  )

  # For rows that matched: replace canonical_name with accepted_name
  # For rows with no mapping (shouldn't happen due to self-map): keep original
  has_resolved <- !is.na(expanded$accepted_name)
  expanded$canonical_name[has_resolved] <- expanded$accepted_name[has_resolved]
  expanded$accepted_name <- NULL

  # Deduplicate by canonical_name (+ group_cols)
  if (!is.null(group_cols) && length(group_cols) > 0L) {
    dedup_key <- do.call(
      paste,
      c(expanded[c("canonical_name", group_cols)], list(sep = "\x1f"))
    )
  } else {
    dedup_key <- expanded$canonical_name
  }
  expanded <- expanded[!duplicated(dedup_key), ]

  rownames(expanded) <- NULL

  if (verbose) {
    message(sprintf(
      "  Final enrichment: %s rows (was %s)",
      format(nrow(expanded), big.mark = ","),
      format(nrow(df), big.mark = ",")
    ))
  }

  expanded
}
