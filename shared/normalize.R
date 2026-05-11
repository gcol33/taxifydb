# ---- Shared normalization for taxify backbone builds ----
#
# Each backend downloads raw data in different formats (SQLite, TSV, pipe-
# delimited). This module normalizes them to a unified Darwin Core-like schema
# suitable for the taxify matching engine.
#
# The output schema matches what taxify's match_exact_compiled() expects:
#   taxon_id, canonical_name, taxon_rank, taxonomic_status,
#   accepted_name_usage_id, family, genus, specific_epithet,
#   authorship, infraspecific_epithet
#
# Plus precomputed keys and embedded synonym info (added by precompute.R).

#' Normalize a raw backbone data.frame to the unified schema
#'
#' Renames source-specific columns to canonical names and ensures consistent
#' types and formatting (uppercase ranks/statuses, trimmed whitespace, etc.).
#'
#' @param df The raw data.frame from a backend's download/parse step.
#' @param col_map A named list mapping canonical names to source column names:
#'   - taxon_id, canonical_name, taxon_rank, taxonomic_status,
#'   - accepted_name_usage_id, family, genus, specific_epithet,
#'   - authorship, infraspecific_epithet
#' @param extra_cols Optional named list of additional columns to keep
#'   (canonical_name = source_name).
#' @return A data.frame with standardized column names and formatting.
normalize_backbone <- function(df, col_map, extra_cols = NULL) {
  required <- c("taxon_id", "canonical_name", "taxon_rank",
                 "taxonomic_status", "accepted_name_usage_id",
                 "family", "genus", "specific_epithet")

  # Check all required mappings exist

  missing <- setdiff(required, names(col_map))
  if (length(missing) > 0L) {
    stop(sprintf("col_map missing required entries: %s",
                 paste(missing, collapse = ", ")))
  }

  # Build output data.frame by renaming columns
  n <- nrow(df)
  all_cols <- c(col_map, extra_cols)

  col_list <- lapply(names(all_cols), function(canon_name) {
    src_name <- all_cols[[canon_name]]
    if (src_name %in% names(df)) df[[src_name]] else rep(NA_character_, n)
  })
  names(col_list) <- names(all_cols)
  out <- as.data.frame(col_list, stringsAsFactors = FALSE)

  # Ensure required columns that may be missing from col_map have defaults
  if (!"authorship" %in% names(out)) {
    out$authorship <- NA_character_
  }
  if (!"infraspecific_epithet" %in% names(out)) {
    out$infraspecific_epithet <- NA_character_
  }

  # ---- Standardize formatting ----

  # Uppercase rank and status
  out$taxon_rank <- toupper(trimws(out$taxon_rank))
  out$taxonomic_status <- toupper(trimws(out$taxonomic_status))

  # Trim whitespace from text columns
  text_cols <- c("canonical_name", "family", "genus", "specific_epithet",
                 "authorship", "infraspecific_epithet")
  for (col in text_cols) {
    if (col %in% names(out)) {
      out[[col]] <- trimws(out[[col]])
      out[[col]] <- ifelse(nzchar(out[[col]]), out[[col]], NA_character_)
    }
  }

  out
}


#' Resolve family/genus by walking a parent-child hierarchy
#'
#' Many backends (ITIS, NCBI, OTT) store taxonomy as a parent-child tree
#' without explicit family/genus columns. This function walks up the tree
#' to find the nearest ancestor at each target rank.
#'
#' @param df A data.frame with columns `id`, `parent_id`, `rank`, `name`.
#' @param target_ranks Character vector of ranks to resolve (e.g.,
#'   `c("family", "genus")`).
#' @param max_depth Maximum number of hops (default 20).
#' @return The input data.frame with new columns named after target_ranks,
#'   containing the resolved ancestor name at that rank.
resolve_hierarchy <- function(df, target_ranks = c("family", "genus"),
                              max_depth = 20L) {
  # Build lookup: id -> row index
  id_to_row <- match(df$id, df$id)  # identity (but needed for parent lookup)
  names(id_to_row) <- df$id

  # Initialize result columns
  for (rank in target_ranks) {
    col_name <- paste0("resolved_", rank)
    df[[col_name]] <- ifelse(
      tolower(df$rank) == rank,
      df$name,
      NA_character_
    )
  }

  # Walk up the tree
  parent_row <- match(df$parent_id, df$id)
  current_parent <- parent_row

  for (depth in seq_len(max_depth)) {
    all_resolved <- TRUE

    for (rank in target_ranks) {
      col_name <- paste0("resolved_", rank)
      needs_resolution <- is.na(df[[col_name]]) & !is.na(current_parent)

      if (!any(needs_resolution)) next
      all_resolved <- FALSE

      # Check if parent is at target rank
      parent_rank <- tolower(df$rank[current_parent[needs_resolution]])
      is_match <- parent_rank == rank
      match_idx <- which(needs_resolution)[is_match]

      if (length(match_idx) > 0L) {
        df[[col_name]][match_idx] <- df$name[current_parent[match_idx]]
      }
    }

    if (all_resolved) break

    # Move up one level
    next_parent <- rep(NA_integer_, nrow(df))
    has_parent <- !is.na(current_parent)
    next_parent[has_parent] <- match(df$parent_id[current_parent[has_parent]],
                                     df$id)
    current_parent <- next_parent
  }

  df
}
