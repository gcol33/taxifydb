# Shared schema normalization for taxify backbone builds.
#
# Each backend downloads raw data in different formats (SQLite, TSV, pipe-
# delimited). This module normalizes them to a unified Darwin Core-like schema
# suitable for the taxify matching engine:
#
#   taxon_id, canonical_name, taxon_rank, taxonomic_status,
#   accepted_name_usage_id, family, genus, specific_epithet,
#   authorship, infraspecific_epithet
#
# Precomputed keys and embedded synonym info are added by precompute.R.

# Infraspecific rank connecting terms, as written in a rendered botanical name.
# Shared by the backbones that parse infraspecific names out of a rendered
# string (Euro+Med) or reconstruct one that a source dropped (GBIF, #45).
.infraspecific_markers <- c("subsp.", "var.", "f.", "nothosubsp.", "subvar.",
                            "convar.", "proles", "race", "grex", "subf.",
                            "nothovar.", "nothof.")

#' Normalize a raw backbone data.frame to the unified schema
#'
#' Renames source-specific columns to canonical names and ensures consistent
#' types and formatting (uppercase ranks/statuses, trimmed whitespace, etc.).
#'
#' @param df The raw data.frame from a backend's download/parse step.
#' @param col_map A named list mapping canonical names to source column names:
#'   `taxon_id`, `canonical_name`, `taxon_rank`, `taxonomic_status`,
#'   `accepted_name_usage_id`, `family`, `genus`, `specific_epithet`,
#'   `authorship`, `infraspecific_epithet`.
#' @param extra_cols Optional named list of additional columns to keep
#'   (`canonical_name = source_name`).
#' @return A data.frame with standardized column names and formatting.
#' @export
normalize_backbone <- function(df, col_map, extra_cols = NULL) {
  required <- c("taxon_id", "canonical_name", "taxon_rank",
                "taxonomic_status", "accepted_name_usage_id",
                "family", "genus", "specific_epithet")

  missing <- setdiff(required, names(col_map))
  if (length(missing) > 0L) {
    stop(sprintf("col_map missing required entries: %s",
                 paste(missing, collapse = ", ")))
  }

  n <- nrow(df)
  all_cols <- c(col_map, extra_cols)

  col_list <- lapply(names(all_cols), function(canon_name) {
    src_name <- all_cols[[canon_name]]
    if (src_name %in% names(df)) df[[src_name]] else rep(NA_character_, n)
  })
  names(col_list) <- names(all_cols)
  out <- as.data.frame(col_list, stringsAsFactors = FALSE)

  if (!"authorship" %in% names(out)) {
    out$authorship <- NA_character_
  }
  if (!"infraspecific_epithet" %in% names(out)) {
    out$infraspecific_epithet <- NA_character_
  }

  out$taxon_rank <- toupper(trimws(out$taxon_rank))
  out$taxonomic_status <- toupper(trimws(out$taxonomic_status))

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
#' @export
resolve_hierarchy <- function(df, target_ranks = c("family", "genus"),
                              max_depth = 20L) {
  id_to_row <- match(df$id, df$id)
  names(id_to_row) <- df$id

  for (rank in target_ranks) {
    col_name <- paste0("resolved_", rank)
    df[[col_name]] <- ifelse(
      tolower(df$rank) == rank,
      df$name,
      NA_character_
    )
  }

  parent_row <- match(df$parent_id, df$id)
  current_parent <- parent_row

  for (depth in seq_len(max_depth)) {
    all_resolved <- TRUE

    for (rank in target_ranks) {
      col_name <- paste0("resolved_", rank)
      needs_resolution <- is.na(df[[col_name]]) & !is.na(current_parent)

      if (!any(needs_resolution)) next
      all_resolved <- FALSE

      parent_rank <- tolower(df$rank[current_parent[needs_resolution]])
      is_match <- parent_rank == rank
      match_idx <- which(needs_resolution)[is_match]

      if (length(match_idx) > 0L) {
        df[[col_name]][match_idx] <- df$name[current_parent[match_idx]]
      }
    }

    if (all_resolved) break

    next_parent <- rep(NA_integer_, nrow(df))
    has_parent <- !is.na(current_parent)
    next_parent[has_parent] <- match(df$parent_id[current_parent[has_parent]],
                                     df$id)
    current_parent <- next_parent
  }

  df
}


#' Split a scientific name into genus and epithets
#'
#' Several sources publish a name without the parsed parts the unified schema
#' carries, so the genus, specific epithet and infraspecific epithet are taken
#' from the name itself.
#'
#' When `genus` is supplied and the name begins with it, the split is anchored
#' there; otherwise the first word of the name is taken as the genus. Anchoring
#' on a known genus rather than on a rank vocabulary means a rank spelling this
#' function has never seen still resolves, and it keeps a two-word genus from
#' being read as a binomial.
#'
#' The infraspecific epithet is the last word following the specific epithet,
#' so a name carrying a rank marker (`Poa annua subsp. exilis`) and a bare
#' trinomial (`Larus fuscus graellsii`) both resolve to the epithet itself. A
#' free-standing hybrid multiplication sign is dropped rather than read as an
#' epithet.
#'
#' The caller decides which rows to apply this to: a name above genus rank
#' still returns its first word as `genus`, so a source that stores family and
#' order rows in the same table gates on rank itself.
#'
#' @param name Character vector of scientific names.
#' @param genus Character vector of known genus names (same length as `name`),
#'   or `NULL` to take the genus from the name.
#' @return A list of three character vectors: `genus`, `specific` and
#'   `infraspecific`, each the length of `name`. Absent parts are `NA`.
#' @export
#' @examples
#' split_scientific_name("Poa annua subsp. exilis")
#' split_scientific_name("Larus fuscus graellsii")
#' split_scientific_name("Quercus", genus = "Quercus")
split_scientific_name <- function(name, genus = NULL) {
  n <- gsub("\\s+", " ", trimws(name))
  n[!is.na(n) & !nzchar(n)] <- NA_character_

  first <- ifelse(is.na(n), NA_character_, sub(" .*$", "", n))

  if (is.null(genus)) {
    g <- first
  } else {
    g <- trimws(genus)
    g[!is.na(g) & !nzchar(g)] <- NA_character_
    # A supplied genus is only usable as the anchor when the name actually
    # starts with it; otherwise the name leads and the first word wins.
    usable <- !is.na(g) & !is.na(n) & (n == g | startsWith(n, paste0(g, " ")))
    g <- ifelse(usable, g, first)
  }

  rest <- ifelse(is.na(g) | is.na(n) | n == g, NA_character_,
                 substring(n, nchar(g) + 2L))

  rest <- gsub("(^| )\u00d7(?= )", "", rest, perl = TRUE)
  rest <- trimws(gsub("\\s+", " ", rest))
  rest[!is.na(rest) & !nzchar(rest)] <- NA_character_

  specific <- ifelse(is.na(rest), NA_character_, sub(" .*$", "", rest))

  # Counting spaces is the cheap vectorized way to ask whether anything follows
  # the specific epithet.
  extra <- ifelse(is.na(rest), 0L,
                  nchar(rest) - nchar(gsub(" ", "", rest, fixed = TRUE)))
  infraspecific <- ifelse(!is.na(rest) & extra >= 1L,
                          sub("^.* ", "", rest), NA_character_)

  list(genus = g, specific = specific, infraspecific = infraspecific)
}
