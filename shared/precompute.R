# ---- Precompute matching keys and embed synonym info ----
#
# After normalize.R produces a unified schema, this module adds the
# precomputed columns that taxify's matching engine expects:
#   key_ci, key_normalized, key_species  (for lookup passes)
#   accepted_name, accepted_family, accepted_genus, accepted_taxon_id,
#   is_synonym  (embedded synonym resolution)
#
# These are the same operations that taxify does in-package for the existing
# WFO/COL/GBIF backends. Extracted here so the build pipeline can produce
# identical .vtr files for any backend.

#' Normalize Latin epithets for fuzzy orthographic matching
#'
#' Handles common Latin character variants:
#'   ae/oe ligatures, accented vowels, × hybrid markers, etc.
#'
#' @param x Character vector of names.
#' @return Character vector with normalized epithets (lowercased).
normalize_epithets <- function(x) {
  x <- tolower(x)
  x <- gsub("\u00e6", "ae", x)  # æ → ae
  x <- gsub("\u0153", "oe", x)  # œ → oe
  x <- gsub("\u00f8", "o", x)   # ø → o
  x <- gsub("\u00d7", "x", x)   # × → x
  x <- gsub("[\u00e0\u00e1\u00e2\u00e3\u00e4]", "a", x)

x <- gsub("[\u00e8\u00e9\u00ea\u00eb]", "e", x)
  x <- gsub("[\u00ec\u00ed\u00ee\u00ef]", "i", x)
  x <- gsub("[\u00f2\u00f3\u00f4\u00f5\u00f6]", "o", x)
  x <- gsub("[\u00f9\u00fa\u00fb\u00fc]", "u", x)
  x <- gsub("[\u00fd\u00ff]", "y", x)
  x <- gsub("\u00f1", "n", x)   # ñ → n
  x <- gsub("\u00e7", "c", x)   # ç → c
  x
}


#' Add precomputed matching keys to a normalized backbone
#'
#' @param df A normalized backbone data.frame (from normalize_backbone()).
#' @return The data.frame with added key_ci, key_normalized, key_species columns.
precompute_keys <- function(df) {
  df$key_ci <- tolower(df$canonical_name)

  df$key_normalized <- normalize_epithets(df$canonical_name)

  # key_species: "Genus epithet" for infraspecific names (3+ words)
  has_genus <- !is.na(df$genus) & nzchar(df$genus)
  has_epithet <- !is.na(df$specific_epithet) & nzchar(df$specific_epithet)
  df$key_species <- ifelse(
    has_genus & has_epithet,
    paste(df$genus, df$specific_epithet),
    NA_character_
  )

  df
}


#' Embed accepted taxon info via synonym self-join
#'
#' For every synonym row, resolves the accepted taxon and embeds its name,
#' family, and genus directly. Handles synonym chains (max 10 hops).
#'
#' @param df A normalized backbone data.frame.
#' @param synonym_pattern Regex pattern to detect synonyms in
#'   taxonomic_status column.
#' @return The data.frame with added columns: accepted_name, accepted_family,
#'   accepted_genus, accepted_taxon_id, is_synonym.
embed_accepted <- function(df, synonym_pattern = "SYNONYM") {
  is_syn <- !is.na(df$taxonomic_status) &
    grepl(synonym_pattern, df$taxonomic_status)

  # Default: accepted = self
  df$accepted_name     <- df$canonical_name
  df$accepted_family   <- df$family
  df$accepted_genus    <- df$genus
  df$accepted_taxon_id <- df$taxon_id
  df$is_synonym        <- FALSE

  # Build ID -> row index lookup
  id_to_row <- match(df$accepted_name_usage_id, df$taxon_id)

  # One-hop resolution
  resolved <- is_syn & !is.na(id_to_row)
  if (any(resolved)) {
    target_rows <- id_to_row[resolved]
    df$accepted_name[resolved]     <- df$canonical_name[target_rows]
    df$accepted_family[resolved]   <- df$family[target_rows]
    df$accepted_genus[resolved]    <- df$genus[target_rows]
    df$accepted_taxon_id[resolved] <- df$taxon_id[target_rows]
    df$is_synonym[resolved]        <- TRUE
  }

  # Unresolvable synonyms
  unresolved_syn <- is_syn & !resolved
  df$is_synonym[unresolved_syn] <- TRUE

  # Chase synonym chains (max 10 hops)
  for (iter in seq_len(10L)) {
    acc_rows <- match(df$accepted_taxon_id, df$taxon_id)
    chain <- df$is_synonym &
      !is.na(acc_rows) &
      df$is_synonym[acc_rows] &
      df$accepted_taxon_id != df$taxon_id

    chain[is.na(chain)] <- FALSE
    if (!any(chain)) break

    chain_target <- acc_rows[chain]
    next_acc_id <- df$accepted_taxon_id[chain_target]
    next_row <- match(next_acc_id, df$taxon_id)
    has_next <- !is.na(next_row)

    if (!any(has_next)) break
    chain_idx <- which(chain)[has_next]
    next_row <- next_row[has_next]

    df$accepted_name[chain_idx]     <- df$canonical_name[next_row]
    df$accepted_family[chain_idx]   <- df$family[next_row]
    df$accepted_genus[chain_idx]    <- df$genus[next_row]
    df$accepted_taxon_id[chain_idx] <- df$taxon_id[next_row]
  }

  df
}


#' Full precompute pipeline: keys + synonym embedding
#'
#' @param df A normalized backbone data.frame.
#' @param synonym_pattern Regex for synonym detection.
#' @return The data.frame ready for write_vtr().
precompute_backbone <- function(df, synonym_pattern = "SYNONYM") {
  df <- precompute_keys(df)
  df <- embed_accepted(df, synonym_pattern = synonym_pattern)
  df
}
