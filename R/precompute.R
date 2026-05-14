# Precompute matching keys and embed synonym info.
#
# After normalize_backbone() produces a unified schema, this module adds the
# precomputed columns that taxify's matching engine expects:
#   key_ci, key_normalized, key_species  (for lookup passes)
#   accepted_name, accepted_family, accepted_genus, accepted_taxon_id,
#   is_synonym  (embedded synonym resolution)

#' Normalize Latin epithets for fuzzy orthographic matching
#'
#' Handles common Latin character variants: ae/oe ligatures, accented vowels,
#' \eqn{\times} hybrid markers, etc.
#'
#' @param x Character vector of names.
#' @return Character vector with normalized epithets (lowercased).
#' @export
normalize_epithets <- function(x) {
  x <- tolower(x)
  x <- gsub("æ", "ae", x)
  x <- gsub("œ", "oe", x)
  x <- gsub("ø", "o", x)
  x <- gsub("×", "x", x)
  x <- gsub("[àáâãä]", "a", x)
  x <- gsub("[èéêë]", "e", x)
  x <- gsub("[ìíîï]", "i", x)
  x <- gsub("[òóôõö]", "o", x)
  x <- gsub("[ùúûü]", "u", x)
  x <- gsub("[ýÿ]", "y", x)
  x <- gsub("ñ", "n", x)
  x <- gsub("ç", "c", x)
  x
}


#' Add precomputed matching keys to a normalized backbone
#'
#' @param df A normalized backbone data.frame (from `normalize_backbone()`).
#' @return The data.frame with added `key_ci`, `key_normalized`, `key_species`.
#' @export
precompute_keys <- function(df) {
  df$key_ci <- tolower(df$canonical_name)
  df$key_normalized <- normalize_epithets(df$canonical_name)

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
#' @param synonym_pattern Regex pattern to detect synonyms in the
#'   `taxonomic_status` column.
#' @return The data.frame with added `accepted_name`, `accepted_family`,
#'   `accepted_genus`, `accepted_taxon_id`, `is_synonym` columns.
#' @export
embed_accepted <- function(df, synonym_pattern = "SYNONYM") {
  is_syn <- !is.na(df$taxonomic_status) &
    grepl(synonym_pattern, df$taxonomic_status)

  df$accepted_name     <- df$canonical_name
  df$accepted_family   <- df$family
  df$accepted_genus    <- df$genus
  df$accepted_taxon_id <- df$taxon_id
  df$is_synonym        <- FALSE

  id_to_row <- match(df$accepted_name_usage_id, df$taxon_id)

  resolved <- is_syn & !is.na(id_to_row)
  if (any(resolved)) {
    target_rows <- id_to_row[resolved]
    df$accepted_name[resolved]     <- df$canonical_name[target_rows]
    df$accepted_family[resolved]   <- df$family[target_rows]
    df$accepted_genus[resolved]    <- df$genus[target_rows]
    df$accepted_taxon_id[resolved] <- df$taxon_id[target_rows]
    df$is_synonym[resolved]        <- TRUE
  }

  unresolved_syn <- is_syn & !resolved
  df$is_synonym[unresolved_syn] <- TRUE

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
#' @return The data.frame ready for `build_vtr()`.
#' @export
precompute_backbone <- function(df, synonym_pattern = "SYNONYM") {
  df <- precompute_keys(df)
  df <- embed_accepted(df, synonym_pattern = synonym_pattern)
  df
}
