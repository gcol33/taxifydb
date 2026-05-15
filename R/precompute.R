# Precompute matching keys and embed synonym info.
#
# After normalize_backbone() produces a unified schema, this module adds the
# precomputed columns that taxify's matching engine expects:
#   key_ci, key_normalized, key_species, fuzzy_block  (for lookup passes)
#   accepted_name, accepted_family, accepted_genus, accepted_taxon_id,
#   is_synonym  (embedded synonym resolution)
#
# Single source of truth lives in taxify so that the runtime and build sides
# can never silently diverge. This module just hardcodes the normalized column
# names and forwards.

#' Full precompute pipeline: keys + synonym embedding
#'
#' Adds the matching-key columns and embedded synonym info that taxify's
#' runtime expects. Operates on a normalized backbone (column names from
#' [normalize_backbone()]).
#'
#' @param df A normalized backbone data.frame.
#' @param synonym_pattern Regex for synonym detection.
#' @return The data.frame ready for `build_vtr()`.
#' @export
precompute_backbone <- function(df, synonym_pattern = "SYNONYM") {
  df <- taxify::precompute_keys(
    df,
    name_col    = "canonical_name",
    genus_col   = "genus",
    epithet_col = "specific_epithet"
  )
  df <- taxify::embed_accepted(
    df,
    id_col          = "taxon_id",
    acc_id_col      = "accepted_name_usage_id",
    name_col        = "canonical_name",
    family_col      = "family",
    genus_col       = "genus",
    status_col      = "taxonomic_status",
    synonym_pattern = synonym_pattern
  )
  df
}
