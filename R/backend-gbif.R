# GBIF backbone: simple.txt.gz -> normalized data.frame -> .vtr
#
# GBIF publishes the global backbone taxonomy on hosted-datasets.gbif.org as
# a 30-column positional TSV (no header), gzipped. \N denotes NULL.
#
# Key quirks:
# - No family text column — only family_key FK; resolved via self-join.
# - Synonyms point to their accepted taxon via parent_key (not
#   acceptedNameUsageID).
# - Status values include HOMOTYPIC_SYNONYM, HETEROTYPIC_SYNONYM,
#   PROXY_SYNONYM, MISAPPLIED, ... — collapsed to ACCEPTED/SYNONYM.
# - canonical_name (no authorship) is already separate from scientific_name.

.gbif_url <- "https://hosted-datasets.gbif.org/datasets/backbone/current/simple.txt.gz"
.gbif_version_default <- "current"

# Positional column names for simple.txt (30 columns, no header)
.gbif_col_names <- c(
  "id", "parent_key", "basionym_key", "is_synonym", "status",
  "rank", "nom_status", "constituent_key", "origin", "source_taxon_key",
  "kingdom_key", "phylum_key", "class_key", "order_key", "family_key",
  "genus_key", "species_key", "name_id", "scientific_name", "canonical_name",
  "genus_or_above", "specific_epithet", "infra_specific_epithet", "notho_type",
  "authorship", "year", "bracket_authorship", "bracket_year",
  "name_published_in", "issues"
)

# Extra columns preserved for add_gbif_info() and resolve_kingdom_via_gbif() at
# runtime. parent_key is required by taxify::resolve_kingdom_via_gbif() to walk
# the GBIF hierarchy upward from a genus row to its KINGDOM-rank ancestor.
.gbif_extra_cols <- c(
  "parent_key",
  "notho_type",
  "nom_status",
  "bracket_authorship",
  "bracket_year",
  "year",
  "name_published_in",
  "origin",
  "issues",
  "scientific_name"
)


#' Download the GBIF backbone simple.txt.gz
#'
#' @param dest Character. Destination directory.
#' @param verbose Logical.
#' @return Path to the .gz file.
#' @export
download_gbif <- function(dest = tempdir(), verbose = TRUE) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)

  if (verbose) {
    message("Downloading GBIF backbone (~1.5 GB)...")
    message(sprintf("  URL: %s", .gbif_url))
  }
  gz_path <- download_curl_file(.gbif_url, dest, "gbif_simple.txt.gz")

  gz_path
}


#' Read and normalize the GBIF backbone
#'
#' @param gz_path Character. Path to simple.txt.gz.
#' @param verbose Logical.
#' @return A normalized data.frame ready for [precompute_backbone()].
#' @export
read_gbif <- function(gz_path, verbose = TRUE) {
  if (verbose) message("Reading simple.txt.gz (this may take a while)...")
  df <- utils::read.delim(
    gz_path,
    header = FALSE,
    col.names = .gbif_col_names,
    stringsAsFactors = FALSE,
    quote = "",
    na.strings = "\\N",
    fileEncoding = "UTF-8"
  )
  if (verbose) message(sprintf("  %s rows", format(nrow(df), big.mark = ",")))

  if (verbose) message("Denormalizing family names via self-join...")
  df$family <- gbif_resolve_higher(df, df$family_key)

  df$status <- toupper(df$status)
  df$rank <- toupper(df$rank)

  # Build accepted_id: for synonyms, parent_key = accepted taxon ID
  is_synonym_flag <- df$is_synonym == "t"
  df$accepted_id <- ifelse(is_synonym_flag, as.character(df$parent_key),
                           NA_character_)

  df$id <- as.character(df$id)
  df$parent_key <- as.character(df$parent_key)

  # Map raw GBIF status to ACCEPTED/SYNONYM
  df$status <- gbif_status_to_standard(df$status)

  text_cols <- intersect(
    c("canonical_name", "scientific_name", "family", "genus_or_above",
      "specific_epithet", "authorship"),
    names(df)
  )
  for (col in text_cols) {
    df[[col]] <- trimws(df[[col]])
  }

  # Filter unmatchable rows: empty canonical, UNRANKED, higher taxonomy
  n_before <- nrow(df)
  df <- df[!is.na(df$canonical_name) & nzchar(df$canonical_name), ]
  df <- df[df$rank != "UNRANKED", ]
  df <- df[!df$rank %in% c("KINGDOM", "PHYLUM", "CLASS", "ORDER"), ]
  if (verbose) {
    message(sprintf("  Filtered %s rows (%s -> %s)",
                    format(n_before - nrow(df), big.mark = ","),
                    format(n_before, big.mark = ","),
                    format(nrow(df), big.mark = ",")))
  }

  if (verbose) message("Normalizing to unified schema...")
  col_map <- list(
    taxon_id                = "id",
    canonical_name          = "canonical_name",
    taxon_rank              = "rank",
    taxonomic_status        = "status",
    accepted_name_usage_id  = "accepted_id",
    family                  = "family",
    genus                   = "genus_or_above",
    specific_epithet        = "specific_epithet",
    authorship              = "authorship",
    infraspecific_epithet   = "infra_specific_epithet"
  )

  extra_cols <- list()
  for (col in .gbif_extra_cols) {
    if (col %in% names(df)) extra_cols[[col]] <- col
  }

  normalize_backbone(df, col_map, extra_cols)
}


#' Build the GBIF backbone .vtr
#'
#' @param output_dir Character.
#' @param version Character or NULL.
#' @param verbose Logical.
#' @return Path to the .vtr file (invisibly).
#' @export
build_gbif <- function(output_dir = "output/gbif", version = NULL,
                       verbose = TRUE) {
  if (is.null(version)) version <- .gbif_version_default

  tmp <- tempfile("gbif_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  gz_path <- download_gbif(dest = tmp, verbose = verbose)
  df <- read_gbif(gz_path, verbose = verbose)

  if (verbose) message("Precomputing keys and embedding synonyms...")
  df <- precompute_backbone(df)

  vtr_path <- file.path(output_dir, "gbif.vtr")
  build_vtr(df, vtr_path, "gbif", version, .gbif_url)

  invisible(vtr_path)
}


#' Resolve higher classification keys to names via self-join on `id`
#'
#' @param df The full GBIF data.frame.
#' @param key_col Integer vector of keys to resolve.
#' @return Character vector of resolved names.
#' @noRd
gbif_resolve_higher <- function(df, key_col) {
  lookup <- stats::setNames(df$canonical_name, as.character(df$id))
  resolved <- lookup[as.character(key_col)]
  unname(resolved)
}


#' Map GBIF status values to standard ACCEPTED/SYNONYM
#'
#' @param status Character vector of GBIF status values.
#' @return Character vector with "ACCEPTED" or "SYNONYM".
#' @noRd
gbif_status_to_standard <- function(status) {
  ifelse(
    status %in% c("ACCEPTED", "DOUBTFUL", "PROVISIONALLY_ACCEPTED"),
    "ACCEPTED",
    "SYNONYM"
  )
}
