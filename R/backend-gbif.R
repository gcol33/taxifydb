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

  normalize_gbif(df, gbif_higher_lookup(df$id, df$canonical_name),
                 verbose = verbose)
}


#' Normalize one block of GBIF rows to the unified schema
#'
#' Split out of [read_gbif()] so the streaming build can apply it to a chunk at
#' a time. Everything here depends only on the row in hand once `higher` is
#' supplied, which is the whole reason that lookup is a separate argument: the
#' `*_key` columns point at rows anywhere in the file.
#'
#' @param df A data.frame of raw GBIF rows, named by [.gbif_col_names].
#' @param higher Named character vector of canonical names by id, from
#'   [gbif_higher_lookup()].
#' @param verbose Logical.
#' @return A normalized data.frame ready for [precompute_backbone()].
#' @export
normalize_gbif <- function(df, higher, verbose = TRUE) {
  if (verbose) message("Denormalizing higher classification via self-join...")
  # Resolve the denormalized ancestor names from each row's *_key columns while
  # the KINGDOM/PHYLUM/CLASS/ORDER rows are still present (they are dropped
  # below). Without this, only family is carried and the classification cannot
  # be walked upward: taxify::upstream()/downstream() above family return zero
  # rows. See gcol33/taxifydb#24.
  df$kingdom <- gbif_resolve_higher(higher, df$kingdom_key)
  df$phylum  <- gbif_resolve_higher(higher, df$phylum_key)
  df$class   <- gbif_resolve_higher(higher, df$class_key)
  df$order   <- gbif_resolve_higher(higher, df$order_key)
  df$family  <- gbif_resolve_higher(higher, df$family_key)

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
    c("canonical_name", "scientific_name", "kingdom", "phylum", "class",
      "order", "family", "genus_or_above", "specific_epithet", "authorship"),
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
    kingdom                 = "kingdom",
    phylum                  = "phylum",
    class                   = "class",
    order                   = "order",
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
  txt_path <- gunzip_file(gz_path, file.path(tmp, "gbif_simple.txt"),
                          verbose = verbose)
  unlink(gz_path)

  # The five *_key columns name rows anywhere in the file, and the rows they
  # name at kingdom, phylum, class and order rank are dropped by the filter
  # below, so the lookup is built before the streaming pass rather than during
  # it. Only referenced ids are kept, which is a few tens of thousands of rows.
  if (verbose) message("Building higher-classification lookup...")
  higher <- delim_fk_lookup(
    txt_path, id_col = "id", value_col = "canonical_name",
    key_cols = c("kingdom_key", "phylum_key", "class_key", "order_key",
                 "family_key"),
    quote = "", na_strings = "\\N", col_names = .gbif_col_names,
    verbose = verbose
  )

  # Staged a block at a time rather than assembled in memory. The parsing
  # arguments match read_gbif(): simple.txt carries no header row, quoting is
  # off, and \N is the NULL marker.
  vtr_path <- file.path(output_dir, "gbif.vtr")
  build_vtr_streamed(
    delim_chunk_feed(txt_path,
                     normalize = function(chunk) {
                       normalize_gbif(chunk, higher, verbose = FALSE)
                     },
                     quote = "", na_strings = "\\N",
                     col_names = .gbif_col_names, verbose = verbose),
    vtr_path, "gbif", version, .gbif_url, verbose = verbose
  )

  invisible(vtr_path)
}


#' Build the id -> canonical name lookup the `*_key` columns resolve against
#'
#' @param id Vector of row identifiers.
#' @param canonical_name Character vector of canonical names.
#' @return A named character vector of names by id.
#' @noRd
gbif_higher_lookup <- function(id, canonical_name) {
  stats::setNames(canonical_name, as.character(id))
}


#' Resolve higher classification keys to names
#'
#' @param higher Named character vector from [gbif_higher_lookup()].
#' @param key_col Vector of keys to resolve.
#' @return Character vector of resolved names.
#' @noRd
gbif_resolve_higher <- function(higher, key_col) {
  unname(higher[as.character(key_col)])
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
