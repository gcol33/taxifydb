# COL (Catalogue of Life): Taxon.tsv -> normalized data.frame -> .vtr
#
# COL publishes annual Darwin Core Archives on ChecklistBank. The Taxon.tsv
# uses dwc:/col: namespace prefixes on column names (stripped on read),
# scientificName includes authorship (canonical computed by subtraction),
# and the column for genus is `genericName` (not `genus`).
#
# COL uses MISAPPLIED as a third status in addition to ACCEPTED/SYNONYM —
# we treat it as a synonym for matching purposes by passing the appropriate
# synonym_pattern to precompute_backbone().

.col_url <- "https://download.checklistbank.org/col/annual/2025_dwca.zip"
.col_version_default <- "2025"

# Columns needed for matching (after stripping namespace prefixes)
.col_match_cols <- c(
  "taxonID",
  "scientificName",
  "taxonRank",
  "taxonomicStatus",
  "acceptedNameUsageID",
  "family",
  "genericName",
  "specificEpithet",
  "scientificNameAuthorship",
  "infraspecificEpithet",
  "parentNameUsageID"
)

# Extra columns preserved for add_col_info() at runtime
.col_extra_cols <- c(
  "notho",
  "nomenclaturalCode",
  "nomenclaturalStatus",
  "namePublishedIn",
  "nameAccordingTo",
  "kingdom",
  "phylum",
  "class",
  "order",
  "superfamily",
  "subfamily",
  "tribe",
  "taxonRemarks",
  "references",
  "scientificNameID",
  "infragenericEpithet",
  "cultivarEpithet"
)


#' Download and extract the COL DwC-A
#'
#' @param dest Character. Destination directory.
#' @param verbose Logical.
#' @return Path to the extracted COL directory.
#' @export
download_col <- function(dest = tempdir(), verbose = TRUE) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)

  if (verbose) {
    message("Downloading COL backbone from ChecklistBank (~600 MB)...")
    message(sprintf("  URL: %s", .col_url))
  }
  zip_path <- download_curl_file(.col_url, dest, "col_download.zip")

  if (verbose) message("Extracting Taxon.tsv ...")
  txt_files <- utils::unzip(zip_path, list = TRUE)$Name
  taxon_target <- txt_files[grepl("Taxon\\.tsv$", txt_files)]
  if (length(taxon_target) == 0L) {
    stop("Taxon.tsv not found in COL archive.", call. = FALSE)
  }
  utils::unzip(zip_path, files = taxon_target[1L], exdir = dest,
               junkpaths = TRUE)

  # SpeciesProfile.tsv (habitat/extinct/marine flags) — optional
  sp_target <- txt_files[grepl("SpeciesProfile\\.tsv$", txt_files)]
  if (length(sp_target) > 0L) {
    if (verbose) message("Extracting SpeciesProfile.tsv ...")
    utils::unzip(zip_path, files = sp_target[1L], exdir = dest,
                 junkpaths = TRUE)
  }

  unlink(zip_path)
  dest
}


#' Read and normalize the COL Taxon.tsv
#'
#' @param col_dir Character. Path to the extracted COL directory.
#' @param verbose Logical.
#' @return A normalized data.frame.
#' @export
read_col <- function(col_dir, verbose = TRUE) {
  tsv_path <- file.path(col_dir, "Taxon.tsv")
  if (!file.exists(tsv_path)) {
    stop("Taxon.tsv not found in: ", col_dir, call. = FALSE)
  }

  if (verbose) message("Reading Taxon.tsv ...")
  df <- utils::read.delim(
    tsv_path,
    fileEncoding = "UTF-8",
    stringsAsFactors = FALSE,
    quote = "",
    na.strings = "",
    check.names = FALSE
  )
  if (verbose) message(sprintf("  %s rows", format(nrow(df), big.mark = ",")))

  # Strip namespace prefixes (dwc:taxonID -> taxonID)
  names(df) <- sub("^[a-z]+:", "", names(df))

  keep <- intersect(c(.col_match_cols, .col_extra_cols), names(df))
  df <- df[, keep, drop = FALSE]

  if ("taxonomicStatus" %in% names(df)) {
    df$taxonomicStatus <- toupper(df$taxonomicStatus)
  }
  if ("taxonRank" %in% names(df)) {
    df$taxonRank <- toupper(df$taxonRank)
  }

  # Build canonical name (strip authorship from scientificName)
  if (all(c("scientificName", "scientificNameAuthorship") %in% names(df))) {
    df$canonicalName <- strip_authorship(df$scientificName,
                                         df$scientificNameAuthorship)
  } else {
    df$canonicalName <- df$scientificName
  }

  if (verbose) message("Denormalizing family names...")
  df$family <- col_resolve_family(df)

  text_cols <- intersect(
    c("canonicalName", "scientificName", "family", "genericName",
      "specificEpithet", "scientificNameAuthorship"),
    names(df)
  )
  for (col in text_cols) {
    df[[col]] <- trimws(df[[col]])
  }

  if (verbose) message("Normalizing to unified schema...")
  col_map <- list(
    taxon_id                = "taxonID",
    canonical_name          = "canonicalName",
    taxon_rank              = "taxonRank",
    taxonomic_status        = "taxonomicStatus",
    accepted_name_usage_id  = "acceptedNameUsageID",
    family                  = "family",
    genus                   = "genericName",
    specific_epithet        = "specificEpithet",
    authorship              = "scientificNameAuthorship",
    infraspecific_epithet   = "infraspecificEpithet"
  )

  extra_cols <- list()
  for (col in c(.col_extra_cols, "scientificName")) {
    if (col %in% names(df)) extra_cols[[col]] <- col
  }

  normalize_backbone(df, col_map, extra_cols)
}


#' Build the COL backbone .vtr
#'
#' Also writes the SpeciesProfile.tsv (extinct/marine flags) as a separate
#' `col_species_profile.vtr` next to the main backbone, if present.
#'
#' @param output_dir Character.
#' @param version Character or NULL.
#' @param verbose Logical.
#' @return Path to the .vtr file (invisibly).
#' @export
build_col <- function(output_dir = "output/col", version = NULL,
                      verbose = TRUE) {
  if (is.null(version)) version <- .col_version_default

  tmp <- tempfile("col_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  col_dir <- download_col(dest = tmp, verbose = verbose)
  df <- read_col(col_dir, verbose = verbose)

  if (verbose) message("Precomputing keys and embedding synonyms...")
  df <- precompute_backbone(df, synonym_pattern = "SYNONYM|MISAPPLIED")

  vtr_path <- file.path(output_dir, "col.vtr")
  build_vtr(df, vtr_path, "col", version, .col_url)

  sp_path <- file.path(col_dir, "SpeciesProfile.tsv")
  if (file.exists(sp_path)) {
    if (verbose) message("Converting SpeciesProfile.tsv to .vtr ...")
    sp_df <- utils::read.delim(
      sp_path,
      fileEncoding = "UTF-8",
      stringsAsFactors = FALSE,
      quote = "",
      na.strings = "",
      check.names = FALSE
    )
    names(sp_df) <- sub("^[a-z]+:", "", names(sp_df))
    sp_vtr <- file.path(output_dir, "col_species_profile.vtr")
    vectra::write_vtr(sp_df, sp_vtr)
    if (verbose) message(sprintf("  SpeciesProfile: %s rows",
                                 format(nrow(sp_df), big.mark = ",")))
  }

  invisible(vtr_path)
}


#' Resolve family names for all COL rows via vectorized tree propagation
#'
#' Family-rank rows seed their own name; the parent chain propagates the
#' family down to descendants. Remaining gaps are filled by genericName ->
#' genus family lookup.
#'
#' @param df The full COL data.frame with columns taxonID, taxonRank,
#'   canonicalName, parentNameUsageID, genericName.
#' @return Character vector of family names (same length as `nrow(df)`).
#' @noRd
col_resolve_family <- function(df) {
  n <- nrow(df)
  family <- rep(NA_character_, n)

  rank <- toupper(df$taxonRank)

  is_family <- rank == "FAMILY"
  family[is_family] <- df$canonicalName[is_family]

  id_to_idx <- stats::setNames(seq_len(n), df$taxonID)
  parent_idx <- unname(id_to_idx[df$parentNameUsageID])

  for (iter in seq_len(15L)) {
    missing <- is.na(family) & !is.na(parent_idx)
    if (!any(missing)) break
    parent_fam <- family[parent_idx[missing]]
    resolved <- !is.na(parent_fam)
    if (!any(resolved)) break
    idx_missing <- which(missing)
    family[idx_missing[resolved]] <- parent_fam[resolved]
  }

  is_genus <- rank == "GENUS"
  genus_names <- df$canonicalName[is_genus]
  genus_families <- family[is_genus]
  has_fam <- !is.na(genus_families)
  if (any(has_fam)) {
    genus_fam_lookup <- stats::setNames(genus_families[has_fam],
                                        genus_names[has_fam])
    needs_family <- is.na(family) & !is.na(df$genericName)
    if (any(needs_family)) {
      family[needs_family] <- unname(
        genus_fam_lookup[df$genericName[needs_family]]
      )
    }
  }

  family
}
