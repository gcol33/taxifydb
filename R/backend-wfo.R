# WFO (World Flora Online): classification.txt -> normalized data.frame -> .vtr
#
# WFO publishes annual Darwin Core backbone snapshots on Zenodo. The archive
# contains classification.txt, a TSV with the canonical DwC columns
# (taxonID, scientificName, taxonRank, taxonomicStatus, ...). scientificName
# is already authorship-free (WFO stores authorship separately), so no
# canonical-name extraction is needed.
#
# WFO quirks: latin1-encoded TSV, mojibake on the multiplication sign
# (UTF-8 "×" read as "Ã—"), uppercase status/rank normalization.

.wfo_url <- "https://zenodo.org/records/14538251/files/_DwC_backbone_R.zip"
.wfo_version_default <- "2024-12"

# Core matching columns + authorship + infraspecific epithet
.wfo_match_cols <- c(
  "taxonID",
  "scientificName",
  "taxonRank",
  "taxonomicStatus",
  "acceptedNameUsageID",
  "family",
  "genus",
  "specificEpithet",
  "scientificNameAuthorship",
  "infraspecificEpithet"
)

# Extra columns preserved for add_wfo_info() at runtime
.wfo_extra_cols <- c(
  "scientificNameID",
  "parentNameUsageID",
  "namePublishedIn",
  "nomenclaturalStatus",
  "taxonRemarks",
  "subfamily",
  "tribe",
  "subtribe",
  "subgenus"
)


#' Download and extract the WFO classification file
#'
#' @param dest Character. Destination directory.
#' @param verbose Logical.
#' @return Path to the extracted classification file.
#' @export
download_wfo <- function(dest = tempdir(), verbose = TRUE) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)

  if (verbose) {
    message("Downloading WFO backbone from Zenodo (~120 MB)...")
    message(sprintf("  URL: %s", .wfo_url))
  }
  zip_path <- download_curl_file(.wfo_url, dest, "wfo_download.zip")

  if (verbose) message("Extracting classification file...")
  txt_files <- utils::unzip(zip_path, list = TRUE)$Name
  txt_target <- txt_files[grepl("classification\\.(txt|csv)$", txt_files)]
  if (length(txt_target) == 0L) {
    stop("classification.txt/.csv not found in WFO archive.", call. = FALSE)
  }
  utils::unzip(zip_path, files = txt_target[1L], exdir = dest,
               junkpaths = TRUE)
  txt_path <- file.path(dest, basename(txt_target[1L]))

  unlink(zip_path)
  txt_path
}


#' Read and normalize the WFO classification file
#'
#' @param txt_path Character. Path to the WFO classification.txt file.
#' @param verbose Logical.
#' @return A normalized data.frame ready for [precompute_backbone()].
#' @export
read_wfo <- function(txt_path, verbose = TRUE) {
  if (verbose) message("Reading classification file...")
  # Every column this backbone keeps is a name, identifier, rank or status, so
  # the types are fixed rather than inferred. That also keeps this read and the
  # block read of build_wfo() agreeing on them without either having to observe
  # what the other did.
  df <- utils::read.delim(
    txt_path,
    fileEncoding = "latin1",
    stringsAsFactors = FALSE,
    na.strings = "",
    colClasses = "character"
  )
  if (verbose) message(sprintf("  %s rows", format(nrow(df), big.mark = ",")))
  normalize_wfo(df, verbose = verbose)
}


#' Normalize one block of WFO rows to the unified schema
#'
#' Split out of [read_wfo()] so the streaming build can apply it to a chunk at a
#' time. Nothing here depends on rows outside the block.
#'
#' The block must already be decoded the way [read_wfo()] decodes it. WFO ships
#' UTF-8 bytes that the reader takes as latin1, which turns the multiplication
#' sign of a hybrid name into two characters; the repair below expects exactly
#' that pair, so a block decoded some other way would arrive with nothing to
#' repair and the hybrid marker left wrong.
#'
#' @param df A data.frame of raw WFO rows, with the file's own column names.
#' @param verbose Logical.
#' @return A normalized data.frame.
#' @export
normalize_wfo <- function(df, verbose = TRUE) {
  keep <- intersect(c(.wfo_match_cols, .wfo_extra_cols), names(df))
  df <- df[, keep, drop = FALSE]

  if ("taxonomicStatus" %in% names(df)) {
    df$taxonomicStatus <- toupper(df$taxonomicStatus)
  }
  if ("taxonRank" %in% names(df)) {
    df$taxonRank <- toupper(df$taxonRank)
  }

  # Fix mojibake: UTF-8 × misread as latin1
  text_cols <- intersect(
    c("scientificName", "family", "genus", "specificEpithet",
      "scientificNameAuthorship"),
    names(df)
  )
  for (col in text_cols) {
    df[[col]] <- trimws(df[[col]])
    df[[col]] <- gsub("Ã", "×", df[[col]], fixed = TRUE)
  }

  # WFO-specific: extra normalized name column kept alongside canonical
  df$normalizedName <- taxify::normalize_epithets(df$scientificName)

  if (verbose) message("Normalizing to unified schema...")
  col_map <- list(
    taxon_id                = "taxonID",
    canonical_name          = "scientificName",
    taxon_rank              = "taxonRank",
    taxonomic_status        = "taxonomicStatus",
    accepted_name_usage_id  = "acceptedNameUsageID",
    family                  = "family",
    genus                   = "genus",
    specific_epithet        = "specificEpithet",
    authorship              = "scientificNameAuthorship",
    infraspecific_epithet   = "infraspecificEpithet"
  )

  extra_cols <- list()
  for (col in c(.wfo_extra_cols, "normalizedName")) {
    if (col %in% names(df)) extra_cols[[col]] <- col
  }

  normalize_backbone(df, col_map, extra_cols)
}


#' Build the WFO backbone .vtr from source
#'
#' @param output_dir Character. Output directory.
#' @param version Character or NULL. Defaults to the bundled WFO release tag.
#' @param verbose Logical.
#' @return Path to the .vtr file (invisibly).
#' @export
build_wfo <- function(output_dir = "output/wfo", version = NULL,
                      verbose = TRUE) {
  if (is.null(version)) version <- .wfo_version_default

  tmp <- tempfile("wfo_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  txt_path <- download_wfo(dest = tmp, verbose = verbose)

  # classification.csv inflates to roughly 900 MB, so it is staged a block at a
  # time rather than assembled in memory. The parsing arguments are the ones
  # read_wfo() hands read.delim(): tab separated, double-quoted fields, an
  # empty field is NA, and the bytes are decoded as latin1.
  vtr_path <- file.path(output_dir, "wfo.vtr")
  build_vtr_streamed(
    delim_chunk_feed(txt_path,
                     normalize = function(chunk) {
                       normalize_wfo(chunk, verbose = FALSE)
                     },
                     quote = "\"", na_strings = "", file_encoding = "latin1",
                     verbose = verbose),
    vtr_path, "wfo", version, .wfo_url, verbose = verbose
  )

  invisible(vtr_path)
}
