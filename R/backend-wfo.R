# WFO (World Flora Online): classification.txt -> normalized data.frame -> .vtr
#
# WFO publishes a Plant List edition on Zenodo every June and December, each as
# a new version of one concept record, with the Darwin Core backbone attached as
# _DwC_backbone_R.zip. The build resolves the newest edition through that
# concept record, so the download URL and the version come from one record and
# a new edition is picked up without editing this file.
#
# The archive contains classification.csv, a TSV with the canonical DwC columns
# (taxonID, scientificName, taxonRank, taxonomicStatus, ...). scientificName
# is already authorship-free (WFO stores authorship separately), so no
# canonical-name extraction is needed.
#
# WFO quirks: UTF-8 TSV with double-quoted fields, taxonRemarks cut at a fixed
# byte width that can split a character, uppercase status/rank normalization.

.wfo_concept_record <- "7460141"
.wfo_backbone_asset <- "_DwC_backbone_R.zip"

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
  "originalNameUsageID",
  "namePublishedIn",
  "nomenclaturalStatus",
  "taxonRemarks",
  "subfamily",
  "tribe",
  "subtribe",
  "subgenus"
)


#' Resolve the newest WFO Plant List edition on Zenodo
#'
#' Zenodo answers a concept record with its latest version. The edition label
#' that version carries (`2026-06`) is the release the backbone is built as.
#'
#' @param verbose Logical.
#' @return A list with `record` (Zenodo record id), `edition` (as WFO labels
#'   it), `version` (`YYYY.MM`) and `url` (the Darwin Core backbone archive).
#' @export
wfo_latest_edition <- function(verbose = TRUE) {
  api <- sprintf("https://zenodo.org/api/records/%s", .wfo_concept_record)
  rec <- tryCatch(
    jsonlite::fromJSON(api, simplifyVector = FALSE),
    error = function(e) {
      stop("Could not resolve the latest WFO Plant List on Zenodo: ",
           conditionMessage(e), call. = FALSE)
    }
  )
  files <- vapply(rec$files, function(f) f$key %||% "", character(1L))
  if (!.wfo_backbone_asset %in% files) {
    stop(sprintf("Zenodo record %s carries no %s.", rec$id,
                 .wfo_backbone_asset), call. = FALSE)
  }

  out <- list(
    record  = as.character(rec$id),
    edition = rec$metadata$version %||% "",
    version = release_version_from_date(rec$metadata$version %||% ""),
    url     = sprintf("https://zenodo.org/records/%s/files/%s", rec$id,
                      .wfo_backbone_asset)
  )
  if (verbose) {
    message(sprintf("Latest WFO Plant List: %s (Zenodo record %s)",
                    out$edition, out$record))
  }
  out
}


#' Download and extract the WFO classification file
#'
#' @param dest Character. Destination directory.
#' @param verbose Logical.
#' @param url Character or NULL. Backbone archive URL; the newest edition's,
#'   from [wfo_latest_edition()], when `NULL`.
#' @return Path to the extracted classification file.
#' @export
download_wfo <- function(dest = tempdir(), verbose = TRUE, url = NULL) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  url <- url %||% wfo_latest_edition(verbose = verbose)$url

  if (verbose) {
    message("Downloading WFO backbone from Zenodo (~120 MB)...")
    message(sprintf("  URL: %s", url))
  }
  zip_path <- download_curl_file(url, dest, "wfo_download.zip")

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


#' Feed the WFO classification file a block of raw rows at a time
#'
#' The one definition of how the file is parsed, used by both [read_wfo()] and
#' [build_wfo()]: tab separated, double-quoted fields, an empty field is `NA`,
#' and the bytes are UTF-8. The file is UTF-8 throughout (`Bañares` is stored as
#' `42 61 c3 b1 61 72 65 73`), so decoding it as anything else turns every
#' non-ASCII character into two.
#'
#' @param txt_path Character. Path to the WFO classification file.
#' @param normalize Function applied to each raw block.
#' @param chunk_rows Integer. Rows per block.
#' @param verbose Logical.
#' @return A feed function, as from [delim_chunk_feed()].
#' @noRd
wfo_feed <- function(txt_path, normalize, chunk_rows = 500000L,
                     verbose = TRUE) {
  delim_chunk_feed(txt_path, normalize = normalize, chunk_rows = chunk_rows,
                   quote = "\"", na_strings = "", file_encoding = "UTF-8",
                   verbose = verbose)
}


#' Read and normalize the WFO classification file
#'
#' @param txt_path Character. Path to the WFO classification.txt file.
#' @param verbose Logical.
#' @return A normalized data.frame ready for [precompute_backbone()].
#' @export
read_wfo <- function(txt_path, verbose = TRUE) {
  if (verbose) message("Reading classification file...")
  feed <- wfo_feed(txt_path, normalize = identity, verbose = FALSE)
  blocks <- list()
  repeat {
    block <- feed()
    if (is.null(block)) break
    blocks[[length(blocks) + 1L]] <- block
  }
  df <- do.call(rbind, blocks)
  if (verbose) message(sprintf("  %s rows", format(nrow(df), big.mark = ",")))
  normalize_wfo(df, verbose = verbose)
}


#' Normalize one block of WFO rows to the unified schema
#'
#' Split out of [read_wfo()] so the streaming build can apply it to a chunk at a
#' time. Nothing here depends on rows outside the block.
#'
#' @param df A data.frame of raw WFO rows, with the file's own column names,
#'   decoded from UTF-8.
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

  text_cols <- intersect(
    c("scientificName", "family", "genus", "specificEpithet",
      "scientificNameAuthorship"),
    names(df)
  )
  for (col in text_cols) {
    df[[col]] <- trimws(df[[col]])
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
#' @param version Character or NULL. Defaults to the edition
#'   [wfo_latest_edition()] resolves.
#' @param verbose Logical.
#' @return Path to the .vtr file (invisibly).
#' @export
build_wfo <- function(output_dir = "output/wfo", version = NULL,
                      verbose = TRUE) {
  edition <- wfo_latest_edition(verbose = verbose)
  if (is.null(version)) version <- edition$version

  tmp <- tempfile("wfo_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  txt_path <- download_wfo(dest = tmp, verbose = verbose, url = edition$url)

  # classification.csv inflates to roughly 900 MB, so it is staged a block at a
  # time rather than assembled in memory, parsed exactly as read_wfo() parses it.
  vtr_path <- file.path(output_dir, "wfo.vtr")
  build_vtr_streamed(
    wfo_feed(txt_path,
             normalize = function(chunk) normalize_wfo(chunk, verbose = FALSE),
             verbose = verbose),
    vtr_path, "wfo", version, edition$url, verbose = verbose
  )

  invisible(vtr_path)
}
