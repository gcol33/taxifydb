# COL XR (Catalogue of Life Extended Release): flat DwC-A -> data.frame -> .vtr
#
# The Extended Release builds on the COL Base Release by programmatically
# integrating additional taxonomic and nomenclatural sources through
# ChecklistBank, and is the taxonomy GBIF.org serves by default. It is
# published monthly, each release under its own ChecklistBank dataset key, so
# the release to build is resolved from the ChecklistBank API rather than
# pinned to a fixed URL.
#
# The DwC-A export is a single flat TSV carrying dwc:/clb: namespace prefixes
# on the column names (stripped on read). Three things differ from the Base
# Release read by read_col():
#
#   scientificName is canonical. Authorship sits in its own column and is not
#   repeated in the name, so no authorship subtraction is needed.
#
#   The higher classification arrives denormalized on every row, so the
#   parent-tree propagation col_resolve_classification() performs for the Base
#   Release (which ships those Darwin Core columns empty) has nothing to do.
#
#   There are no epithet columns, so the specific and infraspecific epithets
#   are split off the canonical name against the genus column.
#
# Identifiers are alphanumeric (CRLT8, G7PX), not the integers the legacy GBIF
# backbone used.

.colxr_api_base <- "https://api.checklistbank.org"

# Alias form of a Catalogue of Life Extended Release, e.g. "COL26.7 XR".
# ChecklistBank also serves xrelease datasets contributed by other projects,
# which this pattern leaves out.
.colxr_alias_pattern <- "^COL[0-9]+\\.[0-9]+ XR$"

# Columns needed for matching (after stripping namespace prefixes)
.colxr_match_cols <- c(
  "taxonID",
  "parentNameUsageID",
  "acceptedNameUsageID",
  "taxonomicStatus",
  "taxonRank",
  "scientificName",
  "scientificNameAuthorship",
  "kingdom",
  "phylum",
  "class",
  "order",
  "family",
  "genus"
)

# Extra columns preserved in the .vtr for runtime consumers
.colxr_extra_cols <- c(
  "superfamily",
  "subfamily",
  "tribe",
  "subtribe",
  "subgenus",
  "higherClassification",
  "taxGroup"
)


#' Resolve the latest Catalogue of Life Extended Release
#'
#' Queries ChecklistBank for the most recently issued COL XR dataset. Each
#' monthly release carries its own dataset key, so the key is looked up rather
#' than hard-coded.
#'
#' @param verbose Logical.
#' @return A list with `key`, `alias`, `version` and `issued`.
#' @export
colxr_latest_release <- function(verbose = TRUE) {
  url <- paste0(.colxr_api_base, "/dataset?origin=xrelease&limit=200")
  txt <- tryCatch(
    paste(readLines(url, warn = FALSE), collapse = ""),
    error = function(e) {
      stop("Could not reach ChecklistBank to resolve the latest COL XR: ",
           conditionMessage(e), call. = FALSE)
    }
  )
  res <- jsonlite::fromJSON(txt, simplifyDataFrame = FALSE)$result
  keep <- Filter(function(d) {
    !is.null(d$alias) && grepl(.colxr_alias_pattern, d$alias)
  }, res)
  if (length(keep) == 0L) {
    stop("No Catalogue of Life Extended Release found on ChecklistBank.",
         call. = FALSE)
  }

  issued <- vapply(keep, function(d) d$issued %||% "", character(1L))
  best <- keep[[which.max(as.Date(issued))]]

  out <- list(
    key     = as.character(best$key),
    alias   = best$alias,
    version = best$version %||% best$issued,
    issued  = best$issued
  )
  if (verbose) {
    message(sprintf("Latest COL XR: %s (issued %s, dataset key %s)",
                    out$alias, out$issued, out$key))
  }
  out
}


#' Download and extract the COL XR Darwin Core Archive
#'
#' @param dest Character. Destination directory.
#' @param key Character or NULL. ChecklistBank dataset key. Resolved from
#'   [colxr_latest_release()] when `NULL`.
#' @param verbose Logical.
#' @return Path to the extracted TSV.
#' @export
download_colxr <- function(dest = tempdir(), key = NULL, verbose = TRUE) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  if (is.null(key)) key <- colxr_latest_release(verbose = verbose)$key

  url <- colxr_export_url(key)
  if (verbose) {
    message("Downloading COL XR from ChecklistBank (~140 MB)...")
    message(sprintf("  URL: %s", url))
  }
  zip_path <- download_curl_file(url, dest, "colxr_download.zip")

  entries <- utils::unzip(zip_path, list = TRUE)$Name
  target <- entries[grepl("\\.tsv$", entries)]
  if (length(target) == 0L) {
    stop("No .tsv found in the COL XR archive.", call. = FALSE)
  }
  if (verbose) message(sprintf("Extracting %s ...", basename(target[1L])))
  utils::unzip(zip_path, files = target[1L], exdir = dest, junkpaths = TRUE)

  unlink(zip_path)
  file.path(dest, basename(target[1L]))
}


#' ChecklistBank export URL for one dataset key
#'
#' @param key Character. ChecklistBank dataset key.
#' @return The export URL. It redirects to the generated archive, which
#'   [download_curl_file()] follows.
#' @noRd
colxr_export_url <- function(key) {
  sprintf("%s/dataset/%s/export.zip?format=DwCA", .colxr_api_base, key)
}


#' Read and normalize the COL XR export
#'
#' @param tsv_path Character. Path to the extracted COL XR TSV.
#' @param verbose Logical.
#' @return A normalized data.frame ready for [precompute_backbone()].
#' @export
read_colxr <- function(tsv_path, verbose = TRUE) {
  if (!file.exists(tsv_path)) {
    stop("COL XR TSV not found: ", tsv_path, call. = FALSE)
  }

  if (verbose) message("Reading COL XR export...")
  # quote = "" keeps genuine embedded double-quotes in informal names literal;
  # ChecklistBank ships the export without field-wrapping quotes.
  if (requireNamespace("data.table", quietly = TRUE)) {
    df <- as.data.frame(
      data.table::fread(tsv_path, sep = "\t", quote = "",
                        na.strings = c("", "NA"), encoding = "UTF-8",
                        colClasses = "character", showProgress = FALSE),
      stringsAsFactors = FALSE
    )
  } else {
    df <- utils::read.delim(tsv_path, quote = "", comment.char = "",
                            stringsAsFactors = FALSE, na.strings = "",
                            fileEncoding = "UTF-8", check.names = FALSE,
                            colClasses = "character")
  }
  if (verbose) message(sprintf("  %s rows", format(nrow(df), big.mark = ",")))
  normalize_colxr(df, verbose = verbose)
}


#' Normalize one block of COL XR rows to the unified schema
#'
#' Split out of [read_colxr()] so the streaming build can apply it to a chunk
#' at a time. Nothing here depends on rows outside the block.
#'
#' @param df A data.frame of raw COL XR rows, with the export's own column
#'   names.
#' @param verbose Logical.
#' @return A normalized data.frame.
#' @export
normalize_colxr <- function(df, verbose = TRUE) {
  # Strip namespace prefixes (dwc:taxonID -> taxonID, clb:taxGroup -> taxGroup)
  names(df) <- sub("^[a-z]+:", "", names(df))

  keep <- intersect(c(.colxr_match_cols, .colxr_extra_cols), names(df))
  df <- df[, keep, drop = FALSE]

  text_cols <- intersect(
    c("scientificName", "scientificNameAuthorship", "kingdom", "phylum",
      "class", "order", "family", "genus"),
    names(df)
  )
  for (col in text_cols) df[[col]] <- trimws(df[[col]])

  if (verbose) message("Splitting epithets off the canonical name...")
  ep <- split_scientific_name(df$scientificName, df$genus)
  df$specificEpithet <- ep$specific
  df$infraspecificEpithet <- ep$infraspecific

  if (verbose) message("Normalizing to unified schema...")
  col_map <- list(
    taxon_id                = "taxonID",
    canonical_name          = "scientificName",
    taxon_rank              = "taxonRank",
    taxonomic_status        = "taxonomicStatus",
    accepted_name_usage_id  = "acceptedNameUsageID",
    kingdom                 = "kingdom",
    phylum                  = "phylum",
    class                   = "class",
    order                   = "order",
    family                  = "family",
    genus                   = "genus",
    specific_epithet        = "specificEpithet",
    authorship              = "scientificNameAuthorship",
    infraspecific_epithet   = "infraspecificEpithet"
  )

  extra_cols <- list()
  for (col in c(.colxr_extra_cols, "parentNameUsageID")) {
    if (col %in% names(df)) extra_cols[[col]] <- col
  }

  normalize_backbone(df, col_map, extra_cols)
}


#' Build the COL XR backbone .vtr
#'
#' @param output_dir Character. Output directory.
#' @param version Character or NULL. The COL XR release version, resolved from
#'   ChecklistBank when `NULL` so the stamped version is the date of the data
#'   rather than the date of the build.
#' @param verbose Logical.
#' @return Path to the .vtr file (invisibly).
#' @export
build_colxr <- function(output_dir = "output/colxr", version = NULL,
                        verbose = TRUE) {
  release <- colxr_latest_release(verbose = verbose)
  if (is.null(version)) version <- release$version

  tmp <- tempfile("colxr_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  tsv_path <- download_colxr(dest = tmp, key = release$key, verbose = verbose)

  # The export inflates to a multi-gigabyte TSV, so it is staged a block at a
  # time rather than assembled in memory. COL uses MISAPPLIED alongside
  # ACCEPTED/SYNONYM, and both flavours of synonym are treated as synonyms for
  # matching, exactly as for the Base Release. PROVISIONALLY ACCEPTED stays an
  # accepted concept.
  vtr_path <- file.path(output_dir, "colxr.vtr")
  build_vtr_streamed(
    delim_chunk_feed(tsv_path,
                     normalize = function(chunk) {
                       normalize_colxr(chunk, verbose = FALSE)
                     },
                     verbose = verbose),
    vtr_path, "colxr", version, colxr_export_url(release$key),
    synonym_pattern = "SYNONYM|MISAPPLIED",
    verbose = verbose
  )

  invisible(vtr_path)
}
