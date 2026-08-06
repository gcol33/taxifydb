# WCVP (World Checklist of Vascular Plants): wcvp_names.csv -> .vtr
#
# Kew publishes the WCVP as a pipe-delimited bulk download (wcvp.zip) containing
# wcvp_names.csv, wcvp_distribution.csv and a README. The names file is the
# taxonomic backbone: one row per name, with the canonical name already rendered
# in `taxon_name` (hybrid signs and infraspecific markers included) and
# authorship kept separately in `taxon_authors`, so no canonical-name extraction
# is needed.
#
# Acceptance is carried by `accepted_plant_name_id`: an Accepted / Artificial
# Hybrid / Local Biotype name points at itself, a Synonym / Illegitimate /
# Invalid / Misapplied / Orthographic name points at its accepted name, and an
# Unplaced name has no target. Deriving status from that link (rather than the
# nine `taxon_status` spellings) keeps the mapping to ACCEPTED/SYNONYM exact.
#
# WCVP quirk: 0.06% of names carry a genuine embedded double-quote (informal /
# provisional epithets like `f. "A"`). Kew does not field-wrap the pipe-
# delimited file, so those quotes are literal data -- the reader disables quote
# processing (quote = "") to keep them intact.

.wcvp_url <- "https://sftp.kew.org/pub/data-repositories/WCVP/wcvp.zip"
.wcvp_source_doi <- "10.1038/s41597-021-00997-6"
.wcvp_version_default <- "2026.06"

# Source columns read from wcvp_names.csv (a subset of the 31 available).
.wcvp_read_cols <- c(
  "plant_name_id", "taxon_rank", "taxon_status", "family", "genus",
  "species", "infraspecies", "taxon_name", "taxon_authors",
  "accepted_plant_name_id", "ipni_id", "powo_id",
  "lifeform_description", "climate_description", "geographic_area",
  "first_published"
)


#' Download and extract the WCVP names file
#'
#' @param dest Character. Destination directory.
#' @param verbose Logical.
#' @return Path to the extracted `wcvp_names.csv`.
#' @export
download_wcvp <- function(dest = tempdir(), verbose = TRUE) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)

  zip_path <- file.path(dest, "wcvp.zip")

  if (verbose) {
    message("Downloading WCVP from Kew (~85 MB)...")
    message(sprintf("  URL: %s", .wcvp_url))
  }
  h <- curl::new_handle(connecttimeout = 60, timeout = 1800)
  curl::curl_download(.wcvp_url, zip_path, handle = h, quiet = !verbose)

  if (verbose) message("Extracting names file...")
  entries <- utils::unzip(zip_path, list = TRUE)$Name
  names_target <- entries[grepl("wcvp_names\\.csv$", entries,
                                ignore.case = TRUE)]
  if (length(names_target) == 0L) {
    stop("wcvp_names.csv not found in WCVP archive.", call. = FALSE)
  }
  utils::unzip(zip_path, files = names_target[1L], exdir = dest,
               junkpaths = TRUE)

  unlink(zip_path)
  file.path(dest, basename(names_target[1L]))
}


#' Read and normalize the WCVP names file
#'
#' @param names_path Character. Path to `wcvp_names.csv`.
#' @param verbose Logical.
#' @return A normalized data.frame ready for [precompute_backbone()].
#' @export
read_wcvp <- function(names_path, verbose = TRUE) {
  if (verbose) message("Reading WCVP names...")
  # quote = "" keeps genuine embedded double-quotes in informal names literal;
  # Kew ships the pipe-delimited file without field-wrapping quotes.
  if (requireNamespace("data.table", quietly = TRUE)) {
    df <- as.data.frame(
      data.table::fread(names_path, sep = "|", quote = "",
                        select = .wcvp_read_cols, na.strings = c("", "NA"),
                        encoding = "UTF-8", showProgress = FALSE,
                        colClasses = list(character = c("plant_name_id",
                                                        "accepted_plant_name_id"))),
      stringsAsFactors = FALSE
    )
  } else {
    df <- utils::read.delim(names_path, sep = "|", quote = "",
                            comment.char = "", stringsAsFactors = FALSE,
                            na.strings = c("", "NA"), fileEncoding = "UTF-8",
                            check.names = FALSE)
    df <- df[, intersect(.wcvp_read_cols, names(df)), drop = FALSE]
    df$plant_name_id <- as.character(df$plant_name_id)
    df$accepted_plant_name_id <- as.character(df$accepted_plant_name_id)
  }
  if (verbose) message(sprintf("  %s rows", format(nrow(df), big.mark = ",")))
  normalize_wcvp(df, verbose = verbose)
}


#' Normalize one block of WCVP rows to the unified schema
#'
#' Split out of [read_wcvp()] so the streaming build can apply it to a chunk at
#' a time. Acceptance is read from a comparison of two columns of the same row,
#' so nothing here depends on rows outside the block.
#'
#' @param df A data.frame of raw WCVP rows.
#' @param verbose Logical.
#' @return A normalized data.frame ready for [precompute_backbone()].
#' @export
normalize_wcvp <- function(df, verbose = TRUE) {
  # Acceptance from the accepted_plant_name_id link: a name pointing at another
  # name is a synonym (of any WCVP flavour); a self-pointing or unplaced name is
  # its own accepted concept.
  id  <- df$plant_name_id
  acc <- df$accepted_plant_name_id
  is_syn <- !is.na(acc) & nzchar(acc) & acc != id

  df$taxonomic_status <- ifelse(is_syn, "SYNONYM", "ACCEPTED")
  df$accepted_name_usage_id <- ifelse(is_syn, acc, NA_character_)

  if (verbose) message("Normalizing to unified schema...")
  col_map <- list(
    taxon_id                = "plant_name_id",
    canonical_name          = "taxon_name",
    taxon_rank              = "taxon_rank",
    taxonomic_status        = "taxonomic_status",
    accepted_name_usage_id  = "accepted_name_usage_id",
    family                  = "family",
    genus                   = "genus",
    specific_epithet        = "species",
    authorship              = "taxon_authors",
    infraspecific_epithet   = "infraspecies"
  )

  extra_cols <- list()
  for (col in c("ipni_id", "powo_id", "lifeform_description",
                "climate_description", "geographic_area", "first_published")) {
    if (col %in% names(df)) extra_cols[[col]] <- col
  }

  normalize_backbone(df, col_map, extra_cols)
}


#' Build the WCVP backbone .vtr from source
#'
#' @param output_dir Character. Output directory.
#' @param version Character or NULL. Defaults to the bundled WCVP snapshot tag.
#' @param verbose Logical.
#' @return Path to the .vtr file (invisibly).
#' @export
build_wcvp <- function(output_dir = "output/wcvp", version = NULL,
                       verbose = TRUE) {
  if (is.null(version)) version <- .wcvp_version_default

  tmp <- tempfile("wcvp_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  names_path <- download_wcvp(dest = tmp, verbose = verbose)

  # Staged a block at a time rather than assembled in memory. The parsing
  # arguments match read_wcvp() exactly: quote = "" keeps the genuine embedded
  # double-quotes in informal names literal, since Kew ships the pipe-delimited
  # file without field-wrapping quotes.
  vtr_path <- file.path(output_dir, "wcvp.vtr")
  build_vtr_streamed(
    delim_chunk_feed(names_path,
                     normalize = function(chunk) {
                       normalize_wcvp(chunk, verbose = FALSE)
                     },
                     sep = "|", quote = "", encoding = "UTF-8",
                     select = .wcvp_read_cols, verbose = verbose),
    vtr_path, "wcvp", version, .wcvp_url, verbose = verbose
  )

  invisible(vtr_path)
}
