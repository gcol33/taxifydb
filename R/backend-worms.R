# WoRMS: DwC-A -> normalized data.frame -> .vtr
#
# WoRMS (World Register of Marine Species) publishes a DwC-A via GBIF
# ChecklistBank. The archive contains Taxon.tsv (core taxon records) and
# SpeciesProfile.tsv (habitat flags: marine/brackish/freshwater/terrestrial).
#
# Specifics: taxonomicStatus uses "accepted"/"unaccepted" (non-standard DwC);
# taxonID may be an LSID (urn:lsid:marinespecies.org:taxname:NNNNN);
# scientificName includes authorship (strip to get canonical);
# classification columns (kingdom, family, genus) are denormalized.

.worms_url <- "https://api.checklistbank.org/dataset/2011/archive"


#' Download and extract WoRMS DwC-A
#'
#' @param dest Character. Destination directory.
#' @param verbose Logical.
#' @return Path to the extracted DwC-A directory.
#' @export
download_worms <- function(dest = tempdir(), verbose = TRUE) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)

  zip_path <- file.path(dest, "worms_dwca.zip")

  if (verbose) message("Downloading WoRMS DwC-A from ChecklistBank (~308 MB)...")
  h <- curl::new_handle(connecttimeout = 60, low_speed_limit = 1000,
                        low_speed_time = 300)
  curl::curl_download(.worms_url, zip_path, handle = h, quiet = !verbose)

  if (verbose) message("Extracting...")
  txt_files <- utils::unzip(zip_path, list = TRUE)$Name

  taxon_target <- txt_files[grepl("Taxon\\.tsv$|taxon\\.txt$",
                                  txt_files, ignore.case = TRUE)]
  if (length(taxon_target) == 0L) {
    stop("Taxon file not found in WoRMS DwC-A archive.")
  }
  utils::unzip(zip_path, files = taxon_target[1L], exdir = dest,
               junkpaths = TRUE)

  sp_target <- txt_files[grepl("SpeciesProfile|speciesprofile",
                               txt_files, ignore.case = TRUE)]
  if (length(sp_target) > 0L) {
    utils::unzip(zip_path, files = sp_target[1L], exdir = dest,
                 junkpaths = TRUE)
  }

  unlink(zip_path)
  dest
}


#' Read and normalize the WoRMS taxonomy
#'
#' @param worms_dir Character. Path to the extracted DwC-A directory.
#' @param verbose Logical.
#' @return A normalized data.frame.
#' @export
read_worms <- function(worms_dir, verbose = TRUE) {
  taxon_file <- worms_taxon_file(worms_dir)

  if (verbose) message("Reading WoRMS taxon data...")
  df <- read_worms_tsv(taxon_file)
  assert_worms_taxon_core(df)
  if (verbose) message(sprintf("  %s rows", format(nrow(df), big.mark = ",")))
  normalize_worms(df, verbose = verbose)
}


#' Read one WoRMS TSV
#'
#' ChecklistBank double-quotes its TSV fields, so they are parsed as quoted and
#' the wrapping quotes come off. Only `"` is a quoting character, not `'`, which
#' leaves the apostrophe of an authorship (d'Orbigny, O'Brien) alone.
#'
#' [utils::read.delim()] cannot read this file. It carries 4,626 newlines and 59
#' lone carriage returns inside quoted fields; R reads a lone `CR` as the end of
#' a line and, in text mode, loses bytes doing it, so 49 of the file's quote
#' characters go missing and the quoting stops balancing. `scan()` then reaches
#' a record it cannot close, keeps what it has and says so only in a warning --
#' on the 2026-08-01 archive it returned 1,363,240 of 1,562,065 rows, the last
#' few hundred of them filled with fragments of the citation that broke it
#' (gcol33/taxifydb#43). `fread()` reads the bytes and returns every record.
#'
#' What `fread()` does not do is unescape a doubled quote, so that is done after
#' it. The 11 names carrying a genuine quote (`Gyrodactylus barbatuli f. "A"`)
#' come back with one, and no field keeps the wrapping quotes that broke every
#' marine enrichment join before worms-2026.07.
#'
#' @param path Character. Path to the TSV.
#' @return A data.frame of character columns.
#' @noRd
read_worms_tsv <- function(path) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required to read the WoRMS archive.",
         call. = FALSE)
  }
  df <- data.table::fread(
    path, sep = "\t", quote = "\"", na.strings = "",
    # Types fixed rather than inferred, so every read agrees on them.
    colClasses = "character", showProgress = FALSE, data.table = FALSE
  )
  unescape_quotes(df, "\"")
}


#' Check that a WoRMS taxon core identifies its rows
#'
#' A taxon core names every row exactly once, so a missing or repeated
#' `taxonID` means the file was not read as its records. That is what a reader
#' stopping partway through looks like from the inside: the rows it did return
#' stay well formed, and only the identifiers show that a record was cut in two.
#'
#' @param df A data.frame of raw WoRMS rows.
#' @return `df`, invisibly.
#' @noRd
assert_worms_taxon_core <- function(df) {
  if (nrow(df) == 0L) {
    stop("The WoRMS taxon file parsed to no rows.", call. = FALSE)
  }
  n_bad <- sum(is.na(df$taxonID) | !nzchar(df$taxonID))
  n_dup <- anyDuplicated(df$taxonID)
  if (n_bad > 0L || n_dup > 0L) {
    stop(sprintf(
      "The WoRMS taxon core does not identify its rows: %s without a taxonID, %s repeated. The file was not read as its records.",
      format(n_bad, big.mark = ","),
      format(sum(duplicated(df$taxonID)), big.mark = ",")), call. = FALSE)
  }
  invisible(df)
}


#' Locate the taxon core of an extracted WoRMS DwC-A
#'
#' @param worms_dir Character. Path to the extracted archive.
#' @return Path to the taxon file.
#' @noRd
worms_taxon_file <- function(worms_dir) {
  taxon_files <- list.files(worms_dir,
                            pattern = "Taxon\\.tsv$|taxon\\.txt$",
                            ignore.case = TRUE, full.names = TRUE)
  if (length(taxon_files) == 0L) {
    stop("Taxon file not found in WoRMS directory.")
  }
  taxon_files[1L]
}


#' Normalize one block of WoRMS rows to the unified schema
#'
#' Split out of [read_worms()] so the streaming build can apply it to a chunk at
#' a time. Nothing here depends on rows outside the block.
#'
#' @param df A data.frame of raw WoRMS rows, with the archive's own column
#'   names.
#' @param verbose Logical.
#' @return A normalized data.frame.
#' @export
normalize_worms <- function(df, verbose = TRUE) {
  names(df) <- sub("^[a-z]+:", "", names(df))

  if ("taxonID" %in% names(df)) {
    is_lsid <- grepl("^urn:lsid:", df$taxonID, perl = TRUE)
    if (any(is_lsid)) {
      df$taxonID[is_lsid] <- sub("^.*:", "", df$taxonID[is_lsid])
    }
  }

  if ("acceptedNameUsageID" %in% names(df)) {
    is_lsid_acc <- !is.na(df$acceptedNameUsageID) &
      grepl("^urn:lsid:", df$acceptedNameUsageID, perl = TRUE)
    if (any(is_lsid_acc)) {
      df$acceptedNameUsageID[is_lsid_acc] <- sub(
        "^.*:", "", df$acceptedNameUsageID[is_lsid_acc])
    }
  }

  if (verbose) message("Building canonical names...")
  authorship <- if ("scientificNameAuthorship" %in% names(df)) {
    df$scientificNameAuthorship
  } else {
    NA_character_
  }

  df$canonicalName <- strip_authorship(df$scientificName, authorship)

  if (verbose) message("Mapping taxonomic status...")
  raw_status <- tolower(df$taxonomicStatus)
  df$taxonomicStatus <- ifelse(
    raw_status %in% c("accepted", "valid"),
    "ACCEPTED",
    "SYNONYM"
  )

  genus_col <- if ("genus" %in% names(df)) {
    df$genus
  } else if ("genericName" %in% names(df)) {
    df$genericName
  } else {
    sub(" .*", "", df$canonicalName)
  }
  df$genus <- genus_col

  if (!"specificEpithet" %in% names(df)) {
    words <- strsplit(df$canonicalName, " ", fixed = TRUE)
    df$specificEpithet <- vapply(words, function(w) {
      if (length(w) >= 2L) w[2L] else NA_character_
    }, character(1L))
  }

  if (verbose) message("Normalizing to unified schema...")
  col_map <- list(
    taxon_id                = "taxonID",
    canonical_name          = "canonicalName",
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
  if ("kingdom" %in% names(df)) extra_cols$kingdom <- "kingdom"
  if ("phylum" %in% names(df))  extra_cols$phylum  <- "phylum"
  if ("class" %in% names(df))   extra_cols$class   <- "class"
  if ("order" %in% names(df))   extra_cols$order   <- "order"

  normalize_backbone(df, col_map, extra_cols)
}


#' Build the WoRMS backbone .vtr
#'
#' Also writes the SpeciesProfile.tsv (habitat flags) as a separate
#' `worms_species_profile.vtr` next to the main backbone.
#'
#' @param output_dir Character.
#' @param version Character or NULL.
#' @param verbose Logical.
#' @return Path to the .vtr file (invisibly).
#' @export
build_worms <- function(output_dir = "output/worms", version = NULL,
                        verbose = TRUE) {
  if (is.null(version)) version <- format(Sys.Date(), "%Y.%m")

  tmp <- tempfile("worms_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  worms_dir <- download_worms(dest = tmp, verbose = verbose)
  taxon_file <- worms_taxon_file(worms_dir)

  # The taxon core is over a gigabyte unpacked, so it is staged a block at a
  # time rather than assembled in memory. The parsing arguments are the ones
  # read_worms() reads it with: tab separated, double-quoted fields, an empty
  # field is NA.
  vtr_path <- file.path(output_dir, "worms.vtr")
  build_vtr_streamed(
    delim_chunk_feed(taxon_file,
                     normalize = function(chunk) {
                       normalize_worms(chunk, verbose = FALSE)
                     },
                     quote = "\"", na_strings = "", verbose = verbose),
    vtr_path, "worms", version, .worms_url, verbose = verbose
  )

  sp_files <- list.files(tmp, pattern = "SpeciesProfile|speciesprofile",
                         ignore.case = TRUE, full.names = TRUE)
  if (length(sp_files) > 0L) {
    if (verbose) message("Converting SpeciesProfile to .vtr...")
    sp_df <- read_worms_tsv(sp_files[1L])
    names(sp_df) <- sub("^[a-z]+:", "", names(sp_df))
    sp_vtr <- file.path(output_dir, "worms_species_profile.vtr")
    vectra::write_vtr(sp_df, sp_vtr)
    if (verbose) message(sprintf("  SpeciesProfile: %s rows",
                                 format(nrow(sp_df), big.mark = ",")))
  }

  invisible(vtr_path)
}
