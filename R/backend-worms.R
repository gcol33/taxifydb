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
  taxon_files <- list.files(worms_dir,
                            pattern = "Taxon\\.tsv$|taxon\\.txt$",
                            ignore.case = TRUE, full.names = TRUE)
  if (length(taxon_files) == 0L) {
    stop("Taxon file not found in WoRMS directory.")
  }

  if (verbose) message("Reading WoRMS taxon data...")
  # ChecklistBank double-quotes its TSV fields; parse them as quotes so the
  # surrounding quote characters are stripped. Only " is a quote (not '), so
  # apostrophes in authorship (d'Orbigny, O'Brien) stay intact.
  df <- utils::read.delim(
    taxon_files[1L],
    fileEncoding = "UTF-8",
    stringsAsFactors = FALSE,
    quote = "\"",
    na.strings = "",
    check.names = FALSE
  )
  if (verbose) message(sprintf("  %s rows", format(nrow(df), big.mark = ",")))

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
  df <- read_worms(worms_dir, verbose = verbose)

  if (verbose) message("Precomputing keys and embedding synonyms...")
  df <- precompute_backbone(df)

  vtr_path <- file.path(output_dir, "worms.vtr")
  build_vtr(df, vtr_path, "worms", version, .worms_url)

  sp_files <- list.files(tmp, pattern = "SpeciesProfile|speciesprofile",
                         ignore.case = TRUE, full.names = TRUE)
  if (length(sp_files) > 0L) {
    if (verbose) message("Converting SpeciesProfile to .vtr...")
    sp_df <- utils::read.delim(
      sp_files[1L],
      fileEncoding = "UTF-8",
      stringsAsFactors = FALSE,
      quote = "\"",
      na.strings = "",
      check.names = FALSE
    )
    names(sp_df) <- sub("^[a-z]+:", "", names(sp_df))
    sp_vtr <- file.path(output_dir, "worms_species_profile.vtr")
    vectra::write_vtr(sp_df, sp_vtr)
    if (verbose) message(sprintf("  SpeciesProfile: %s rows",
                                 format(nrow(sp_df), big.mark = ",")))
  }

  invisible(vtr_path)
}
