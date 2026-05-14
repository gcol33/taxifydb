# ---- WoRMS: DwC-A -> normalized data.frame -> .vtr ----
#
# WoRMS (World Register of Marine Species) publishes a DwC-A via GBIF
# ChecklistBank (dataset key 2d59e5db-57ad-41ff-97d6-11f5fb264527).
#
# The archive contains:
#   - Taxon.tsv: core taxon records (accepted + unaccepted)
#   - SpeciesProfile.tsv: habitat flags (marine/brackish/freshwater/terrestrial)
#
# WoRMS specifics:
#   - taxonomicStatus uses "accepted"/"unaccepted" (non-standard DwC)
#   - taxonID may be LSID (urn:lsid:marinespecies.org:taxname:NNNNN)
#   - scientificName includes authorship (strip to get canonical)
#   - Classification columns (kingdom, family, genus) are denormalized
#     — no hierarchy walk needed

source("shared/normalize.R")
source("shared/precompute.R")
source("shared/build.R")

.worms_url <- "https://api.checklistbank.org/dataset/2011/archive"


#' Download and extract WoRMS DwC-A
#'
#' @param dest Character. Destination directory.
#' @param verbose Logical.
#' @return Path to the extracted Taxon file.
download_worms <- function(dest = tempdir(), verbose = TRUE) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)

  zip_path <- file.path(dest, "worms_dwca.zip")

  if (verbose) message("Downloading WoRMS DwC-A from ChecklistBank (~308 MB)...")
  # Use curl for reliable download (archive endpoint is slow, default timeout
  # in download.file is too short)
  h <- curl::new_handle(connecttimeout = 60, low_speed_limit = 1000,
                        low_speed_time = 300)
  curl::curl_download(.worms_url, zip_path, handle = h, quiet = !verbose)

  if (verbose) message("Extracting...")
  txt_files <- utils::unzip(zip_path, list = TRUE)$Name

  # Find Taxon file
  taxon_target <- txt_files[grepl("Taxon\\.tsv$|taxon\\.txt$", txt_files,
                                  ignore.case = TRUE)]
  if (length(taxon_target) == 0L) {
    stop("Taxon file not found in WoRMS DwC-A archive.")
  }
  utils::unzip(zip_path, files = taxon_target[1L], exdir = dest,
               junkpaths = TRUE)

  # Also extract SpeciesProfile if present
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
#' @param worms_dir Character. Path to extracted DwC-A directory.
#' @param verbose Logical.
#' @return A normalized data.frame.
read_worms <- function(worms_dir, verbose = TRUE) {
  # ---- 1. Find and read Taxon file ----
  taxon_files <- list.files(worms_dir,
                            pattern = "Taxon\\.tsv$|taxon\\.txt$",
                            ignore.case = TRUE, full.names = TRUE)
  if (length(taxon_files) == 0L) {
    stop("Taxon file not found in WoRMS directory.")
  }

  if (verbose) message("Reading WoRMS taxon data...")
  df <- utils::read.delim(
    taxon_files[1L],
    fileEncoding = "UTF-8",
    stringsAsFactors = FALSE,
    quote = "",
    na.strings = "",
    check.names = FALSE
  )
  if (verbose) message(sprintf("  %s rows", format(nrow(df), big.mark = ",")))

  # Strip namespace prefixes (dwc:taxonID -> taxonID)
  names(df) <- sub("^[a-z]+:", "", names(df))

  # ---- 2. Extract numeric AphiaID from LSID ----
  if ("taxonID" %in% names(df)) {
    is_lsid <- grepl("^urn:lsid:", df$taxonID, perl = TRUE)
    if (any(is_lsid)) {
      df$taxonID[is_lsid] <- sub("^.*:", "", df$taxonID[is_lsid])
    }
  }

  # Same for acceptedNameUsageID
  if ("acceptedNameUsageID" %in% names(df)) {
    is_lsid_acc <- !is.na(df$acceptedNameUsageID) &
      grepl("^urn:lsid:", df$acceptedNameUsageID, perl = TRUE)
    if (any(is_lsid_acc)) {
      df$acceptedNameUsageID[is_lsid_acc] <- sub(
        "^.*:", "", df$acceptedNameUsageID[is_lsid_acc])
    }
  }

  # ---- 3. Build canonical name (strip authorship) ----
  if (verbose) message("Building canonical names...")
  authorship <- if ("scientificNameAuthorship" %in% names(df)) {
    df$scientificNameAuthorship
  } else {
    NA_character_
  }

  canonical <- df$scientificName
  has_both <- !is.na(canonical) & !is.na(authorship) & nzchar(authorship)
  if (any(has_both)) {
    sn <- canonical[has_both]
    au <- authorship[has_both]
    # Only strip when scientificName actually ends with the authorship.
    # The previous arithmetic strip (sn_len - au_len) silently truncated
    # to a tiny prefix when the suffix didn't match, producing 60k+ bogus
    # one-letter canonical_name values like "A", "B", ...
    suffix_match <- endsWith(sn, au)
    if (any(suffix_match)) {
      sn_match <- sn[suffix_match]
      au_match <- au[suffix_match]
      stripped <- trimws(substr(sn_match, 1L,
                                 nchar(sn_match) - nchar(au_match)))
      # Sanity guard: keep the original scientificName when the strip
      # produces something obviously degenerate (empty, single char, or
      # short with no space).
      degenerate <- !nzchar(stripped) |
                    (nchar(stripped) < 3L & !grepl(" ", stripped))
      stripped[degenerate] <- sn_match[degenerate]
      canonical[has_both][suffix_match] <- stripped
    }
  }
  df$canonicalName <- canonical

  # ---- 4. Map status ----
  # WoRMS uses "accepted"/"unaccepted" (lowercase, non-standard DwC)
  if (verbose) message("Mapping taxonomic status...")
  raw_status <- tolower(df$taxonomicStatus)
  df$taxonomicStatus <- ifelse(
    raw_status %in% c("accepted", "valid"),
    "ACCEPTED",
    "SYNONYM"
  )

  # ---- 5. Resolve genus if missing ----
  genus_col <- if ("genus" %in% names(df)) {
    df$genus
  } else if ("genericName" %in% names(df)) {
    df$genericName
  } else {
    sub(" .*", "", df$canonicalName)
  }
  df$genus <- genus_col

  # ---- 6. Parse epithet if missing ----
  if (!"specificEpithet" %in% names(df)) {
    words <- strsplit(df$canonicalName, " ", fixed = TRUE)
    df$specificEpithet <- vapply(words, function(w) {
      if (length(w) >= 2L) w[2L] else NA_character_
    }, character(1L))
  }

  # ---- 7. Normalize ----
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

  # Extra columns to preserve
  extra_cols <- list()
  if ("kingdom" %in% names(df)) extra_cols$kingdom <- "kingdom"
  if ("phylum" %in% names(df)) extra_cols$phylum <- "phylum"
  if ("class" %in% names(df)) extra_cols$class <- "class"
  if ("order" %in% names(df)) extra_cols$order <- "order"

  normalize_backbone(df, col_map, extra_cols)
}


#' Build the WoRMS backbone .vtr
#'
#' @param output_dir Character.
#' @param version Character or NULL.
#' @param verbose Logical.
#' @return Path to the .vtr file (invisibly).
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

  # Also convert SpeciesProfile if present
  sp_files <- list.files(tmp, pattern = "SpeciesProfile|speciesprofile",
                         ignore.case = TRUE, full.names = TRUE)
  if (length(sp_files) > 0L) {
    if (verbose) message("Converting SpeciesProfile to .vtr...")
    sp_df <- utils::read.delim(
      sp_files[1L],
      fileEncoding = "UTF-8",
      stringsAsFactors = FALSE,
      quote = "",
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


if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  output_dir <- if (length(args) >= 1L) args[1L] else "output/worms"
  version <- if (length(args) >= 2L) args[2L] else NULL
  build_worms(output_dir, version)
}
