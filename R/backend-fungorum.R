# Species Fungorum Plus: ColDP archive -> normalized data.frame -> .vtr
#
# Curated fungal taxonomy (~329k names, 95% complete, CC BY). Source: GBIF
# ChecklistBank dataset 2073, served as ColDP (Catalogue of Life Data
# Package). The /archive endpoint returns the cached DwC-A-style zip
# (the /export endpoint returns a JSON tree, not a downloadable archive).
#
# ColDP layout used here:
#   - Taxon.tsv: accepted rows (ID, parentID, nameID, family, genus, ...)
#   - Name.tsv: scientific names (ID, scientificName, rank, specificEpithet,
#     infraspecificEpithet, authorship, genus, ...)
#   - Synonym.tsv: (taxonID, nameID) pairs — synonym name -> accepted taxon

.fungorum_url <- "https://api.checklistbank.org/dataset/2073/archive"
.fungorum_version_default <- "2025.04"

# ColDP rank token -> unified rank vocabulary. Anything below subspecies that
# we don't explicitly map (Greek letters, digit codes, asterisks, single
# letters) is collapsed to INFRASPECIFIC.
.fungorum_rank_map <- c(
  "sp."        = "SPECIES",
  "sp"         = "SPECIES",
  "subsp."     = "SUBSPECIES",
  "var."       = "VARIETY",
  "var"        = "VARIETY",
  "subvar."    = "SUBVARIETY",
  "f."         = "FORM",
  "f"          = "FORM",
  "subf."      = "SUBFORM",
  "subsubf."   = "SUBFORM",
  "f.sp."      = "FORMA SPECIALIS",
  "subgen."    = "SUBGENUS",
  "sect."      = "SECTION",
  "subsect."   = "SUBSECTION",
  "ser."       = "SERIES",
  "nothosp."   = "NOTHOSPECIES",
  "[unranked]" = "UNRANKED"
)


#' Download and extract the Species Fungorum Plus ColDP archive
#'
#' @param dest Character. Destination directory.
#' @param verbose Logical.
#' @return Path to the directory containing the extracted ColDP files.
#' @export
download_fungorum <- function(dest = tempdir(), verbose = TRUE) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)

  zip_path <- file.path(dest, "fungorum_coldp.zip")

  if (verbose) {
    message("Downloading Species Fungorum Plus from ChecklistBank...")
    message(sprintf("  URL: %s", .fungorum_url))
  }
  utils::download.file(.fungorum_url, zip_path, mode = "wb",
                       quiet = !verbose)

  if (verbose) message("Extracting ColDP files...")
  needed <- c("Taxon.tsv", "Name.tsv", "Synonym.tsv")
  utils::unzip(zip_path, files = needed, exdir = dest, junkpaths = TRUE)

  unlink(zip_path)
  dest
}


#' Read and normalize the Species Fungorum Plus ColDP files
#'
#' @param fungorum_dir Character. Directory containing Taxon.tsv, Name.tsv,
#'   Synonym.tsv.
#' @param verbose Logical.
#' @return A normalized data.frame.
#' @export
read_fungorum <- function(fungorum_dir, verbose = TRUE) {
  read_coldp <- function(name) {
    path <- file.path(fungorum_dir, name)
    if (!file.exists(path)) {
      stop(sprintf("%s not found in %s", name, fungorum_dir), call. = FALSE)
    }
    utils::read.delim(
      path,
      fileEncoding = "UTF-8",
      stringsAsFactors = FALSE,
      quote = "",
      na.strings = "",
      check.names = FALSE
    )
  }

  if (verbose) message("Reading Taxon.tsv ...")
  taxon <- read_coldp("Taxon.tsv")
  if (verbose) message("Reading Name.tsv ...")
  name <- read_coldp("Name.tsv")
  if (verbose) message("Reading Synonym.tsv ...")
  synonym <- read_coldp("Synonym.tsv")

  if (verbose) {
    message(sprintf("  taxa=%s  names=%s  synonyms=%s",
                    format(nrow(taxon), big.mark = ","),
                    format(nrow(name), big.mark = ","),
                    format(nrow(synonym), big.mark = ",")))
    message("Building unified frame (Taxon ⋈ Name, Synonym ⋈ Name ⋈ Taxon)...")
  }
  df <- fungorum_build_unified(taxon, name, synonym)

  if (verbose) message("Normalizing to unified schema...")
  col_map <- list(
    taxon_id                = "taxon_id",
    canonical_name          = "canonical_name",
    taxon_rank              = "taxon_rank",
    taxonomic_status        = "taxonomic_status",
    accepted_name_usage_id  = "accepted_name_usage_id",
    family                  = "family",
    genus                   = "genus",
    specific_epithet        = "specific_epithet",
    authorship              = "authorship",
    infraspecific_epithet   = "infraspecific_epithet"
  )

  normalize_backbone(df, col_map)
}


#' Build the Species Fungorum Plus backbone .vtr
#'
#' @param output_dir Character.
#' @param version Character or NULL.
#' @param verbose Logical.
#' @return Path to the .vtr file (invisibly).
#' @export
build_fungorum <- function(output_dir = "output/fungorum", version = NULL,
                           verbose = TRUE) {
  if (is.null(version)) version <- .fungorum_version_default

  tmp <- tempfile("fungorum_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  fungorum_dir <- download_fungorum(dest = tmp, verbose = verbose)
  df <- read_fungorum(fungorum_dir, verbose = verbose)

  if (verbose) message("Precomputing keys and embedding synonyms...")
  df <- precompute_backbone(df)

  vtr_path <- file.path(output_dir, "fungorum.vtr")
  build_vtr(df, vtr_path, "fungorum", version, .fungorum_url)

  invisible(vtr_path)
}


#' Build the unified backbone data.frame from ColDP Taxon/Name/Synonym tables
#'
#' Fungorum's archive is ColDP (not DwC-A): Taxon rows hold denormalized
#' higher classification but reference their scientific name via
#' `nameID` -> Name.ID. Synonym rows link an accepted Taxon (`taxonID`)
#' to a synonym Name (`nameID`).
#'
#' @noRd
fungorum_build_unified <- function(taxon, name, synonym) {
  name_idx <- match(taxon$nameID, name$ID)
  if (anyNA(name_idx)) {
    n_missing <- sum(is.na(name_idx))
    warning(sprintf("Fungorum: %d Taxon rows reference unknown nameID",
                    n_missing), call. = FALSE)
  }

  acc_canonical <- name$scientificName[name_idx]
  acc_rank_raw  <- tolower(name$rank[name_idx])
  acc_rank      <- unname(.fungorum_rank_map[acc_rank_raw])
  acc_rank[is.na(acc_rank) & !is.na(acc_rank_raw)] <- "INFRASPECIFIC"

  accepted <- data.frame(
    taxon_id                = as.character(taxon$ID),
    canonical_name          = trimws(acc_canonical),
    taxon_rank              = acc_rank,
    taxonomic_status        = "ACCEPTED",
    accepted_name_usage_id  = NA_character_,
    family                  = trimws(taxon$family),
    genus                   = trimws(taxon$genus),
    specific_epithet        = trimws(name$specificEpithet[name_idx]),
    authorship              = trimws(name$authorship[name_idx]),
    infraspecific_epithet   = trimws(name$infraspecificEpithet[name_idx]),
    stringsAsFactors        = FALSE
  )

  syn_name_idx <- match(synonym$nameID, name$ID)
  syn_taxon_idx <- match(synonym$taxonID, taxon$ID)

  if (anyNA(syn_name_idx) || anyNA(syn_taxon_idx)) {
    bad <- sum(is.na(syn_name_idx) | is.na(syn_taxon_idx))
    warning(sprintf("Fungorum: %d Synonym rows have unresolved name/taxon refs",
                    bad), call. = FALSE)
  }

  syn_canonical <- name$scientificName[syn_name_idx]
  syn_rank_raw  <- tolower(name$rank[syn_name_idx])
  syn_rank      <- unname(.fungorum_rank_map[syn_rank_raw])
  syn_rank[is.na(syn_rank) & !is.na(syn_rank_raw)] <- "INFRASPECIFIC"

  synonyms <- data.frame(
    taxon_id                = paste0("syn_", as.character(synonym$taxonID),
                                     "_", as.character(synonym$nameID)),
    canonical_name          = trimws(syn_canonical),
    taxon_rank              = syn_rank,
    taxonomic_status        = "SYNONYM",
    accepted_name_usage_id  = as.character(synonym$taxonID),
    family                  = trimws(taxon$family[syn_taxon_idx]),
    genus                   = trimws(name$genus[syn_name_idx]),
    specific_epithet        = trimws(name$specificEpithet[syn_name_idx]),
    authorship              = trimws(name$authorship[syn_name_idx]),
    infraspecific_epithet   = trimws(name$infraspecificEpithet[syn_name_idx]),
    stringsAsFactors        = FALSE
  )

  out <- rbind(accepted, synonyms)
  out <- out[!is.na(out$canonical_name) & nzchar(out$canonical_name), ]
  rownames(out) <- NULL
  out
}
