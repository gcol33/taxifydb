# Euro+Med PlantBase: EuroMed.csv -> normalized data.frame -> .vtr
#
# Euro+Med is the taxonomic reference for European and Mediterranean vascular
# plants (~133k taxa). Source: semicolon-delimited CSV, UUID-based IDs,
# 2020 v1.2 snapshot. License: CC-BY-SA-3.0 (applies to derived .vtr).
#
# Strengths: authoritative for European/Mediterranean flora; fine-grained
# infraspecific taxonomy. Used by EVA (European Vegetation Archive).

.euromed_url <- "https://germansl.infinitenature.org/EuroMed/version1/EuroMed.zip"
.euromed_version_default <- "2020.1"


#' Download and extract the Euro+Med CSV
#'
#' @param dest Character. Destination directory.
#' @param verbose Logical.
#' @return Path to the extracted CSV.
#' @export
download_euromed <- function(dest = tempdir(), verbose = TRUE) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)

  zip_path <- file.path(dest, "EuroMed.zip")

  if (verbose) {
    message("Downloading Euro+Med PlantBase (~8 MB)...")
    message(sprintf("  URL: %s", .euromed_url))
  }
  curl::curl_download(.euromed_url, zip_path, quiet = !verbose)

  if (verbose) message("Extracting...")
  utils::unzip(zip_path, exdir = dest)

  csv_files <- list.files(dest, pattern = "\\.csv$",
                          recursive = TRUE, full.names = TRUE)
  if (length(csv_files) == 0L) {
    stop("No .csv file found in Euro+Med download.", call. = FALSE)
  }

  unlink(zip_path)
  csv_files[1L]
}


#' Read and normalize the Euro+Med CSV
#'
#' Resolves the parent-child hierarchy for family/genus, maps synonym
#' relationships via TaxonConceptID, and produces the unified backbone schema.
#'
#' @param csv_path Character. Path to the EuroMed.csv file.
#' @param verbose Logical.
#' @return A normalized data.frame.
#' @export
read_euromed <- function(csv_path, verbose = TRUE) {
  if (verbose) message("Reading Euro+Med CSV...")
  df <- tryCatch(
    utils::read.csv(csv_path, sep = ";", stringsAsFactors = FALSE,
                    fileEncoding = "UTF-8"),
    error = function(e) {
      utils::read.csv(csv_path, sep = ";", stringsAsFactors = FALSE,
                      fileEncoding = "latin1")
    }
  )
  if (verbose) message(sprintf("  %s rows", format(nrow(df), big.mark = ",")))

  # ---- Map status ----
  # Taxon -> ACCEPTED; Synonym, Misapplication, p.p. Synonym -> SYNONYM
  df$taxonomic_status <- ifelse(df$status == "Taxon", "ACCEPTED", "SYNONYM")

  # ---- Synonym resolution via TaxonConceptID ----
  df$accepted_name_usage_id <- ifelse(
    df$taxonomic_status == "SYNONYM",
    df$TaxonConceptID,
    NA_character_
  )

  # ---- Extract authorship ----
  # fullname = canonical + authorship. For infraspecific autonyms, the species
  # authorship is inserted mid-name, so TaxonName isn't a substring of fullname.
  infra_markers <- c("subsp.", "var.", "f.", "nothosubsp.", "subvar.",
                     "convar.", "proles", "race", "grex", "subf.")
  marker_re <- paste0("\\b(",
                      paste(gsub("\\.", "\\\\.", infra_markers),
                            collapse = "|"),
                      ")\\s+\\S+")

  df$authorship <- vapply(seq_len(nrow(df)), function(i) {
    fn <- df$fullname[i]
    tn <- df$TaxonName[i]
    if (is.na(fn) || !nzchar(fn)) return(NA_character_)
    auth <- trimws(sub(tn, "", fn, fixed = TRUE))
    if (nzchar(auth) && auth != fn) return(auth)
    m <- gregexpr(marker_re, fn, perl = TRUE)[[1L]]
    if (m[1L] == -1L) return(NA_character_)
    last_end <- m[length(m)] + attr(m, "match.length")[length(m)] - 1L
    trimws(substring(fn, last_end + 1L))
  }, character(1L))
  df$authorship[!nzchar(df$authorship)] <- NA_character_

  # ---- Parse epithets from TaxonName ----
  words <- strsplit(df$TaxonName, "\\s+")
  df$specific_epithet <- vapply(words, function(w) {
    if (length(w) >= 2L) w[2L] else NA_character_
  }, character(1L))

  rank_markers <- c("subsp.", "var.", "f.", "nothosubsp.", "subvar.",
                    "convar.", "proles", "race", "grex")
  df$infraspecific_epithet <- vapply(words, function(w) {
    if (length(w) < 3L) return(NA_character_)
    marker_pos <- which(w %in% rank_markers)
    if (length(marker_pos) > 0L && marker_pos[1L] < length(w)) {
      w[marker_pos[1L] + 1L]
    } else if (length(w) >= 3L) {
      w[3L]
    } else {
      NA_character_
    }
  }, character(1L))

  # ---- Hierarchy walk for family/genus ----
  if (verbose) message("Walking hierarchy for family/genus...")

  id <- df$TaxonUsageID
  parent_id <- ifelse(
    !is.na(df$IsChildTaxonOfID) & nzchar(df$IsChildTaxonOfID),
    df$IsChildTaxonOfID,
    NA_character_
  )
  rank_lower <- tolower(df$TaxonRank)

  parent_row <- match(parent_id, id)

  family <- ifelse(rank_lower == "family", df$TaxonName, NA_character_)
  genus <- ifelse(rank_lower == "genus", df$TaxonName, NA_character_)

  current_parent <- parent_row
  for (depth in seq_len(20L)) {
    needs_family <- is.na(family) & !is.na(current_parent)
    needs_genus <- is.na(genus) & !is.na(current_parent)

    if (!any(needs_family) && !any(needs_genus)) break

    if (any(needs_family)) {
      is_family <- rank_lower[current_parent[needs_family]] == "family"
      match_idx <- which(needs_family)[is_family]
      if (length(match_idx) > 0L) {
        family[match_idx] <- df$TaxonName[current_parent[match_idx]]
      }
    }

    if (any(needs_genus)) {
      is_genus <- rank_lower[current_parent[needs_genus]] == "genus"
      match_idx <- which(needs_genus)[is_genus]
      if (length(match_idx) > 0L) {
        genus[match_idx] <- df$TaxonName[current_parent[match_idx]]
      }
    }

    next_parent <- rep(NA_integer_, nrow(df))
    has_p <- !is.na(current_parent)
    next_parent[has_p] <- match(parent_id[current_parent[has_p]], id)
    current_parent <- next_parent
  }

  # For synonyms: inherit family/genus from their accepted taxon
  is_syn <- df$taxonomic_status == "SYNONYM" &
    !is.na(df$accepted_name_usage_id)
  acc_row <- match(df$accepted_name_usage_id[is_syn], id)
  has_acc <- !is.na(acc_row)
  syn_idx <- which(is_syn)[has_acc]
  acc_idx <- acc_row[has_acc]

  needs_fam <- is.na(family[syn_idx])
  if (any(needs_fam)) family[syn_idx[needs_fam]] <- family[acc_idx[needs_fam]]
  needs_gen <- is.na(genus[syn_idx])
  if (any(needs_gen)) genus[syn_idx[needs_gen]] <- genus[acc_idx[needs_gen]]

  # Fallback: parse genus from first word of TaxonName for species-rank rows
  # where the hierarchy walk failed (Section/Tribe nodes break the chain).
  species_ranks <- c("species", "subspecies", "variety", "form",
                     "subvariety", "proles", "race", "grex")
  needs_genus <- is.na(genus) & rank_lower %in% species_ranks
  if (any(needs_genus)) {
    genus[needs_genus] <- sub(" .*", "", df$TaxonName[needs_genus])
  }

  # ---- Assemble normalized frame ----
  out <- data.frame(
    taxon_id                = df$TaxonUsageID,
    canonical_name          = trimws(df$TaxonName),
    taxon_rank              = toupper(trimws(df$TaxonRank)),
    taxonomic_status        = df$taxonomic_status,
    accepted_name_usage_id  = df$accepted_name_usage_id,
    family                  = trimws(family),
    genus                   = trimws(genus),
    specific_epithet        = df$specific_epithet,
    authorship              = df$authorship,
    infraspecific_epithet   = df$infraspecific_epithet,
    stringsAsFactors        = FALSE
  )

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

  normalize_backbone(out, col_map)
}


#' Build the Euro+Med backbone .vtr
#'
#' @param output_dir Character.
#' @param version Character or NULL.
#' @param verbose Logical.
#' @return Path to the .vtr file (invisibly).
#' @export
build_euromed <- function(output_dir = "output/euromed", version = NULL,
                          verbose = TRUE) {
  if (is.null(version)) version <- .euromed_version_default

  tmp <- tempfile("euromed_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  csv_path <- download_euromed(dest = tmp, verbose = verbose)
  df <- read_euromed(csv_path, verbose = verbose)

  if (verbose) message("Precomputing keys and embedding synonyms...")
  df <- precompute_backbone(df)

  vtr_path <- file.path(output_dir, "euromed.vtr")
  build_vtr(df, vtr_path, "euromed", version, .euromed_url)

  invisible(vtr_path)
}
