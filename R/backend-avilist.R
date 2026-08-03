# AviList (Global Avian Checklist): AviList extended .xlsx -> .vtr
#
# AviList is the unified global bird checklist that merged the long-standing
# IOC / Clements / BirdLife split. It ships as a single Excel workbook whose
# first sheet is one flat table of accepted concepts at five ranks (order,
# family, genus, species, subspecies), with the higher classification
# denormalized onto every row.
#
# ---- Synonyms come from Protonym, and only from there --------------------
#
# AviList publishes no synonym table and no acceptance column: every row is a
# currently accepted concept. What it does carry is `Protonym`, the original
# combination from the species' description, and where a species has since been
# moved to another genus that protonym IS a (homotypic) synonym of the current
# name: "Parus caeruleus" -> "Cyanistes caeruleus".
#
# 8,384 of the 11,131 species have a protonym differing from their current name,
# and 6,891 of those reduce to a clean binomial. That is a thinner synonymy than
# a curated table (one name per species, homotypic only, so two species later
# merged are not covered), but genus reassignment is the dominant reason bird
# checklists disagree, which is what this recovers.
#
# Three things the protonyms need, each measured against the file:
#
#   1. 1,280 keep the 19th-century capitalised epithet ("Apteryx Owenii"), so
#      the epithet is lower-cased before use.
#   2. 42 carry an abbreviated genus or epithet ("Casuarius N. Hollandiae") and
#      695 run to three or more tokens. Neither reduces to a binomial that can
#      be matched, so both are dropped rather than guessed at.
#   3. THREE protonyms are themselves the accepted name of a different living
#      species. "Muscicapa striata" is the protonym of Setophaga striata (the
#      blackpoll warbler) and the accepted name of the spotted flycatcher.
#      Emitting it would aim a European flycatcher query at an American warbler,
#      so a protonym colliding with any accepted name is dropped.
#
# The sheet's first data row repeats the column captions ("RANK", "SCIENTIFIC
# NAME", Sequence "#") and is dropped.
#
# AviList carries no kingdom/phylum/class columns, so the backbone stamps the
# fixed higher ranks (Animalia / Chordata / Aves).
#
# Licence: CC BY 4.0 ("AviList: The Global Avian Checklist (c) 2026 by AviList
# Core Team is licensed under CC BY 4.0").

.avilist_url <- paste0("https://www.avilist.org/wp-content/uploads/2026/06/",
                       "AviList-v2025b-10Jun2026-extended.xlsx")
.avilist_source_doi <- "10.2173/avilist.v2025b"
.avilist_version_default <- "2025b"


#' Download the AviList extended checklist workbook
#'
#' @param dest Character. Destination directory.
#' @param verbose Logical.
#' @return Path to the downloaded `.xlsx`.
#' @export
download_avilist <- function(dest = tempdir(), verbose = TRUE) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  xlsx_path <- file.path(dest, "avilist.xlsx")

  if (verbose) {
    message("Downloading AviList extended checklist (~9 MB)...")
    message(sprintf("  URL: %s", .avilist_url))
  }
  h <- curl::new_handle(connecttimeout = 60, timeout = 600)
  curl::curl_download(.avilist_url, xlsx_path, handle = h, quiet = !verbose)

  xlsx_path
}


# Lower-case everything after the genus. Done by splitting rather than a regex
# backreference so the transformation is readable and the genus is untouched.
.avilist_lower_epithet <- function(x) {
  parts <- strsplit(x, "[[:space:]]+")
  vapply(parts, function(p) {
    if (length(p) < 2L) return(NA_character_)
    paste(p[[1L]], tolower(paste(p[-1L], collapse = " ")))
  }, character(1L))
}


#' Read and normalize the AviList checklist
#'
#' @param xlsx_path Character. Path to the AviList extended workbook.
#' @param verbose Logical.
#' @return A normalized data.frame ready for [precompute_backbone()].
#' @export
read_avilist <- function(xlsx_path, verbose = TRUE) {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop("Reading AviList requires the 'openxlsx2' package.", call. = FALSE)
  }
  if (verbose) message("Reading AviList workbook...")
  d <- openxlsx2::wb_to_df(xlsx_path, sheet = 1)

  rank <- tolower(trimws(as.character(d$Taxon_rank)))
  name <- trimws(as.character(d$Scientific_name))

  # Drop the repeated caption row and anything without a rank or a name.
  keep <- !is.na(rank) & rank != "rank" & !is.na(name) & nzchar(name)
  d <- d[keep, , drop = FALSE]
  rank <- rank[keep]
  name <- name[keep]
  if (verbose) {
    message(sprintf("  %s accepted concepts", format(nrow(d), big.mark = ",")))
  }

  seq_id <- as.character(d$Sequence)
  order_ <- trimws(as.character(d$Order))
  family <- trimws(as.character(d$Family))
  auth   <- trimws(as.character(d$Authority))
  auth[!nzchar(auth)] <- NA_character_

  tok    <- strsplit(name, "[[:space:]]+")
  ntok   <- lengths(tok)
  tok1   <- vapply(tok, function(p) p[[1L]], character(1L))
  tok2   <- vapply(tok, function(p) if (length(p) >= 2L) p[[2L]] else NA_character_, character(1L))
  tok3   <- vapply(tok, function(p) if (length(p) >= 3L) p[[3L]] else NA_character_, character(1L))

  # Genus is the name itself on a genus row and the first token below it; the
  # order and family rows have no genus.
  genus <- ifelse(rank == "genus", name,
                  ifelse(rank %in% c("species", "subspecies"), tok1, NA_character_))
  epithet <- ifelse(rank %in% c("species", "subspecies"), tok2, NA_character_)
  infra   <- ifelse(rank == "subspecies" & ntok >= 3L, tok3, NA_character_)

  extinct <- trimws(as.character(d$Extinct_or_possibly_extinct))
  extinct[!nzchar(extinct) | extinct == "EXTINCTION STATUS"] <- NA_character_

  accepted <- data.frame(
    taxon_id               = seq_id,
    canonical_name         = name,
    taxon_rank             = rank,
    taxonomic_status       = "ACCEPTED",
    accepted_name_usage_id = NA_character_,
    family                 = family,
    genus                  = genus,
    specific_epithet       = epithet,
    authorship             = auth,
    infraspecific_epithet  = infra,
    order                  = order_,
    extinct                = extinct,
    stringsAsFactors       = FALSE
  )

  # ---- synonyms recovered from the protonyms of species rows ----
  is_sp  <- rank == "species"
  proto  <- trimws(as.character(d$Protonym))
  cand   <- is_sp & !is.na(proto) & nzchar(proto) & proto != name
  low    <- rep(NA_character_, length(proto))
  low[cand] <- .avilist_lower_epithet(proto[cand])

  usable <- cand & !is.na(low) &
    grepl("^[A-Z][a-z]+ [a-z-]+$", low) &
    !grepl(".", proto, fixed = TRUE) &
    low != name &
    !(low %in% name)          # never shadow a name AviList itself accepts

  if (verbose) {
    message(sprintf("  %s synonyms recovered from protonyms (%s protonyms differ)",
                    format(sum(usable), big.mark = ","),
                    format(sum(cand), big.mark = ",")))
  }

  syn_tok <- strsplit(low[usable], "[[:space:]]+")
  synonyms <- data.frame(
    taxon_id               = paste0("avilist_syn_", seq_id[usable]),
    canonical_name         = low[usable],
    taxon_rank             = "species",
    taxonomic_status       = "SYNONYM",
    accepted_name_usage_id = seq_id[usable],
    family                 = family[usable],
    genus                  = vapply(syn_tok, function(p) p[[1L]], character(1L)),
    specific_epithet       = vapply(syn_tok, function(p) p[[2L]], character(1L)),
    authorship             = auth[usable],
    infraspecific_epithet  = NA_character_,
    order                  = order_[usable],
    extinct                = NA_character_,
    stringsAsFactors       = FALSE
  )

  out <- rbind(accepted, synonyms)

  # ---- fixed higher classification (birds only) ----
  out$kingdom <- "Animalia"
  out$phylum  <- "Chordata"
  out$class   <- "Aves"

  if (verbose) message("Normalizing to unified schema...")
  col_map <- list(
    taxon_id               = "taxon_id",
    canonical_name         = "canonical_name",
    taxon_rank             = "taxon_rank",
    taxonomic_status       = "taxonomic_status",
    accepted_name_usage_id = "accepted_name_usage_id",
    family                 = "family",
    genus                  = "genus",
    specific_epithet       = "specific_epithet",
    authorship             = "authorship",
    infraspecific_epithet  = "infraspecific_epithet"
  )
  extra_cols <- list(kingdom = "kingdom", phylum = "phylum",
                     class = "class", order = "order", extinct = "extinct")

  normalize_backbone(out, col_map, extra_cols)
}


#' Build the AviList backbone .vtr from source
#'
#' @param output_dir Character. Output directory.
#' @param version Character or NULL. Defaults to the bundled AviList version.
#' @param verbose Logical.
#' @return Path to the .vtr file (invisibly).
#' @export
build_avilist <- function(output_dir = "output/avilist", version = NULL,
                          verbose = TRUE) {
  if (is.null(version)) version <- .avilist_version_default

  tmp <- tempfile("avilist_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  xlsx_path <- download_avilist(dest = tmp, verbose = verbose)
  df <- read_avilist(xlsx_path, verbose = verbose)

  if (verbose) message("Precomputing keys and embedding synonyms...")
  df <- precompute_backbone(df)

  vtr_path <- file.path(output_dir, "avilist.vtr")
  build_vtr(df, vtr_path, "avilist", version, .avilist_url)

  invisible(vtr_path)
}
