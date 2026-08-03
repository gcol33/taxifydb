# LCVP (Leipzig Catalogue of Vascular Plants): tab_lcvp -> .vtr
#
# The LCVP is distributed as an R data package (idiv-biodiversity/LCVP) whose
# `tab_lcvp` table is the taxonomic reference list. Each row is an input name
# described by its parsed parts (Input.Genus, Input.Epitheton, Rank,
# Input.Subspecies.Epitheton, Input.Authors) rather than a pre-rendered string,
# so the canonical name is assembled from those parts. Acceptance is carried by
# `Status` (accepted / synonym / unresolved) and the synonym -> accepted link by
# `globalId.of.Output.Taxon`, which points at the accepted row's `global.Id`.
#
# Unresolved names (status could be valid or synonym, undecided upstream) are
# kept as their own accepted concept: they stay matchable without asserting a
# synonymy the source itself declines to make. LCVP carries no hybrids in its
# input columns, so no hybrid-marker handling is needed.
#
# The `.rda` is downloaded and `load()`ed directly, so building LCVP needs no
# extra package dependency.

.lcvp_url <- "https://raw.githubusercontent.com/idiv-biodiversity/LCVP/master/data/tab_lcvp.rda"
.lcvp_source_doi <- "10.1038/s41597-020-00702-z"
.lcvp_version_default <- "3.0.1"


#' Download the LCVP `tab_lcvp` data file
#'
#' @param dest Character. Destination directory.
#' @param verbose Logical.
#' @return Path to the downloaded `tab_lcvp.rda`.
#' @export
download_lcvp <- function(dest = tempdir(), verbose = TRUE) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)

  rda_path <- file.path(dest, "tab_lcvp.rda")

  if (verbose) {
    message("Downloading LCVP tab_lcvp (~18 MB)...")
    message(sprintf("  URL: %s", .lcvp_url))
  }
  h <- curl::new_handle(connecttimeout = 60, timeout = 600)
  curl::curl_download(.lcvp_url, rda_path, handle = h, quiet = !verbose)

  rda_path
}


#' Read and normalize the LCVP reference table
#'
#' @param rda_path Character. Path to `tab_lcvp.rda`.
#' @param verbose Logical.
#' @return A normalized data.frame ready for [precompute_backbone()].
#' @export
read_lcvp <- function(rda_path, verbose = TRUE) {
  if (verbose) message("Loading LCVP table...")
  env <- new.env(parent = emptyenv())
  loaded <- load(rda_path, envir = env)
  tab <- get(loaded[1L], envir = env)
  tab <- as.data.frame(tab, stringsAsFactors = FALSE)
  if (verbose) message(sprintf("  %s rows", format(nrow(tab), big.mark = ",")))

  genus   <- trimws(as.character(tab$Input.Genus))
  epithet <- trimws(as.character(tab$Input.Epitheton))
  rank    <- trimws(as.character(tab$Rank))
  sub     <- trimws(as.character(tab$Input.Subspecies.Epitheton))
  authors <- trimws(as.character(tab$Input.Authors))

  # "nil" is LCVP's sentinel for "no infraspecific epithet" (species rank).
  is_infra <- !is.na(sub) & nzchar(sub) & sub != "nil"
  sub[!is_infra] <- NA_character_
  authors[!is.na(authors) & authors == "nil"] <- NA_character_

  # "---" is LCVP's sentinel for a rank it does not place, in BOTH family and
  # order (60,956 rows each). Passed through it becomes a literal value: it was
  # the single most common "order" among LCVP-derived genera in the register,
  # ahead of every real one, and any tabulation by order gained a spurious
  # largest category. NA is what "not placed" means.
  placed <- function(v) {
    v[!is.na(v) & !grepl("[A-Za-z]", v)] <- NA_character_
    v
  }

  # Standard infraspecific marker rendering (forma -> "f."); the other LCVP
  # ranks (var., subsp., subvar.) are already botanical abbreviations.
  marker <- ifelse(rank == "forma", "f.", rank)
  canonical <- ifelse(
    is_infra,
    paste(genus, epithet, marker, sub),
    paste(genus, epithet)
  )

  is_syn <- tolower(trimws(as.character(tab$Status))) == "synonym"
  acc_id <- ifelse(is_syn,
                   as.character(tab$globalId.of.Output.Taxon),
                   NA_character_)

  out <- data.frame(
    taxon_id               = as.character(tab$global.Id),
    canonical_name         = canonical,
    taxon_rank             = rank,
    taxonomic_status       = ifelse(is_syn, "SYNONYM", "ACCEPTED"),
    accepted_name_usage_id = acc_id,
    family                 = placed(trimws(as.character(tab$Family))),
    genus                  = genus,
    specific_epithet       = epithet,
    authorship             = authors,
    infraspecific_epithet  = sub,
    order                  = placed(trimws(as.character(tab$Order))),
    stringsAsFactors       = FALSE
  )

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
  extra_cols <- list(order = "order")

  normalize_backbone(out, col_map, extra_cols)
}


#' Build the LCVP backbone .vtr from source
#'
#' @param output_dir Character. Output directory.
#' @param version Character or NULL. Defaults to the bundled LCVP data version.
#' @param verbose Logical.
#' @return Path to the .vtr file (invisibly).
#' @export
build_lcvp <- function(output_dir = "output/lcvp", version = NULL,
                       verbose = TRUE) {
  if (is.null(version)) version <- .lcvp_version_default

  tmp <- tempfile("lcvp_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  rda_path <- download_lcvp(dest = tmp, verbose = verbose)
  df <- read_lcvp(rda_path, verbose = verbose)

  if (verbose) message("Precomputing keys and embedding synonyms...")
  df <- precompute_backbone(df)

  vtr_path <- file.path(output_dir, "lcvp.vtr")
  build_vtr(df, vtr_path, "lcvp", version, .lcvp_url)

  invisible(vtr_path)
}
