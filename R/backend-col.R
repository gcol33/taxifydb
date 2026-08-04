# COL (Catalogue of Life): Taxon.tsv -> normalized data.frame -> .vtr
#
# COL publishes annual Darwin Core Archives on ChecklistBank. The Taxon.tsv
# uses dwc:/col: namespace prefixes on column names (stripped on read),
# scientificName includes authorship (canonical computed by subtraction),
# and the column for genus is `genericName` (not `genus`).
#
# COL uses MISAPPLIED as a third status in addition to ACCEPTED/SYNONYM —
# we treat it as a synonym for matching purposes by passing the appropriate
# synonym_pattern to precompute_backbone().

.col_url <- "https://download.checklistbank.org/col/annual/2025_dwca.zip"
.col_version_default <- "2025"

# Columns needed for matching (after stripping namespace prefixes)
.col_match_cols <- c(
  "taxonID",
  "scientificName",
  "taxonRank",
  "taxonomicStatus",
  "acceptedNameUsageID",
  "family",
  "genericName",
  "specificEpithet",
  "scientificNameAuthorship",
  "infraspecificEpithet",
  "parentNameUsageID"
)

# Extra columns preserved for add_col_info() at runtime.
# kingdom/phylum/class/order are NOT taken from the source: COL's Taxon.tsv
# leaves those Darwin Core columns empty (the higher classification is a
# parent-tree of separate rows, not denormalized). They are resolved by walking
# parentNameUsageID in col_resolve_classification() and mapped in read_col().
.col_extra_cols <- c(
  "notho",
  "nomenclaturalCode",
  "nomenclaturalStatus",
  "namePublishedIn",
  "nameAccordingTo",
  "superfamily",
  "subfamily",
  "tribe",
  "taxonRemarks",
  "references",
  "scientificNameID",
  "infragenericEpithet",
  "cultivarEpithet"
)


#' Download and extract the COL DwC-A
#'
#' @param dest Character. Destination directory.
#' @param verbose Logical.
#' @return Path to the extracted COL directory.
#' @export
download_col <- function(dest = tempdir(), verbose = TRUE) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)

  if (verbose) {
    message("Downloading COL backbone from ChecklistBank (~600 MB)...")
    message(sprintf("  URL: %s", .col_url))
  }
  zip_path <- download_curl_file(.col_url, dest, "col_download.zip")

  if (verbose) message("Extracting Taxon.tsv ...")
  txt_files <- utils::unzip(zip_path, list = TRUE)$Name
  taxon_target <- txt_files[grepl("Taxon\\.tsv$", txt_files)]
  if (length(taxon_target) == 0L) {
    stop("Taxon.tsv not found in COL archive.", call. = FALSE)
  }
  utils::unzip(zip_path, files = taxon_target[1L], exdir = dest,
               junkpaths = TRUE)

  # SpeciesProfile.tsv (habitat/extinct/marine flags) — optional
  sp_target <- txt_files[grepl("SpeciesProfile\\.tsv$", txt_files)]
  if (length(sp_target) > 0L) {
    if (verbose) message("Extracting SpeciesProfile.tsv ...")
    utils::unzip(zip_path, files = sp_target[1L], exdir = dest,
                 junkpaths = TRUE)
  }

  unlink(zip_path)
  dest
}


#' Read and normalize the COL Taxon.tsv
#'
#' @param col_dir Character. Path to the extracted COL directory.
#' @param verbose Logical.
#' @return A normalized data.frame.
#' @export
read_col <- function(col_dir, verbose = TRUE) {
  tsv_path <- file.path(col_dir, "Taxon.tsv")
  if (!file.exists(tsv_path)) {
    stop("Taxon.tsv not found in: ", col_dir, call. = FALSE)
  }

  if (verbose) message("Reading Taxon.tsv ...")
  df <- utils::read.delim(
    tsv_path,
    fileEncoding = "UTF-8",
    stringsAsFactors = FALSE,
    quote = "",
    na.strings = "",
    check.names = FALSE
  )
  if (verbose) message(sprintf("  %s rows", format(nrow(df), big.mark = ",")))

  # Strip namespace prefixes (dwc:taxonID -> taxonID)
  names(df) <- sub("^[a-z]+:", "", names(df))

  keep <- intersect(c(.col_match_cols, .col_extra_cols), names(df))
  df <- df[, keep, drop = FALSE]

  if ("taxonomicStatus" %in% names(df)) {
    df$taxonomicStatus <- toupper(df$taxonomicStatus)
  }
  if ("taxonRank" %in% names(df)) {
    df$taxonRank <- toupper(df$taxonRank)
  }

  # Build canonical name (strip authorship from scientificName)
  if (all(c("scientificName", "scientificNameAuthorship") %in% names(df))) {
    df$canonicalName <- strip_authorship(df$scientificName,
                                         df$scientificNameAuthorship)
  } else {
    df$canonicalName <- df$scientificName
  }

  if (verbose) message("Denormalizing higher classification (kingdom..family)...")
  cls <- col_resolve_classification(df)
  df$kingdom <- cls$kingdom
  df$phylum  <- cls$phylum
  df$class   <- cls$class
  df$order   <- cls$order
  df$family  <- cls$family

  text_cols <- intersect(
    c("canonicalName", "scientificName", "kingdom", "phylum", "class", "order",
      "family", "genericName", "specificEpithet", "scientificNameAuthorship"),
    names(df)
  )
  for (col in text_cols) {
    df[[col]] <- trimws(df[[col]])
  }

  if (verbose) message("Normalizing to unified schema...")
  col_map <- list(
    taxon_id                = "taxonID",
    canonical_name          = "canonicalName",
    taxon_rank              = "taxonRank",
    taxonomic_status        = "taxonomicStatus",
    accepted_name_usage_id  = "acceptedNameUsageID",
    kingdom                 = "kingdom",
    phylum                  = "phylum",
    class                   = "class",
    order                   = "order",
    family                  = "family",
    genus                   = "genericName",
    specific_epithet        = "specificEpithet",
    authorship              = "scientificNameAuthorship",
    infraspecific_epithet   = "infraspecificEpithet"
  )

  extra_cols <- list()
  for (col in c(.col_extra_cols, "scientificName")) {
    if (col %in% names(df)) extra_cols[[col]] <- col
  }

  normalize_backbone(df, col_map, extra_cols)
}


#' Build the COL backbone .vtr
#'
#' Also writes the SpeciesProfile.tsv (extinct/marine flags) as a separate
#' `col_species_profile.vtr` next to the main backbone, if present.
#'
#' @param output_dir Character.
#' @param version Character or NULL.
#' @param verbose Logical.
#' @return Path to the .vtr file (invisibly).
#' @export
build_col <- function(output_dir = "output/col", version = NULL,
                      verbose = TRUE) {
  if (is.null(version)) version <- .col_version_default

  tmp <- tempfile("col_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  col_dir <- download_col(dest = tmp, verbose = verbose)
  df <- read_col(col_dir, verbose = verbose)

  if (verbose) message("Precomputing keys and embedding synonyms...")
  df <- precompute_backbone(df, synonym_pattern = "SYNONYM|MISAPPLIED")

  vtr_path <- file.path(output_dir, "col.vtr")
  build_vtr(df, vtr_path, "col", version, .col_url)

  sp_path <- file.path(col_dir, "SpeciesProfile.tsv")
  if (file.exists(sp_path)) {
    if (verbose) message("Converting SpeciesProfile.tsv to .vtr ...")
    sp_df <- utils::read.delim(
      sp_path,
      fileEncoding = "UTF-8",
      stringsAsFactors = FALSE,
      quote = "",
      na.strings = "",
      check.names = FALSE
    )
    names(sp_df) <- sub("^[a-z]+:", "", names(sp_df))
    sp_vtr <- file.path(output_dir, "col_species_profile.vtr")
    vectra::write_vtr(sp_df, sp_vtr)
    if (verbose) message(sprintf("  SpeciesProfile: %s rows",
                                 format(nrow(sp_df), big.mark = ",")))
  }

  invisible(vtr_path)
}


#' Resolve the higher classification for all COL rows via tree propagation
#'
#' COL stores the higher classification as a parent-tree of separate rows, not
#' as denormalized kingdom/phylum/class/order/family columns (those Darwin Core
#' columns are shipped empty). For each target rank, the rank-matching rows seed
#' their own canonicalName and the parent chain propagates the ancestor name
#' down to every descendant. Family additionally fills any remaining gap from a
#' genericName -> genus-family lookup, so a species whose family link is broken
#' but whose genus resolves still gets a family.
#'
#' The fallback that fills the higher ranks from a family's dominant placement
#' works coarsest rank first and pools only rows already agreeing on kingdom and
#' phylum, because two nomenclatural codes can use one family name for two
#' lineages.
#'
#' @param df The full COL data.frame with columns taxonID, taxonRank,
#'   canonicalName, parentNameUsageID, genericName.
#' @return A named list of character vectors (kingdom, phylum, class, order,
#'   family), each of length `nrow(df)`.
#' @noRd
col_resolve_classification <- function(df) {
  n          <- nrow(df)
  rank       <- toupper(df$taxonRank)
  id_to_idx  <- stats::setNames(seq_len(n), df$taxonID)
  parent_idx <- unname(id_to_idx[df$parentNameUsageID])

  # Propagate one ancestor rank's name from its rank-matching rows down through
  # the parent chain. Level-by-level; converges in tree-depth iterations and
  # breaks early once nothing more resolves. kingdom -> species can be deep, so
  # the cap is generous rather than the family-only 15.
  resolve_rank <- function(rank_label) {
    val <- rep(NA_character_, n)
    seed <- rank == rank_label
    val[seed] <- df$canonicalName[seed]
    for (iter in seq_len(60L)) {
      missing <- is.na(val) & !is.na(parent_idx)
      if (!any(missing)) break
      parent_val <- val[parent_idx[missing]]
      resolved <- !is.na(parent_val)
      if (!any(resolved)) break
      idx_missing <- which(missing)
      val[idx_missing[resolved]] <- parent_val[resolved]
    }
    val
  }

  kingdom <- resolve_rank("KINGDOM")
  phylum  <- resolve_rank("PHYLUM")
  class   <- resolve_rank("CLASS")
  order   <- resolve_rank("ORDER")
  family  <- resolve_rank("FAMILY")

  # Family-only fallback: fill a still-missing family from the row's genus.
  # A genus name can belong to several lineages (Glyptopleura is a daisy and an
  # ostracod), so the lookup is keyed on the genus name together with the
  # kingdom the row already carries, which settles those rows on their own side.
  # A row whose chain gives it no kingdom either has nothing to be keyed on and
  # takes the commonest family for the name.
  is_genus <- rank == "GENUS"
  g_name   <- df$canonicalName[is_genus]
  g_fam    <- family[is_genus]
  g_king   <- kingdom[is_genus]
  usable   <- !is.na(g_name) & !is.na(g_fam)
  if (any(usable)) {
    g_name <- g_name[usable]
    g_fam  <- g_fam[usable]
    g_king <- g_king[usable]

    gk      <- paste(g_name, g_king, sep = "\r")
    keep_gk <- !is.na(g_king) & !duplicated(gk)
    by_genus_kingdom <- stats::setNames(g_fam[keep_gk], gk[keep_gk])

    keep_g        <- !duplicated(g_name)
    by_genus      <- stats::setNames(g_fam[keep_g],  g_name[keep_g])
    by_genus_king <- stats::setNames(g_king[keep_g], g_name[keep_g])

    needs_family <- which(is.na(family) & !is.na(df$genericName))
    if (length(needs_family)) {
      gen      <- df$genericName[needs_family]
      row_king <- kingdom[needs_family]
      fill     <- unname(by_genus_kingdom[paste(gen, row_king, sep = "\r")])

      # Nothing recorded for the row's own kingdom: fall back to the genus name
      # alone, unless the family that name reaches sits in another kingdom.
      # Whichever genus row came first must not hand a plant family to an
      # animal, so those rows keep no family at all.
      miss <- is.na(fill)
      if (any(miss)) {
        cand      <- unname(by_genus[gen[miss]])
        cand_king <- unname(by_genus_king[gen[miss]])
        clash     <- !is.na(cand_king) & !is.na(row_king[miss]) &
                     cand_king != row_king[miss]
        cand[clash] <- NA_character_
        fill[miss]  <- cand
      }
      family[needs_family] <- fill
    }
  }

  # Family-to-higher fallback. Many COL sub-checklists give a family but no
  # kingdom/phylum/class/order in their own parent chain, so those ranks stay
  # empty even where the family's placement is fixed by other rows. Fill each
  # higher rank from the dominant value seen for that family elsewhere, so a
  # well-placed family carries its order/class/phylum/kingdom to sparsely
  # classified members (the same table an external consumer would otherwise
  # rebuild by hand).
  #
  # The group is the family name plus the kingdom and phylum already settled,
  # filled in that order, so each rank pools only rows agreeing above the
  # family. One family split across sub-checklists still pools, since those rows
  # agree all the way up. Two lineages sharing a family name separate at kingdom
  # or phylum, which is where every observed homonym pair parts: a snake and an
  # ostracod Elapidae, a fish and a snail Cepolidae, a fungal and an animal
  # Clavicipitaceae.
  #
  # The key stops at phylum rather than running down to class. COL records no
  # class for Squamata, so a key carrying class would leave every snake row
  # unable to draw the order its own family plainly has. Only a clearly dominant
  # value (>= 90% of the group's classified rows) is taken, which catches a
  # family placed inconsistently within one lineage.
  fill_from_family <- function(target, key) {
    ok    <- !is.na(key)
    known <- ok & !is.na(target) & nzchar(target)
    if (!any(known)) return(target)
    parts <- split(target[known], key[known])
    best  <- vapply(parts, function(x) {
      tb <- sort(table(x), decreasing = TRUE)
      if (tb[[1L]] / length(x) >= 0.9) names(tb)[1L] else NA_character_
    }, character(1L))
    best <- best[!is.na(best)]
    if (!length(best)) return(target)
    need_idx <- which((is.na(target) | !nzchar(target)) & ok)
    fillv <- unname(best[key[need_idx]])
    hit   <- !is.na(fillv)
    target[need_idx[hit]] <- fillv[hit]
    target
  }

  # Extend the group key with a settled rank. A row missing that rank drops out
  # of every later fill rather than pooling with lineages it may not belong to.
  extend_key <- function(key, value) {
    ifelse(is.na(key) | is.na(value) | !nzchar(value), NA_character_,
           paste(key, value, sep = "\r"))
  }

  fam_key <- ifelse(!is.na(family) & nzchar(family), family, NA_character_)
  kingdom <- fill_from_family(kingdom, fam_key)
  fam_key <- extend_key(fam_key, kingdom)
  phylum  <- fill_from_family(phylum, fam_key)
  fam_key <- extend_key(fam_key, phylum)
  class   <- fill_from_family(class, fam_key)
  order   <- fill_from_family(order, fam_key)

  list(kingdom = kingdom, phylum = phylum, class = class,
       order = order, family = family)
}
