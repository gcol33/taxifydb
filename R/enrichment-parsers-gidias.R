# GIDIAS EICAT/SEICAT per-species impact aggregates.
#
# GIDIAS (Bacher et al. 2025, Scientific Data) is the IPBES invasive-species
# assessment's compilation of >22,000 individual impact records for ~3,350 alien
# species. Each record classifies one documented impact to the IUCN standards
# EICAT (impact on nature) and SEICAT (impact on people's activities), on a 0-3
# magnitude scale. The raw impact records carry heterogeneous provenance
# (per-record source, location, method) and are NOT redistributed; only derived
# per-species aggregates are (CC BY 4.0), the same treatment as the InvaCost and
# GloBI rollups.
#
# A species' EICAT category is the standard summary of its impact records: the
# most severe magnitude among its negative (harmful) environmental impacts.
# GIDIAS magnitude 0-3 maps to the five EICAT levels, using the per-record
# global-extinction flag to split magnitude 3 (extinction) into Major (local,
# MR) and Massive (global, MV):
#   0 no impact detected           -> MC (Minimal Concern)
#   1 reduced individual performance-> MN (Minor)
#   2 reduced population size       -> MO (Moderate)
#   3 local extinction             -> MR (Major)
#   3 + global extinction          -> MV (Massive)
# A species with negative records but no scored magnitude is Data Deficient (DD).
# SEICAT is the same construction on the CWB (constituents of well-being) block,
# whose magnitude tops out at 3 (all people stop an activity) = MR.
#
# Like parse_invacost / parse_globi, names are resolved to the accepted grain
# inside the parser (resolve_name_map) and the aggregation happens there, so a
# species whose records are split across a synonym and its accepted name keeps
# the full impact evidence. The registry entry therefore sets resolve_names = FALSE.

#' Map a GIDIAS Nature magnitude (0-3) and global-extinction flag to EICAT
#' @noRd
.gidias_eicat_cat <- function(max_mag, global_ext) {
  if (is.na(max_mag)) return(NA_character_)
  if (max_mag >= 3L) return(if (isTRUE(global_ext)) "MV" else "MR")
  c("MC", "MN", "MO")[max_mag + 1L]
}

#' Map a GIDIAS CWB magnitude (0-3) to SEICAT
#' @noRd
.gidias_seicat_cat <- function(max_mag) {
  if (is.na(max_mag)) return(NA_character_)
  c("MC", "MN", "MO", "MR")[max_mag + 1L]
}

#' Most frequent non-empty value
#' @noRd
.gidias_mode1 <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1L]
}

#' Distinct non-empty values joined; optionally split compound "a; b" cells first
#' @noRd
.gidias_uniq_join <- function(x, split_semi = FALSE) {
  x <- x[!is.na(x) & nzchar(x)]
  if (split_semi) x <- trimws(unlist(strsplit(x, ";", fixed = TRUE)))
  x <- unique(x[nzchar(x)])
  if (!length(x)) return(NA_character_)
  paste(sort(x), collapse = "; ")
}

#' Parse the GIDIAS impact database into per-species EICAT/SEICAT aggregates
#'
#' Reads the GIDIAS machine-readable impact records and reduces them to
#' per-species environmental- and socio-economic-impact indicators. Only these
#' derived aggregates are distributed; the raw impact records are not.
#'
#' `gidias_eicat_category` is the species' EICAT category: the most severe
#' magnitude among its negative (harmful) environmental-impact records, mapped
#' from the GIDIAS 0-3 magnitude scale to the IUCN levels MC/MN/MO/MR/MV (the
#' per-record global-extinction flag splits magnitude 3 into local MR and global
#' MV); a species with negative records but no scored magnitude is `"DD"`.
#' `gidias_seicat_category` applies the same rule to the constituents-of-
#' well-being block (SEICAT, MC/MN/MO/MR). The remaining columns summarise the
#' evidence: the driving mechanism(s), affected well-being constituents, the
#' species' functional group and realms, and record/source counts.
#'
#' @param path Character. Path to the GIDIAS machine-readable `.csv` (or a
#'   directory containing it).
#' @return data.frame with `canonical_name` and the `gidias_*` aggregate columns.
#' @export
parse_gidias <- function(path) {
  file <- if (dir.exists(path)) {
    f <- list.files(path, pattern = "(?i)gidias.*\\.csv$", full.names = TRUE)
    if (!length(f)) stop("No GIDIAS .csv found in: ", path, call. = FALSE)
    f[[1L]]
  } else {
    path
  }

  df <- utils::read.csv(file, header = TRUE, quote = "\"",
                        check.names = FALSE, stringsAsFactors = FALSE,
                        fileEncoding = "UTF-8", na.strings = c("NA", ""))

  chr <- function(col) {
    if (col %in% names(df)) trimws(as.character(df[[col]]))
    else rep(NA_character_, nrow(df))
  }

  species <- chr("Verified.Name.GBIF.Taxon")
  mag_nat <- suppressWarnings(as.integer(chr("magnitude.Nature")))
  dir_nat <- chr("direction.Nature")
  glob    <- toupper(chr("global.extinction")) == "TRUE"
  mech    <- chr("mechanism.Nature.clean")
  mag_cwb <- suppressWarnings(as.integer(chr("magnitude.CWB")))   # "positive" -> NA
  dir_cwb <- tolower(chr("direction.CWB"))
  cwb_aff <- chr("affected.CWB.clean")
  taxon   <- chr("IAS.Taxon")
  kingdom <- chr("Kingdom")
  realm   <- chr("Realm")
  doi     <- chr("DOI")
  ref     <- chr("Reference")
  src     <- ifelse(!is.na(doi) & nzchar(doi), doi, ref)

  keep <- !is.na(species) & nzchar(species) & .is_binomial(species)
  if (!any(keep)) stop("parse_gidias: no usable species rows.", call. = FALSE)
  species <- species[keep]; mag_nat <- mag_nat[keep]; dir_nat <- dir_nat[keep]
  glob <- glob[keep]; mech <- mech[keep]; mag_cwb <- mag_cwb[keep]
  dir_cwb <- dir_cwb[keep]; cwb_aff <- cwb_aff[keep]; taxon <- taxon[keep]
  kingdom <- kingdom[keep]; realm <- realm[keep]; src <- src[keep]

  # Resolve to the accepted grain and expand each record to the accepted name(s)
  # its species maps to across backends (see file header).
  map <- resolve_name_map(unique(species), verbose = FALSE)
  acc <- split(map$accepted_name, map$input_name)
  reps <- lengths(acc[species])
  idx  <- rep(seq_along(species), reps)
  accn <- unlist(acc[species], use.names = FALSE)
  if (!length(accn)) stop("parse_gidias: no names resolved.", call. = FALSE)

  e_mag_nat <- mag_nat[idx]; e_dir_nat <- dir_nat[idx]; e_glob <- glob[idx]
  e_mech    <- mech[idx];    e_mag_cwb <- mag_cwb[idx]; e_dir_cwb <- dir_cwb[idx]
  e_cwb_aff <- cwb_aff[idx]; e_taxon   <- taxon[idx];   e_kingdom <- kingdom[idx]
  e_realm   <- realm[idx];   e_src     <- src[idx]

  groups <- split(seq_along(accn), accn)
  res <- lapply(groups, function(i) {
    neg <- i[which(e_dir_nat[i] == "Negative")]
    if (length(neg)) {
      mags <- e_mag_nat[neg]
      if (all(is.na(mags))) {
        eicat <- "DD"; emag <- NA_integer_
      } else {
        emag  <- max(mags, na.rm = TRUE)
        eicat <- .gidias_eicat_cat(emag, any(e_glob[neg], na.rm = TRUE))
      }
      emech <- .gidias_uniq_join(e_mech[neg], split_semi = TRUE)
    } else {
      eicat <- NA_character_; emag <- NA_integer_; emech <- NA_character_
    }

    negc <- i[which(e_dir_cwb[i] == "negative")]
    if (length(negc)) {
      cmags <- e_mag_cwb[negc]
      if (all(is.na(cmags))) {
        seicat <- "DD"; smag <- NA_integer_
      } else {
        smag <- max(cmags, na.rm = TRUE); seicat <- .gidias_seicat_cat(smag)
      }
      saff <- .gidias_uniq_join(e_cwb_aff[negc], split_semi = TRUE)
    } else {
      seicat <- NA_character_; smag <- NA_integer_; saff <- NA_character_
    }

    s <- e_src[i]
    list(eicat = eicat, emag = emag, emech = emech,
         seicat = seicat, smag = smag, saff = saff,
         taxon = .gidias_mode1(e_taxon[i]), kingdom = .gidias_mode1(e_kingdom[i]),
         realms = .gidias_uniq_join(e_realm[i]),
         n_records = length(i), n_negative = length(neg),
         n_sources = length(unique(s[!is.na(s) & nzchar(s)])),
         glob = any(e_glob[i], na.rm = TRUE))
  })

  g <- function(f, mode) vapply(res, function(z) z[[f]], mode)
  out <- data.frame(
    canonical_name           = names(res),
    gidias_eicat_category    = g("eicat", character(1)),
    gidias_eicat_magnitude   = g("emag", integer(1)),
    gidias_eicat_mechanism   = g("emech", character(1)),
    gidias_seicat_category   = g("seicat", character(1)),
    gidias_seicat_magnitude  = g("smag", integer(1)),
    gidias_seicat_affected   = g("saff", character(1)),
    gidias_ias_taxon         = g("taxon", character(1)),
    gidias_kingdom           = g("kingdom", character(1)),
    gidias_realms            = g("realms", character(1)),
    gidias_n_records         = g("n_records", integer(1)),
    gidias_n_negative        = g("n_negative", integer(1)),
    gidias_n_sources         = g("n_sources", integer(1)),
    gidias_global_extinction = g("glob", logical(1)),
    stringsAsFactors = FALSE
  )
  out <- out[.is_binomial(out$canonical_name), , drop = FALSE]
  out[order(out$canonical_name), , drop = FALSE]
}
