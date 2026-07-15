# GIDIAS EICAT/SEICAT per-species impact aggregates.
#
# GIDIAS (Bacher et al. 2025, Scientific Data) is the IPBES invasive-species
# assessment's compilation of >22,000 individual impact records for ~3,350 alien
# species. Each record classifies one documented impact to the IUCN standards
# EICAT (impact on nature) and SEICAT (impact on people's activities), on a 0-3
# magnitude scale. GIDIAS is CC BY 4.0 throughout, records included, so the
# distribution boundary here is grain, not license: a .vtr is a lookup keyed on
# canonical_name, indexable along at most one group_col, and the raw records key
# on species x study x location x mechanism x impacted taxon. They are an
# evidence table, not a per-species lookup, so this parser reduces them to
# aggregates, as the InvaCost and GloBI rollups do. Use the GIDIAS figshare
# download for the records themselves.
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
# GIDIAS scores a third block, Nature's Contributions to People (NCP), which
# gets no category because the source does not score one. magnitude.NCP is free
# text on the 2,965 rows that carry it, mixing at least four vocabularies in one
# column (EICAT tokens MN/MO/MR/MC/DD; a yes/high/medium ordinal; a verbatim
# scale description "1: Reduced performance of individuals"; "not quantified"),
# so there is no 0-3 scale to map and inventing one would fabricate a rating the
# source does not claim. direction.NCP is clean and better covered than the CWB
# block we do rate: 7,144 rows (4,741 Negative) against direction.CWB's 3,400.
# So NCP is carried as a direction plus the contributions affected, and no
# category. The 18 NCP.* columns are not read: they are exactly the one-hot
# decomposition of affected.NCP.clean (both non-empty on the same 7,294 rows,
# agreeing on all 7,294, neither covering a row the other misses), so rolling
# them up would restate gidias_ncp_affected 18 times.
#
# Each species is aggregated twice: once over all its records (affected_taxon =
# "Any", the default grain) and once per affected native taxon. GIDIAS records
# what a species impacts in Affected.native.species.Taxon, a controlled 5-term
# vocabulary (Plant 6,113 records, Invertebrate 4,514, Vertebrate 3,457, Microbe
# 294, Fungi 2), set on 9,224 of the 10,429 negative Nature records. It buys
# group = "Vertebrate", not group = "Aves": the finer detail lives in
# Affected.native.species.Details, which is uncontrolled free text ("native
# plants", "Laysan Albatross") and cannot be indexed. The split is worth its rows
# because 408 of the 1,466 species with a negative Nature impact and an affected
# taxon recorded (28%) impact two or more of the five, so for those the single
# most-severe category is genuinely lossy.
#
# The "Any" row is not the union of the per-taxon rows and cannot be dropped: it
# is the only row carrying the 12% of negative records with no affected taxon
# recorded, and the only one carrying the two people-facing blocks. The
# affected-taxon axis is an environmental-impact (Nature/EICAT) concept --
# 13,070 of the 14,380 rows that carry an affected taxon have no CWB direction
# at all, only 1,179 of the 2,893 negative-CWB rows carry one, and 11,177 of
# those 14,380 have no NCP direction either -- so slicing SEICAT (impact on
# people's activities) or NCP (contributions to people) by affected native taxon
# would answer a question those columns do not ask. Per-taxon rows therefore
# carry the EICAT block and the record counts; their SEICAT and NCP columns are
# NA.
#
# Like parse_invacost / parse_globi, names are resolved to the accepted grain
# inside the parser (resolve_name_map) and the aggregation happens there, so a
# species whose records are split across a synonym and its accepted name keeps
# the full impact evidence. The registry entry therefore sets resolve_names = FALSE.

#' The affected_taxon value marking a species' all-records aggregate
#' @noRd
.gidias_taxon_any <- "Any"

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
#' per-species environmental- and socio-economic-impact indicators. The `.vtr`
#' carries these derived aggregates; for the individual impact records, use the
#' GIDIAS figshare download.
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
#' Nature's Contributions to People (NCP) is reported as a direction rather than
#' a category: `gidias_ncp_direction` gives the direction(s) recorded
#' (`"Negative"`, `"Positive"`, `"Neutral"`, or a combination for the 16% of
#' species with a mixed record) and `gidias_ncp_affected` the contributions
#' affected. GIDIAS scores no magnitude scale for this block, so no IUCN-style
#' category is derived; the two columns are not paired, so use the source
#' records where the direction of a specific contribution matters.
#'
#' Each species is aggregated twice. `affected_taxon = "Any"` summarises every
#' record, and is the grain to use unless you want one affected group. The
#' other rows summarise the species' impacts on one affected native taxon
#' (`"Plant"`, `"Invertebrate"`, `"Vertebrate"`, `"Microbe"`, `"Fungi"`), which
#' 28% of species split across two or more. Only the `"Any"` row carries SEICAT
#' and the negative records with no affected taxon recorded: the affected-taxon
#' axis slices the environmental-impact block, so the SEICAT columns are `NA` on
#' a per-taxon row.
#'
#' @param path Character. Path to the GIDIAS machine-readable `.csv` (or a
#'   directory containing it).
#' @return data.frame with `canonical_name`, `affected_taxon`, and the
#'   `gidias_*` aggregate columns.
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
  aff_tax <- chr("Affected.native.species.Taxon")
  mag_cwb <- suppressWarnings(as.integer(chr("magnitude.CWB")))   # "positive" -> NA
  dir_cwb <- tolower(chr("direction.CWB"))
  cwb_aff <- chr("affected.CWB.clean")
  dir_ncp <- chr("direction.NCP")
  ncp_aff <- chr("affected.NCP.clean")
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
  aff_tax <- aff_tax[keep]; dir_ncp <- dir_ncp[keep]; ncp_aff <- ncp_aff[keep]

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
  e_realm   <- realm[idx];   e_src     <- src[idx];     e_aff_tax <- aff_tax[idx]
  e_dir_ncp <- dir_ncp[idx]; e_ncp_aff <- ncp_aff[idx]

  # One aggregation over whatever record subset defines a row: the Nature/EICAT
  # block plus the record counts over `nat_i`, and the two people-facing blocks
  # (CWB/SEICAT and NCP) over `ppl_i`. The two subsets coincide for the
  # all-records aggregate; a per-affected-taxon row passes an empty `ppl_i`, so
  # both people-facing blocks fall to NA there (see the file header).
  agg <- function(nat_i, ppl_i) {
    neg <- nat_i[which(e_dir_nat[nat_i] == "Negative")]
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

    negc <- ppl_i[which(e_dir_cwb[ppl_i] == "negative")]
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

    s <- e_src[nat_i]
    list(eicat = eicat, emag = emag, emech = emech,
         seicat = seicat, smag = smag, saff = saff,
         # NCP has no usable magnitude, so direction is the indicator (see the
         # NCP note in the file header). Both columns summarise every NCP
         # record for the species, whatever its direction: affected.NCP.clean
         # names the contribution affected, and direction.NCP scores it
         # separately, so a species with a mixed record ("Negative; Positive",
         # 16% of them) does not pair the two at this grain.
         ncp_dir = .gidias_uniq_join(e_dir_ncp[ppl_i]),
         ncp_aff = .gidias_uniq_join(e_ncp_aff[ppl_i], split_semi = TRUE),
         taxon = .gidias_mode1(e_taxon[nat_i]),
         kingdom = .gidias_mode1(e_kingdom[nat_i]),
         realms = .gidias_uniq_join(e_realm[nat_i]),
         n_records = length(nat_i), n_negative = length(neg),
         n_sources = length(unique(s[!is.na(s) & nzchar(s)])),
         glob = any(e_glob[nat_i], na.rm = TRUE))
  }

  # The all-records aggregate: the default grain, and the only row that keeps
  # the negative records with no affected taxon recorded (12% of them) and the
  # SEICAT block.
  by_name  <- split(seq_along(accn), accn)
  row_name <- names(by_name)
  row_tax  <- rep(.gidias_taxon_any, length(by_name))
  row_nat  <- unname(by_name)
  row_cwb  <- unname(by_name)

  # Per-affected-taxon rows, on top of (never instead of) the aggregate.
  has_aff <- !is.na(e_aff_tax) & nzchar(e_aff_tax)
  if (any(has_aff)) {
    i <- which(has_aff)
    by_tax <- split(i, paste(accn[i], e_aff_tax[i], sep = "\r"))
    # Read the name and taxon back off the first record of each group rather
    # than parsing the split key, so the separator only ever has to group.
    first <- vapply(by_tax, `[[`, integer(1), 1L)
    row_name <- c(row_name, accn[first])
    row_tax  <- c(row_tax,  e_aff_tax[first])
    row_nat  <- c(row_nat,  unname(by_tax))
    row_cwb  <- c(row_cwb,  rep(list(integer(0)), length(by_tax)))
  }

  res <- Map(agg, row_nat, row_cwb)

  g <- function(f, mode) vapply(res, function(z) z[[f]], mode)
  out <- data.frame(
    canonical_name           = row_name,
    affected_taxon           = row_tax,
    gidias_eicat_category    = g("eicat", character(1)),
    gidias_eicat_magnitude   = g("emag", integer(1)),
    gidias_eicat_mechanism   = g("emech", character(1)),
    gidias_seicat_category   = g("seicat", character(1)),
    gidias_seicat_magnitude  = g("smag", integer(1)),
    gidias_seicat_affected   = g("saff", character(1)),
    gidias_ncp_direction     = g("ncp_dir", character(1)),
    gidias_ncp_affected      = g("ncp_aff", character(1)),
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
  out[order(out$canonical_name, out$affected_taxon), , drop = FALSE]
}
