# BETSI Collembola body-length trait values.
#
# Named for exactly what it is. BETSI itself is a multi-trait, multi-taxon
# database whose live portal is offline; this is one exported slice of it, one
# trait of one class, so the enrichment is `betsi_collembola_body_length` rather
# than `betsi`. The rest of BETSI is not here (see gcol33/taxifydb#42).
#
# BETSI (Biological and Ecological Traits of Soil Invertebrates; Hedde et al.,
# portail.betsi.cnrs.fr) is a European soil-fauna trait database. Its live
# portal is offline (confirmed 2026-08 from three independent networks), so this
# enrichment is built from the openly-licensed public export Bonfanti (2018)
# deposited on Zenodo (doi:10.5281/zenodo.1292461, CC BY-NC 4.0): every
# body-length measurement BETSI held for Collembola, requested 2017-06-01, as a
# long table of one row per (species, literature source).
#
# The export is 3,581 per-source measurements, up to 13 for a single species. A
# .vtr is a per-species lookup, so this reduces them to one row per species: the
# median body length (mm), with the min/max and the measurement and source
# counts. Names carry their taxonomic authority ("Genus species (Author,
# year)"), stripped to the canonical binomial for cross-backbone resolution.
#
# This is the body-length slice of BETSI's Collembola coverage, and the clean
# floor of the taxifydb BETSI build: it needs no legend. The multi-trait INRAE
# matrices (doi:10.15454/UU2FQT, /UCYSLH) key their traits on undocumented
# 6-letter species codes with no published code->binomial legend -- a
# string-reconstructed legend matched only 58% of the 129 codes and would
# misassign the rest -- so they are not merged here (see gcol33/taxifydb#42).


#' Strip a taxonomic authority to the canonical binomial
#'
#' `"Sphaeridia pumilis (Krausbauer, 1898)"` and
#' `"Mesaphorura orousseti Najt, Thibaud & Weiner, 1990"` both reduce to
#' `"Genus species"`. Returns `NA` for anything not starting with a
#' capitalised genus and a lowercase epithet.
#' @param x Character vector of names with authority.
#' @return Character vector of canonical binomials (or `NA`).
#' @noRd
.betsi_binomial <- function(x) {
  x <- as.character(x)
  pat <- "^\\s*([A-Z][a-z]+)\\s+([a-z][a-z-]+).*$"
  ifelse(grepl(pat, x), trimws(sub(pat, "\\1 \\2", x)), NA_character_)
}


#' Parse the BETSI Collembola body-length export into per-species values
#'
#' Reads the Zenodo BETSI body-length export and reduces its per-source
#' measurements to one row per Collembola species. `betsi_body_length_mm` is the
#' median of the recorded measurements (mm); `betsi_body_length_min_mm` /
#' `betsi_body_length_max_mm` bound them; `betsi_body_length_n` and
#' `betsi_body_length_sources` are the number of measurements and of distinct
#' literature sources behind the species' value.
#'
#' @param path Character. Path to the BETSI body-length `.xlsx` (or a directory
#'   containing it).
#' @return data.frame with `canonical_name` and the `betsi_body_length_*`
#'   columns, one row per species.
#' @export
parse_betsi_collembola_body_length <- function(path) {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop("Package 'openxlsx2' is required to parse BETSI.", call. = FALSE)
  }
  file <- if (dir.exists(path)) {
    f <- list.files(path, pattern = "(?i)\\.xlsx$", full.names = TRUE)
    if (!length(f)) stop("No BETSI .xlsx found in: ", path, call. = FALSE)
    f[[1L]]
  } else {
    path
  }

  wb <- openxlsx2::wb_load(file)
  sheet <- if ("body length values" %in% wb$sheet_names) {
    "body length values"
  } else {
    wb$sheet_names[[1L]]
  }
  d <- openxlsx2::wb_to_df(wb, sheet = sheet)

  need <- c("Collembola species", "Body length value", "Literature source")
  miss <- setdiff(need, names(d))
  if (length(miss)) {
    stop("parse_betsi: missing column(s): ", paste(miss, collapse = ", "),
         call. = FALSE)
  }

  sp  <- .betsi_binomial(d[["Collembola species"]])
  val <- suppressWarnings(as.numeric(as.character(d[["Body length value"]])))
  src <- trimws(as.character(d[["Literature source"]]))

  keep <- !is.na(sp) & nzchar(sp) & !is.na(val)
  sp <- sp[keep]; val <- val[keep]; src <- src[keep]
  if (!length(sp)) stop("parse_betsi: no usable rows.", call. = FALSE)

  by <- split(seq_along(sp), sp)
  out <- data.frame(
    canonical_name            = names(by),
    betsi_body_length_mm      = vapply(by, function(i) round(stats::median(val[i]), 3L), 0),
    betsi_body_length_min_mm  = vapply(by, function(i) min(val[i]), 0),
    betsi_body_length_max_mm  = vapply(by, function(i) max(val[i]), 0),
    betsi_body_length_n       = vapply(by, function(i) length(i), 0L),
    betsi_body_length_sources = vapply(by, function(i) length(unique(src[i])), 0L),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  out <- out[.is_binomial(out$canonical_name), , drop = FALSE]
  out[order(out$canonical_name), , drop = FALSE]
}
