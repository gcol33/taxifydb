# Ellers et al. Collembola ecological traits (CC0).
#
# Ellers, Berg, Dias, Fontana, Ooms & Moretti (2018, Journal of Animal Ecology
# 87:933-944, doi:10.1111/1365-2656.12838) compiled multidimensional traits for
# European soil fauna. The Collembola table (Dryad doi:10.5061/dryad.m6dn0g8,
# CC0) covers 278 species; taxonomy follows the Checklist of the Collembola of
# the World.
#
# This is the openly-licensed analogue of BETSI's legend-locked multi-trait
# matrices: per species it carries vertical stratification, reproduction mode
# and climatic preferences -- the traits BETSI holds behind an undocumented
# species-code legend. Its body-size column agrees with the taxifydb BETSI
# body-length floor (Pearson r = 0.96 over 262 shared species; the residual is
# Ellers recording maximum size against BETSI's median), which cross-validates
# the species matching. See gcol33/taxifydb#42.


#' Parse the Ellers et al. Collembola trait table into per-species values
#'
#' Reads the CC0 `Collembola_trait_data.csv` (Ellers et al. 2018) and returns
#' one row per species. Ordinal codes whose levels are named in the source are
#' decoded (reproduction, moisture preference); the biogeographic
#' temperature-zone class and the thermal-niche-breadth count are kept as their
#' source ordinal codes.
#'
#' Columns:
#' * `ellers_vertical_distribution` -- soil, subsurface or surface.
#' * `ellers_body_size_mm` -- body size (mm).
#' * `ellers_reproduction` -- asexual or sexual.
#' * `ellers_moisture_pref` -- xerophilous, xero-mesophilous, mesophilous,
#'   hygro-mesophilous or hygrophilous.
#' * `ellers_temperature_pref` -- biogeographic temperature-zone class, ordinal
#'   1-5 (1 = boreal zone only ... 5 = Mediterranean zone only) per the source
#'   codebook.
#' * `ellers_thermal_niche_breadth` -- number of biogeographic zones occupied,
#'   ordinal 1-3.
#'
#' @param path Character. Path to `Collembola_trait_data.csv` (or a directory
#'   containing it).
#' @return data.frame with `canonical_name` and the `ellers_*` trait columns,
#'   one row per species.
#' @export
parse_ellers_collembola <- function(path) {
  file <- if (dir.exists(path)) {
    f <- list.files(path, pattern = "(?i)collembola.*\\.csv$", full.names = TRUE)
    if (!length(f)) f <- list.files(path, pattern = "(?i)\\.csv$", full.names = TRUE)
    if (!length(f)) stop("No Ellers Collembola .csv found in: ", path, call. = FALSE)
    f[[1L]]
  } else {
    path
  }

  d <- utils::read.csv(file, stringsAsFactors = FALSE, check.names = FALSE)
  # "Thermal niche breadth" wraps onto a second physical header row, so the last
  # column parses as "Thermal" and the wrapped fragment lands as a data row with
  # an empty Species. Rename the column and drop that row.
  names(d)[ncol(d)] <- "Thermal niche breadth"
  need <- c("Species", "Vertical distribution", "Body size", "Reproduction mode",
            "Moisture preference", "Temperature preference", "Thermal niche breadth")
  miss <- setdiff(need, names(d))
  if (length(miss)) {
    stop("parse_ellers_collembola: missing column(s): ",
         paste(miss, collapse = ", "), call. = FALSE)
  }
  d <- d[nzchar(trimws(d[["Species"]])), , drop = FALSE]

  pat <- "^\\s*([A-Z][a-z]+)\\s+([a-z][a-z-]+).*$"
  sp <- ifelse(grepl(pat, d[["Species"]]),
               trimws(sub(pat, "\\1 \\2", d[["Species"]])), NA_character_)

  vd <- tolower(trimws(as.character(d[["Vertical distribution"]])))
  vd <- gsub("sub-surface", "subsurface", vd, fixed = TRUE)
  vd[!nzchar(vd) | vd == "na"] <- NA_character_

  repro <- c("1" = "asexual", "2" = "sexual")[trimws(as.character(d[["Reproduction mode"]]))]
  moist <- c("1" = "xerophilous", "2" = "xero-mesophilous", "3" = "mesophilous",
             "4" = "hygro-mesophilous", "5" = "hygrophilous")[
               trimws(as.character(d[["Moisture preference"]]))]

  num  <- function(x) suppressWarnings(as.numeric(as.character(x)))
  ordc <- function(x, lvls) {
    v <- suppressWarnings(as.integer(as.character(x)))
    v[!(v %in% lvls)] <- NA_integer_
    v
  }

  out <- data.frame(
    canonical_name               = sp,
    ellers_vertical_distribution = vd,
    ellers_body_size_mm          = num(d[["Body size"]]),
    ellers_reproduction          = unname(repro),
    ellers_moisture_pref         = unname(moist),
    ellers_temperature_pref      = ordc(d[["Temperature preference"]], 1:5),
    ellers_thermal_niche_breadth = ordc(d[["Thermal niche breadth"]], 1:3),
    stringsAsFactors = FALSE, row.names = NULL
  )
  out <- out[!is.na(out$canonical_name) & nzchar(out$canonical_name), , drop = FALSE]
  out <- out[.is_binomial(out$canonical_name), , drop = FALSE]
  out <- out[!duplicated(out$canonical_name), , drop = FALSE]
  out[order(out$canonical_name), , drop = FALSE]
}
