# Collembola ecomorphosis (CC BY 4.0).
#
# Bonfanti, Krogh, Hedde & Cortet (2022, Applied Soil Ecology,
# doi:10.1016/j.apsoil.2022.104692) reviewed ecomorphosis in European
# Collembola -- the seasonal moult into a distinct resting/"pseudo-pupal" form
# that some species undergo. The accompanying Zenodo list (doi:10.5281/zenodo.
# 7194559, CC BY 4.0) is the curated set of species with a documented ecomorphic
# form, each with the literature record establishing it.
#
# This is a presence list: every row is a species known to display ecomorphosis,
# so the enrichment marks membership (`ecomorphosis = TRUE`); absence from the
# list means undocumented, not confirmed-absent. See gcol33/taxifydb#42.


#' Parse the Bonfanti et al. Collembola ecomorphosis list into per-species rows
#'
#' Reads the Zenodo ecomorphosis species list and returns one row per species
#' known to display ecomorphosis.
#'
#' Columns:
#' * `ecomorphosis` -- `TRUE` for every listed species (a presence list).
#' * `ecomorphosis_area` -- geographical area the record applies to.
#' * `ecomorphosis_reference` -- the literature record establishing it.
#'
#' @param path Character. Path to `Annex_ASE_Online_db.xlsx` (or a directory
#'   containing it).
#' @return data.frame with `canonical_name` and the `ecomorphosis*` columns,
#'   one row per species.
#' @export
parse_ecomorphosis <- function(path) {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop("Package 'openxlsx2' is required to parse ecomorphosis.", call. = FALSE)
  }
  file <- if (dir.exists(path)) {
    f <- list.files(path, pattern = "(?i)\\.xlsx$", full.names = TRUE)
    if (!length(f)) stop("No ecomorphosis .xlsx found in: ", path, call. = FALSE)
    f[[1L]]
  } else {
    path
  }

  wb <- openxlsx2::wb_load(file)
  sheet <- if ("Ecomorphic species list" %in% wb$sheet_names) {
    "Ecomorphic species list"
  } else {
    wb$sheet_names[[length(wb$sheet_names)]]
  }
  d <- openxlsx2::wb_to_df(wb, sheet = sheet)

  name_col <- if ("Valid species name" %in% names(d)) "Valid species name" else names(d)[[3L]]
  pat <- "^\\s*([A-Z][a-z]+)\\s+([a-z][a-z-]+).*$"
  sp <- ifelse(grepl(pat, d[[name_col]]),
               trimws(sub(pat, "\\1 \\2", d[[name_col]])), NA_character_)

  pick <- function(nm) if (nm %in% names(d)) trimws(as.character(d[[nm]])) else NA_character_
  area <- pick("Geographical area")
  ref  <- pick("Literature reference")

  out <- data.frame(
    canonical_name         = sp,
    ecomorphosis           = TRUE,
    ecomorphosis_area      = area,
    ecomorphosis_reference = ref,
    stringsAsFactors = FALSE, row.names = NULL
  )
  out <- out[!is.na(out$canonical_name) & nzchar(out$canonical_name), , drop = FALSE]
  out <- out[.is_binomial(out$canonical_name), , drop = FALSE]
  out <- out[!duplicated(out$canonical_name), , drop = FALSE]
  out[order(out$canonical_name), , drop = FALSE]
}
