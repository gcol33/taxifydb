# InvaCost economic-cost aggregates.
#
# InvaCost (Diagne et al. 2020, Scientific Data) is a compilation of individual
# monetary cost estimates of biological invasions. InvaCost is CC BY 4.0
# throughout, estimate rows included, so the distribution boundary here is
# grain, not license: a .vtr is a lookup keyed on canonical_name, and the raw
# rows key on species x estimate x currency x cost year x method. They are an
# evidence table, not a per-species lookup, so this parser reduces them to
# aggregates, as the GloBI interaction rollup does. Use the invacost package
# for a rigorous cost analysis over the estimate rows themselves.
#
# Like parse_globi, names are resolved to the accepted grain inside the parser
# (resolve_name_map) and the total is summed there, so a species whose costs are
# split across two InvaCost names (a synonym and its accepted name) keeps the
# full total -- the build-time .dedup_keep_richest would otherwise drop one of
# the two collapsing rows. The registry entry therefore sets resolve_names = FALSE.

#' Parse the InvaCost economic-cost database into per-species aggregates
#'
#' Reads the InvaCost v4.1 workbook and reduces the individual cost estimates to
#' three per-species economic-impact indicators. The `.vtr` carries these derived
#' aggregates; for the individual cost estimates, use the invacost package.
#'
#' `cost_total_usd` is the InvaCost-convention cumulative cost: each estimate's
#' standardised annual cost (`Cost_estimate_per_year_2017_USD_exchange_rate`, in
#' 2017 USD at the exchange rate) is expanded across the number of years its
#' documented impact period spans (`Probable_ending_year_adjusted` -
#' `Probable_starting_year_adjusted` + 1, floored at one year where the period is
#' a single year or unrecorded) and summed across every estimate for the species.
#' Temporally or spatially overlapping estimates are not de-duplicated (the
#' `invacost` package's `summarizeCosts()` is the tool for a rigorous cost
#' analysis); `cost_total_usd` is a coarse economic-impact indicator, not an
#' audited figure. `cost_type` is the merged cost type (damage / management /
#' mixed / unspecified) that accounts for the largest share of the species'
#' cumulative cost.
#'
#' @param path Character. Path to `InvaCost_database_v4.1.xlsx`.
#' @return data.frame with `canonical_name`, `cost_total_usd` (2017 USD),
#'   `cost_n` (number of cost estimates), `cost_type` (dominant cost type).
#' @export
parse_invacost <- function(path) {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop("Package 'openxlsx2' is required to parse InvaCost.", call. = FALSE)
  }
  file <- if (dir.exists(path)) {
    f <- list.files(path, pattern = "(?i)invacost.*\\.xlsx$", full.names = TRUE)
    if (!length(f)) stop("No InvaCost .xlsx found in: ", path, call. = FALSE)
    f[[1L]]
  } else {
    path
  }
  df <- openxlsx2::wb_to_df(openxlsx2::wb_load(file), sheet = 1)

  num <- function(col) {
    if (!col %in% names(df)) return(rep(NA_real_, nrow(df)))
    suppressWarnings(as.numeric(df[[col]]))
  }
  species <- trimws(as.character(df[["Species"]]))

  # One "Diverse/Unspecified" cost lumps many taxa; a genus- or higher-level row
  # cannot be attached to a species. Keep only proper binomials (the runtime
  # joins at the species grain anyway).
  keep <- !is.na(species) & nzchar(species) &
    species != "Diverse/Unspecified" & .is_binomial(species)

  cost_year <- num("Cost_estimate_per_year_2017_USD_exchange_rate")
  start_y   <- num("Probable_starting_year_adjusted")
  end_y     <- num("Probable_ending_year_adjusted")
  # Expand the annualised cost across its documented period; a missing or
  # single-year period spans one year.
  duration  <- end_y - start_y + 1
  duration[!is.finite(duration) | duration < 1] <- 1
  row_total <- cost_year * duration

  ctype <- trimws(as.character(df[["Type_of_cost_merged"]]))
  ctype[is.na(ctype) | !nzchar(ctype)] <- "Unspecified"

  keep <- keep & is.finite(row_total) & row_total > 0
  species   <- species[keep]
  row_total <- row_total[keep]
  ctype     <- ctype[keep]
  if (!length(species)) {
    stop("parse_invacost: no usable cost rows after filtering.", call. = FALSE)
  }

  # Resolve to the accepted grain and sum there (see file header). Each source
  # name contributes its costs to every accepted name it maps to across backends.
  map <- resolve_name_map(unique(species), verbose = FALSE)
  acc <- split(map$accepted_name, map$input_name)
  # Expand each cost row to the accepted name(s) of its species.
  idx  <- rep(seq_along(species), lengths(acc[species]))
  accn <- unlist(acc[species], use.names = FALSE)
  if (!length(accn)) {
    stop("parse_invacost: no names resolved against any backend.", call. = FALSE)
  }
  tot  <- row_total[idx]
  typ  <- ctype[idx]

  total_by_acc <- tapply(tot, accn, sum, na.rm = TRUE)
  n_by_acc     <- tapply(tot, accn, length)
  # Dominant type = the type carrying the largest share of an accepted name's
  # cumulative cost (weighted by cost, not by row count).
  type_by_acc <- tapply(seq_along(accn), accn, function(i) {
    by_type <- tapply(tot[i], typ[i], sum, na.rm = TRUE)
    names(by_type)[which.max(by_type)]
  })

  accepted <- names(total_by_acc)
  out <- data.frame(
    canonical_name = accepted,
    cost_total_usd = as.numeric(total_by_acc[accepted]),
    cost_n         = as.integer(n_by_acc[accepted]),
    cost_type      = tolower(as.character(type_by_acc[accepted])),
    stringsAsFactors = FALSE
  )
  out <- out[.is_binomial(out$canonical_name), , drop = FALSE]
  out[order(out$canonical_name), , drop = FALSE]
}
