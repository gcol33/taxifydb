# Wave-A trait parsers: freshwater thermal tolerance (ThermoFresh), plankton
# protist traits (Ramond), freshwater-insect genus traits (EDI CONUS), and
# European bat traits (EuroBaTrait). Each melts its source to a long
# (name, trait, value) table and reuses .pivot_species_traits() for the
# median/mode reduction, so the aggregation lives in one place.


#' Parse the Freshwater thermal-tolerance database (ThermoFresh)
#'
#' Per-test critical thermal limits (Bayat et al.) for freshwater fish,
#' invertebrates and amphibians. Each source row is one tolerance test; the
#' processed tests are joined to the taxonomy table on `tax_id`, restricted to
#' species-level taxa, and reduced to one row per species (median per metric).
#' The retained metrics are the critical thermal maximum (`ctmax`), minimum
#' (`ctmin`), the median lethal temperature (`lt50`) and the lethal maximum /
#' minimum (`ltmax`, `ltmin`); all values are in degrees Celsius.
#'
#' The archive ships the peer-reviewed tables as `data/*_final.csv` and keeps
#' the pre-review submission beside them under `data/initial_submission/`, so
#' the readers below match the `_final` names exactly.
#'
#' @param path Directory holding the extracted archive (with a `data/`
#'   subdirectory) or a path directly to that `data/` directory.
#' @return data.frame with canonical_name + freshwater thermal-limit traits.
#' @export
parse_thermofresh <- function(path) {
  find1 <- function(pat) {
    f <- list.files(path, pattern = pat, recursive = TRUE, full.names = TRUE)
    if (length(f) != 1L)
      stop(sprintf("ThermoFresh: expected one file matching '%s', found %d.",
                   pat, length(f)), call. = FALSE)
    f
  }
  tests <- utils::read.csv(find1("thermtol_tests_processed_final\\.csv$"),
                           stringsAsFactors = FALSE, check.names = FALSE)
  tax   <- utils::read.csv(find1("thermtol_taxonomy_final\\.csv$"),
                           stringsAsFactors = FALSE, check.names = FALSE)

  # The `species` column already holds the full binomial; genus- and
  # higher-level tests have no clean binomial key and are dropped.
  tax <- tax[trimws(as.character(tax$tax_level)) == "species", , drop = FALSE]
  binom <- trimws(as.character(tax$species))
  name_by_id <- stats::setNames(binom, as.character(tax$tax_id))

  keep_metric <- c("ctmax", "ctmin", "lt50", "ltmax", "ltmin")
  metric <- tolower(trimws(as.character(tests$metric)))
  ok <- metric %in% keep_metric &
    as.character(tests$tax_id) %in% names(name_by_id)
  tests <- tests[ok, , drop = FALSE]

  long <- data.frame(
    name  = unname(name_by_id[as.character(tests$tax_id)]),
    trait = metric[ok],
    value = suppressWarnings(as.numeric(tests$tol)),
    stringsAsFactors = FALSE
  )
  spec <- stats::setNames(
    lapply(keep_metric, function(m) list(trait = m, type = "num")),
    keep_metric
  )
  .trait_finalize(.pivot_species_traits(long, spec, keep_all = FALSE))
}


#' Parse the Ramond et al. plankton protist trait database
#'
#' Morphological, behavioural and ecological traits of marine protists,
#' keyed on the finest determined name (`Last`), which is genus-level or higher.
#' The three source tables share the same taxa with overlapping trait columns;
#' they are stacked and reduced to one row per genus (numeric traits -- cell
#' size, in micrometres -- by median, categorical traits by mode). Higher-rank
#' and undetermined entries do not match a backbone genus and drop out at name
#' resolution.
#'
#' @param path Directory holding the three source CSVs.
#' @return data.frame with canonical_name (genus) + protist traits.
#' @export
parse_ramond <- function(path) {
  files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
  if (!length(files)) stop("Ramond: no source CSVs found.", call. = FALSE)
  # Non-trait bookkeeping columns: taxonomy above the key and per-row citations.
  drop_cols <- c("Lineage", "Fam", "Taxogroup", "Taxo1", "Last",
                 paste0("Reference", 1:9))

  long_parts <- lapply(files, function(f) {
    d <- utils::read.csv(f, sep = ";", stringsAsFactors = FALSE,
                         check.names = FALSE, fileEncoding = "latin1")
    d <- d[, nzchar(names(d)), drop = FALSE]        # trailing ';' empty column
    nm <- trimws(as.character(d$Last))
    trait_cols <- setdiff(names(d), drop_cols)
    parts <- lapply(trait_cols, function(cc) {
      data.frame(name = nm, trait = cc,
                 value = trimws(as.character(d[[cc]])),
                 stringsAsFactors = FALSE)
    })
    do.call(rbind, parts)
  })
  long <- do.call(rbind, long_parts)
  long <- long[!is.na(long$name) & nzchar(long$name) &
                 !grepl("ndetermined", long$name, ignore.case = TRUE), ,
               drop = FALSE]
  long <- long[!is.na(long$value) & nzchar(long$value) &
                 toupper(long$value) != "NA", , drop = FALSE]

  spec <- list(
    size_min_um = list(trait = "SizeMin", type = "num"),
    size_max_um = list(trait = "SizeMax", type = "num")
  )
  .trait_finalize(.pivot_species_traits(long, spec, keep_all = TRUE))
}


#' Parse the Freshwater Insects CONUS genus trait table (EDI edi.481)
#'
#' Genus-level ecological and life-history traits of North American freshwater
#' insects, distributed in long form (one row per genus and trait group). Pivoted
#' to one row per genus; every trait group is categorical (the source ships the
#' abbreviated modality codes, kept verbatim).
#'
#' @param path Path to the "Insect Traits by Genus" CSV.
#' @return data.frame with canonical_name (genus) + freshwater-insect traits.
#' @export
parse_fw_insects_conus <- function(path) {
  f <- if (dir.exists(path)) {
    list.files(path, pattern = "\\.csv$", full.names = TRUE)[1L]
  } else path
  d <- utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  long <- data.frame(
    name  = trimws(as.character(d$Genus)),
    trait = trimws(as.character(d$Trait_group)),
    value = trimws(as.character(d$Trait)),
    stringsAsFactors = FALSE
  )
  .trait_finalize(.pivot_species_traits(long, list(), keep_all = TRUE))
}


#' Parse the EuroBaTrait European bat trait dataset
#'
#' Species-level traits of European bats spread across thematic Darwin Core
#' measurement-or-fact tables (morphology, life history, diet, foraging habitat,
#' roost type). Each table is melted to (species, trait, value), stacked, and
#' reduced to one row per species (numeric traits by median, categorical by
#' mode). Underscored source names (`Genus_species`) are normalised to a
#' binomial.
#'
#' @param path Directory holding the thematic trait CSVs.
#' @return data.frame with canonical_name + European bat traits.
#' @export
parse_eurobat <- function(path) {
  files <- list.files(path, pattern = "^[0-9].*\\.csv$", full.names = TRUE)
  if (!length(files)) stop("EuroBaTrait: no thematic CSVs found.", call. = FALSE)
  long_parts <- lapply(files, function(f) {
    d <- utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE,
                         fileEncoding = "UTF-8-BOM")
    need <- c("verbatimScientificName", "verbatimTraitName", "verbatimTraitValue")
    if (!all(need %in% names(d))) return(NULL)
    data.frame(
      name  = gsub("_", " ", trimws(as.character(d$verbatimScientificName))),
      trait = trimws(as.character(d$verbatimTraitName)),
      value = trimws(as.character(d$verbatimTraitValue)),
      stringsAsFactors = FALSE
    )
  })
  long <- do.call(rbind, long_parts[!vapply(long_parts, is.null, logical(1L))])
  long <- long[!is.na(long$value) & nzchar(long$value) &
                 toupper(long$value) != "NA", , drop = FALSE]

  spec <- list(
    forearm_length_mm = list(trait = "ForearmLength", type = "num"),
    body_mass_g       = list(trait = "BodyMass",      type = "num"),
    max_longevity_yr  = list(trait = "MaxLongevity",  type = "num"),
    litter_size       = list(trait = "LitterSize",    type = "num"),
    diet_type         = list(trait = "DietType",      type = "cat"),
    first_main_prey   = list(trait = "FirstMainPreyItem", type = "cat")
  )
  .trait_finalize(.pivot_species_traits(long, spec, keep_all = TRUE))
}
