# Trait parsers (wave 2 file-based: BET bryophytes, PHYLACINE mammals,
# CheloniansTraits turtles, BIRDBASE birds). Each returns a data.frame with
# canonical_name + trait columns for resolve_enrichment_names() +
# build_enrichment_vtr().


# ---- BET: Bryophytes of Europe Traits ---------------------------------------

#' Convert an indicator value to numeric, treating "x" (indifferent) as NA
#' @noRd
.bet_ind <- function(v) {
  x <- trimws(as.character(v))
  x[tolower(x) == "x" | x == "" | x == "NA"] <- NA
  suppressWarnings(as.numeric(x))
}

#' Parse the Bryophytes of Europe Traits (BET) table
#'
#' One row per species. The source is R `write.table` output (space-separated,
#' quoted strings, leading row-id column). Ellenberg-style indicator values use
#' "x" for indifferent, which is mapped to NA.
#'
#' @param path Character. Path to `betdata.txt`.
#' @return data.frame with canonical_name + bryophyte traits.
#' @export
parse_bet <- function(path) {
  df <- utils::read.table(path, header = TRUE, sep = " ", quote = "\"",
                          na.strings = "NA", stringsAsFactors = FALSE,
                          check.names = FALSE, comment.char = "")
  chr <- function(col) {
    if (!col %in% names(df)) return(rep(NA_character_, nrow(df)))
    x <- trimws(as.character(df[[col]]))
    x[x == "" | x == "NA"] <- NA_character_
    x
  }
  num <- function(col) {
    if (!col %in% names(df)) return(rep(NA_real_, nrow(df)))
    suppressWarnings(as.numeric(df[[col]]))
  }
  ind <- function(col) {
    if (!col %in% names(df)) return(rep(NA_real_, nrow(df)))
    .bet_ind(df[[col]])
  }

  cname <- chr("friendly_name")
  out <- data.frame(
    canonical_name      = cname,
    growth_form         = chr("gform"),
    life_form           = chr("lform"),
    life_strategy       = chr("lstrat"),
    sexual_condition    = chr("sex"),
    shoot_size_mm       = num("size"),
    generation_length_y = num("genl"),
    spore_diameter_um   = num("smeand"),
    ind_light           = ind("indL"),
    ind_temperature     = ind("indT"),
    ind_moisture        = ind("indF"),
    ind_reaction_ph     = ind("indR"),
    ind_nitrogen        = ind("indN"),
    substrate_soil      = num("sub_so"),
    substrate_rock      = num("sub_ro"),
    substrate_bark      = num("sub_ba"),
    substrate_deadwood  = num("sub_wo"),
    epiphyte            = num("epiphyte"),
    redlist_category    = chr("category"),
    stringsAsFactors    = FALSE
  )
  out <- .append_all_cols(
    out, df, cname,
    used = c("friendly_name", "gform", "lform", "lstrat", "sex", "size",
             "genl", "smeand", "indL", "indT", "indF", "indR", "indN",
             "sub_so", "sub_ro", "sub_ba", "sub_wo", "epiphyte", "category")
  )
  .trait_finalize(out)
}


# ---- PHYLACINE: mammals including recently extinct --------------------------

#' Classify a PHYLACINE `Mass.Method` string into a provenance tier
#'
#' PHYLACINE 1.2 records how each body-mass value was obtained. Three tiers
#' matter for whether a mass is a measurement or a model output: `reported`
#' (measured/compiled), `imputed` (phylogenetic gap-fill), and `estimated`
#' (everything else -- allometric scaling from a body dimension, or the mass of
#' a similarly sized relative). Only `reported` is an observation; `estimated`
#' and `imputed` are model outputs.
#' @noRd
.phylacine_mass_class <- function(method) {
  cls <- rep(NA_character_, length(method))
  m   <- tolower(trimws(method))
  cls[!is.na(m) & nzchar(m)] <- "estimated"
  cls[m == "reported"] <- "reported"
  cls[m == "imputed"]  <- "imputed"
  cls
}

#' Parse PHYLACINE v1.2 trait data
#'
#' One row per mammal species (including recently and prehistorically extinct).
#' The binomial is stored with an underscore (`Genus_species`) and is converted
#' to a space-separated canonical name.
#'
#' `Mass.g` is partly modelled: PHYLACINE gap-fills data-poor and extinct
#' species. The source `Mass.Method` flag is kept verbatim as `mass_method`,
#' alongside a coarse `mass_method_class` (`reported` / `estimated` / `imputed`)
#' so a modelled mass is never served as a measurement where PHYLACINE is the
#' sole source for a species.
#'
#' @param path Character. Path to `Trait_data.csv`.
#' @return data.frame with canonical_name + mammal traits.
#' @export
parse_phylacine <- function(path) {
  df <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
                        na.strings = "NA")
  num <- function(col) {
    if (!col %in% names(df)) return(rep(NA_real_, nrow(df)))
    suppressWarnings(as.numeric(df[[col]]))
  }
  chr <- function(col) {
    if (!col %in% names(df)) return(rep(NA_character_, nrow(df)))
    x <- trimws(as.character(df[[col]]))
    x[x == "" | x == "NA"] <- NA_character_
    x
  }
  cname       <- trimws(gsub("_", " ", chr("Binomial.1.2")))
  mass_method <- chr("Mass.Method")
  out <- data.frame(
    canonical_name        = cname,
    mass_g                = num("Mass.g"),
    mass_method           = mass_method,
    mass_method_class     = .phylacine_mass_class(mass_method),
    diet_plant_pct        = num("Diet.Plant"),
    diet_vertebrate_pct   = num("Diet.Vertebrate"),
    diet_invertebrate_pct = num("Diet.Invertebrate"),
    terrestrial           = num("Terrestrial"),
    marine                = num("Marine"),
    freshwater            = num("Freshwater"),
    aerial                = num("Aerial"),
    island_endemicity     = chr("Island.Endemicity"),
    iucn_status           = chr("IUCN.Status.1.2"),
    stringsAsFactors      = FALSE
  )
  out <- .append_all_cols(
    out, df, cname,
    used = c("Binomial.1.2", "Mass.g", "Mass.Method", "Diet.Plant",
             "Diet.Vertebrate", "Diet.Invertebrate", "Terrestrial", "Marine",
             "Freshwater", "Aerial", "Island.Endemicity", "IUCN.Status.1.2"),
    cat_cols = "Mass.Method"
  )
  .trait_finalize(out)
}


# ---- CheloniansTraits: turtles ---------------------------------------------

#' Parse a numeric value that may be a "min-max" text range
#'
#' Returns the midpoint of an `a-b` range, or the single value. Non-numeric and
#' "NA" become NA.
#' @noRd
.num_range <- function(v) {
  x <- trimws(as.character(v))
  vapply(x, function(s) {
    if (is.na(s) || !nzchar(s) || s == "NA") return(NA_real_)
    parts <- strsplit(s, "\\s*-\\s*")[[1L]]
    parts <- suppressWarnings(as.numeric(parts))
    parts <- parts[is.finite(parts)]
    if (!length(parts)) NA_real_ else mean(parts)
  }, numeric(1L), USE.NAMES = FALSE)
}

#' Match a column by squished (non-alphanumeric-stripped, case-insensitive) name
#' @noRd
.squish_pick <- function(df, target) {
  sq <- function(s) gsub("[^a-z0-9]", "", tolower(s))
  hit <- which(sq(names(df)) == sq(target))
  if (length(hit)) df[[hit[1L]]] else NULL
}

#' Parse the CheloniansTraits turtle database
#'
#' Reads the data sheet (real header is the second physical row), treats the
#' literal "NA" as missing, and parses "min-max" text ranges to their midpoint.
#' One row per species.
#'
#' @param path Character. Path to `CheloniansTraits dataset.xlsx`.
#' @return data.frame with canonical_name + turtle traits.
#' @export
parse_chelonians <- function(path) {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop("Package 'openxlsx2' is required to parse CheloniansTraits.",
         call. = FALSE)
  }
  sheets <- openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(path))
  data_sheet <- sheets[grepl("data", tolower(sheets))][1L]
  if (is.na(data_sheet)) data_sheet <- sheets[1L]
  df <- openxlsx2::read_xlsx(path, sheet = data_sheet, start_row = 2L)

  numr <- function(target) {
    v <- .squish_pick(df, target)
    if (is.null(v)) rep(NA_real_, nrow(df)) else .num_range(v)
  }
  chr <- function(target) {
    v <- .squish_pick(df, target)
    if (is.null(v)) return(rep(NA_character_, nrow(df)))
    x <- trimws(as.character(v))
    x[x == "" | x == "NA"] <- NA_character_
    x
  }

  cname <- chr("Species")
  out <- data.frame(
    canonical_name     = cname,
    carapace_length_mm = numr("Maximum straight-line carapace length (mm)"),
    max_mass_g         = numr("Maximum mass (g)"),
    clutch_size_mean   = numr("Mean Clutch size"),
    clutch_size_max    = numr("Clutch size (max)"),
    clutches_per_year  = numr("Number of clutches per year"),
    incubation_d       = numr("Incubation period (day)"),
    age_maturity_y     = numr("Age at sexual maturity"),
    max_lifespan_y     = numr("Maximum lifespan (year)"),
    range_size_km2     = numr("Range size (km^2)"),
    diet               = chr("Diet"),
    activity_time      = chr("Activity time"),
    microhabitat       = chr("Microhabitat"),
    habitat_type       = chr("Habitat type"),
    shell_type         = chr("Shell type (Hardshell/Softshell)"),
    stringsAsFactors   = FALSE
  )
  out <- .append_all_cols(
    out, df, cname,
    used = .squish_used(df, c(
      "Species", "Maximum straight-line carapace length (mm)",
      "Maximum mass (g)", "Mean Clutch size", "Clutch size (max)",
      "Number of clutches per year", "Incubation period (day)",
      "Age at sexual maturity", "Maximum lifespan (year)", "Range size (km^2)",
      "Diet", "Activity time", "Microhabitat", "Habitat type",
      "Shell type (Hardshell/Softshell)"))
  )
  .trait_finalize(out)
}


# ---- BIRDBASE: bird traits (biogeography / conservation / life history) -----

#' Parse the BIRDBASE bird trait database
#'
#' Reads the Data sheet (real header is the second physical row). Presence-coded
#' columns (island, restricted-range) use a blank for absent, mapped to 0.
#' Traits redundant with AVONET morphology are not carried. One row per species.
#'
#' @param path Character. Path to the BIRDBASE xlsx.
#' @return data.frame with canonical_name + bird traits.
#' @export
parse_birdbase <- function(path) {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop("Package 'openxlsx2' is required to parse BIRDBASE.", call. = FALSE)
  }
  sheets <- openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(path))
  data_sheet <- sheets[tolower(sheets) == "data"][1L]
  if (is.na(data_sheet)) data_sheet <- sheets[1L]
  df <- openxlsx2::read_xlsx(path, sheet = data_sheet, start_row = 2L)

  pick <- function(target) .squish_pick(df, target)
  chr <- function(target) {
    v <- pick(target)
    if (is.null(v)) return(rep(NA_character_, nrow(df)))
    x <- trimws(as.character(v))
    x[x == "" | x == "NA"] <- NA_character_
    x
  }
  num <- function(target) {
    v <- pick(target)
    if (is.null(v)) return(rep(NA_real_, nrow(df)))
    suppressWarnings(as.numeric(v))
  }
  flag <- function(target) {
    v <- num(target)
    v[is.na(v)] <- 0
    v
  }

  name_v <- pick("Latin (BirdLife > IOC > Clements>AviList)")
  if (is.null(name_v)) name_v <- df[[3L]]
  canonical <- trimws(as.character(name_v))
  canonical[canonical == "" | canonical == "NA"] <- NA_character_

  out <- data.frame(
    canonical_name     = canonical,
    iucn_status        = chr("2024 IUCN Red List category"),
    realm              = chr("RLM"),
    latitudinal_zone   = num("LAT"),
    island_endemic     = flag("ISL"),
    restricted_range   = flag("RR"),
    elevation_min_m    = num("NormMin"),
    elevation_max_m    = num("NormMax"),
    elevation_range_m  = num("Elevational Range"),
    primary_habitat    = chr("Primary Habitat"),
    habitat_breadth    = num("HB"),
    primary_diet       = chr("Primary Diet"),
    diet_breadth       = num("DB"),
    specialization_esi = num("ESI"),
    clutch_min         = num("Clutch_Min"),
    clutch_max         = num("Clutch_Max"),
    nest_type          = chr("Nest_Type"),
    flightlessness     = chr("Flightlessness"),
    stringsAsFactors   = FALSE
  )
  # Mean clutch size from the reported min/max (single-value rows keep that
  # value); feeds the cross-source clutch_litter_size trait.
  cm <- rowMeans(cbind(out$clutch_min, out$clutch_max), na.rm = TRUE)
  cm[is.nan(cm)] <- NA_real_
  out$clutch_mean <- cm
  out <- .append_all_cols(
    out, df, canonical,
    used = .squish_used(df, c(
      "Latin (BirdLife > IOC > Clements>AviList)",
      "2024 IUCN Red List category", "RLM", "LAT", "ISL", "RR",
      "NormMin", "NormMax", "Elevational Range", "Primary Habitat", "HB",
      "Primary Diet", "DB", "ESI", "Clutch_Min", "Clutch_Max",
      "Nest_Type", "Flightlessness"))
  )
  .trait_finalize(out)
}
