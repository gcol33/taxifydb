# Wave 3 trait parsers.
#
# Each parse_<name>() returns a data.frame with canonical_name + species-level
# trait columns, ready for resolve_enrichment_names(). Shared helpers
# (.pivot_species_traits, .trait_finalize) live in
# enrichment-parsers-traits-{,2}.R.


#' Parse Sharkipedia elasmobranch life-history traits
#'
#' Long-format trait table (one row per observation); reduced to one row per
#' species (numeric traits by median) for a curated set of growth, length, age
#' and reproduction traits.
#'
#' @param path Path to the Sharkipedia traits CSV.
#' @return data.frame with canonical_name + traits.
#' @export
parse_sharkipedia <- function(path) {
  d <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
                       fileEncoding = "UTF-8")
  long <- data.frame(
    name  = as.character(d$species_name),
    trait = as.character(d$trait_name),
    value = as.character(d$value),
    stringsAsFactors = FALSE
  )
  spec <- list(
    lmax_cm                = list(trait = "Lmax-observed", type = "num"),
    vbgf_linf_cm           = list(trait = "Linf", type = "num"),
    vbgf_k                 = list(trait = "k", type = "num"),
    vbgf_t0                = list(trait = "t0", type = "num"),
    length_first_maturity_cm = list(trait = "Length at first maturity",
                                    type = "num"),
    length_birth_cm        = list(trait = "Lbirth", type = "num"),
    amax_observed_yr       = list(trait = "Amax-observed", type = "num"),
    age_first_maturity_yr  = list(trait = "Age at first maturity",
                                  type = "num"),
    uterine_fecundity      = list(trait = "Uterine fecundity", type = "num"),
    gestation_length       = list(trait = "Gestation length", type = "num"),
    natural_mortality      = list(trait = "Natural mortality", type = "num")
  )
  .trait_finalize(.pivot_species_traits(long, spec))
}


#' Parse Bird Nest Traits (NestTrait v2)
#'
#' Wide table, one row per bird species, with binary presence flags for nest
#' site, nest structure and nest attachment plus mound-builder and brood-parasite
#' flags. Kept as 0/1 indicators.
#'
#' @param path Path to NestTrait_v2.csv.
#' @return data.frame with canonical_name + nest trait flags.
#' @export
parse_bird_nest <- function(path) {
  d <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
                       fileEncoding = "UTF-8")
  flag <- function(col) {
    if (!col %in% names(d)) return(rep(NA_real_, nrow(d)))
    suppressWarnings(as.numeric(d[[col]]))
  }
  out <- data.frame(
    canonical_name          = trimws(as.character(d$Scientific_name)),
    brood_parasite          = flag("Parasite"),
    mound_builder           = flag("Mound"),
    nestsite_ground         = flag("NestSite_ground"),
    nestsite_tree           = flag("NestSite_tree"),
    nestsite_nontree        = flag("NestSite_nontree"),
    nestsite_cliff_bank     = flag("NestSite_cliff_bank"),
    nestsite_underground    = flag("NestSite_underground"),
    nestsite_waterbody      = flag("NestSite_waterbody"),
    nestsite_termite_ant    = flag("NestSite_termite_ant"),
    neststr_scrape          = flag("NestStr_scrape"),
    neststr_platform        = flag("NestStr_platform"),
    neststr_cup             = flag("NestStr_cup"),
    neststr_dome            = flag("NestStr_dome"),
    neststr_dome_tunnel     = flag("NestStr_dome_tunnel"),
    neststr_primary_cavity  = flag("NestStr_primary_cavity"),
    neststr_second_cavity   = flag("NestStr_second_cavity"),
    nestatt_basal           = flag("NestAtt_basal"),
    nestatt_forked          = flag("NestAtt_forked"),
    nestatt_lateral         = flag("NestAtt_lateral"),
    nestatt_pensile         = flag("NestAtt_pensile"),
    stringsAsFactors        = FALSE
  )
  .trait_finalize(out)
}


#' Parse Octocoral Trait Database (v2.2)
#'
#' Long-format trait table; reduced to one row per species. Geographic and
#' bioregion fields are dropped; biological traits (colony morphology, polyp,
#' skeleton, symbiosis, feeding) are kept (numeric by median, categorical by
#' mode).
#'
#' @param path Path to OctocoralTraits_v2_2.csv.
#' @return data.frame with canonical_name + traits.
#' @export
parse_octocoral <- function(path) {
  d <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
                       fileEncoding = "UTF-8")
  long <- data.frame(
    name  = as.character(d$specie_name),
    trait = as.character(d$trait_name),
    value = as.character(d$value),
    stringsAsFactors = FALSE
  )
  spec <- list(
    colony_height                 = list(trait = "Colony height", type = "num"),
    colony_width                  = list(trait = "Colony width", type = "num"),
    tentacles_per_polyp           = list(trait = "Number of tentacles per polyp",
                                         type = "num"),
    growth_form                   = list(trait = "Growth form", type = "cat"),
    type_of_growth                = list(trait = "Type of growth", type = "cat"),
    type_of_skeleton              = list(trait = "Type of skeleton",
                                         type = "cat"),
    polyp_retractability          = list(trait = "Polyp retractability",
                                         type = "cat"),
    polyp_dimorphism              = list(trait = "Polyp dimorphism",
                                         type = "cat"),
    zooxanthellate                = list(trait = "Zooxanthellate", type = "cat"),
    axis_presence                 = list(trait = "Axis presence", type = "cat"),
    feeding_mechanism             = list(trait = "Feeding mechanism",
                                         type = "cat"),
    coloniality                   = list(trait = "Coloniality", type = "cat"),
    skeletal_rigidity             = list(trait = "Skeletal rigidity",
                                         type = "cat"),
    calcareous_sclerites_presence = list(trait = "Calcareous sclerites presence",
                                         type = "cat")
  )
  .trait_finalize(.pivot_species_traits(long, spec))
}


#' Parse Rimet & Druart phytoplankton metrics
#'
#' Wide table, one row per taxon, of cell and colony morphometrics. Cell-level
#' size/volume/area metrics are kept.
#'
#' @param path Path to the phytoplankton metrics xlsx.
#' @return data.frame with canonical_name + cell metrics.
#' @export
parse_rimet_phyto <- function(path) {
  d <- openxlsx2::read_xlsx(path)
  nm <- names(d)
  pick_num <- function(pat) {
    i <- grep(pat, nm, ignore.case = TRUE)[1]
    if (is.na(i)) rep(NA_real_, nrow(d)) else suppressWarnings(as.numeric(d[[i]]))
  }
  sp_i <- grep("Genus.*species name", nm, ignore.case = TRUE)[1]
  out <- data.frame(
    canonical_name        = trimws(as.character(d[[sp_i]])),
    cell_length_um        = pick_num("^Cell length"),
    cell_width_um         = pick_num("^Cell width"),
    cell_thickness_um     = pick_num("^Cell thickness"),
    cell_surface_area_um2 = pick_num("^Cell surface area"),
    cell_biovolume_um3    = pick_num("^Cell biovolume"),
    stringsAsFactors      = FALSE
  )
  .trait_finalize(out)
}


#' Parse Huang amphibian morphology
#'
#' Three per-order files (Anura, Caudata, Gymnophiona) of per-specimen
#' morphometrics; reduced to species-level medians for the measurements common
#' and comparable across orders (snout-vent length, head length/width, eye
#' diameter, fore/hind-limb length). Order-specific measurements are not carried
#' because column meanings differ between clades.
#'
#' @param path Directory holding Anura.csv / Caudata.csv / Gymnophiona.csv.
#' @return data.frame with canonical_name + morphometrics + taxon_order.
#' @export
parse_huang_amph <- function(path) {
  ord_files <- c(Anura = "Anura.csv", Caudata = "Caudata.csv",
                 Gymnophiona = "Gymnophiona.csv")
  cols <- c(svl_mm = "SVL", head_length_mm = "HL", head_width_mm = "HW",
            eye_diameter_mm = "ED", forelimb_length_mm = "FLL",
            hindlimb_length_mm = "HLL")
  acc <- list()
  for (ord in names(ord_files)) {
    f <- file.path(path, ord_files[[ord]])
    if (!file.exists(f)) next
    d <- utils::read.csv(f, check.names = FALSE, stringsAsFactors = FALSE,
                         fileEncoding = "UTF-8")
    g <- trimws(as.character(if ("Genus" %in% names(d)) d$Genus else rep(NA, nrow(d))))
    s <- trimws(as.character(if ("Species" %in% names(d)) d$Species else rep(NA, nrow(d))))
    # The Species column is inconsistent: sometimes a full binomial
    # ("Rana temporaria"), sometimes the bare epithet ("bufo"). Build a clean
    # binomial: if Species already starts with a capitalised genus take its
    # first two words, otherwise glue Genus + first Species word.
    cn <- vapply(seq_along(s), function(i) {
      w <- strsplit(s[i], "\\s+")[[1]]
      if (length(w) >= 2L && grepl("^[A-Z]", w[1])) paste(w[1], w[2])
      else trimws(paste(g[i], w[1]))
    }, character(1L))
    row <- data.frame(canonical_name = cn, taxon_order = ord,
                      stringsAsFactors = FALSE)
    for (oc in names(cols)) {
      sc <- cols[[oc]]
      row[[oc]] <- if (sc %in% names(d)) suppressWarnings(as.numeric(d[[sc]])) else NA_real_
    }
    acc[[ord]] <- row
  }
  df <- do.call(rbind, acc)
  df <- df[!is.na(df$canonical_name) & nzchar(trimws(df$canonical_name)) &
           grepl(" ", df$canonical_name), , drop = FALSE]
  num <- names(cols)
  agg <- stats::aggregate(
    df[num], by = list(canonical_name = df$canonical_name),
    FUN = function(z) { z <- z[is.finite(z)]; if (!length(z)) NA_real_ else stats::median(z) }
  )
  ord1 <- df[!duplicated(df$canonical_name), c("canonical_name", "taxon_order")]
  out <- merge(agg, ord1, by = "canonical_name")
  .trait_finalize(out)
}


#' Parse Ostwald global bee morphometrics
#'
#' Long-format Darwin Core measurement table; reduced to one row per species.
#' `verbatimAcceptedNameUsage` carries authorship and subgenera, so the join key
#' is reduced to a clean binomial.
#'
#' @param path Path to the morphological dataset CSV.
#' @return data.frame with canonical_name + bee morphometrics.
#' @export
parse_bee_ostwald <- function(path) {
  d <- data.table::fread(path, encoding = "Latin-1", data.table = FALSE)
  nm0 <- as.character(d$verbatimAcceptedNameUsage)
  nm0 <- iconv(nm0, from = "latin1", to = "ASCII//TRANSLIT", sub = "")
  raw <- gsub("\\s*\\([^)]*\\)", "", nm0)
  binom <- vapply(strsplit(trimws(raw), "\\s+"), function(w) {
    if (length(w) >= 2L) paste(w[1], w[2])
    else if (length(w) == 1L) w[1] else NA_character_
  }, character(1L))
  long <- data.frame(
    name  = binom,
    trait = as.character(d$measurementType),
    value = as.character(d$measurementValue),
    stringsAsFactors = FALSE
  )
  spec <- list(
    itd_mm             = list(trait = "ITD", type = "num"),
    forewing_length_mm = list(trait = "forewing length", type = "num"),
    tongue_length_mm   = list(trait = "tongue length", type = "num"),
    tongue_width_mm    = list(trait = "tongue width", type = "num"),
    body_length_mm     = list(trait = "body length", type = "num"),
    thorax_length_mm   = list(trait = "thorax length", type = "num"),
    hair_length_mm     = list(trait = "hair length", type = "num"),
    hair_coverage_pct  = list(trait = "hair coverage", type = "num")
  )
  .trait_finalize(.pivot_species_traits(long, spec))
}


#' Parse Pottier amphibian heat tolerance
#'
#' Per-measurement upper thermal-limit records reduced to species medians.
#'
#' @param path Path to Curated_data.csv.
#' @return data.frame with canonical_name + thermal/size traits.
#' @export
parse_pottier <- function(path) {
  d <- data.table::fread(path, encoding = "Latin-1", data.table = FALSE)
  num <- function(col) {
    if (!col %in% names(d)) return(rep(NA_real_, nrow(d)))
    suppressWarnings(as.numeric(d[[col]]))
  }
  df <- data.frame(
    canonical_name     = trimws(as.character(d$species)),
    heat_tolerance_c   = num("mean_HT"),
    acclimation_temp_c = num("acclimation_temp"),
    svl_mm             = num("SVL"),
    body_mass_g        = num("body_mass"),
    stringsAsFactors   = FALSE
  )
  df <- df[!is.na(df$canonical_name) & nzchar(df$canonical_name) &
           grepl(" ", df$canonical_name), , drop = FALSE]
  cols <- c("heat_tolerance_c", "acclimation_temp_c", "svl_mm", "body_mass_g")
  res <- stats::aggregate(
    df[cols], by = list(canonical_name = df$canonical_name),
    FUN = function(z) { z <- z[is.finite(z)]; if (!length(z)) NA_real_ else stats::median(z) }
  )
  .trait_finalize(res)
}


#' Parse Quimbayo reef-fish traits
#'
#' Wide table, one row per reef-fish species; a curated set of size, ecology,
#' depth, trophic and behavioural traits is kept.
#'
#' @param path Path to the Fish_aspects CSV.
#' @return data.frame with canonical_name + reef-fish traits.
#' @export
parse_quimbayo <- function(path) {
  d <- data.table::fread(path, encoding = "Latin-1", data.table = FALSE)
  num <- function(c) if (c %in% names(d)) suppressWarnings(as.numeric(d[[c]])) else rep(NA_real_, nrow(d))
  chr <- function(c) {
    if (!c %in% names(d)) return(rep(NA_character_, nrow(d)))
    x <- trimws(as.character(d[[c]])); x[x == "" | x == "NA"] <- NA_character_; x
  }
  out <- data.frame(
    canonical_name         = trimws(paste(d$Genus, d$Species)),
    body_size_max_cm       = num("Body_size_max"),
    aspect_ratio           = num("Aspect_ratio"),
    trophic_level          = num("Trophic_level"),
    depth_min_m            = num("Depth_min"),
    depth_max_m            = num("Depth_max"),
    temp_occurrence_mean_c = num("TempOccurrence_mean"),
    home_range             = chr("Home_range"),
    diel_activity          = chr("Diel_activity"),
    water_level            = chr("Level_water"),
    body_shape             = chr("Body_shape"),
    mouth_position         = chr("Mouth_position"),
    diet                   = chr("Diet"),
    spawning               = chr("Spawning"),
    size_group             = chr("Size_group"),
    stringsAsFactors       = FALSE
  )
  .trait_finalize(out)
}
