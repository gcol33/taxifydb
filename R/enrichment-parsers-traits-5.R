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
