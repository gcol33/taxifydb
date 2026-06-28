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


#' Parse Hagge saproxylic beetle morphology
#'
#' Wide table, one row per deadwood-beetle species (underscore names), of body
#' and appendage morphometrics.
#'
#' @param path Path to the trait CSV.
#' @return data.frame with canonical_name + morphometrics.
#' @export
parse_saproxylic <- function(path) {
  d <- data.table::fread(path, encoding = "Latin-1", data.table = FALSE)
  num <- function(c) if (c %in% names(d)) suppressWarnings(as.numeric(d[[c]])) else rep(NA_real_, nrow(d))
  out <- data.frame(
    canonical_name   = gsub("_", " ", trimws(as.character(d$species))),
    body_length_mm   = num("body_length"),
    body_width_mm    = num("body_width"),
    body_height_mm   = num("body_height"),
    mass_mg          = num("mass"),
    colour_lightness = num("colour_lightness"),
    head_length_mm   = num("head_length"),
    pronotum_length_mm = num("pronotum_length"),
    elytra_length_mm = num("elytra_length"),
    wing_length_mm   = num("wing_length"),
    wing_aspect      = num("wing_aspect"),
    antenna_length_mm = num("antenna_length"),
    eye_length_mm    = num("eye_length"),
    stringsAsFactors = FALSE
  )
  .trait_finalize(out)
}


#' Parse Odonate Phenotypic Database (categorical traits)
#'
#' Multi-row table (several records per species); reduced to one row per species
#' (mode) for behavioural and ecological categorical traits. Multi-valued cells
#' are kept verbatim.
#'
#' @param path Path to the OPD CSV.
#' @return data.frame with canonical_name + odonate traits.
#' @export
parse_odonata <- function(path) {
  d <- data.table::fread(path, encoding = "Latin-1", data.table = FALSE)
  cats <- c("territoriality", "flight_mode", "mate_guarding",
            "habitat_openness", "has_wing_pigment")
  long <- do.call(rbind, lapply(cats, function(oc) data.frame(
    name  = as.character(d$GenusSpecies),
    trait = oc,
    value = if (oc %in% names(d)) as.character(d[[oc]]) else NA_character_,
    stringsAsFactors = FALSE
  )))
  spec <- stats::setNames(lapply(cats, function(oc) list(trait = oc, type = "cat")), cats)
  .trait_finalize(.pivot_species_traits(long, spec))
}


#' Parse Pelagic Species Trait Database
#'
#' Wide table, one row per pelagic species (fish, cephalopods, gelatinous), of
#' depth/temperature envelope, size, habitat, defence and gregariousness traits.
#'
#' @param path Path to the pelagic trait CSV.
#' @return data.frame with canonical_name + traits.
#' @export
parse_pelagic <- function(path) {
  d <- data.table::fread(path, encoding = "Latin-1", data.table = FALSE)
  num <- function(c) if (c %in% names(d)) suppressWarnings(as.numeric(d[[c]])) else rep(NA_real_, nrow(d))
  chr <- function(c) {
    if (!c %in% names(d)) return(rep(NA_character_, nrow(d)))
    x <- trimws(as.character(d[[c]])); x[x == "" | x == "NA"] <- NA_character_; x
  }
  out <- data.frame(
    canonical_name   = trimws(as.character(d$sci_name)),
    depth_min_m      = num("depth_min"),
    depth_max_m      = num("depth_max"),
    temp_min_c       = num("temp_min"),
    temp_max_c       = num("temp_max"),
    temp_mean_c      = num("temp_mean"),
    length_min_tl_cm = num("l_min_TL"),
    length_max_tl_cm = num("l_max_TL"),
    trophic_level    = num("trophic_level"),
    vert_habitat     = chr("vert_habitat"),
    horz_habitat     = chr("horz_habitat"),
    body_shape       = chr("body_shape"),
    phys_defense     = chr("phys_defense"),
    gregarious       = chr("gregarious"),
    stringsAsFactors = FALSE
  )
  .trait_finalize(out)
}


#' Parse Frugivoria (Neotropical frugivore traits)
#'
#' Combines the "simple" mammal and bird tables on a shared core (diet, body
#' size/mass, longevity, generation time) with a taxon_group discriminator.
#'
#' @param path Directory holding mammal.csv and bird.csv.
#' @return data.frame with canonical_name + traits.
#' @export
parse_frugivoria <- function(path) {
  rd <- function(f, grp, dietcol) {
    p <- file.path(path, f)
    if (!file.exists(p)) return(NULL)
    d <- data.table::fread(p, encoding = "Latin-1", data.table = FALSE)
    num <- function(c) if (c %in% names(d)) suppressWarnings(as.numeric(d[[c]])) else rep(NA_real_, nrow(d))
    chr <- function(c) {
      if (!c %in% names(d)) return(rep(NA_character_, nrow(d)))
      x <- trimws(as.character(d[[c]])); x[x == "" | x == "NA"] <- NA_character_; x
    }
    data.frame(
      canonical_name     = trimws(as.character(d$IUCN_species_name)),
      taxon_group        = grp,
      diet_category      = chr(dietcol),
      diet_breadth       = num("diet_breadth"),
      body_mass_g        = num("body_mass_e"),
      body_size_mm       = num("body_size_mm"),
      longevity          = num("longevity"),
      generation_time    = num("generation_time"),
      stringsAsFactors   = FALSE
    )
  }
  out <- rbind(rd("mammal.csv", "mammal", "diet_cat"),
               rd("bird.csv", "bird", "diet_cat_e"))
  .trait_finalize(out)
}


#' Parse Parravicini reef-fish trophic guilds
#'
#' Each species carries one trophic-guild code per contributing expert; the
#' cross-expert consensus (mode) is taken as the species trophic guild.
#'
#' @param path Path to converted_experts_classification.csv.
#' @return data.frame with canonical_name + trophic_guild.
#' @export
parse_parravicini <- function(path) {
  d <- data.table::fread(path, encoding = "Latin-1", data.table = FALSE)
  expert_cols <- names(d)[grepl("^r[a-z]+$", names(d))]
  guild <- vapply(seq_len(nrow(d)), function(i) {
    v <- unlist(d[i, expert_cols], use.names = FALSE)
    v <- trimws(as.character(v)); v <- v[!is.na(v) & nzchar(v) & v != "NA"]
    if (!length(v)) return(NA_character_)
    names(sort(table(v), decreasing = TRUE))[1]
  }, character(1L))
  out <- data.frame(
    canonical_name = trimws(as.character(d$Genus_and_species)),
    trophic_guild  = guild,
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$trophic_guild), , drop = FALSE]
  .trait_finalize(out)
}


#' Parse Beukhof marine fish traits
#'
#' Per-region records (one row per species x marine region) reduced to
#' species-level values (numeric by median, categorical by mode).
#'
#' @param path Path to the Beukhof trait xlsx.
#' @return data.frame with canonical_name + marine-fish traits.
#' @export
parse_beukhof <- function(path) {
  d <- openxlsx2::read_xlsx(path, sheet = 1)
  sp <- trimws(as.character(d$taxon))
  num <- function(c) if (c %in% names(d)) suppressWarnings(as.numeric(d[[c]])) else rep(NA_real_, nrow(d))
  chr <- function(c) {
    if (!c %in% names(d)) return(rep(NA_character_, nrow(d)))
    x <- trimws(as.character(d[[c]])); x[x == "" | x == "NA"] <- NA_character_; x
  }
  base <- data.frame(canonical_name = sp, stringsAsFactors = FALSE,
    trophic_level = num("tl"), aspect_ratio = num("AR"),
    offspring_size = num("offspring.size"), age_maturity = num("age.maturity"),
    fecundity = num("fecundity"), length_infinity_cm = num("length.infinity"),
    growth_coefficient = num("growth.coefficient"), length_max_cm = num("length.max"),
    habitat = chr("habitat"), feeding_mode = chr("feeding.mode"),
    body_shape = chr("body.shape"), fin_shape = chr("fin.shape"),
    spawning_type = chr("spawning.type"))
  base <- base[!is.na(base$canonical_name) & grepl(" ", base$canonical_name), , drop = FALSE]
  num_cols <- c("trophic_level", "aspect_ratio", "offspring_size", "age_maturity",
                "fecundity", "length_infinity_cm", "growth_coefficient", "length_max_cm")
  cat_cols <- c("habitat", "feeding_mode", "body_shape", "fin_shape", "spawning_type")
  na <- stats::aggregate(base[num_cols], by = list(canonical_name = base$canonical_name),
    FUN = function(z) { z <- z[is.finite(z)]; if (!length(z)) NA_real_ else stats::median(z) })
  ca <- stats::aggregate(base[cat_cols], by = list(canonical_name = base$canonical_name),
    FUN = function(z) { z <- z[!is.na(z)]; if (!length(z)) NA_character_ else names(sort(table(z), decreasing = TRUE))[1] })
  .trait_finalize(merge(na, ca, by = "canonical_name"))
}


#' Parse Global Zooplankton Trait Database
#'
#' Long-format trait records reduced to one row per species (numeric by median,
#' categorical by mode). Binary one-hot decompositions of the categorical traits
#' are not carried (the categorical parent is kept instead).
#'
#' @param path Path to the level-2 trait CSV.
#' @return data.frame with canonical_name + zooplankton traits.
#' @export
parse_zooplankton <- function(path) {
  d <- data.table::fread(path, encoding = "Latin-1", data.table = FALSE)
  long <- data.frame(
    name  = as.character(d$scientificName),
    trait = as.character(d$traitName),
    value = as.character(d$traitValue),
    stringsAsFactors = FALSE
  )
  spec <- list(
    body_length_max_mm      = list(trait = "bodyLengthMax", type = "num"),
    carbon_weight_mg        = list(trait = "carbonWeight", type = "num"),
    nitrogen_pdw_pct        = list(trait = "nitrogenPDW", type = "num"),
    vertical_distribution   = list(trait = "verticalDistribution", type = "cat"),
    reproduction_mode       = list(trait = "reproductionMode", type = "cat"),
    trophic_group           = list(trait = "trophicGroup", type = "cat"),
    feeding_mode            = list(trait = "feedingMode", type = "cat"),
    myelination             = list(trait = "myelination", type = "cat"),
    habitat_association     = list(trait = "habitatAssociation", type = "cat"),
    diel_vertical_migration = list(trait = "dielVerticalMigration", type = "cat"),
    bioluminescence         = list(trait = "bioluminescence", type = "cat")
  )
  .trait_finalize(.pivot_species_traits(long, spec))
}


#' Parse EuPollTrait (European bees + hoverflies)
#'
#' Long-format Darwin Core measurement-or-fact table joined to the taxon core
#' (genus + specific epithet) for clean binomials; reduced to one row per species
#' for a curated set of morphological, biogeographic and ecological traits.
#'
#' @param path Directory holding mof.csv and taxon.csv.
#' @return data.frame with canonical_name + pollinator traits.
#' @export
parse_eupolltrait <- function(path) {
  mof <- data.table::fread(file.path(path, "mof.csv"), encoding = "Latin-1",
                           data.table = FALSE)
  tax <- data.table::fread(file.path(path, "taxon.csv"), encoding = "Latin-1",
                           data.table = FALSE)
  binom <- trimws(paste(tax$genus, tax$specificEpithet))
  nm <- binom[match(mof$taxonID, tax$taxonID)]
  long <- data.frame(
    name  = nm,
    trait = as.character(mof$measurementType),
    value = as.character(mof$measurementValue),
    stringsAsFactors = FALSE
  )
  spec <- list(
    itd_mm                       = list(trait = "intertegular_distance", type = "num"),
    tongue_length_mm             = list(trait = "tongue_length", type = "num"),
    species_temperature_index    = list(trait = "STI_(Species_temperature_index)", type = "num"),
    species_continentality_index = list(trait = "SCI_(Species_continentality_index)", type = "num"),
    area_of_occupancy            = list(trait = "AOO_(Area_of_occupancy)", type = "num"),
    extent_of_occurrence         = list(trait = "EOO_(Extent_of_occurrence)", type = "num"),
    sociality                    = list(trait = "Sociality", type = "cat"),
    nest                         = list(trait = "Nest", type = "cat"),
    larval_nutrition             = list(trait = "Larval_nutrition", type = "cat"),
    body_length_category         = list(trait = "Body_length_(categories)", type = "cat")
  )
  .trait_finalize(.pivot_species_traits(long, spec))
}


#' Parse HomeRange mammal home-range database
#'
#' Per-individual home-range records reduced to species medians (home range in
#' km2, body mass in kg).
#'
#' @param path Path to HomeRangeData CSV.
#' @return data.frame with canonical_name + home_range_km2 + body_mass_kg.
#' @export
parse_homerange <- function(path) {
  d <- data.table::fread(path, encoding = "Latin-1", data.table = FALSE)
  df <- data.frame(
    canonical_name = gsub("_", " ", trimws(as.character(d$Species))),
    home_range_km2 = suppressWarnings(as.numeric(d$Home_Range_km2)),
    body_mass_kg   = suppressWarnings(as.numeric(d$Body_mass_kg)),
    stringsAsFactors = FALSE
  )
  df <- df[!is.na(df$canonical_name) & grepl(" ", df$canonical_name), , drop = FALSE]
  cols <- c("home_range_km2", "body_mass_kg")
  res <- stats::aggregate(df[cols], by = list(canonical_name = df$canonical_name),
    FUN = function(z) { z <- z[is.finite(z)]; if (!length(z)) NA_real_ else stats::median(z) })
  .trait_finalize(res)
}


#' Parse TetraDENSITY population density
#'
#' Per-record population densities reduced to species medians. Only the dominant
#' `ind/km2` unit is kept (mixing with pairs/km2 or ind/ha would be incorrect).
#'
#' @param path Path to TetraDENSITY CSV.
#' @return data.frame with canonical_name + density_ind_km2.
#' @export
parse_tetradensity <- function(path) {
  d <- data.table::fread(path, encoding = "Latin-1", data.table = FALSE)
  d <- d[!is.na(d$Density_unit) & d$Density_unit == "ind/km2", , drop = FALSE]
  df <- data.frame(
    canonical_name  = trimws(paste(d$Genus, d$Species)),
    density_ind_km2 = suppressWarnings(as.numeric(d$Density)),
    stringsAsFactors = FALSE
  )
  df <- df[!is.na(df$canonical_name) & grepl(" ", df$canonical_name), , drop = FALSE]
  res <- stats::aggregate(df["density_ind_km2"],
    by = list(canonical_name = df$canonical_name),
    FUN = function(z) { z <- z[is.finite(z)]; if (!length(z)) NA_real_ else stats::median(z) })
  .trait_finalize(res)
}


#' Parse BROT 2.0 Mediterranean plant traits
#'
#' Long-format trait records reduced to one row per species. Numeric units follow
#' the standardised BROT 2.0 conventions (seed mass mg, SLA mm2/mg, height m,
#' leaf area mm2).
#'
#' @param path Path to BROT2_dat.csv.
#' @return data.frame with canonical_name + plant traits.
#' @export
parse_brot <- function(path) {
  d <- data.table::fread(path, encoding = "Latin-1", data.table = FALSE)
  long <- data.frame(
    name  = as.character(d$Taxon),
    trait = as.character(d$Trait),
    value = as.character(d$Data),
    stringsAsFactors = FALSE
  )
  spec <- list(
    seed_mass_mg       = list(trait = "SeedMass", type = "num"),
    sla_mm2_mg         = list(trait = "SLA", type = "num"),
    height_m           = list(trait = "Height", type = "num"),
    leaf_area_mm2      = list(trait = "LeafArea", type = "num"),
    resp_fire          = list(trait = "RespFire", type = "cat"),
    growth_form        = list(trait = "GrowthForm", type = "cat"),
    disp_mode          = list(trait = "DispMode", type = "cat"),
    fruit_type         = list(trait = "FruitType", type = "cat"),
    soil_seed_bank     = list(trait = "SoilSeedBank", type = "cat"),
    seedling_emergence = list(trait = "SeedlEmerg", type = "cat")
  )
  .trait_finalize(.pivot_species_traits(long, spec))
}
