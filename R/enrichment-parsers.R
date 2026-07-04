# Per-enrichment parse functions.
#
# Each parser takes a path (file or directory) returned by a download_fn and
# returns a data.frame with at least a `canonical_name` column plus trait
# columns. The dispatcher in build_enrichment.R passes the result through
# resolve_enrichment_names() and writes a .vtr.
#
# Parsers are ported faithfully from taxify::R/enrichment-build.R.


#' Parse Zanne et al. 2014 woodiness CSV
#' @param path Character. Path to GlobalWoodinessDatabase.csv.
#' @return data.frame with canonical_name + woodiness.
#' @export
parse_woodiness <- function(path) {
  df <- utils::read.csv(path, stringsAsFactors = FALSE)

  if ("gs" %in% names(df)) {
    name_col <- "gs"
  } else if ("Species" %in% names(df)) {
    name_col <- "Species"
  } else {
    name_col <- names(df)[1L]
  }

  wood_col <- grep("wood", names(df), ignore.case = TRUE, value = TRUE)
  if (length(wood_col) == 0L) {
    stop("Cannot find woodiness column. Columns: ",
         paste(names(df), collapse = ", "), call. = FALSE)
  }
  wood_col <- wood_col[1L]

  raw <- tolower(trimws(df[[wood_col]]))
  woodiness <- ifelse(grepl("^h", raw), "herbaceous",
               ifelse(grepl("^w", raw), "woody",
               ifelse(grepl("^v", raw), "variable", NA_character_)))

  cname <- trimws(df[[name_col]])
  out <- data.frame(
    canonical_name = cname,
    woodiness      = woodiness,
    stringsAsFactors = FALSE
  )
  out <- .append_all_cols(out, df, cname, used = c(name_col, wood_col))
  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]
  out[!duplicated(out$canonical_name), ]
}


#' Parse EIVE 1.0 ecological indicator values (XLSX)
#' @param path Character. Path to EIVE_1.0.xlsx.
#' @return data.frame with canonical_name + indicator columns.
#' @export
parse_eive <- function(path) {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop("openxlsx2 is required to read EIVE xlsx. ",
         "Install with: install.packages('openxlsx2')", call. = FALSE)
  }

  df <- as.data.frame(
    openxlsx2::read_xlsx(path, sheet = "mainTable"),
    stringsAsFactors = FALSE
  )

  name_col <- if ("TaxonConcept" %in% names(df)) "TaxonConcept" else names(df)[1L]

  find_col <- function(patterns) {
    for (p in patterns) {
      m <- grep(p, names(df), value = TRUE)
      if (length(m) > 0L) return(m[1L])
    }
    NA_character_
  }

  light_col <- find_col(c("^EIVEres-L$", "^EIVEres.L$"))
  temp_col  <- find_col(c("^EIVEres-T$", "^EIVEres.T$"))
  moist_col <- find_col(c("^EIVEres-M$", "^EIVEres.M$"))
  react_col <- find_col(c("^EIVEres-R$", "^EIVEres.R$"))
  nutr_col  <- find_col(c("^EIVEres-N$", "^EIVEres.N$"))

  safe_num <- function(x) suppressWarnings(as.numeric(x))

  cname <- trimws(df[[name_col]])
  out <- data.frame(
    canonical_name = cname,
    stringsAsFactors = FALSE
  )
  if (!is.na(light_col)) out$light       <- safe_num(df[[light_col]])
  if (!is.na(temp_col))  out$temperature <- safe_num(df[[temp_col]])
  if (!is.na(moist_col)) out$moisture    <- safe_num(df[[moist_col]])
  if (!is.na(react_col)) out$reaction    <- safe_num(df[[react_col]])
  if (!is.na(nutr_col))  out$nutrients   <- safe_num(df[[nutr_col]])

  used <- c(name_col, light_col, temp_col, moist_col, react_col, nutr_col)
  used <- used[!is.na(used)]
  out <- .append_all_cols(out, df, cname, used = used)
  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]
  out[!duplicated(out$canonical_name), ]
}


#' Parse EltonTraits 1.0 birds + mammals TSVs
#' @param birds_path Character. Path to BirdFuncDat.txt.
#' @param mammals_path Character. Path to MamFuncDat.txt.
#' @return data.frame with canonical_name + diet/foraging/body mass columns.
#' @export
# Derive one diet-guild label per species from the ten EltonTraits diet-fraction
# columns. Fractions are summed within each guild first (the four vertebrate/fish
# columns are all carnivory), then the dominant guild is taken; a species with no
# guild reaching 50 percent, or a tie across guilds, is omnivore. Grounded on the
# EltonTraits fractions: this label agrees 93% with EltonTraits' own diet_5cat and
# 83% with AVONET's independent trophic_niche on shared species.
.elton_diet_guild <- function(out) {
  diet_cols <- c("diet_inv", "diet_vend", "diet_vect", "diet_vfish", "diet_vunk",
                 "diet_scav", "diet_fruit", "diet_nect", "diet_seed",
                 "diet_plantother")
  diet_cols <- intersect(diet_cols, names(out))
  if (!length(diet_cols)) return(rep(NA_character_, nrow(out)))
  col2guild <- c(diet_inv = "invertivore", diet_vend = "carnivore",
                 diet_vect = "carnivore", diet_vfish = "carnivore",
                 diet_vunk = "carnivore", diet_scav = "scavenger",
                 diet_fruit = "frugivore", diet_nect = "nectarivore",
                 diet_seed = "granivore", diet_plantother = "herbivore")
  M <- as.matrix(out[, diet_cols, drop = FALSE])
  storage.mode(M) <- "numeric"
  guilds <- unique(col2guild[diet_cols])
  G <- vapply(guilds, function(g) {
    rowSums(M[, diet_cols[col2guild[diet_cols] == g], drop = FALSE], na.rm = TRUE)
  }, numeric(nrow(M)))
  if (is.null(dim(G))) {
    G <- matrix(G, nrow = nrow(M), dimnames = list(NULL, guilds))
  }
  allzero <- rowSums(!is.na(M)) == 0 | rowSums(M, na.rm = TRUE) == 0
  gmax <- apply(G, 1L, max)
  pick <- guilds[apply(G, 1L, which.max)]
  ntie <- apply(G, 1L, function(r) sum(r == max(r)))
  ifelse(allzero, NA_character_,
         ifelse(gmax < 50 | ntie > 1L, "omnivore", pick))
}

parse_elton_traits <- function(birds_path, mammals_path) {
  col_map <- list(
    diet_inv        = c("Diet.Inv", "Diet-Inv"),
    diet_vend       = c("Diet.Vend", "Diet-Vend"),
    diet_vect       = c("Diet.Vect", "Diet-Vect"),
    diet_vfish      = c("Diet.Vfish", "Diet-Vfish"),
    diet_vunk       = c("Diet.Vunk", "Diet-Vunk"),
    diet_scav       = c("Diet.Scav", "Diet-Scav"),
    diet_fruit      = c("Diet.Fruit", "Diet-Fruit"),
    diet_nect       = c("Diet.Nect", "Diet-Nect"),
    diet_seed       = c("Diet.Seed", "Diet-Seed"),
    diet_plantother = c("Diet.PlantO", "Diet-PlantO"),
    foraging_water      = c("ForStrat.watbelowsurf", "ForStrat-watbelowsurf"),
    foraging_ground     = c("ForStrat.ground", "ForStrat-ground"),
    foraging_understory = c("ForStrat.understory", "ForStrat-understory"),
    foraging_midhigh    = c("ForStrat.midhigh", "ForStrat-midhigh"),
    foraging_canopy     = c("ForStrat.canopy", "ForStrat-canopy"),
    foraging_aerial     = c("ForStrat.aerial", "ForStrat-aerial"),
    body_mass_g     = c("BodyMass.Value", "BodyMass-Value"),
    nocturnal       = c("Nocturnal", "Activity.Nocturnal", "Activity-Nocturnal")
  )

  resolve_col <- function(df, candidates) {
    for (cand in candidates) {
      if (cand %in% names(df)) return(cand)
      cand_dot <- gsub("-", ".", cand, fixed = TRUE)
      if (cand_dot %in% names(df)) return(cand_dot)
    }
    NULL
  }

  extract_one <- function(df) {
    name_col <- intersect(
      names(df), c("Scientific", "Scientific.Name", "ScientificName")
    )
    if (length(name_col) == 0L) name_col <- names(df)[1L] else name_col <- name_col[1L]

    cname <- trimws(df[[name_col]])
    out <- data.frame(
      canonical_name = cname,
      stringsAsFactors = FALSE
    )
    used_srcs <- character(0)
    for (out_name in names(col_map)) {
      src <- resolve_col(df, col_map[[out_name]])
      out[[out_name]] <- if (!is.null(src)) {
        used_srcs <- c(used_srcs, src)
        suppressWarnings(as.numeric(df[[src]]))
      } else {
        NA_real_
      }
    }
    .append_all_cols(out, df, cname, used = c(name_col, used_srcs))
  }

  # Bird and mammal tables carry different extra columns; union them so no
  # source column is dropped when the two curated sub-frames are stacked.
  bind_union <- function(a, b) {
    cols <- union(names(a), names(b))
    for (cc in setdiff(cols, names(a))) a[[cc]] <- NA
    for (cc in setdiff(cols, names(b))) b[[cc]] <- NA
    rbind(a[cols], b[cols])
  }

  birds <- utils::read.delim(birds_path, stringsAsFactors = FALSE, quote = "")
  mammals <- utils::read.delim(mammals_path, stringsAsFactors = FALSE,
                               quote = "")

  out <- bind_union(extract_one(birds), extract_one(mammals))
  out$diet_guild <- .elton_diet_guild(out)
  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]
  out[!duplicated(out$canonical_name), ]
}


#' Parse AVONET bird morphology XLSX
#' @param path Character. Path to AVONET_BirdLife.xlsx.
#' @return data.frame with canonical_name + morphology columns.
#' @export
parse_avonet <- function(path) {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop("openxlsx2 is required to read AVONET xlsx. ",
         "Install with: install.packages('openxlsx2')", call. = FALSE)
  }

  sheets <- openxlsx2::wb_load(path) |> openxlsx2::wb_get_sheet_names()
  sp_sheet <- grep("AVONET.*Birdlife|species|averages", sheets,
                   ignore.case = TRUE, value = TRUE)
  if (length(sp_sheet) == 0L) {
    sp_sheet <- sheets[min(2L, length(sheets))]
  } else {
    sp_sheet <- sp_sheet[1L]
  }

  df <- as.data.frame(
    openxlsx2::read_xlsx(path, sheet = sp_sheet),
    stringsAsFactors = FALSE
  )

  name_col <- intersect(
    names(df),
    c("Species1", "Species1_BirdLife", "Species", "Scientific",
      "ScientificName", "species_name")
  )
  if (length(name_col) == 0L) name_col <- names(df)[1L] else name_col <- name_col[1L]

  find_col <- function(patterns) {
    for (p in patterns) {
      m <- grep(paste0("^", p, "$"), names(df), ignore.case = TRUE,
                value = TRUE)
      if (length(m) > 0L) return(m[1L])
    }
    for (p in patterns) {
      m <- grep(p, names(df), ignore.case = TRUE, value = TRUE)
      if (length(m) > 0L) return(m[1L])
    }
    NULL
  }

  safe_num <- function(col_name) {
    if (is.null(col_name)) return(rep(NA_real_, nrow(df)))
    suppressWarnings(as.numeric(df[[col_name]]))
  }
  safe_chr <- function(col_name) {
    if (is.null(col_name)) return(rep(NA_character_, nrow(df)))
    as.character(df[[col_name]])
  }

  cname            <- trimws(df[[name_col]])
  beak_length_col  <- find_col(c("Beak.Length_Culmen", "Beak.Length",
                                 "culmen_length", "Bill.Length"))
  beak_depth_col   <- find_col(c("Beak.Depth", "bill_depth", "Bill.Depth"))
  wing_length_col  <- find_col(c("Wing.Length", "wing_length"))
  tail_length_col  <- find_col(c("Tail.Length", "tail_length"))
  tarsus_length_col <- find_col(c("Tarsus.Length", "tarsus_length"))
  body_mass_col    <- find_col(c("Mass", "Body.Mass", "body_mass",
                                 "BodyMass", "Mass.g"))
  hand_wing_col    <- find_col(c("Hand.Wing.Index", "Hand-Wing.Index",
                                 "HWI", "hand_wing_index"))
  habitat_col      <- find_col(c("Habitat", "Primary.Lifestyle", "habitat"))
  trophic_level_col <- find_col(c("Trophic.Level", "trophic_level"))
  trophic_niche_col <- find_col(c("Trophic.Niche", "trophic_niche"))
  migration_col    <- find_col(c("Migration", "migration"))

  out <- data.frame(
    canonical_name  = cname,
    beak_length     = safe_num(beak_length_col),
    beak_depth      = safe_num(beak_depth_col),
    wing_length     = safe_num(wing_length_col),
    tail_length     = safe_num(tail_length_col),
    tarsus_length   = safe_num(tarsus_length_col),
    body_mass_g     = safe_num(body_mass_col),
    hand_wing_index = safe_num(hand_wing_col),
    habitat         = safe_chr(habitat_col),
    trophic_level   = safe_chr(trophic_level_col),
    trophic_niche   = safe_chr(trophic_niche_col),
    migration       = safe_chr(migration_col),
    stringsAsFactors = FALSE
  )

  if (!all(is.na(out$migration))) {
    mig <- tolower(trimws(out$migration))
    out$migration <- ifelse(grepl("^1$|^sedentar|^resident", mig), "sedentary",
                    ifelse(grepl("^2$|^partial", mig), "partial",
                    ifelse(grepl("^3$|^full|^migra", mig), "full",
                    NA_character_)))
  }

  used <- c(name_col, beak_length_col, beak_depth_col, wing_length_col,
            tail_length_col, tarsus_length_col, body_mass_col, hand_wing_col,
            habitat_col, trophic_level_col, trophic_niche_col, migration_col)
  out <- .append_all_cols(out, df, cname, used = used)
  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]
  out[!duplicated(out$canonical_name), ]
}


#' Parse PanTHERIA mammal life-history traits (TSV)
#' @param path Character. Path to PanTHERIA.txt.
#' @return data.frame with canonical_name + life-history columns.
#' @export
parse_pantheria <- function(path) {
  df <- utils::read.delim(path, stringsAsFactors = FALSE,
                          na.strings = c("-999", "-999.00"))

  name_col <- intersect(
    names(df), c("MSW05_Binomial", "MSW93_Binomial", "Scientific_Name")
  )
  if (length(name_col) == 0L) name_col <- names(df)[1L] else name_col <- name_col[1L]

  find_col <- function(patterns) {
    for (p in patterns) {
      m <- grep(p, names(df), ignore.case = TRUE, value = TRUE)
      if (length(m) > 0L) return(m[1L])
    }
    NULL
  }

  safe_num <- function(col_name) {
    if (is.null(col_name)) return(rep(NA_real_, nrow(df)))
    x <- suppressWarnings(as.numeric(df[[col_name]]))
    x[x == -999] <- NA_real_
    x
  }

  cname             <- trimws(df[[name_col]])
  body_mass_col     <- find_col(c("AdultBodyMass_g", "X5.1_AdultBodyMass",
                                  "BodyMass"))
  longevity_col     <- find_col(c("MaxLongevity_m", "X17.1_MaxLongevity"))
  litter_size_col   <- find_col(c("LitterSize", "X15.1_LitterSize"))
  gestation_col     <- find_col(c("GestationLen_d", "X9.1_GestationLen"))
  weaning_col       <- find_col(c("WeaningAge_d", "X25.1_WeaningAge"))
  home_range_col    <- find_col(c("HomeRange_km2", "X22.1_HomeRange",
                                  "HomeRange_Indiv_km2"))
  diet_breadth_col  <- find_col(c("DietBreadth", "X6.2_TrophicLevel",
                                  "diet_breadth"))
  habitat_breadth_col <- find_col(c("HabitatBreadth", "X12.2_HabitatBreadth",
                                    "habitat_breadth"))

  out <- data.frame(
    canonical_name  = cname,
    body_mass_g     = safe_num(body_mass_col),
    longevity_mo    = safe_num(longevity_col),
    litter_size     = safe_num(litter_size_col),
    gestation_d     = safe_num(gestation_col),
    weaning_d       = safe_num(weaning_col),
    home_range_km2  = safe_num(home_range_col),
    diet_breadth    = safe_num(diet_breadth_col),
    habitat_breadth = safe_num(habitat_breadth_col),
    stringsAsFactors = FALSE
  )

  # PanTHERIA codes numeric missing values as -999; neutralize on the raw
  # source before widening (NA-safe: read.delim's na.strings already handled
  # most, this guards any residual).
  df[!is.na(df) & df == -999] <- NA
  used <- c(name_col, body_mass_col, longevity_col, litter_size_col,
            gestation_col, weaning_col, home_range_col, diet_breadth_col,
            habitat_breadth_col)
  out <- .append_all_cols(out, df, cname, used = used)
  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]
  out[!duplicated(out$canonical_name), ]
}


#' Parse AmphiBIO amphibian traits (CSV from ZIP)
#' @param path Character. Path to the AmphiBIO CSV.
#' @return data.frame with canonical_name + trait columns.
#' @export
parse_amphibio <- function(path) {
  df <- utils::read.csv(path, stringsAsFactors = FALSE)

  name_col <- intersect(names(df), c("Species", "species", "Scientific"))
  if (length(name_col) == 0L) name_col <- names(df)[1L] else name_col <- name_col[1L]

  find_col <- function(patterns) {
    for (p in patterns) {
      m <- grep(paste0("^", p, "$"), names(df), ignore.case = TRUE,
                value = TRUE)
      if (length(m) > 0L) return(m[1L])
    }
    for (p in patterns) {
      m <- grep(p, names(df), ignore.case = TRUE, value = TRUE)
      if (length(m) > 0L) return(m[1L])
    }
    NULL
  }

  safe_num <- function(col_name) {
    if (is.null(col_name)) return(rep(NA_real_, nrow(df)))
    suppressWarnings(as.numeric(df[[col_name]]))
  }
  safe_int <- function(col_name) {
    if (is.null(col_name)) return(rep(NA_integer_, nrow(df)))
    suppressWarnings(as.integer(df[[col_name]]))
  }

  cname               <- trimws(df[[name_col]])
  body_size_col       <- find_col(c("Body_size_mm", "Body.size.mm",
                                    "SVL_mm", "Body_length_mm"))
  age_maturity_col    <- find_col(c("Age_at_maturity_min_y", "Age_at_maturity",
                                    "Age.at.maturity"))
  longevity_col       <- find_col(c("Longevity_max_y", "Longevity_max",
                                    "Longevity"))
  litter_size_col     <- find_col(c("Litter_size_max_n", "Litter.size",
                                    "Clutch_size"))
  reproductive_col    <- find_col(c("Reproductive_output_y",
                                    "Reproductive.output"))
  offspring_size_col  <- find_col(c("Offspring_size_mm", "Offspring.size"))
  direct_dev_col      <- find_col(c("Dir", "Direct_development", "Devel_direct"))
  larval_col          <- find_col(c("Lar", "Larval", "Has_larva"))
  aquatic_col         <- find_col(c("Aqu", "Aquatic"))
  fossorial_col       <- find_col(c("Fos", "Fossorial"))
  arboreal_col        <- find_col(c("Arb", "Arboreal"))
  diurnal_col         <- find_col(c("Diu", "Diurnal"))
  nocturnal_col       <- find_col(c("Noc", "Nocturnal"))

  out <- data.frame(
    canonical_name      = cname,
    body_size_mm        = safe_num(body_size_col),
    age_maturity_y      = safe_num(age_maturity_col),
    longevity_yr        = safe_num(longevity_col),
    litter_size         = safe_num(litter_size_col),
    reproductive_output = safe_num(reproductive_col),
    offspring_size_mm   = safe_num(offspring_size_col),
    direct_development  = safe_int(direct_dev_col),
    larval              = safe_int(larval_col),
    aquatic             = safe_int(aquatic_col),
    fossorial           = safe_int(fossorial_col),
    arboreal            = safe_int(arboreal_col),
    diurnal             = safe_int(diurnal_col),
    nocturnal_amphibio  = safe_int(nocturnal_col),
    stringsAsFactors = FALSE
  )

  used <- c(name_col, body_size_col, age_maturity_col, longevity_col,
            litter_size_col, reproductive_col, offspring_size_col,
            direct_dev_col, larval_col, aquatic_col, fossorial_col,
            arboreal_col, diurnal_col, nocturnal_col)
  out <- .append_all_cols(out, df, cname, used = used)
  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]
  out[!duplicated(out$canonical_name), ]
}


#' Parse FISHMORPH freshwater fish morphological traits (CSV)
#' @param path Character. Path to FISHMORPH_Database.csv.
#' @return data.frame with canonical_name + morphology columns.
#' @export
parse_fish_traits <- function(path) {
  df <- utils::read.csv2(path, stringsAsFactors = FALSE,
                         fileEncoding = "latin1", dec = ".")

  name_col <- intersect(
    names(df), c("Genus.species", "Genus species", "Species",
                 "scientificNameStd")
  )
  if (length(name_col) == 0L) name_col <- names(df)[1L] else name_col <- name_col[1L]

  find_col <- function(patterns) {
    for (p in patterns) {
      m <- grep(paste0("^", p, "$"), names(df), ignore.case = TRUE,
                value = TRUE)
      if (length(m) > 0L) return(m[1L])
    }
    for (p in patterns) {
      m <- grep(p, names(df), ignore.case = TRUE, value = TRUE)
      if (length(m) > 0L) return(m[1L])
    }
    NULL
  }

  safe_num <- function(col_name) {
    if (is.null(col_name)) return(rep(NA_real_, nrow(df)))
    suppressWarnings(as.numeric(df[[col_name]]))
  }

  cname                 <- trimws(gsub("_", " ", df[[name_col]]))
  max_body_length_col   <- find_col(c("MBl", "MBI", "Max_body_length"))
  body_elongation_col   <- find_col(c("BEl", "Body_elongation"))
  vertical_eye_col      <- find_col(c("VEp", "Vertical_eye_position"))
  relative_eye_col      <- find_col(c("REs", "Relative_eye_size"))
  oral_gape_col         <- find_col(c("OGp", "Oral_gape_position"))
  relative_maxillary_col <- find_col(c("RMl", "Relative_maxillary_length"))
  body_lateral_col      <- find_col(c("BLs", "Body_lateral_shape"))
  pectoral_position_col <- find_col(c("PFv", "Pectoral_fin_vertical"))
  pectoral_size_col     <- find_col(c("PFs", "Pectoral_fin_size"))
  caudal_peduncle_col   <- find_col(c("CPt", "Caudal_peduncle_throttling"))

  out <- data.frame(
    canonical_name             = cname,
    max_body_length            = safe_num(max_body_length_col),
    body_elongation            = safe_num(body_elongation_col),
    vertical_eye_position      = safe_num(vertical_eye_col),
    relative_eye_size          = safe_num(relative_eye_col),
    oral_gape_position         = safe_num(oral_gape_col),
    relative_maxillary_length  = safe_num(relative_maxillary_col),
    body_lateral_shape         = safe_num(body_lateral_col),
    pectoral_fin_position      = safe_num(pectoral_position_col),
    pectoral_fin_size          = safe_num(pectoral_size_col),
    caudal_peduncle_throttling = safe_num(caudal_peduncle_col),
    stringsAsFactors = FALSE
  )

  used <- c(name_col, max_body_length_col, body_elongation_col,
            vertical_eye_col, relative_eye_col, oral_gape_col,
            relative_maxillary_col, body_lateral_col, pectoral_position_col,
            pectoral_size_col, caudal_peduncle_col)
  out <- .append_all_cols(out, df, cname, used = used)
  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]
  out[!duplicated(out$canonical_name), ]
}


#' Parse LEDA trait files (multiple semicolon/tab-delimited files)
#' @param dir_path Character. Directory containing the LEDA *.txt files.
#' @return data.frame with canonical_name + 10 LEDA trait columns.
#' @export
parse_leda <- function(dir_path) {
  trait_files <- list(
    life_form     = "life_form.txt",
    dispersal     = "dispersal_type.txt",
    tv            = "TV.txt",
    seed_mass     = "seed_mass.txt",
    canopy_height = "canopy_height.txt",
    leaf_mass     = "leaf_mass.txt",
    sla           = "SLA.txt",
    clonal_growth = "clonal_growth.txt",
    buoyancy      = "buoyancy.txt",
    age_flower    = "age_of_first_flowering.txt",
    branching     = "branching.txt",
    bud_seas      = "buds_seasonality.txt",
    buds_vert     = "buds_vertical_dist.txt",
    leaf_dist     = "leaf_distribution.txt",
    ldmc          = "LDMC_und_Geo.txt",
    leaf_size     = "leaf_size.txt",
    morph_disp    = "morphology_dispersal_unit.txt",
    life_span     = "plant_life_span.txt",
    rel_height    = "releasing_height.txt",
    seed_long     = "seed_longevity.txt",
    seed_number   = "seed_number.txt",
    seed_shape    = "seed_shape.txt",
    shoot_gf      = "shoot_growth_form.txt",
    ssd           = "ssd.txt"
  )

  read_leda_trait <- function(path) {
    # LEDA text dumps prefix the data table with an SQL query preamble.
    # Some files (e.g. SLA.txt) pad the preamble with semicolons to match
    # the data column count, so a semicolon-count heuristic is unreliable.
    # Universal LEDA tables are keyed on "SBS name" or "SBS number", so
    # use that prefix to locate the header row.
    find_header_skip <- function(p, max_scan = 50L) {
      con <- file(p, encoding = "latin1")
      on.exit(close(con))
      lines <- .to_utf8(readLines(con, n = max_scan, warn = FALSE))
      hits <- which(grepl("^SBS (name|number)\\s*;", lines,
                          ignore.case = TRUE))
      if (length(hits) == 0L) {
        hits <- which(vapply(lines, function(l) {
          sc <- sum(charToRaw(l) == charToRaw(";"))
          sc >= 3L && !grepl("(SELECT |FROM |WHERE |\\(|^The following)", l)
        }, logical(1L)))
      }
      if (length(hits) == 0L) return(0L)
      hits[1L] - 1L
    }

    df0 <- tryCatch({
      skip_n <- find_header_skip(path)
      df <- utils::read.csv(path, sep = ";", stringsAsFactors = FALSE,
                            fileEncoding = "latin1", skip = skip_n,
                            check.names = FALSE, quote = "", row.names = NULL)
      if (ncol(df) <= 1L) {
        df <- utils::read.delim(path, stringsAsFactors = FALSE,
                                fileEncoding = "latin1", skip = skip_n,
                                check.names = FALSE, quote = "", row.names = NULL)
      }
      df
    }, error = function(e) {
      tryCatch(
        utils::read.delim(path, stringsAsFactors = FALSE, skip = 0L,
                          check.names = FALSE, quote = "", row.names = NULL),
        error = function(e2) NULL
      )
    })
    if (is.null(df0)) return(NULL)
    names(df0) <- .to_utf8(names(df0))
    for (j in seq_along(df0)) {
      if (is.character(df0[[j]])) df0[[j]] <- .to_utf8(df0[[j]])
    }
    df0
  }

  find_name_col <- function(df) {
    candidates <- c("SBS_name", "species", "Species", "SBS.name",
                    "species_name", "name", "taxon")
    col <- intersect(names(df), candidates)
    if (length(col) > 0L) return(col[1L])
    col <- grep("species|name|SBS", names(df), ignore.case = TRUE,
                value = TRUE)
    if (length(col) > 0L) return(col[1L])
    names(df)[1L]
  }

  merge_trait <- function(master, path, trait_col_patterns, out_col,
                          as_type = "numeric") {
    if (!file.exists(path)) return(master)
    df <- read_leda_trait(path)
    if (is.null(df) || nrow(df) == 0L) return(master)

    nc <- find_name_col(df)
    tc <- NULL
    for (p in trait_col_patterns) {
      m <- grep(p, names(df), ignore.case = TRUE, value = TRUE)
      if (length(m) > 0L) { tc <- m[1L]; break }
    }
    if (is.null(tc)) tc <- names(df)[ncol(df)]

    vals <- if (as_type == "numeric") {
      suppressWarnings(as.numeric(df[[tc]]))
    } else if (as_type == "integer") {
      suppressWarnings(as.integer(df[[tc]]))
    } else {
      as.character(df[[tc]])
    }

    trait_df <- data.frame(
      canonical_name = trimws(df[[nc]]),
      val = vals,
      stringsAsFactors = FALSE
    )
    names(trait_df)[2L] <- out_col

    if (as_type %in% c("numeric", "integer")) {
      trait_df <- stats::aggregate(
        trait_df[[out_col]],
        by = list(canonical_name = trait_df$canonical_name),
        FUN = function(x) stats::median(x, na.rm = TRUE)
      )
      names(trait_df)[2L] <- out_col
    } else {
      trait_df <- trait_df[!duplicated(trait_df$canonical_name), ]
    }

    if (is.null(master)) return(trait_df)
    merge(master, trait_df, by = "canonical_name", all = TRUE)
  }

  master <- NULL

  lf_path <- file.path(dir_path, trait_files$life_form)
  if (file.exists(lf_path)) {
    df <- read_leda_trait(lf_path)
    if (!is.null(df) && nrow(df) > 0L) {
      nc <- find_name_col(df)
      lf_col <- grep("life.form|raunkiaer|lf_", names(df),
                     ignore.case = TRUE, value = TRUE)
      if (length(lf_col) > 0L) {
        trait_df <- data.frame(
          canonical_name = trimws(df[[nc]]),
          raunkiaer_life_form = trimws(df[[lf_col[1L]]]),
          stringsAsFactors = FALSE
        )
        counts <- table(trait_df$canonical_name)
        variable_spp <- names(counts[counts > 1L])
        trait_df <- trait_df[!duplicated(trait_df$canonical_name), ]
        trait_df$raunkiaer_variable <- as.integer(
          trait_df$canonical_name %in% variable_spp
        )
        master <- trait_df
      }
    }
  }

  master <- merge_trait(
    master, file.path(dir_path, trait_files$dispersal),
    c("dispersal.*type", "dispersal_type", "disp"),
    "dispersal_type", "character"
  )
  master <- merge_trait(
    master, file.path(dir_path, trait_files$tv),
    c("terminal.*velocity", "tv", "TV"),
    "terminal_velocity_ms", "numeric"
  )
  master <- merge_trait(
    master, file.path(dir_path, trait_files$seed_mass),
    c("seed.*mass", "sm_mean", "mass"),
    "leda_seed_mass_mg", "numeric"
  )
  master <- merge_trait(
    master, file.path(dir_path, trait_files$canopy_height),
    c("canopy.*height", "ch_mean", "height"),
    "canopy_height_m", "numeric"
  )
  master <- merge_trait(
    master, file.path(dir_path, trait_files$leaf_mass),
    c("leaf.*mass", "lm_mean", "mass"),
    "leaf_mass_mg", "numeric"
  )
  master <- merge_trait(
    master, file.path(dir_path, trait_files$sla),
    c("sla", "SLA", "specific.*leaf"),
    "sla_mm2_mg", "numeric"
  )
  master <- merge_trait(
    master, file.path(dir_path, trait_files$clonal_growth),
    c("clonal", "CGO", "cgo"),
    "clonal_growth", "integer"
  )
  master <- merge_trait(
    master, file.path(dir_path, trait_files$buoyancy),
    c("buoyancy", "buoy"),
    "buoyancy", "character"
  )

  # Remaining LEDA trait files (value column patterns verified against the
  # downloaded headers; seed_bank / SNP omitted -- empty upstream).
  master <- merge_trait(master, file.path(dir_path, trait_files$age_flower),
    c("age of first flowering"), "age_first_flowering", "character")
  master <- merge_trait(master, file.path(dir_path, trait_files$branching),
    c("^branching$", "branching"), "branching", "character")
  master <- merge_trait(master, file.path(dir_path, trait_files$bud_seas),
    c("BBS above ground", "budb seas"), "bud_bank_seasonality", "character")
  master <- merge_trait(master, file.path(dir_path, trait_files$buds_vert),
    c("buds above ground", "buds in layer"), "buds_vertical_distribution",
    "character")
  master <- merge_trait(master, file.path(dir_path, trait_files$leaf_dist),
    c("leaf distribution"), "leaf_distribution", "character")
  master <- merge_trait(master, file.path(dir_path, trait_files$ldmc),
    c("mean LMDC", "single value .mg/g", "LDMC"), "ldmc_mg_g", "numeric")
  master <- merge_trait(master, file.path(dir_path, trait_files$leaf_size),
    c("mean LS", "single value .mm.2", "leaf.*size"), "leaf_size_mm2", "numeric")
  master <- merge_trait(master, file.path(dir_path, trait_files$morph_disp),
    c("^diaspore type$", "diaspore type"), "diaspore_type", "character")
  master <- merge_trait(master, file.path(dir_path, trait_files$life_span),
    c("^plant lifespan$", "plant lifespan", "life span"), "plant_life_span",
    "character")
  master <- merge_trait(master, file.path(dir_path, trait_files$rel_height),
    c("mean RH", "single value .m."), "releasing_height_m", "numeric")
  master <- merge_trait(master, file.path(dir_path, trait_files$seed_long),
    c("seed longevity index", "max longevity"), "seed_longevity_index",
    "numeric")
  master <- merge_trait(master, file.path(dir_path, trait_files$seed_number),
    c("average SNP", "single value", "seed number"), "seed_number_per_plant",
    "numeric")
  master <- merge_trait(master, file.path(dir_path, trait_files$seed_shape),
    c("length .single value", "length"), "seed_length_mm", "numeric")
  master <- merge_trait(master, file.path(dir_path, trait_files$shoot_gf),
    c("shoot growth form"), "shoot_growth_form", "character")
  master <- merge_trait(master, file.path(dir_path, trait_files$ssd),
    c("mean SSD", "SSD .g/cm"), "ssd_g_cm3", "numeric")

  if (is.null(master) || nrow(master) == 0L) {
    stop("No LEDA data could be parsed from downloaded files.", call. = FALSE)
  }

  expected <- c("canonical_name", "raunkiaer_life_form", "raunkiaer_variable",
                "dispersal_type", "terminal_velocity_ms", "leda_seed_mass_mg",
                "canopy_height_m", "leaf_mass_mg", "sla_mm2_mg",
                "clonal_growth", "buoyancy")
  for (col in expected) {
    if (!col %in% names(master)) master[[col]] <- NA
  }

  # LEDA files are latin1; make names and character traits valid UTF-8 so the
  # downstream name resolution and .vtr write do not choke on stray bytes.
  master$canonical_name <- .to_utf8(master$canonical_name)
  for (cc in names(master)) {
    if (is.character(master[[cc]])) master[[cc]] <- .to_utf8(master[[cc]])
  }

  master <- master[!is.na(master$canonical_name) &
                     nchar(master$canonical_name) > 0L, ]
  master[!duplicated(master$canonical_name), ]
}


#' Parse Diaz et al. 2022 supplementary traits (XLSX)
#' @param path Character. Path to the Diaz 2022 XLSX.
#' @return data.frame with canonical_name + seed_mass_mg + plant_height_m.
#' @export
parse_diaz_traits <- function(path) {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop("openxlsx2 is required to read Diaz supplementary xlsx. ",
         "Install with: install.packages('openxlsx2')", call. = FALSE)
  }

  df <- as.data.frame(
    openxlsx2::read_xlsx(path, sheet = 1L),
    stringsAsFactors = FALSE
  )

  name_col <- intersect(
    names(df),
    c("Species", "species", "SpecName", "Taxon", "Scientific_name",
      "AccSpeciesName", "Species name standardized against TPL")
  )
  if (length(name_col) == 0L) {
    cands <- grep("species.*name|standardized|scientific.*name|^name$|taxon.*name",
                  names(df), ignore.case = TRUE, value = TRUE)
    cands <- setdiff(cands, grep("\\bid\\b|_id$|^id_|number|count|\\bn\\.o\\.",
                                  cands, ignore.case = TRUE, value = TRUE))
    if (length(cands) == 0L) {
      cands <- grep("species|taxon", names(df), ignore.case = TRUE, value = TRUE)
      cands <- setdiff(cands, grep("\\bid\\b|_id$|^id_|number|count|\\bn\\.o\\.|level|status|group",
                                    cands, ignore.case = TRUE, value = TRUE))
    }
    name_col <- if (length(cands) > 0L) cands else names(df)[1L]
  }
  name_col <- name_col[1L]

  find_col <- function(patterns) {
    for (p in patterns) {
      m <- grep(p, names(df), ignore.case = TRUE, value = TRUE)
      if (length(m) > 0L) return(m[1L])
    }
    NULL
  }

  safe_num <- function(col_name) {
    if (is.null(col_name)) return(rep(NA_real_, nrow(df)))
    suppressWarnings(as.numeric(df[[col_name]]))
  }

  seed_col <- find_col(c("seed.*mass", "Seed.mass", "sm_", "SeedMass",
                         "Diaspore.mass"))
  height_col <- find_col(c("plant.*height", "Height", "PlantHeight",
                           "Hmax", "height_m"))

  cname <- trimws(df[[name_col]])
  out <- data.frame(
    canonical_name = cname,
    seed_mass_mg   = safe_num(seed_col),
    plant_height_m = safe_num(height_col),
    stringsAsFactors = FALSE
  )

  if (!all(is.na(out$seed_mass_mg))) {
    median_val <- stats::median(out$seed_mass_mg, na.rm = TRUE)
    if (median_val < 1) {
      out$seed_mass_mg <- out$seed_mass_mg * 1000
    }
  }
  if (!all(is.na(out$plant_height_m))) {
    median_val <- stats::median(out$plant_height_m, na.rm = TRUE)
    if (median_val > 100) {
      out$plant_height_m <- out$plant_height_m / 100
    }
  }

  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]
  out <- .append_all_cols(out, df, cname,
                          used = c(name_col, seed_col, height_col))
  # Keep a species if it carries ANY trait (seed/height OR an appended one).
  .trait_finalize(out)
}


#' Parse GRIIS Country Compendium CSV
#' @param path Character. Path to GRIIS_Country_Compendium_V1_0.csv.
#' @return data.frame with canonical_name + country_code + invasive_status.
#' @export
parse_griis <- function(path) {
  df <- utils::read.csv(path, stringsAsFactors = FALSE)

  if ("species" %in% names(df)) {
    name_col <- "species"
  } else {
    name_col <- intersect(
      names(df),
      c("scientificName", "canonicalName", "taxonName", "Scientific.Name",
        "accepted_name")
    )
    if (length(name_col) == 0L) {
      name_col <- grep("scien|canon|species|taxon|name", names(df),
                       ignore.case = TRUE, value = TRUE)
      if (length(name_col) == 0L) name_col <- names(df)[1L]
    }
    name_col <- name_col[1L]
  }

  cc_col <- if ("countryCode_alpha2" %in% names(df)) {
    "countryCode_alpha2"
  } else {
    cc <- grep("countryCode|country_code", names(df), ignore.case = TRUE,
               value = TRUE)
    if (length(cc) > 0L) cc[1L] else NULL
  }

  country_codes <- if (!is.null(cc_col)) {
    toupper(trimws(df[[cc_col]]))
  } else {
    rep(NA_character_, nrow(df))
  }

  is_inv <- if ("isInvasive" %in% names(df)) {
    tolower(trimws(df$isInvasive))
  } else {
    rep("null", nrow(df))
  }
  estab <- if ("establishmentMeans" %in% names(df)) {
    tolower(trimws(df$establishmentMeans))
  } else {
    rep("", nrow(df))
  }

  invasive_status <- ifelse(
    is_inv == "invasive", "invasive",
    ifelse(estab %in% c("alien", "introduced"), "introduced",
    ifelse(estab == "native", "native", "introduced"))
  )

  cname <- trimws(df[[name_col]])
  out <- data.frame(
    canonical_name  = cname,
    country_code    = country_codes,
    invasive_status = invasive_status,
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]
  out <- out[!is.na(out$country_code) & nchar(out$country_code) == 2L, ]
  out <- out[!duplicated(paste(out$canonical_name, out$country_code)), ]

  # Carry the rest of the GRIIS record (raw establishmentMeans, habitat,
  # kingdom/phylum, taxonRank, ...) keyed on (species, country).
  .append_all_cols(
    out, df, cname,
    group = "country_code", group_row = country_codes,
    used = c(name_col, cc_col, "isInvasive", "establishmentMeans")
  )
}
