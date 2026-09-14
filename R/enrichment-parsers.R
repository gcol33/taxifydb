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
parse_zanne <- function(path) {
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

#' Parse EltonTraits 1.0 birds + mammals TSVs
#' @param birds_path Character. Path to BirdFuncDat.txt.
#' @param mammals_path Character. Path to MamFuncDat.txt.
#' @return data.frame with canonical_name + diet/foraging/body mass columns.
#' @export
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
    name_col <- .first_col(
      df, c("Scientific", "Scientific.Name", "ScientificName"),
      fallback = names(df)[1L]
    )

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

  name_col <- .first_col(
    df,
    c("Species1", "Species1_BirdLife", "Species", "Scientific",
      "ScientificName", "species_name"),
    fallback = names(df)[1L]
  )

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

  name_col <- .first_col(
    df, c("MSW05_Binomial", "MSW93_Binomial", "Scientific_Name"),
    fallback = names(df)[1L]
  )

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

  name_col <- .first_col(df, c("Species", "species", "Scientific"),
                         fallback = names(df)[1L])

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
parse_fishmorph <- function(path) {
  df <- utils::read.csv2(path, stringsAsFactors = FALSE,
                         fileEncoding = "latin1", dec = ".")

  name_col <- .first_col(
    df, c("Genus.species", "Genus species", "Species", "scientificNameStd"),
    fallback = names(df)[1L]
  )

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


#' Parse LEDA trait files
#'
#' Each LEDA text dump is a query export: an SQL preamble closed by an
#' `on <date> .` line, then a `;`-separated table whose first line is the header.
#' [.read_leda_table()] reads that structure, and every output column is taken
#' from one named header field of one file, listed in `.leda_trait_spec()`. A
#' named field missing from its file stops the parse rather than falling back to
#' another column.
#'
#' Numeric traits read LEDA's `single value` field. LEDA fills it for every
#' record as the record's value: the mean where one was reported, otherwise the
#' median, otherwise the reported extreme or the midpoint of the reported range.
#' Records are reduced to the species median, with the gated `<col>_min` /
#' `<col>_max` / `<col>_n` spread of `.num_group_spread()`. Categorical traits
#' report the species' most frequent value. Units are LEDA's own (Knevel et al.
#' 2003, *Collecting and measuring standards of life-history traits of the
#' Northwest European flora*): seed mass mg, seed length mm, canopy height m,
#' leaf mass mg, leaf size mm2, SLA mm2/mg, LDMC mg/g, terminal velocity m/s,
#' releasing height m.
#'
#' Two traits need more than a field:
#' * stem specific density (`ssd.txt`) has no `single value` field, so a record
#'   reads its mean, else its median, else the midpoint of its range. The
#'   header says g/cm3 and LEDA's validity range is 0-1.5 g/cm3, but most
#'   records carry wood densities in kg/m3 (Quercus robur 689); a value above
#'   1.5 is read as kg/m3 and divided by 1000.
#' * floating capacity (`buoyancy.txt`) is the percentage of diaspores still
#'   floating after a given time; `floating_capacity_1week_pct` reads the records
#'   at LEDA's standard final interval, `T6 - 1 week`.
#'
#' Each trait column `<col>` read from a file that records its references has a
#' `<col>_source` column naming the references behind the value: for a
#' categorical trait the records that state the reported value, for a numeric
#' trait every record entering the median. A record's reference is its
#' `original reference` where LEDA gives one (the publication the value was
#' taken from), with the contributing `reference` kept as `via`; otherwise its
#' `reference`. Ids are the first 12 hex digits of the md5 of the citation and
#' `via`, and resolve against the table attached with [attach_references()].
#' `dispersal_type.txt` is an aggregated query export with no reference field,
#' so `dispersal_type` has no provenance column.
#'
#' @param dir_path Character. Directory containing the LEDA *.txt files.
#' @return data.frame with canonical_name + LEDA trait columns.
#' @export
parse_leda <- function(dir_path) {
  spec <- .leda_trait_spec()
  tables <- list()
  table_of <- function(file) {
    if (is.null(tables[[file]])) {
      path <- file.path(dir_path, file)
      tables[[file]] <<- if (file.exists(path)) .read_leda_table(path) else NA
    }
    tables[[file]]
  }

  ref_tab <- NULL
  record_refs <- function(df) {
    nm <- names(df)
    if (!"reference" %in% nm) return(NULL)
    clean <- function(v) {
      v <- trimws(as.character(v))
      v[is.na(v) | !nzchar(v) | v == "NA"] <- NA_character_
      v
    }
    ref  <- clean(df[["reference"]])
    orig <- if ("original reference" %in% nm) clean(df[["original reference"]])
            else rep(NA_character_, nrow(df))
    cit  <- ifelse(is.na(orig), ref, orig)
    via  <- ifelse(is.na(orig) | (!is.na(ref) & ref == orig), NA_character_, ref)
    id   <- .leda_ref_id(cit, via)
    new  <- unique(data.frame(ref_id = id, citation = cit, via = via,
                              stringsAsFactors = FALSE)[!is.na(id), ,
                                                        drop = FALSE])
    ref_tab <<- unique(rbind(ref_tab, new))
    id
  }

  cols <- list()
  for (oc in names(spec)) {
    s  <- spec[[oc]]
    df <- table_of(s$file)
    if (!is.data.frame(df)) next
    need <- setdiff(c("SBS name", s$col, names(s$where)), names(df))
    if (length(need)) {
      stop(sprintf("LEDA %s has no field %s.", s$file,
                   paste(sprintf("'%s'", need), collapse = ", ")), call. = FALSE)
    }
    keep <- rep(TRUE, nrow(df))
    for (w in names(s$where)) keep <- keep & df[[w]] %in% s$where[[w]]
    rid  <- record_refs(df)
    if (!is.null(rid)) rid <- rid[keep]
    df   <- df[keep, , drop = FALSE]
    val  <- if (is.function(s$value)) s$value(df) else df[[s$col]]
    cols[[oc]] <- list(name = trimws(df[["SBS name"]]), value = val, ref = rid,
                       type = s$type)
  }
  if (!length(cols)) {
    stop("No LEDA data could be parsed from downloaded files.", call. = FALSE)
  }

  names_all <- unlist(lapply(cols, function(c) c$name), use.names = FALSE)
  species <- sort(unique(names_all[!is.na(names_all) & nzchar(names_all)]))
  master <- data.frame(canonical_name = species, stringsAsFactors = FALSE)
  for (oc in names(spec)) {
    c <- cols[[oc]]
    if (is.null(c)) {
      master[[oc]] <- if (spec[[oc]]$type == "num") NA_real_ else NA_character_
      next
    }
    if (c$type == "num") {
      v <- suppressWarnings(as.numeric(c$value))
      master <- .attach_num_spread(master, oc, .num_group_spread(v, c$name),
                                   species)
    } else {
      v <- trimws(as.character(c$value))
      v[!is.na(v) & !nzchar(v)] <- NA_character_
      in_set <- !is.na(c$name) & c$name %in% species
      agg <- tapply(v[in_set], factor(c$name[in_set], levels = species), .cat_mode)
      master[[oc]] <- as.character(agg[species])
    }
    if (!is.null(c$ref)) {
      master[[.source_colname(oc, names(master))]] <- .value_refs(
        c$name, v, c$ref, master[[oc]], species, type = c$type)
    }
  }

  if (!is.null(cols$raunkiaer_life_form)) {
    lf <- cols$raunkiaer_life_form
    v  <- trimws(lf$value)
    ok <- !is.na(v) & nzchar(v) & lf$name %in% species
    n_forms <- tapply(v[ok], factor(lf$name[ok], levels = species),
                      function(x) length(unique(x)))
    master$raunkiaer_variable <- ifelse(is.na(master$raunkiaer_life_form),
                                        NA_integer_,
                                        as.integer(n_forms[species] > 1L))
  }

  trait_cols <- setdiff(names(master),
                        c("canonical_name", .reference_cols(names(master))))
  master <- master[rowSums(!is.na(master[trait_cols])) > 0L, , drop = FALSE]
  rownames(master) <- NULL
  if (is.null(ref_tab)) return(master)
  ref_tab$doi <- .extract_doi(ref_tab$citation)
  attach_references(master, ref_tab[c("ref_id", "citation", "doi", "via")])
}


#' What each LEDA output column reads
#'
#' One entry per output column: the file, the header field(s) it needs, the kind
#' (`num` or `cat`), an optional `where` (field -> accepted values) selecting
#' records, and an optional `value` function computing the record value.
#' @noRd
.leda_trait_spec <- function() {
  num <- function(file, col, where = NULL, value = NULL) {
    list(file = file, col = col, type = "num", where = where, value = value)
  }
  cat <- function(file, col) {
    list(file = file, col = col, type = "cat", where = NULL, value = NULL)
  }
  list(
    raunkiaer_life_form         = cat("life_form.txt", "plant growth form"),
    dispersal_type              = cat("dispersal_type.txt", "dispersal type"),
    terminal_velocity_ms        = num("TV.txt", "single value [m/s]"),
    leda_seed_mass_mg           = num("seed_mass.txt", "single value [mg]"),
    canopy_height_m             = num("canopy_height.txt", "single value [m]"),
    leaf_mass_mg                = num("leaf_mass.txt", "single value [mg]"),
    sla_mm2_mg                  = num("SLA.txt", "single value [mm^2/mg]"),
    clonal_growth_organ         = cat("clonal_growth.txt", "clonal growth organ 1"),
    floating_capacity_1week_pct = num("buoyancy.txt", "single value [%]",
                                      where = list(`fixed time step` = "T6 - 1 week")),
    age_first_flowering         = cat("age_of_first_flowering.txt", "age of first flowering"),
    branching                   = cat("branching.txt", "branching"),
    bud_bank_seasonality        = cat("buds_seasonality.txt", "BBS above ground"),
    buds_vertical_distribution  = cat("buds_vertical_dist.txt", "buds above ground"),
    leaf_distribution           = cat("leaf_distribution.txt", "leaf distribution"),
    ldmc_mg_g                   = num("LDMC_und_Geo.txt", "single value [mg/g]"),
    leaf_size_mm2               = num("leaf_size.txt", "single value [mm^2]"),
    diaspore_type               = cat("morphology_dispersal_unit.txt", "diaspore type"),
    plant_life_span             = cat("plant_life_span.txt", "plant lifespan"),
    releasing_height_m          = num("releasing_height.txt", "single value [m]"),
    seed_longevity_index        = num("seed_longevity.txt", "SSB seed longevity index"),
    seed_number_per_plant       = num("seed_number.txt", "single value"),
    seed_length_mm              = num("seed_shape.txt", "length (single value) [mm]"),
    shoot_growth_form           = cat("shoot_growth_form.txt", "shoot growth form"),
    ssd_g_cm3                   = num("ssd.txt", c("mean SSD [g/cm^3]",
                                                   "median SSD [g/cm^3]",
                                                   "minimum SSD [g/cm^3]",
                                                   "maximum SSD [g/cm^3]"),
                                      value = .leda_ssd_value)
  )
}


#' Stem specific density of each LEDA `ssd.txt` record, in g/cm3
#'
#' The mean, else the median, else the midpoint of the reported range. A value
#' above LEDA's validity range for stem specific density (0-1.5 g/cm3) is a
#' wood density in kg/m3 and is divided by 1000.
#' @noRd
.leda_ssd_value <- function(df) {
  n <- function(col) suppressWarnings(as.numeric(df[[col]]))
  mean_ <- n("mean SSD [g/cm^3]")
  med   <- n("median SSD [g/cm^3]")
  mid   <- (n("minimum SSD [g/cm^3]") + n("maximum SSD [g/cm^3]")) / 2
  v <- ifelse(!is.na(mean_), mean_, ifelse(!is.na(med), med, mid))
  ifelse(!is.na(v) & v > 1.5, v / 1000, v)
}


#' Read one LEDA query export into a table
#'
#' The file opens with the SQL query that produced it and an `on <date> .` line;
#' the first non-empty line after that is the `;`-separated header. Fields are
#' read on the header's own field count. A few free-text fields contain `; `
#' (`Same [S]; nicht heteromorph [n]`), which gives their rows more fields than
#' the header; the extra separators of such a row are exactly those followed by
#' a space, and a row where that does not account for the excess stops the read.
#' Lines are decoded one by one, since the dumps are latin1 with UTF-8 mixed in.
#'
#' @param path Character. Path to one LEDA `.txt` file.
#' @return data.frame of character columns named by the trimmed header, empty
#'   fields as `NA`.
#' @noRd
.read_leda_table <- function(path) {
  raw <- readBin(path, "raw", file.size(path))
  lines <- strsplit(rawToChar(raw), "\n", fixed = TRUE, useBytes = TRUE)[[1L]]
  lines <- .to_utf8(sub("\r$", "", lines, useBytes = TRUE))

  on_re <- paste0("^on [A-Z][a-z]{2} [A-Z][a-z]{2} +[0-9]{1,2} ",
                  "[0-9]{2}:[0-9]{2}:[0-9]{2} [A-Z]+ [0-9]{4} [.];*$")
  on <- grep(on_re, lines)
  if (!length(on)) {
    stop(sprintf("LEDA %s: no 'on <date> .' line closing the query preamble.",
                 basename(path)), call. = FALSE)
  }
  has_text <- function(x) grepl("[^; \t]", x)
  after <- if (on[1L] < length(lines)) seq.int(on[1L] + 1L, length(lines)) else integer(0)
  h <- after[has_text(lines[after])][1L]
  if (is.na(h)) {
    stop(sprintf("LEDA %s: no header after the query preamble.", basename(path)),
         call. = FALSE)
  }

  fields <- function(x) {
    lapply(strsplit(paste0(x, ";|"), ";", fixed = TRUE),
           function(v) v[-length(v)])
  }
  header <- trimws(fields(lines[h])[[1L]])
  body   <- if (h < length(lines)) lines[seq.int(h + 1L, length(lines))] else character(0)
  body   <- body[has_text(body)]
  rows   <- fields(body)
  k      <- length(header)

  for (i in which(lengths(rows) > k)) {
    parts <- rows[[i]]
    extra <- length(parts) - k
    join  <- which(startsWith(parts[-1L], " "))
    if (length(join) != extra) {
      stop(sprintf(paste0("LEDA %s: row %d has %d more field(s) than the header ",
                          "and %d separator(s) followed by a space."),
                   basename(path), i, extra, length(join)), call. = FALSE)
    }
    for (j in rev(join)) {
      parts[j] <- paste0(parts[j], ";", parts[j + 1L])
      parts <- parts[-(j + 1L)]
    }
    rows[[i]] <- parts
  }
  short <- lengths(rows) < k
  rows[short] <- lapply(rows[short], function(v) c(v, rep("", k - length(v))))

  m <- matrix(unlist(rows, use.names = FALSE), ncol = k, byrow = TRUE)
  m[!nzchar(trimws(m))] <- NA_character_
  out <- as.data.frame(m, stringsAsFactors = FALSE)
  names(out) <- header
  out
}


#' Stable id of a LEDA reference
#'
#' The first 12 hex digits of the md5 of the citation and the contributing
#' reference it came through. `NA` where the record names no reference.
#' @noRd
.leda_ref_id <- function(citation, via) {
  key <- paste(citation, ifelse(is.na(via), "", via), sep = "\x1f")
  out <- vapply(key, function(k) substr(digest::digest(k, algo = "md5",
                                                       serialize = FALSE),
                                        1L, 12L),
                character(1L), USE.NAMES = FALSE)
  out[is.na(citation)] <- NA_character_
  out
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

  name_col <- .first_col(
    df,
    c("Species", "species", "SpecName", "Taxon", "Scientific_name",
      "AccSpeciesName", "Species name standardized against TPL")
  )
  if (is.null(name_col)) {
    cands <- grep("species.*name|standardized|scientific.*name|^name$|taxon.*name",
                  names(df), ignore.case = TRUE, value = TRUE)
    cands <- setdiff(cands, grep("\\bid\\b|_id$|^id_|number|count|\\bn\\.o\\.",
                                  cands, ignore.case = TRUE, value = TRUE))
    if (length(cands) == 0L) {
      cands <- grep("species|taxon", names(df), ignore.case = TRUE, value = TRUE)
      cands <- setdiff(cands, grep("\\bid\\b|_id$|^id_|number|count|\\bn\\.o\\.|level|status|group",
                                    cands, ignore.case = TRUE, value = TRUE))
    }
    name_col <- if (length(cands) > 0L) cands[1L] else names(df)[1L]
  }

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
    name_col <- .first_col(
      df,
      c("scientificName", "canonicalName", "taxonName", "Scientific.Name",
        "accepted_name")
    )
    if (is.null(name_col)) {
      loose <- grep("scien|canon|species|taxon|name", names(df),
                    ignore.case = TRUE, value = TRUE)
      name_col <- if (length(loose) > 0L) loose[1L] else names(df)[1L]
    }
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
