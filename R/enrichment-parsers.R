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

  out <- data.frame(
    canonical_name = trimws(df[[name_col]]),
    woodiness      = woodiness,
    stringsAsFactors = FALSE
  )
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

  out <- data.frame(
    canonical_name = trimws(df[[name_col]]),
    stringsAsFactors = FALSE
  )
  if (!is.na(light_col)) out$light       <- safe_num(df[[light_col]])
  if (!is.na(temp_col))  out$temperature <- safe_num(df[[temp_col]])
  if (!is.na(moist_col)) out$moisture    <- safe_num(df[[moist_col]])
  if (!is.na(react_col)) out$reaction    <- safe_num(df[[react_col]])
  if (!is.na(nutr_col))  out$nutrients   <- safe_num(df[[nutr_col]])

  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]
  out[!duplicated(out$canonical_name), ]
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
    name_col <- intersect(
      names(df), c("Scientific", "Scientific.Name", "ScientificName")
    )
    if (length(name_col) == 0L) name_col <- names(df)[1L] else name_col <- name_col[1L]

    out <- data.frame(
      canonical_name = trimws(df[[name_col]]),
      stringsAsFactors = FALSE
    )
    for (out_name in names(col_map)) {
      src <- resolve_col(df, col_map[[out_name]])
      out[[out_name]] <- if (!is.null(src)) {
        suppressWarnings(as.numeric(df[[src]]))
      } else {
        NA_real_
      }
    }
    out
  }

  birds <- utils::read.delim(birds_path, stringsAsFactors = FALSE, quote = "")
  mammals <- utils::read.delim(mammals_path, stringsAsFactors = FALSE,
                               quote = "")

  out <- rbind(extract_one(birds), extract_one(mammals))
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

  out <- data.frame(
    canonical_name  = trimws(df[[name_col]]),
    beak_length     = safe_num(find_col(c("Beak.Length_Culmen", "Beak.Length",
                                          "culmen_length", "Bill.Length"))),
    beak_depth      = safe_num(find_col(c("Beak.Depth", "bill_depth",
                                          "Bill.Depth"))),
    wing_length     = safe_num(find_col(c("Wing.Length", "wing_length"))),
    tail_length     = safe_num(find_col(c("Tail.Length", "tail_length"))),
    tarsus_length   = safe_num(find_col(c("Tarsus.Length", "tarsus_length"))),
    body_mass_g     = safe_num(find_col(c("Mass", "Body.Mass", "body_mass",
                                          "BodyMass", "Mass.g"))),
    hand_wing_index = safe_num(find_col(c("Hand.Wing.Index", "Hand-Wing.Index",
                                          "HWI", "hand_wing_index"))),
    habitat         = safe_chr(find_col(c("Habitat", "Primary.Lifestyle",
                                          "habitat"))),
    trophic_level   = safe_chr(find_col(c("Trophic.Level", "trophic_level"))),
    trophic_niche   = safe_chr(find_col(c("Trophic.Niche", "trophic_niche"))),
    migration       = safe_chr(find_col(c("Migration", "migration"))),
    stringsAsFactors = FALSE
  )

  if (!all(is.na(out$migration))) {
    mig <- tolower(trimws(out$migration))
    out$migration <- ifelse(grepl("^1$|^sedentar|^resident", mig), "sedentary",
                    ifelse(grepl("^2$|^partial", mig), "partial",
                    ifelse(grepl("^3$|^full|^migra", mig), "full",
                    NA_character_)))
  }

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

  out <- data.frame(
    canonical_name  = trimws(df[[name_col]]),
    body_mass_g     = safe_num(find_col(c("AdultBodyMass_g",
                                          "X5.1_AdultBodyMass",
                                          "BodyMass"))),
    longevity_mo    = safe_num(find_col(c("MaxLongevity_m",
                                          "X17.1_MaxLongevity"))),
    litter_size     = safe_num(find_col(c("LitterSize",
                                          "X15.1_LitterSize"))),
    gestation_d     = safe_num(find_col(c("GestationLen_d",
                                          "X9.1_GestationLen"))),
    weaning_d       = safe_num(find_col(c("WeaningAge_d",
                                          "X25.1_WeaningAge"))),
    home_range_km2  = safe_num(find_col(c("HomeRange_km2",
                                          "X22.1_HomeRange",
                                          "HomeRange_Indiv_km2"))),
    diet_breadth    = safe_num(find_col(c("DietBreadth",
                                          "X6.2_TrophicLevel",
                                          "diet_breadth"))),
    habitat_breadth = safe_num(find_col(c("HabitatBreadth",
                                          "X12.2_HabitatBreadth",
                                          "habitat_breadth"))),
    stringsAsFactors = FALSE
  )

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

  out <- data.frame(
    canonical_name      = trimws(df[[name_col]]),
    body_size_mm        = safe_num(find_col(c("Body_size_mm", "Body.size.mm",
                                              "SVL_mm", "Body_length_mm"))),
    age_maturity_y      = safe_num(find_col(c("Age_at_maturity_min_y",
                                              "Age_at_maturity",
                                              "Age.at.maturity"))),
    longevity_yr        = safe_num(find_col(c("Longevity_max_y", "Longevity_max",
                                              "Longevity"))),
    litter_size         = safe_num(find_col(c("Litter_size_max_n",
                                              "Litter.size", "Clutch_size"))),
    reproductive_output = safe_num(find_col(c("Reproductive_output_y",
                                              "Reproductive.output"))),
    offspring_size_mm   = safe_num(find_col(c("Offspring_size_mm",
                                              "Offspring.size"))),
    direct_development  = safe_int(find_col(c("Dir", "Direct_development",
                                              "Devel_direct"))),
    larval              = safe_int(find_col(c("Lar", "Larval", "Has_larva"))),
    aquatic             = safe_int(find_col(c("Aqu", "Aquatic"))),
    fossorial           = safe_int(find_col(c("Fos", "Fossorial"))),
    arboreal            = safe_int(find_col(c("Arb", "Arboreal"))),
    diurnal             = safe_int(find_col(c("Diu", "Diurnal"))),
    nocturnal_amphibio  = safe_int(find_col(c("Noc", "Nocturnal"))),
    stringsAsFactors = FALSE
  )

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

  out <- data.frame(
    canonical_name             = trimws(gsub("_", " ", df[[name_col]])),
    max_body_length            = safe_num(find_col(c("MBl", "MBI",
                                                     "Max_body_length"))),
    body_elongation            = safe_num(find_col(c("BEl",
                                                     "Body_elongation"))),
    vertical_eye_position      = safe_num(find_col(c("VEp",
                                                     "Vertical_eye_position"))),
    relative_eye_size          = safe_num(find_col(c("REs",
                                                     "Relative_eye_size"))),
    oral_gape_position         = safe_num(find_col(c("OGp",
                                                     "Oral_gape_position"))),
    relative_maxillary_length  = safe_num(find_col(c("RMl",
                                                     "Relative_maxillary_length"))),
    body_lateral_shape         = safe_num(find_col(c("BLs",
                                                     "Body_lateral_shape"))),
    pectoral_fin_position      = safe_num(find_col(c("PFv",
                                                     "Pectoral_fin_vertical"))),
    pectoral_fin_size          = safe_num(find_col(c("PFs",
                                                     "Pectoral_fin_size"))),
    caudal_peduncle_throttling = safe_num(find_col(c("CPt",
                                                     "Caudal_peduncle_throttling"))),
    stringsAsFactors = FALSE
  )

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
    buoyancy      = "buoyancy.txt"
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
      lines <- readLines(con, n = max_scan, warn = FALSE)
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

    tryCatch({
      skip_n <- find_header_skip(path)
      df <- utils::read.csv(path, sep = ";", stringsAsFactors = FALSE,
                            fileEncoding = "latin1", skip = skip_n,
                            check.names = FALSE)
      if (ncol(df) <= 1L) {
        df <- utils::read.delim(path, stringsAsFactors = FALSE,
                                fileEncoding = "latin1", skip = skip_n,
                                check.names = FALSE)
      }
      df
    }, error = function(e) {
      tryCatch(
        utils::read.delim(path, stringsAsFactors = FALSE, skip = 0L,
                          check.names = FALSE),
        error = function(e2) NULL
      )
    })
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
  master <- master[, expected]

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

  out <- data.frame(
    canonical_name = trimws(df[[name_col]]),
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
  has_data <- !is.na(out$seed_mass_mg) | !is.na(out$plant_height_m)
  out <- out[has_data, ]
  out[!duplicated(out$canonical_name), ]
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

  out <- data.frame(
    canonical_name  = trimws(df[[name_col]]),
    country_code    = country_codes,
    invasive_status = invasive_status,
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]
  out <- out[!is.na(out$country_code) & nchar(out$country_code) == 2L, ]
  out[!duplicated(paste(out$canonical_name, out$country_code)), ]
}
