# Remaining trait parsers: FUNGuild, FishBase, FungalTraits, AlgaeTraits,
# lizard traits, LepTraits, AnimalTraits, NW European arthropod traits,
# AnAge, GloNAF.


#' Parse FUNGuild JSON database dump
#'
#' Reads the JSON array returned by the FUNGuild API, filters to genus and
#' species-level entries, and returns a clean data.frame with `canonical_name`
#' and trait columns.
#'
#' @param path Character. Path to the downloaded JSON/HTML file.
#' @return data.frame with canonical_name + trophic_mode + guild +
#'   growth_morphology + confidence_ranking.
#' @export
parse_funguild <- function(path) {
  txt <- readLines(path, warn = FALSE)
  txt <- paste(txt, collapse = "\n")
  # stbates.org wraps the JSON array in <html>...<body>...JSON...</body></html>.
  # Extract the JSON array between the first '[' and the matching trailing ']'.
  start <- regexpr("\\[", txt)
  end   <- max(gregexpr("\\]", txt)[[1L]])
  if (start <= 0L || end <= start) {
    stop("Cannot locate JSON array in FUNGuild response.", call. = FALSE)
  }
  json_str <- substr(txt, start, end)
  raw <- jsonlite::fromJSON(json_str, simplifyVector = TRUE)

  # Index Fungorum numeric taxonomic levels:
  # 12=family, 13=genus, 20=species, 25=variety, 26=form, 27=subspecies.
  level_col <- intersect(names(raw), c("taxonomicLevel", "taxonLevel"))
  if (length(level_col) == 0L) {
    stop("FUNGuild response missing taxonomicLevel/taxonLevel column.",
         call. = FALSE)
  }
  level_raw <- trimws(as.character(raw[[level_col[1L]]]))
  level_norm <- ifelse(grepl("^[0-9]+$", level_raw),
    c("13" = "genus", "20" = "species", "25" = "species",
      "26" = "species", "27" = "species")[level_raw],
    tolower(level_raw)
  )
  raw$taxonLevel <- level_norm

  keep <- level_norm %in% c("genus", "species")
  df <- raw[keep, ]

  if (nrow(df) == 0L) {
    stop("No genus/species-level entries found in FUNGuild JSON.",
         call. = FALSE)
  }

  taxon <- trimws(df$taxon)
  taxon_level <- tolower(trimws(df$taxonLevel))

  trophic <- if ("trophicMode" %in% names(df)) {
    trimws(df$trophicMode)
  } else if ("trophic_mode" %in% names(df)) {
    trimws(df$trophic_mode)
  } else {
    NA_character_
  }

  guild <- if ("guild" %in% names(df)) {
    trimws(df$guild)
  } else {
    NA_character_
  }

  growth <- if ("growthMorphology" %in% names(df)) {
    trimws(df$growthMorphology)
  } else if ("growthForm" %in% names(df)) {
    trimws(df$growthForm)
  } else if ("growth_morphology" %in% names(df)) {
    trimws(df$growth_morphology)
  } else {
    NA_character_
  }

  confidence <- if ("confidenceRanking" %in% names(df)) {
    trimws(df$confidenceRanking)
  } else if ("confidence_ranking" %in% names(df)) {
    trimws(df$confidence_ranking)
  } else if ("confidence" %in% names(df)) {
    trimws(df$confidence)
  } else {
    NA_character_
  }

  trophic[!nzchar(trophic)] <- NA_character_
  guild[!nzchar(guild)] <- NA_character_
  growth[!nzchar(growth)] <- NA_character_
  confidence[!nzchar(confidence)] <- NA_character_

  out <- data.frame(
    canonical_name     = taxon,
    taxon_level        = taxon_level,
    trophic_mode       = trophic,
    guild              = guild,
    growth_morphology  = growth,
    confidence_ranking = confidence,
    stringsAsFactors   = FALSE
  )

  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]

  # Species-level entries beat genus-level for the same canonical_name
  out <- out[order(out$taxon_level != "species", out$canonical_name), ]
  out <- out[!duplicated(out$canonical_name), ]
  out$taxon_level <- NULL

  out
}


#' Parse FishBase species and ecology tables (via rfishbase)
#'
#' Pulls the `species` and `ecology` tables from rfishbase, joins them, and
#' builds a `canonical_name` + trait data.frame.
#'
#' @param path Character. Not used (rfishbase fetches data directly), kept
#'   for interface consistency.
#' @return data.frame with canonical_name + body/depth/diet columns.
#' @export
parse_fishbase <- function(path) {
  if (!requireNamespace("rfishbase", quietly = TRUE)) {
    stop("rfishbase is required to build the FishBase enrichment from source.\n",
         "Install it with: install.packages(\"rfishbase\")", call. = FALSE)
  }

  sp <- rfishbase::species(server = "fishbase")
  eco <- rfishbase::ecology(server = "fishbase")

  sp$canonical_name <- trimws(paste(sp$Genus, sp$Species))

  eco_sub <- eco[, intersect(
    names(eco),
    c("SpecCode", "FeedingType", "DietTroph")
  ), drop = FALSE]
  eco_sub <- eco_sub[!duplicated(eco_sub$SpecCode), ]
  merged <- merge(sp, eco_sub, by = "SpecCode", all.x = TRUE)

  safe_num <- function(col_name) {
    if (!col_name %in% names(merged)) return(rep(NA_real_, nrow(merged)))
    suppressWarnings(as.numeric(merged[[col_name]]))
  }

  safe_chr <- function(col_name) {
    if (!col_name %in% names(merged)) return(rep(NA_character_, nrow(merged)))
    x <- as.character(merged[[col_name]])
    x[is.na(x) | nchar(trimws(x)) == 0L] <- NA_character_
    trimws(x)
  }

  out <- data.frame(
    canonical_name  = merged$canonical_name,
    body_length_cm  = safe_num("Length"),
    body_mass_g     = safe_num("Weight"),
    trophic_level   = safe_num("DietTroph"),
    depth_min_m     = safe_num("DepthRangeShallow"),
    depth_max_m     = safe_num("DepthRangeDeep"),
    vulnerability   = safe_num("Vulnerability"),
    habitat         = safe_chr("DemersPelag"),
    importance      = safe_chr("Importance"),
    stringsAsFactors = FALSE
  )

  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]
  out[!duplicated(out$canonical_name), ]
}


#' Parse FungalTraits XLSX (Table S1, genus-level traits)
#'
#' Reads the Polme et al. (2020) supplementary XLSX, selects the most
#' informative trait columns, and returns a data.frame keyed on `genus`.
#'
#' @param path Character. Path to the downloaded XLSX file.
#' @return data.frame with `genus` + 9 trait columns.
#' @export
parse_fungal_traits <- function(path) {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop("openxlsx2 is required to read FungalTraits xlsx. ",
         "Install with: install.packages('openxlsx2')", call. = FALSE)
  }

  df <- as.data.frame(
    openxlsx2::read_xlsx(path, sheet = 1L),
    stringsAsFactors = FALSE
  )

  find_col <- function(patterns) {
    for (p in patterns) {
      m <- grep(p, names(df), value = TRUE, ignore.case = TRUE)
      if (length(m) > 0L) return(m[1L])
    }
    NA_character_
  }

  genus_col      <- find_col(c("^GENUS$", "^genus$"))
  primary_col    <- find_col(c("primary_lifestyle", "Primary.lifestyle"))
  secondary_col  <- find_col(c("secondary_lifestyle", "Secondary.lifestyle"))
  growth_col     <- find_col(c("growth_form", "Growth.form"))
  fruit_col      <- find_col(c("fruitbody_type", "Fruitbody.type"))
  decay_col      <- find_col(c("decay_substrate", "Decay.substrate",
                               "Decay.type"))
  plant_path_col <- find_col(c("plant_pathogenic_capacity",
                               "Plant.pathogenic.capacity",
                               "Plant.pathogenic"))
  animal_col     <- find_col(c("animal_biotrophic_capacity",
                               "Animal.biotrophic.capacity",
                               "Animal.biotrophic"))
  endo_col       <- find_col(c("endophytic_interaction_capability",
                               "Endophytic.interaction",
                               "Endophyte"))
  ecto_col       <- find_col(c("ectomycorrhiza_exploration_type",
                               "Ectomycorrhiza.exploration.type",
                               "Exploration.type"))

  if (is.na(genus_col)) {
    stop("Could not find a 'GENUS' column in FungalTraits XLSX.",
         call. = FALSE)
  }

  safe_char <- function(x) {
    x <- as.character(x)
    x[x %in% c("", "NA", "na", "N/A")] <- NA_character_
    trimws(x)
  }

  genus_clean <- trimws(df[[genus_col]])
  out <- data.frame(canonical_name = genus_clean,
                    genus          = genus_clean,
                    stringsAsFactors = FALSE)

  if (!is.na(primary_col))    out$primary_lifestyle    <- safe_char(df[[primary_col]])
  if (!is.na(secondary_col))  out$secondary_lifestyle  <- safe_char(df[[secondary_col]])
  if (!is.na(growth_col))     out$growth_form          <- safe_char(df[[growth_col]])
  if (!is.na(fruit_col))      out$fruitbody_type       <- safe_char(df[[fruit_col]])
  if (!is.na(decay_col))      out$decay_substrate      <- safe_char(df[[decay_col]])
  if (!is.na(plant_path_col)) out$plant_pathogenic_capacity     <- safe_char(df[[plant_path_col]])
  if (!is.na(animal_col))     out$animal_biotrophic_capacity    <- safe_char(df[[animal_col]])
  if (!is.na(endo_col))       out$endophytic_interaction_capability <- safe_char(df[[endo_col]])
  if (!is.na(ecto_col))       out$ectomycorrhiza_exploration_type   <- safe_char(df[[ecto_col]])

  out <- out[!is.na(out$genus) & nchar(out$genus) > 0L, ]

  out <- out[!duplicated(out$genus), , drop = FALSE]
  rownames(out) <- NULL
  out
}


#' Parse AlgaeTraits macroalgal traits (WoRMS ZIP export)
#' @param path Character. Directory holding CSV/TXT/XLSX files extracted from
#'   the AlgaeTraits archive.
#' @return data.frame with canonical_name + algae trait columns.
#' @export
parse_algae_traits <- function(path) {
  csvs <- list.files(path, pattern = "\\.csv$", full.names = TRUE,
                     recursive = TRUE, ignore.case = TRUE)
  txts <- list.files(path, pattern = "\\.txt$", full.names = TRUE,
                     recursive = TRUE, ignore.case = TRUE)
  xlsxs <- list.files(path, pattern = "\\.xlsx$", full.names = TRUE,
                      recursive = TRUE, ignore.case = TRUE)
  candidates <- c(csvs, txts)

  if (length(candidates) == 0L && length(xlsxs) > 0L) {
    if (!requireNamespace("openxlsx2", quietly = TRUE)) {
      stop("openxlsx2 is required to read AlgaeTraits XLSX files.\n",
           "Install with: install.packages('openxlsx2')", call. = FALSE)
    }
    sizes <- file.size(xlsxs)
    main_file <- xlsxs[which.max(sizes)]
    df <- as.data.frame(
      openxlsx2::read_xlsx(main_file),
      stringsAsFactors = FALSE
    )
  } else if (length(candidates) > 0L) {
    sizes <- file.size(candidates)
    main_file <- candidates[which.max(sizes)]
    first_line <- readLines(main_file, n = 1L, warn = FALSE)
    sep <- if (grepl("\t", first_line)) "\t" else ","
    df <- utils::read.delim(main_file, sep = sep, stringsAsFactors = FALSE,
                            quote = "\"", fill = TRUE, comment.char = "")
  } else {
    stop("No data files found in AlgaeTraits archive.\n",
         "Contents: ", paste(list.files(path, recursive = TRUE),
                             collapse = ", "), call. = FALSE)
  }

  names(df) <- tolower(trimws(names(df)))

  name_col <- NULL
  name_candidates <- c("scientificname", "scientific_name", "species",
                       "taxon", "valid_name", "validname",
                       "canonical_name", "scientificnameaccepted")
  for (nc in name_candidates) {
    if (nc %in% names(df)) { name_col <- nc; break }
  }
  if (is.null(name_col)) {
    for (i in seq_len(ncol(df))) {
      if (is.character(df[[i]])) { name_col <- names(df)[i]; break }
    }
  }
  if (is.null(name_col)) {
    stop("Cannot identify species name column. Columns: ",
         paste(names(df), collapse = ", "), call. = FALSE)
  }

  trait_col <- NULL
  value_col <- NULL
  long_trait_names <- c("measurementtype", "measurement_type", "traitname",
                        "trait_name", "trait", "category", "attributename")
  long_value_names <- c("measurementvalue", "measurement_value", "traitvalue",
                        "trait_value", "value", "attributevalue")
  for (tc in long_trait_names) {
    if (tc %in% names(df)) { trait_col <- tc; break }
  }
  for (vc in long_value_names) {
    if (vc %in% names(df)) { value_col <- vc; break }
  }

  if (!is.null(trait_col) && !is.null(value_col)) {
    out <- .pivot_algae_long(df, name_col, trait_col, value_col)
  } else {
    out <- .extract_algae_wide(df, name_col)
  }

  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]
  out[!duplicated(out$canonical_name), ]
}


#' Pivot long-format AlgaeTraits data to wide
#' @noRd
.pivot_algae_long <- function(df, name_col, trait_col, value_col) {
  species <- trimws(df[[name_col]])
  traits  <- tolower(trimws(df[[trait_col]]))
  values  <- trimws(as.character(df[[value_col]]))

  unique_species <- unique(species[!is.na(species) & nchar(species) > 0L])

  trait_map <- list(
    body_size_cm  = c("body size", "bodysize", "thallus length",
                      "thallus_length", "maximum length", "size"),
    growth_form   = c("body shape", "bodyshape", "growth form",
                      "growth_form", "morphology", "morphological type"),
    calcification = c("calcification", "calcified"),
    life_span     = c("life span", "lifespan", "life_span", "longevity"),
    tidal_zone    = c("tidal zonation", "tidal_zonation", "tidal zone",
                      "tidal_zone", "zonation"),
    wave_exposure = c("wave exposure", "wave_exposure", "exposure"),
    environment   = c("environment", "habitat", "salinity regime"),
    substrate     = c("environmental position", "environmental_position",
                      "substrate", "substratum", "attachment")
  )

  out <- data.frame(
    canonical_name = unique_species,
    body_size_cm   = NA_real_,
    growth_form    = NA_character_,
    calcification  = NA_character_,
    life_span      = NA_character_,
    tidal_zone     = NA_character_,
    wave_exposure  = NA_character_,
    environment    = NA_character_,
    substrate      = NA_character_,
    stringsAsFactors = FALSE
  )

  sp_idx <- stats::setNames(seq_along(unique_species), unique_species)

  for (trait_out in names(trait_map)) {
    patterns <- trait_map[[trait_out]]
    mask <- traits %in% patterns
    if (!any(mask)) next

    sub_sp  <- species[mask]
    sub_val <- values[mask]

    for (j in seq_along(sub_sp)) {
      s <- sub_sp[j]
      v <- sub_val[j]
      if (is.na(s) || is.na(v) || nchar(v) == 0L) next
      row <- sp_idx[s]
      if (is.na(row)) next

      if (trait_out == "body_size_cm") {
        if (is.na(out$body_size_cm[row])) {
          num <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", v)))
          out$body_size_cm[row] <- num
        }
      } else {
        if (is.na(out[[trait_out]][row])) {
          out[[trait_out]][row] <- v
        }
      }
    }
  }

  out
}


#' Extract trait columns from wide-format AlgaeTraits data
#' @noRd
.extract_algae_wide <- function(df, name_col) {
  find_col <- function(patterns) {
    for (p in patterns) {
      m <- grep(p, names(df), value = TRUE, ignore.case = TRUE)
      if (length(m) > 0L) return(m[1L])
    }
    NA_character_
  }

  size_col  <- find_col(c("body.?size", "thallus.?length", "max.?length",
                          "^size$"))
  form_col  <- find_col(c("body.?shape", "growth.?form", "morpholog"))
  calc_col  <- find_col(c("calcif"))
  span_col  <- find_col(c("life.?span", "longevity"))
  tide_col  <- find_col(c("tidal", "zonation"))
  wave_col  <- find_col(c("wave", "exposure"))
  env_col   <- find_col(c("^environment$", "^habitat$", "salinity"))
  sub_col   <- find_col(c("environmental.?position", "substrate",
                          "substratum", "attachment"))

  safe_num <- function(col) {
    if (is.na(col)) return(rep(NA_real_, nrow(df)))
    suppressWarnings(as.numeric(df[[col]]))
  }

  safe_chr <- function(col) {
    if (is.na(col)) return(rep(NA_character_, nrow(df)))
    x <- trimws(as.character(df[[col]]))
    x[!nzchar(x)] <- NA_character_
    x
  }

  data.frame(
    canonical_name = trimws(df[[name_col]]),
    body_size_cm   = safe_num(size_col),
    growth_form    = safe_chr(form_col),
    calcification  = safe_chr(calc_col),
    life_span      = safe_chr(span_col),
    tidal_zone     = safe_chr(tide_col),
    wave_exposure  = safe_chr(wave_col),
    environment    = safe_chr(env_col),
    substrate      = safe_chr(sub_col),
    stringsAsFactors = FALSE
  )
}


#' Parse Meiri (2018) lizard traits (XLSX from Figshare)
#' @param path Character. Path to the ReptTraits/Meiri XLSX (or CSV/TSV).
#' @return data.frame with canonical_name + lizard trait columns.
#' @export
parse_lizard_traits <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") {
    df <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else if (ext %in% c("tsv", "txt")) {
    df <- utils::read.delim(path, stringsAsFactors = FALSE,
                            check.names = FALSE)
  } else {
    if (!requireNamespace("openxlsx2", quietly = TRUE)) {
      stop("Package 'openxlsx2' is required to parse lizard traits XLSX.",
           call. = FALSE)
    }
    sheets <- openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(path))
    pick <- sheets[tolower(sheets) %in% c("data", "data sheet", "trait data")]
    if (length(pick) == 0L) {
      ncols <- vapply(sheets, function(s) {
        h <- tryCatch(openxlsx2::read_xlsx(path, sheet = s, rows = 1:1),
                      error = function(e) NULL)
        if (is.null(h)) 0L else ncol(h)
      }, integer(1L))
      pick <- sheets[which.max(ncols)]
    }
    df <- openxlsx2::read_xlsx(path, sheet = pick[1L])
  }

  find_col <- function(patterns) {
    for (p in patterns) {
      m <- grep(p, names(df), ignore.case = TRUE, value = TRUE)
      if (length(m) > 0L) return(m[1L])
    }
    NULL
  }

  name_col <- find_col(c("^species$", "^binomial$", "^scientific.?name$"))
  if (is.null(name_col)) name_col <- names(df)[1L]

  safe_num <- function(col_name) {
    if (is.null(col_name)) return(rep(NA_real_, nrow(df)))
    suppressWarnings(as.numeric(df[[col_name]]))
  }

  safe_chr <- function(col_name) {
    if (is.null(col_name)) return(rep(NA_character_, nrow(df)))
    x <- as.character(df[[col_name]])
    x[x == "" | x == "NA"] <- NA_character_
    trimws(x)
  }

  out <- data.frame(
    canonical_name    = trimws(gsub("_", " ", df[[name_col]])),
    body_mass_g       = safe_num(find_col(c("mass", "body.?mass", "weight"))),
    svl_mm            = safe_num(find_col(c("SVL", "snout.?vent", "SVL_mm"))),
    tail_length_mm    = safe_num(find_col(c("tail", "tail.?length"))),
    clutch_size       = safe_num(find_col(c("clutch.?size", "litter.?size",
                                            "litter.?clutch"))),
    clutch_frequency  = safe_num(find_col(c("clutch.?freq", "clutches.?per",
                                            "reproductive.?freq"))),
    longevity_yr      = safe_num(find_col(c("longevity", "max.?age",
                                            "maximum.?longevity"))),
    diet              = safe_chr(find_col(c("^diet$", "diet.?type",
                                            "trophic"))),
    habitat           = safe_chr(find_col(c("^habitat$", "habitat.?type",
                                            "microhabitat"))),
    activity_time     = safe_chr(find_col(c("activity", "activity.?time",
                                            "diel"))),
    foraging_mode     = safe_chr(find_col(c("foraging", "foraging.?mode",
                                            "forage"))),
    stringsAsFactors = FALSE
  )

  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]
  out[!duplicated(out$canonical_name), ]
}


#' Parse LepTraits 1.0 butterfly consensus CSV
#' @param path Character. Path to consensus.csv.
#' @return data.frame with canonical_name + butterfly trait columns.
#' @export
parse_leptraits <- function(path) {
  df <- utils::read.csv(path, stringsAsFactors = FALSE)

  name_col <- intersect(names(df), c("Species", "species", "Scientific"))
  if (length(name_col) == 0L) name_col <- names(df)[1L] else name_col <- name_col[1L]

  safe_num <- function(col_name) {
    if (is.null(col_name) || !col_name %in% names(df)) {
      return(rep(NA_real_, nrow(df)))
    }
    suppressWarnings(as.numeric(df[[col_name]]))
  }

  safe_chr <- function(col_name) {
    if (is.null(col_name) || !col_name %in% names(df)) {
      return(rep(NA_character_, nrow(df)))
    }
    x <- as.character(df[[col_name]])
    x[x == "" | x == "NA"] <- NA_character_
    trimws(x)
  }

  ws_l <- safe_num("WS_L")
  ws_u <- safe_num("WS_U")
  wingspan_mm <- ifelse(!is.na(ws_l) & !is.na(ws_u), (ws_l + ws_u) / 2,
                 ifelse(!is.na(ws_l), ws_l,
                 ifelse(!is.na(ws_u), ws_u, NA_real_)))

  month_cols <- intersect(
    names(df),
    c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  )
  if (length(month_cols) > 0L) {
    month_mat <- as.matrix(df[, month_cols, drop = FALSE])
    month_mat <- suppressWarnings(apply(month_mat, 2L, as.numeric))
    flight_months <- as.integer(rowSums(month_mat, na.rm = TRUE))
    flight_months[rowSums(!is.na(month_mat)) == 0L] <- NA_integer_
  } else {
    flight_months <- safe_num("FlightDuration")
  }

  out <- data.frame(
    canonical_name       = trimws(df[[name_col]]),
    wingspan_mm          = wingspan_mm,
    voltinism            = safe_num("Voltinism"),
    diapause_stage       = safe_chr("DiapauseStage"),
    canopy_affinity      = safe_chr("CanopyAffinity"),
    edge_affinity        = safe_chr("EdgeAffinity"),
    moisture_affinity    = safe_chr("MoistureAffinity"),
    disturbance_affinity = safe_chr("DisturbanceAffinity"),
    n_hostplant_families = suppressWarnings(as.integer(df[["NumberOfHostplantFamilies"]])),
    flight_months        = flight_months,
    stringsAsFactors = FALSE
  )

  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]
  trait_cols <- setdiff(names(out), "canonical_name")
  has_data <- rowSums(!is.na(out[, trait_cols, drop = FALSE])) > 0L
  out <- out[has_data, ]
  out[!duplicated(out$canonical_name), ]
}


#' Parse AnimalTraits observations CSV (aggregate to species medians)
#' @param path Character. Path to observations.csv.
#' @return data.frame with canonical_name + body_mass_kg + metabolic_rate_w.
#' @export
parse_animaltraits <- function(path) {
  df <- utils::read.csv(path, stringsAsFactors = FALSE,
                        fileEncoding = "UTF-8")

  name_col <- intersect(names(df),
                        c("species", "Species", "scientificName"))
  if (length(name_col) == 0L) name_col <- names(df)[1L] else name_col <- name_col[1L]

  bm_col <- intersect(names(df), c("body.mass", "body mass", "Body.mass"))
  if (length(bm_col) == 0L) {
    bm_col <- grep("^body[._]?mass$", names(df), ignore.case = TRUE,
                   value = TRUE)
  }
  mr_col <- intersect(names(df), c("metabolic.rate", "metabolic rate",
                                   "Metabolic.rate"))
  if (length(mr_col) == 0L) {
    mr_col <- grep("^metabolic[._]?rate$", names(df), ignore.case = TRUE,
                   value = TRUE)
  }

  canonical <- trimws(df[[name_col]])

  bm <- if (length(bm_col) > 0L) {
    suppressWarnings(as.numeric(df[[bm_col[1L]]]))
  } else {
    rep(NA_real_, nrow(df))
  }

  mr <- if (length(mr_col) > 0L) {
    suppressWarnings(as.numeric(df[[mr_col[1L]]]))
  } else {
    rep(NA_real_, nrow(df))
  }

  obs <- data.frame(
    canonical_name = canonical,
    body_mass_kg   = bm,
    metabolic_rate_w = mr,
    stringsAsFactors = FALSE
  )
  obs <- obs[!is.na(obs$canonical_name) & nchar(obs$canonical_name) > 0L, ]

  species <- unique(obs$canonical_name)
  out <- data.frame(
    canonical_name   = species,
    body_mass_kg     = NA_real_,
    metabolic_rate_w = NA_real_,
    stringsAsFactors = FALSE
  )

  idx <- match(obs$canonical_name, species)
  bm_split <- split(obs$body_mass_kg, idx)
  mr_split <- split(obs$metabolic_rate_w, idx)

  out$body_mass_kg <- vapply(bm_split, function(x) {
    x <- x[!is.na(x) & x > 0]
    if (length(x) == 0L) NA_real_ else stats::median(x)
  }, numeric(1L))

  out$metabolic_rate_w <- vapply(mr_split, function(x) {
    x <- x[!is.na(x) & x > 0]
    if (length(x) == 0L) NA_real_ else stats::median(x)
  }, numeric(1L))

  has_data <- !is.na(out$body_mass_kg) | !is.na(out$metabolic_rate_w)
  out[has_data, ]
}


#' Parse NW European Arthropod DwC-A (taxon + measurement + description)
#' @param dir_path Character. Directory holding the DwC archive.
#' @return data.frame with canonical_name + arthropod trait columns.
#' @export
parse_arthropod_traits <- function(dir_path) {
  find_file <- function(patterns) {
    for (p in patterns) {
      f <- list.files(dir_path, pattern = p, full.names = TRUE,
                      recursive = TRUE, ignore.case = TRUE)
      if (length(f) > 0L) return(f[1L])
    }
    NULL
  }

  taxon_file <- find_file(c("taxon\\.txt$", "taxon\\.csv$"))
  mof_file <- find_file(c("measurementorfact", "measurement"))
  desc_file <- find_file(c("description\\.txt$", "description\\.csv$"))

  if (is.null(taxon_file)) {
    stop("Cannot find taxon file in DwC archive.\nContents: ",
         paste(list.files(dir_path, recursive = TRUE), collapse = ", "),
         call. = FALSE)
  }

  taxon <- utils::read.delim(taxon_file, stringsAsFactors = FALSE, quote = "")

  name_col <- intersect(names(taxon), c("scientificName", "canonicalName",
                                        "species"))
  if (length(name_col) == 0L) name_col <- names(taxon)[2L] else name_col <- name_col[1L]
  raw_names <- taxon[[name_col]]

  canonical <- vapply(raw_names, function(n) {
    parts <- strsplit(trimws(n), "\\s+")[[1L]]
    if (length(parts) >= 2L) paste(parts[1L], parts[2L]) else trimws(n)
  }, character(1L), USE.NAMES = FALSE)

  id_col <- intersect(names(taxon), c("id", "taxonID", "ID"))
  if (length(id_col) == 0L) id_col <- names(taxon)[1L] else id_col <- id_col[1L]

  out <- data.frame(
    canonical_name = canonical,
    taxon_id       = taxon[[id_col]],
    stringsAsFactors = FALSE
  )

  if (!is.null(mof_file)) {
    mof <- utils::read.delim(mof_file, stringsAsFactors = FALSE, quote = "")
    mof_id <- intersect(names(mof), c("id", "taxonID", "coreid"))
    if (length(mof_id) == 0L) mof_id <- names(mof)[1L] else mof_id <- mof_id[1L]

    type_col <- intersect(names(mof), c("measurementType", "type"))
    if (length(type_col) == 0L) type_col <- names(mof)[2L] else type_col <- type_col[1L]
    val_col <- intersect(names(mof), c("measurementValue", "value"))
    if (length(val_col) == 0L) val_col <- names(mof)[3L] else val_col <- val_col[1L]

    quant_map <- c(
      "Body_size"        = "body_size_mm",
      "Dispersal_ability" = "dispersal",
      "Voltinism_mean"   = "voltinism",
      "Fecundity"        = "fecundity",
      "Development_time" = "development_d",
      "Lifespan"         = "lifespan_d",
      "Thermal_mean"     = "thermal_mean"
    )

    for (mtype in names(quant_map)) {
      out_col <- quant_map[[mtype]]
      rows <- mof[[type_col]] == mtype
      if (!any(rows)) {
        out[[out_col]] <- NA_real_
        next
      }
      sub <- mof[rows, c(mof_id, val_col), drop = FALSE]
      names(sub) <- c("taxon_id", "val")
      sub$val <- suppressWarnings(as.numeric(sub$val))
      sub <- sub[!is.na(sub$val), ]
      sub <- sub[!duplicated(sub$taxon_id), ]
      idx <- match(out$taxon_id, sub$taxon_id)
      out[[out_col]] <- sub$val[idx]
    }
  }

  if (!is.null(desc_file)) {
    desc <- utils::read.delim(desc_file, stringsAsFactors = FALSE, quote = "")
    desc_id <- intersect(names(desc), c("id", "taxonID", "coreid"))
    if (length(desc_id) == 0L) desc_id <- names(desc)[1L] else desc_id <- desc_id[1L]

    desc_col <- intersect(names(desc), c("description", "value"))
    if (length(desc_col) == 0L) desc_col <- names(desc)[2L] else desc_col <- desc_col[1L]
    type_col2 <- intersect(names(desc), c("type", "measurementType"))
    if (length(type_col2) == 0L) type_col2 <- names(desc)[3L] else type_col2 <- type_col2[1L]

    cat_map <- c(
      "Diurnality"          = "diurnality",
      "Feeding_guild_adult" = "feeding_guild",
      "Trophic_range_adult" = "trophic_range"
    )

    for (dtype in names(cat_map)) {
      out_col <- cat_map[[dtype]]
      rows <- desc[[type_col2]] == dtype
      if (!any(rows)) {
        out[[out_col]] <- NA_character_
        next
      }
      sub <- desc[rows, c(desc_id, desc_col), drop = FALSE]
      names(sub) <- c("taxon_id", "val")
      sub$val <- trimws(sub$val)
      sub$val[sub$val == "" | sub$val == "NA"] <- NA_character_
      sub <- sub[!duplicated(sub$taxon_id), ]
      idx <- match(out$taxon_id, sub$taxon_id)
      out[[out_col]] <- sub$val[idx]
    }
  }

  out$taxon_id <- NULL

  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]
  trait_cols <- setdiff(names(out), "canonical_name")
  has_data <- rowSums(!is.na(out[, trait_cols, drop = FALSE])) > 0L
  out <- out[has_data, ]
  out[!duplicated(out$canonical_name), ]
}


#' Parse AnAge longevity and life-history traits (TSV from ZIP)
#' @param path Character. Path to anage_*.txt.
#' @return data.frame with canonical_name + AnAge trait columns.
#' @export
parse_anage <- function(path) {
  df <- utils::read.delim(path, stringsAsFactors = FALSE, quote = "")

  genus_col <- intersect(names(df), c("Genus", "genus"))
  sp_col <- intersect(names(df), c("Species", "species"))

  if (length(genus_col) > 0L && length(sp_col) > 0L) {
    canonical <- trimws(paste(df[[genus_col[1L]]], df[[sp_col[1L]]]))
  } else {
    name_col <- intersect(
      names(df),
      c("Scientific_name", "ScientificName", "scientific_name",
        "Common_name", "Binomial")
    )
    if (length(name_col) == 0L) name_col <- names(df)[1L] else name_col <- name_col[1L]
    canonical <- trimws(df[[name_col]])
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
    x <- suppressWarnings(as.numeric(df[[col_name]]))
    x[x < 0] <- NA_real_
    x
  }

  longevity_col <- find_col(c(
    "Maximum.longevity..yrs.", "Maximum_longevity_yrs",
    "Maximum.longevity", "MaxLongevity", "max_longevity"
  ))

  out <- data.frame(
    canonical_name         = canonical,
    max_longevity_yr       = safe_num(longevity_col),
    body_mass_g            = safe_num(find_col(c(
      "Body.mass..g.", "Body_mass_g", "Adult.weight..g.",
      "AdultWeight", "body_mass"
    ))),
    metabolic_rate_w       = safe_num(find_col(c(
      "Metabolic.rate..W.", "Metabolic_rate_W", "MetabolicRate",
      "metabolic_rate"
    ))),
    female_maturity_d      = safe_num(find_col(c(
      "Female.maturity..days.", "Female_maturity_days",
      "FemaleMaturity", "female_maturity"
    ))),
    male_maturity_d        = safe_num(find_col(c(
      "Male.maturity..days.", "Male_maturity_days",
      "MaleMaturity", "male_maturity"
    ))),
    gestation_incubation_d = safe_num(find_col(c(
      "Gestation.Incubation..days.", "Gestation_Incubation_days",
      "GestationIncubation", "gestation_incubation"
    ))),
    litter_size            = safe_num(find_col(c(
      "Litter.Clutch.size", "Litter_Clutch_size",
      "LitterClutchSize", "litter_clutch_size"
    ))),
    birth_mass_g           = safe_num(find_col(c(
      "Birth.weight..g.", "Birth_weight_g",
      "BirthWeight", "birth_weight"
    ))),
    growth_rate            = safe_num(find_col(c(
      "Growth.rate..1.days.", "Growth_rate",
      "GrowthRate", "growth_rate"
    ))),
    temperature_k          = safe_num(find_col(c(
      "Temperature..K.", "Temperature_K",
      "Temperature", "temperature"
    ))),
    stringsAsFactors = FALSE
  )

  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]
  trait_cols <- setdiff(names(out), "canonical_name")
  has_data <- rowSums(!is.na(out[, trait_cols, drop = FALSE])) > 0L
  out <- out[has_data, ]
  out[!duplicated(out$canonical_name), ]
}


#' Parse GloNAF taxon-region data (multiple CSVs/XLSXs from ZIP)
#' @param dir_path Character. Directory containing the GloNAF files.
#' @return data.frame with canonical_name + region_id + naturalized.
#' @export
parse_glonaf <- function(dir_path) {
  find_file <- function(patterns) {
    for (p in patterns) {
      f <- list.files(dir_path, pattern = p, full.names = TRUE,
                      recursive = TRUE, ignore.case = TRUE)
      if (length(f) > 0L) return(f[1L])
    }
    NULL
  }

  read_table <- function(path) {
    if (is.null(path)) return(NULL)
    ext <- tolower(tools::file_ext(path))
    if (ext == "csv") {
      utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      if (!requireNamespace("openxlsx2", quietly = TRUE)) {
        stop("openxlsx2 is required to read GloNAF XLSX files. ",
             "Install with: install.packages('openxlsx2')", call. = FALSE)
      }
      as.data.frame(openxlsx2::read_xlsx(path), stringsAsFactors = FALSE)
    } else {
      utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE)
    }
  }

  flora_file <- find_file(c(
    "glonaf_flora.*\\.xlsx", "glonaf_flora.*\\.csv",
    "flora2?\\.xlsx",        "flora2?\\.csv"
  ))
  if (is.null(flora_file)) {
    flora_file <- find_file(c("glonaf_TxR.*\\.xlsx", "glonaf_TxR.*\\.csv",
                              "TxR\\.csv"))
  }
  if (is.null(flora_file)) {
    stop("Cannot find GloNAF flora/TxR table.\nContents: ",
         paste(list.files(dir_path, recursive = TRUE), collapse = ", "),
         call. = FALSE)
  }

  taxon_file <- find_file(c(
    "glonaf_taxon.*\\.xlsx", "glonaf_taxon.*\\.csv",
    "taxon_wcvp.*\\.xlsx",   "taxon_wcvp.*\\.csv"
  ))
  if (!is.null(taxon_file) && grepl("datadictionary", taxon_file,
                                    ignore.case = TRUE)) {
    taxon_file <- find_file(c(
      "^glonaf_taxon[^_]*\\.xlsx$", "^glonaf_taxon[^_]*\\.csv$",
      "^glonaf_taxon_wcvp\\.xlsx$",  "^glonaf_taxon_wcvp\\.csv$"
    ))
  }

  region_file <- find_file(c(
    "glonaf_region.*\\.xlsx", "glonaf_region.*\\.csv",
    "region\\.csv"
  ))
  if (!is.null(region_file) && grepl("datadictionary", region_file,
                                     ignore.case = TRUE)) {
    region_file <- find_file(c(
      "^glonaf_region[^_]*\\.xlsx$", "^glonaf_region[^_]*\\.csv$"
    ))
  }

  flora <- read_table(flora_file)

  if (!is.null(taxon_file)) {
    taxon <- read_table(taxon_file)
    flora_taxon_col <- intersect(
      names(flora),
      c("taxon_wcvp_id", "taxon_id", "id", "ID")
    )
    taxon_id_col <- intersect(
      names(taxon),
      c("id", "taxon_wcvp_id", "taxon_id", "ID")
    )
    taxon_name_col <- intersect(
      names(taxon),
      c("taxa_accepted", "taxon_corrected", "species_name",
        "accepted_name", "taxon_name", "name",
        "scientificName", "canonical_name")
    )

    if (length(flora_taxon_col) > 0L && length(taxon_id_col) > 0L &&
        length(taxon_name_col) > 0L) {
      taxon_lookup <- taxon[, c(taxon_id_col[1L], taxon_name_col[1L])]
      names(taxon_lookup) <- c("join_key", "canonical_name")
      taxon_lookup <- taxon_lookup[!duplicated(taxon_lookup$join_key), ]
      flora$join_key <- flora[[flora_taxon_col[1L]]]
      flora <- merge(flora, taxon_lookup, by = "join_key", all.x = TRUE)
    }
  }

  if (!"canonical_name" %in% names(flora)) {
    name_col <- intersect(
      names(flora),
      c("species_name", "accepted_name", "taxon_name", "scientificName",
        "canonical_name", "species")
    )
    if (length(name_col) == 0L) {
      stop("Cannot resolve species names in GloNAF data.", call. = FALSE)
    }
    flora$canonical_name <- trimws(flora[[name_col[1L]]])
  }

  if (!is.null(region_file)) {
    region <- read_table(region_file)
    region_id_col <- intersect(
      names(region),
      c("region_id", "OBJIDsic", "id", "ID")
    )
    region_code_col <- intersect(
      names(region),
      c("code", "tdwg4_code", "tdwg3_code", "tdwg2_code",
        "iso_equivalent", "country_code", "region_code", "name")
    )
    flora_region_col <- intersect(
      names(flora),
      c("region_id", "OBJIDsic", "region")
    )

    if (length(region_id_col) > 0L && length(region_code_col) > 0L &&
        length(flora_region_col) > 0L) {
      region_lookup <- region[, c(region_id_col[1L], region_code_col[1L])]
      names(region_lookup) <- c("region_join", "region_code_resolved")
      region_lookup <- region_lookup[!duplicated(region_lookup$region_join), ]
      flora$region_join <- flora[[flora_region_col[1L]]]
      flora <- merge(flora, region_lookup, by = "region_join", all.x = TRUE)
      flora$region_id <- as.character(flora$region_code_resolved)
    }
  }

  if (!"region_id" %in% names(flora) || all(is.na(flora$region_id))) {
    region_col <- intersect(
      names(flora),
      c("region_id", "region", "OBJIDsic", "region_id_raw")
    )
    if (length(region_col) > 0L) {
      flora$region_id <- as.character(flora[[region_col[1L]]])
    } else {
      stop("Cannot resolve region identifiers in GloNAF data.", call. = FALSE)
    }
  }

  out <- data.frame(
    canonical_name = trimws(flora$canonical_name),
    region_id      = as.character(flora$region_id),
    naturalized    = 1L,
    stringsAsFactors = FALSE
  )

  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]
  out <- out[!is.na(out$region_id) & nchar(out$region_id) > 0L, ]
  out[!duplicated(paste(out$canonical_name, out$region_id)), ]
}


#' Fold French accented characters to ASCII
#'
#' Locale-independent accent removal (chartr, character-for-character) so
#' value matching does not depend on source encoding or the running locale.
#' @param x Character vector.
#' @return `x` with accented Latin-1 letters mapped to their ASCII base.
#' @noRd
fold_accents <- function(x) {
  # Latin-1 code points for: e-acute e-grave e-circ e-uml a-grave a-circ
  # a-uml i-uml i-circ o-circ o-uml u-grave u-circ u-uml c-cedilla
  from <- intToUtf8(c(0xE9, 0xE8, 0xEA, 0xEB, 0xE0, 0xE2, 0xE4, 0xEF,
                      0xEE, 0xF4, 0xF6, 0xF9, 0xFB, 0xFC, 0xE7))
  chartr(from, "eeeeaaaiioouuuc", x)
}


#' Parse Baseflor (Catminat / Julve) French flora trait spreadsheet
#'
#' Reads the `baseflor` sheet of Julve's `baseflor.xlsx`, builds a clean
#' binomial `canonical_name` from the `nomH`/`nomB`/`nomA` columns, splits the
#' `floraison` flowering-period field into begin/end months, and recodes the
#' French categorical traits (pollination vector, dispersal mode, breeding
#' system, flower colour, fruit type, woody growth form) to English. Also keeps
#' the two Ellenberg-style axes absent from EIVE: continentality and salinity.
#'
#' Indicator values L/T/M/R/N are intentionally not emitted here (the
#' European-calibration EIVE enrichment already covers them); Raunkiaer life
#' form is left to the `leda` enrichment for the same flora; maximum vegetative
#' height is omitted because the source column is empty.
#'
#' @param path Character. Path to the downloaded `baseflor.xlsx`.
#' @return data.frame with `canonical_name` + 10 trait columns.
#' @export
parse_baseflor <- function(path) {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop("Package 'openxlsx2' is required to parse Baseflor XLSX.",
         call. = FALSE)
  }
  wb <- openxlsx2::wb_load(path)
  sheets <- openxlsx2::wb_get_sheet_names(wb)
  pick <- sheets[tolower(sheets) == "baseflor"]
  if (length(pick) == 0L) pick <- sheets[1L]
  df <- openxlsx2::wb_to_df(wb, sheet = pick[1L], col_names = TRUE)

  # Resolve columns by accent-folded, lower-cased name so the source stays
  # ASCII and matching is independent of locale/encoding.
  cn <- names(df)
  cn_key <- fold_accents(tolower(cn))
  gc <- function(key) {
    i <- which(cn_key == key)
    if (length(i) == 0L) return(rep(NA, nrow(df)))
    df[[cn[i[1L]]]]
  }

  # --- canonical name: clean binomial, coalescing nomH -> nomB -> nomA ------
  clean_nm <- function(x) {
    x <- as.character(x)
    x <- gsub("&amp", "&", x, fixed = TRUE)
    x <- gsub(";", "", x, fixed = TRUE)
    x <- gsub("\\s+\\*$", "", x)
    x <- gsub("\\s+[HBA]$", "", x)
    x <- trimws(x)
    x[x == ""] <- NA_character_
    x
  }
  nm <- clean_nm(gc("nomh"))
  nb <- clean_nm(gc("nomb"))
  na_ <- clean_nm(gc("noma"))
  empty <- is.na(nm); nm[empty] <- nb[empty]
  empty <- is.na(nm); nm[empty] <- na_[empty]

  # --- flowering months from "floraison" ("M" or "M-M", may wrap e.g. 10-6) -
  fl <- as.character(gc("floraison"))
  beg <- suppressWarnings(as.integer(sub("^([0-9]{1,2}).*$", "\\1", fl)))
  end <- suppressWarnings(as.integer(sub("^[0-9]{1,2}-([0-9]{1,2})$", "\\1", fl)))
  no_dash <- !is.na(fl) & !grepl("-", fl)
  end[no_dash] <- beg[no_dash]
  beg[is.na(beg) | beg < 1L | beg > 12L] <- NA_integer_
  end[is.na(end) | end < 1L | end > 12L] <- NA_integer_

  # --- categorical recodes (folded, lower-cased French token -> English) ----
  recode_multi <- function(x, map) {
    vapply(as.character(x), function(v) {
      if (is.na(v) || !nzchar(v)) return(NA_character_)
      toks <- fold_accents(tolower(trimws(strsplit(v, ",")[[1L]])))
      mapped <- unname(map[toks])
      mapped[is.na(mapped)] <- toks[is.na(mapped)]
      paste(unique(mapped), collapse = ", ")
    }, character(1L), USE.NAMES = FALSE)
  }
  recode_single <- function(x, map) {
    v <- fold_accents(tolower(trimws(as.character(x))))
    unname(map[v])
  }

  poll_map <- c(anemogame = "wind", autogame = "self", entomogame = "insect",
                hydrogame = "water", apogame = "apogamy")
  disp_map <- c(barochore = "barochory", anemochore = "anemochory",
                epizoochore = "epizoochory", endozoochore = "endozoochory",
                myrmecochore = "myrmecochory", hydrochore = "hydrochory",
                autochore = "autochory", dyszoochore = "dyszoochory")
  breed_map <- c(hermaphrodite = "hermaphroditic", monoique = "monoecious",
                 dioique = "dioecious", gynodioique = "gynodioecious",
                 androdioique = "androdioecious", polygame = "polygamous",
                 gynomonoique = "gynomonoecious")
  colour_map <- c(jaune = "yellow", blanc = "white", rose = "pink",
                  vert = "green", bleu = "blue", marron = "brown",
                  noir = "black")
  fruit_map <- c(akene = "achene", capsule = "capsule", caryopse = "caryopsis",
                 drupe = "drupe", gousse = "legume", silique = "silique",
                 baie = "berry", follicule = "follicle", cone = "cone",
                 samare = "samara", pyxide = "pyxid")
  growth_map <- c("sous-arbrisseau" = "subshrub", arbrisseau = "shrub",
                  arbuste = "bush", "petit arbre" = "small tree",
                  "grand arbre" = "large tree", arbre = "tree",
                  liane = "liana", parasite = "parasite")

  safe_int <- function(key, lo, hi) {
    v <- suppressWarnings(as.integer(gc(key)))
    v[is.na(v) | v < lo | v > hi] <- NA_integer_
    v
  }

  out <- data.frame(
    canonical_name     = nm,
    flower_begin_month = beg,
    flower_end_month   = end,
    pollination_vector = recode_multi(gc("pollinisation"), poll_map),
    dispersal_mode     = recode_multi(gc("dissemination"), disp_map),
    breeding_system    = recode_multi(gc("sexualite"), breed_map),
    flower_colour      = recode_multi(gc("couleur_fleur"), colour_map),
    fruit_type         = recode_single(gc("fruit"), fruit_map),
    woody_growth_form  = recode_single(gc("type_ligneux"), growth_map),
    continentality     = safe_int("continentalite", 1L, 9L),
    salinity           = safe_int("salinite", 0L, 9L),
    stringsAsFactors   = FALSE
  )

  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]
  trait_cols <- setdiff(names(out), "canonical_name")
  has_data <- rowSums(!is.na(out[, trait_cols, drop = FALSE])) > 0L
  out <- out[has_data, ]
  out[!duplicated(out$canonical_name), ]
}
