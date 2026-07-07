# Wave-B trait parsers: marine copepod traits (Brun et al.), US freshwater fish
# traits (FishTraits v14), and North-West European benthos biological traits
# (Cefas Btrait). Each melts its source to a long (name, trait, value) table and
# reuses .pivot_species_traits() for the median/mode reduction, so the
# aggregation lives in one place.


#' Parse the Brun et al. marine copepod trait database
#'
#' Species-level functional traits of marine copepods, distributed as a
#' multi-sheet PANGAEA workbook (one sheet per trait family). Each trait sheet
#' is melted to (taxon, trait, value); the binary feeding-mode and
#' spawning-strategy flags are first collapsed to a single categorical label per
#' record. Records are then reduced to one row per taxon (numeric traits by
#' median, categorical by mode). Subgenus qualifiers in parentheses
#' (`Acartia (Acartiura) clausi`) are stripped so the name matches a backbone
#' binomial or genus. Retained traits: body length (mm), egg outer diameter
#' (micrometres), clutch size (egg count), feeding mode, feeder type, spawning
#' strategy, presence of myelination and of resting eggs.
#'
#' @param path Path to the `Brun-etal_2016_Copepode_trait.xlsx` workbook.
#' @return data.frame with canonical_name + copepod traits.
#' @export
parse_copepod_traits <- function(path) {
  f <- if (dir.exists(path)) {
    list.files(path, pattern = "\\.xlsx$", full.names = TRUE)[1L]
  } else path
  wb <- openxlsx2::wb_load(f)
  sh <- openxlsx2::wb_get_sheet_names(wb)
  rd <- function(s) {
    if (!s %in% sh) return(NULL)
    openxlsx2::wb_to_df(wb, sheet = s, col_names = TRUE, na.strings = "")
  }
  # A binary 0/1 flag column, coerced from whatever type openxlsx2 returns.
  flag <- function(x) suppressWarnings(as.numeric(x)) %in% 1

  # First flagged label among a set of mutually-informative 0/1 columns, in
  # priority order (earlier column wins when several are set).
  first_flag <- function(d, cols) {
    cols <- cols[cols %in% names(d)]
    out <- rep(NA_character_, nrow(d))
    for (cc in rev(cols)) out[flag(d[[cc]])] <- cc
    out
  }

  parts <- list()
  add <- function(taxon, trait, value) {
    parts[[length(parts) + 1L]] <<- data.frame(
      name = taxon, trait = trait, value = as.character(value),
      stringsAsFactors = FALSE)
  }

  bs <- rd("Body size")
  if (!is.null(bs)) add(bs$Taxon, "body_length_mm", bs$Mean)

  fm <- rd("Feeding mode")
  if (!is.null(fm)) {
    add(fm$Taxon, "feeding_mode", first_flag(fm, c("Active", "Mixed", "Passive")))
    add(fm$Taxon, "feeder_type", first_flag(
      fm, c("Feeding current", "Cruise feeder", "Ambush feeder",
            "Particle feeder", "Parasitic")))
  }

  eg <- rd("Egg_size")
  if (!is.null(eg)) {
    diam <- eg[["Estimated outer diameter"]]
    diam[is.na(diam)] <- eg$Mean[is.na(diam)]
    add(eg$Taxon, "egg_diameter_um", diam)
  }

  cl <- rd("Clutch size")
  if (!is.null(cl)) add(cl$Taxon, "clutch_size", cl$Mean)

  ss <- rd("Spawning strategy")
  if (!is.null(ss)) add(ss$Taxon, "spawning_strategy",
                        first_flag(ss, c("Free spawner", "Egg carrier")))

  my <- rd("Myelination")
  if (!is.null(my)) add(my$Taxon, "myelination",
                        ifelse(flag(my$Myelination), "yes", "no"))

  re <- rd("Resting eggs")
  if (!is.null(re)) add(re$Taxon, "resting_eggs",
                        ifelse(flag(re[["Resting eggs"]]), "yes", NA))

  long <- do.call(rbind, parts)
  # Strip subgenus in parentheses and collapse whitespace to a clean binomial.
  long$name <- trimws(gsub("\\s+", " ", gsub("\\s*\\([^)]*\\)", "", long$name)))
  long <- long[!is.na(long$value) & nzchar(long$value) &
                 toupper(long$value) != "NA", , drop = FALSE]

  spec <- list(
    body_length_mm    = list(trait = "body_length_mm",  type = "num"),
    egg_diameter_um   = list(trait = "egg_diameter_um", type = "num"),
    clutch_size       = list(trait = "clutch_size",     type = "num"),
    feeding_mode      = list(trait = "feeding_mode",      type = "cat"),
    feeder_type       = list(trait = "feeder_type",       type = "cat"),
    spawning_strategy = list(trait = "spawning_strategy", type = "cat"),
    myelination       = list(trait = "myelination",       type = "cat"),
    resting_eggs      = list(trait = "resting_eggs",      type = "cat")
  )
  .trait_finalize(.pivot_species_traits(long, spec, keep_all = FALSE))
}


#' Parse the FishTraits v14 database (Frimpong & Angermeier)
#'
#' Species-level traits of United States freshwater fishes, one row per species
#' in a legacy Excel workbook. The table is already at the species grain, so it
#' is read wide and a curated, documented subset of columns is retained: the
#' binary trophic-guild indicators, life-history quantities (maximum total
#' length in cm, age at maturity and longevity in years, maximum fecundity,
#' spawning-season length), salinity and temperature tolerances (30-year mean
#' January minimum and July maximum at the range centroid, in degrees C), and
#' conservation flags. The 25 Balon reproductive-guild indicator columns are
#' collapsed to a single `repro_guild` code (the flagged guild). The ITIS TSN is
#' carried as a cross-reference. Field meanings follow the dataset's
#' AttributeDefinitions.
#'
#' @param path Path to `FishTraits_14.3.xls` (or a directory containing it).
#' @return data.frame with canonical_name + US freshwater-fish traits.
#' @export
parse_fishtraits <- function(path) {
  f <- if (dir.exists(path)) {
    list.files(path, pattern = "\\.xls$", full.names = TRUE,
               ignore.case = TRUE)[1L]
  } else path
  df <- as.data.frame(
    readxl::read_excel(f, sheet = 1L, .name_repair = "minimal"),
    stringsAsFactors = FALSE, check.names = FALSE)

  need <- c("GENUS", "SPECIES", "ITISTSN", "COMMONNAME", "MAXTL", "NATIVE")
  miss <- setdiff(need, names(df))
  if (length(miss)) {
    stop("FishTraits missing columns: ", paste(miss, collapse = ", "),
         call. = FALSE)
  }

  chr <- function(c) {
    x <- trimws(as.character(df[[c]])); x[x == "" | x == "NA"] <- NA_character_; x
  }
  # FishTraits codes missing continuous/count values as -999 or -555; genuine
  # negatives occur only in MINTEMP (January minima down to -22.5 degrees C).
  num <- function(c) {
    v <- suppressWarnings(as.numeric(df[[c]])); v[v %in% c(-999, -555)] <- NA_real_; v
  }
  bin <- function(c) {
    v <- suppressWarnings(as.numeric(df[[c]])); v[!v %in% c(0, 1)] <- NA_real_; v
  }

  # Balon reproductive-guild indicators (A_*, B_*, C1_*) collapse to one code:
  # the flagged guild, in column order (each species carries a single guild).
  guild_cols <- grep("^[ABC][_0-9]", names(df), value = TRUE)
  repro <- rep(NA_character_, nrow(df))
  for (cc in rev(guild_cols)) repro[bin(cc) %in% 1] <- cc

  out <- data.frame(
    canonical_name           = trimws(paste(chr("GENUS"), chr("SPECIES"))),
    itis_tsn                 = chr("ITISTSN"),
    common_name              = chr("COMMONNAME"),
    native                   = bin("NATIVE"),
    diet_nonfeeding          = bin("NONFEED"),
    diet_benthic             = bin("BENTHIC"),
    diet_surface_watercolumn = bin("SURWCOL"),
    diet_algae_phyto         = bin("ALGPHYTO"),
    diet_macrophyte          = bin("MACVASCU"),
    diet_detritus            = bin("DETRITUS"),
    diet_invertebrates       = bin("INVLVFSH"),
    diet_fish_crustacea      = bin("FSHCRCRB"),
    diet_blood               = bin("BLOOD"),
    diet_eggs                = bin("EGGS"),
    max_length_cm            = num("MAXTL"),
    maturity_age_yr          = num("MATUAGE"),
    longevity_yr             = num("LONGEVITY"),
    fecundity_max            = num("FECUNDITY"),
    serial_spawner           = bin("SERIAL"),
    spawning_season_months   = num("SEASON"),
    repro_guild              = repro,
    euryhaline               = bin("EURYHALINE"),
    min_temp_c               = num("MINTEMP"),
    max_temp_c               = num("MAXTEMP"),
    potamodromous_anadromous = bin("POTANADR"),
    prefers_lotic            = bin("PREFLOT"),
    prefers_lentic           = bin("PREFLEN"),
    listed                   = bin("LISTED"),
    extinct                  = chr("EXTINCT"),
    stringsAsFactors         = FALSE
  )
  .trait_finalize(out)
}


#' Parse the Cefas biological-traits database (North-West European benthos)
#'
#' Fuzzy-coded biological traits of North-West European benthic invertebrates,
#' distributed as a flat one-row-per-taxon matrix at genus rank and above. Each
#' of the ten biological traits is split across several modality columns holding
#' a 0-3 fuzzy score (0 = no evidence ... 3 = strong evidence); each trait is
#' collapsed to the single modality with the highest score. The traits are body
#' size (mm class), morphology, lifespan (year class), egg development, larval
#' development, living habit, sediment position, feeding mode, mobility and
#' bioturbation mode. The WoRMS `AphiaID` is present in the source but the
#' `Genus`-rank name is used as the resolution key.
#'
#' @param path Path to the Trait Matrix CSV (or a directory containing it).
#' @return data.frame with canonical_name + benthic biological traits.
#' @export
parse_cefas_btrait <- function(path) {
  f <- if (dir.exists(path)) {
    list.files(path, pattern = "\\.csv$", full.names = TRUE)[1L]
  } else path
  d <- utils::read.csv(f, check.names = FALSE, stringsAsFactors = FALSE)
  if (!"Genus" %in% names(d)) {
    stop("Cefas Btrait: missing 'Genus' column.", call. = FALSE)
  }

  # trait -> column-name prefix; each prefix names a set of modality columns.
  groups <- c(body_size = "sr", morphology = "m", lifespan = "l",
              egg_development = "ed", larval_development = "ld",
              living_habit = "lh", sediment_position = "sp",
              feeding_mode = "f", mobility = "mob", bioturbation = "b")

  # Modality with the strongest fuzzy score; NA when the trait is unscored.
  argmax_label <- function(cols) {
    m <- vapply(d[cols], function(x) suppressWarnings(as.numeric(x)),
                numeric(nrow(d)))
    lab <- gsub("_", " ", sub("^[a-z]+_", "", cols))
    idx <- apply(m, 1L, function(r) {
      if (all(is.na(r)) || max(r, na.rm = TRUE) <= 0) NA_integer_ else which.max(r)
    })
    ifelse(is.na(idx), NA_character_, lab[idx])
  }

  out <- data.frame(canonical_name = trimws(as.character(d$Genus)),
                    stringsAsFactors = FALSE)
  for (nm in names(groups)) {
    cols <- grep(paste0("^", groups[[nm]], "_"), names(d), value = TRUE)
    out[[nm]] <- argmax_label(cols)
  }
  .trait_finalize(out)
}
