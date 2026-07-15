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

  # The workbook follows the SAS missing convention: "." marks an absent text
  # value (it fills 92% of OTHERNAMES, and stands in for the epithet of the
  # family- and genus-rank entries for undescribed fishes).
  chr <- function(c) {
    x <- trimws(as.character(df[[c]]))
    x[x == "" | x == "NA" | x == "."] <- NA_character_
    x
  }
  # FishTraits codes missing continuous/count values as -999 or -555; genuine
  # negatives occur only in MINTEMP (January minima down to -22.5 degrees C).
  num <- function(c) {
    v <- suppressWarnings(as.numeric(df[[c]])); v[v %in% c(-999, -555)] <- NA_real_; v
  }
  bin <- function(c) {
    v <- suppressWarnings(as.numeric(df[[c]])); v[!v %in% c(0, 1)] <- NA_real_; v
  }

  # A third code, -1, marks "no mapped native range in the conterminous US". It
  # spans the whole range-derived block (area, perimeter, patches, latitudinal
  # and longitudinal range, and the two PRISM temperatures read at the range
  # centroid) plus the conservation block, on the same introduced species. It
  # cannot be stripped per column: -1 is also a genuine January minimum, which
  # MINTEMP records on a continuous 0.1-degree grid running through it. Rows
  # carrying a mapped range are therefore left untouched.
  no_range <- !is.na(num("AREAKM2")) & num("AREAKM2") == -1
  rng <- function(c) { v <- num(c); v[no_range] <- NA_real_; v }

  # Balon reproductive-guild indicators (A_*, B_*, C1_*) collapse to one code:
  # the flagged guild, in column order (each species carries a single guild).
  guild_cols <- grep("^[ABC][_0-9]", names(df), value = TRUE)
  repro <- rep(NA_character_, nrow(df))
  for (cc in rev(guild_cols)) repro[bin(cc) %in% 1] <- cc

  # Entries for undescribed fishes carry a family or genus name with no epithet,
  # so they have no species-rank key to join on and .trait_finalize() drops them.
  gen <- chr("GENUS")
  epi <- chr("SPECIES")
  binom <- ifelse(is.na(gen) | is.na(epi), NA_character_,
                  trimws(paste(gen, epi)))

  out <- data.frame(
    canonical_name           = binom,
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
    min_temp_c               = rng("MINTEMP"),
    max_temp_c               = rng("MAXTEMP"),
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


#' Parse the Kew Plant DNA C-values database (genome size)
#'
#' Genome-size estimates for plants, scraped from the paginated Kew C-values
#' search interface as one HTML page per 100 records (the source exposes no bulk
#' export). The data table on each page (Family, Genus, Species, chromosome
#' number, ploidy, 1C DNA amount) is read; the Mean/Min/Max summary-statistic
#' tables are ignored. Records are keyed on the binomial and reduced to one row
#' per taxon: genome size as the 1C DNA amount in picograms, plus chromosome
#' number (2n) and ploidy level where a clean numeric value is given. The 1C DNA
#' amount is reduced by median; the chromosome number and ploidy level are
#' discrete counts whose repeated records are cytotype variants of one species,
#' so they are reduced to the base cytotype by minimum, with `_min` / `_max`
#' carrying the range.
#'
#' Each record names the paper the value was measured in and the method used, so
#' both are kept: `original_reference` and `estimation_method` list every
#' distinct value a species' records carry, joined by `"; "`. The method matters
#' for interpreting the 1C amount, since Feulgen densitometry and flow cytometry
#' are different measurements.
#'
#' @param path Directory of downloaded C-values HTML pages (or a single page).
#' @return data.frame with canonical_name + genome-size traits.
#' @export
parse_kew_cvalues <- function(path) {
  files <- if (dir.exists(path)) {
    list.files(path, pattern = "\\.html?$", full.names = TRUE)
  } else path
  if (!length(files)) stop("Kew C-values: no HTML pages found.", call. = FALSE)

  # The results page carries three tables; the data table is the one with the
  # Genus/Species columns (the other two are Mean/Min/Max summary statistics).
  data_table <- function(f) {
    tbls <- tryCatch(rvest::html_table(rvest::read_html(f)),
                     error = function(e) list())
    for (t in tbls) {
      if (all(c("Genus", "Species") %in% names(t)) && ncol(t) >= 6L) {
        return(as.data.frame(t, check.names = FALSE, stringsAsFactors = FALSE))
      }
    }
    NULL
  }
  parts <- lapply(files, data_table)
  parts <- parts[!vapply(parts, is.null, logical(1L))]
  if (!length(parts)) stop("Kew C-values: no data table on any page.",
                           call. = FALSE)
  d <- do.call(rbind, parts)

  dna_col <- grep("DNA Amount", names(d), value = TRUE)[1L]
  chr_col <- grep("Chromosome", names(d), value = TRUE)[1L]
  plo_col <- grep("Ploidy",     names(d), value = TRUE)[1L]
  ref_col <- grep("Reference",  names(d), value = TRUE)[1L]
  est_col <- grep("Estimation", names(d), value = TRUE)[1L]

  # Drop identification qualifiers so the binomial matches a backbone name.
  sp <- sub("^(cf\\.|aff\\.)\\s*", "", trimws(as.character(d$Species)))
  name <- trimws(paste(trimws(as.character(d$Genus)), sp))

  mk <- function(trait, col) {
    if (is.na(col)) return(NULL)
    data.frame(name = name, trait = trait,
               value = as.character(d[[col]]), stringsAsFactors = FALSE)
  }
  long <- do.call(rbind, list(
    mk("genome_size_1c_pg",  dna_col),
    mk("chromosome_2n",      chr_col),
    mk("ploidy_x",           plo_col),
    mk("original_reference", ref_col),
    mk("estimation_method",  est_col)
  ))
  long$value <- trimws(long$value)
  long <- long[nzchar(trimws(long$name)) & !is.na(long$value) &
                 !long$value %in% c("", "-"), , drop = FALSE]

  # The search returns one prime estimate per C-values species entry, so a
  # binomial collects several records only where entries that differ by
  # accession or cytotype (the source's Subspecies field: "(Octoploid)",
  # "line TPG 17-79") share it. Those records are cytotype variants of one
  # species, which the chromosome number and ploidy level count discretely, so a
  # median interpolates between them: a diploid 2n = 10 and a tetraploid 2n = 20
  # give 15, a count neither record reports. The minimum names the base
  # cytotype, and chromosome_2n_min/_max carry the range across the rest. The 1C
  # DNA amount is a continuous measurement, so it keeps the median.
  #
  # An odd 2n is left untouched wherever the source reports one: a triploid
  # carries an odd somatic number by definition (Tahiti lime 2n = 3x = 27), as
  # do aneuploid hybrids and the gametophytic counts of haploid-dominant
  # bryophytes. The minimum returns a reported record, so those survive.
  # The reference and estimation method are per-record provenance, so they are
  # joined rather than reduced to the commonest: where a binomial collects
  # several records the papers behind them are all part of the answer, and a
  # 1C value measured by Feulgen densitometry is not interchangeable with one
  # measured by flow cytometry.
  spec <- list(
    genome_size_1c_pg  = list(trait = "genome_size_1c_pg", type = "num"),
    chromosome_2n      = list(trait = "chromosome_2n",     type = "num",
                              reduce = "min"),
    ploidy_x           = list(trait = "ploidy_x",          type = "num",
                              reduce = "min"),
    original_reference = list(trait = "original_reference", type = "cat",
                              reduce = "join"),
    estimation_method  = list(trait = "estimation_method",  type = "cat",
                              reduce = "join")
  )
  .trait_finalize(.pivot_species_traits(long, spec, keep_all = FALSE))
}


#' Parse the EPA Freshwater Biological Traits database (macroinvertebrates)
#'
#' Ecological and life-history traits of North-American freshwater
#' macroinvertebrates, read from the transposed distribution (one row per taxon
#' and source citation). A curated set of interpretable primary trait columns is
#' melted to (taxon, trait, value) and reduced to one row per taxon by mode, so
#' the multiple per-taxon citation records collapse to the dominant state. The
#' retained traits are primary feeding mode, habit, voltinism, thermal
#' preference, maximum body-size class, body shape, rheophily, primary
#' oviposition behaviour and diapause. `Other (specify in comments)` placeholder
#' entries are dropped. The taxon grain is mixed (species and genus); the ITIS
#' TSN is present in the source. This dataset shares its lineage (USGS Data
#' Series 187, Vieira et al. 2006) with `freshwater_insects_conus`, which is at
#' genus grain; the species-grain records here are the net-new contribution.
#'
#' @param path Path to the transposed traits `.xls` (or its directory).
#' @return data.frame with canonical_name + freshwater macroinvertebrate traits.
#' @export
parse_epa_freshwater <- function(path) {
  f <- if (dir.exists(path)) {
    list.files(path, pattern = "Transposed.*\\.xls$", full.names = TRUE,
               ignore.case = TRUE)[1L]
  } else path
  # Read every column as text: the sheet mixes types and a text read avoids
  # coercion warnings and preserves the categorical labels verbatim.
  d <- as.data.frame(
    readxl::read_excel(f, sheet = "DataTable", col_types = "text",
                       .name_repair = "minimal"),
    check.names = FALSE, stringsAsFactors = FALSE)
  if (!"Taxon" %in% names(d)) {
    stop("EPA Freshwater: missing 'Taxon' column.", call. = FALSE)
  }

  # curated trait -> source column (primary state of each functional trait)
  traits <- c(feeding_mode = "Feed_mode_prim", habit = "Habit_prim",
              voltinism = "Voltinism", thermal_preference = "Thermal_pref",
              body_size_class = "Max_body_size", body_shape = "Body_shape",
              rheophily = "Rheophily_abbrev",
              oviposition_behavior = "Ovipos_behav_prim", diapause = "Diapause")
  traits <- traits[traits %in% names(d)]

  # Upper-case only the first letter so case variants (predator/Predator)
  # collapse to one state without altering interior casing (units, hyphens).
  norm <- function(x) {
    x <- trimws(as.character(x))
    x[x == "" | x == "NA" | x == "Unknown" |
        grepl("^Other \\(specify", x)] <- NA_character_
    sub("^(\\w)", "\\U\\1", x, perl = TRUE)
  }

  taxon <- trimws(as.character(d$Taxon))
  long <- do.call(rbind, lapply(names(traits), function(tr) {
    data.frame(name = taxon, trait = tr, value = norm(d[[traits[[tr]]]]),
               stringsAsFactors = FALSE)
  }))
  long <- long[!is.na(long$value), , drop = FALSE]

  spec <- lapply(names(traits), function(tr) list(trait = tr, type = "cat"))
  names(spec) <- names(traits)
  .trait_finalize(.pivot_species_traits(long, spec, keep_all = FALSE))
}
