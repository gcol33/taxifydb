# Trait parsers (wave: GlobTherm, Madin, Coral Trait DB, World Spider Trait DB,
# Amniote LHD, COMBINE, AusTraits, Global Wood Density DB v2). Each returns a
# data.frame with canonical_name + trait columns, ready for
# resolve_enrichment_names() + build_enrichment_vtr().


# ---- shared long-format helpers --------------------------------------------

#' Most frequent non-missing value of a character vector
#' @noRd
.cat_mode <- function(v) {
  v <- .to_utf8(v)
  v <- v[!is.na(v) & nzchar(trimws(v))]
  if (!length(v)) return(NA_character_)
  names(sort(table(trimws(v)), decreasing = TRUE))[1L]
}

#' Every distinct non-missing value of a character vector, joined
#'
#' The counterpart to `.cat_mode()` for a column where a species genuinely
#' carries several values and picking the commonest would hide the rest (the
#' papers behind a species' records, the methods they used). Values are sorted so
#' the result does not depend on record order.
#' @noRd
.cat_join <- function(v, sep = "; ") {
  v <- .to_utf8(v)
  v <- trimws(v[!is.na(v) & nzchar(trimws(v))])
  if (!length(v)) return(NA_character_)
  paste(sort(unique(v)), collapse = sep)
}

#' Augment a curated spec with every remaining distinct trait
#'
#' Keeps the caller's curated output columns (nice names, forced types) and adds
#' one column for every other distinct `trait` value present in `long`, named by
#' its sanitized label and typed by numeric-share inference. This is what makes
#' the long-format parsers keep all traits rather than only the curated set.
#' @noRd
.augment_spec_all <- function(long, spec) {
  curated <- if (length(spec)) vapply(spec, function(s) s$trait, character(1L))
             else character(0)
  taken   <- c("canonical_name", names(spec))
  extra   <- setdiff(unique(long$trait), curated)
  extra   <- extra[!is.na(extra) & nzchar(trimws(extra))]
  extra   <- extra[!.is_bookkeeping_col(.sanitize_col(extra))]
  for (tr in extra) {
    oc <- .uniq_colname(.sanitize_col(tr), taken)
    taken <- c(taken, oc)
    type  <- if (.mostly_numeric(long$value[long$trait == tr])) "num" else "cat"
    spec[[oc]] <- list(trait = tr, type = type)
  }
  spec
}

#' Pivot a long (name, trait, value) table to one row per species
#'
#' `spec` is a named list mapping each output column to
#' `list(trait = <source trait name>, type = "num" | "cat")`. Numeric traits are
#' aggregated by median, categorical traits by mode, across all records of a
#' species. A numeric entry may set `reduce = "min" | "max" | "mean"` to pick a
#' different headline statistic (see `.num_group_spread()`); the spread columns
#' are unaffected. A categorical entry may set `reduce = "join"` to keep every
#' distinct value instead of the commonest. With `keep_all = TRUE` (the default),
#' every distinct `trait` value not named in `spec` is also pivoted (sanitized
#' name, inferred type), so no source trait is dropped; pass `spec = list()` to
#' keep every trait with auto-generated names only.
#' @noRd
.pivot_species_traits <- function(long, spec = list(), keep_all = TRUE) {
  stopifnot(all(c("name", "trait", "value") %in% names(long)))
  # Source labels/names/values may carry invalid UTF-8; make them safe before
  # any gsub/table/sort touches them.
  long$name  <- .to_utf8(long$name)
  long$trait <- .to_utf8(long$trait)
  long$value <- .to_utf8(long$value)
  long <- long[!is.na(long$name) & nzchar(trimws(long$name)), , drop = FALSE]
  long$name <- trimws(long$name)

  if (isTRUE(keep_all)) spec <- .augment_spec_all(long, spec)

  wanted <- vapply(spec, function(s) s$trait, character(1L))
  long <- long[long$trait %in% wanted, , drop = FALSE]

  species <- sort(unique(long$name))
  res <- data.frame(canonical_name = species, stringsAsFactors = FALSE)

  for (oc in names(spec)) {
    s <- spec[[oc]]
    sub <- long[long$trait == s$trait, , drop = FALSE]
    if (nrow(sub) == 0L) {
      res[[oc]] <- if (identical(s$type, "num")) NA_real_ else NA_character_
      next
    }
    if (identical(s$type, "num")) {
      spread <- .num_group_spread(sub$value, sub$name,
                                  reduce = if (is.null(s$reduce)) "median"
                                           else s$reduce)
      res    <- .attach_num_spread(res, oc, spread, species)
    } else {
      fn  <- if (identical(s$reduce, "join")) .cat_join else .cat_mode
      agg <- tapply(sub$value, sub$name, fn)
      res[[oc]] <- as.character(agg[species])
    }
  }
  res
}


# ---- GlobTherm: thermal tolerance, cross-taxa ------------------------------

#' Parse the GlobTherm thermal-tolerance database
#'
#' One row per species, wide format with two measurement blocks (upper/heat and
#' lower/cold limit), each carrying its own metric definition. The metric
#' columns are kept because a temperature value is ambiguous without them
#' (CTmax, LT50 and upper thermoneutral zone are different quantities).
#'
#' @param path Character. Path to `GlobalTherm_upload_*.csv` (latin-1 encoded,
#'   with duplicate header names handled by column position).
#' @return data.frame with canonical_name + thermal-limit traits.
#' @export
parse_globtherm <- function(path) {
  df <- utils::read.csv(path, fileEncoding = "latin1", check.names = FALSE,
                        stringsAsFactors = FALSE)
  if (ncol(df) < 26L) {
    stop("GlobTherm file has fewer columns than expected.", call. = FALSE)
  }
  nm <- names(df)
  # Header carries duplicate names (N, error, ...), so select by position and
  # assert the key columns sit where expected.
  expect <- c(`1` = "Genus", `2` = "Species", `4` = "Tmax",
              `5` = "max_metric", `23` = "tmin", `24` = "min_metric")
  for (i in names(expect)) {
    idx <- as.integer(i)
    if (!identical(tolower(trimws(nm[idx])), tolower(expect[[i]]))) {
      stop(sprintf("GlobTherm column %d is '%s', expected '%s'.",
                   idx, nm[idx], expect[[i]]), call. = FALSE)
    }
  }

  chr <- function(j) {
    x <- trimws(as.character(df[[j]]))
    x[x == "" | x == "NA"] <- NA_character_
    x
  }
  num <- function(j) suppressWarnings(as.numeric(df[[j]]))

  cname <- trimws(paste(chr(1L), chr(2L)))
  out <- data.frame(
    canonical_name      = cname,
    thermal_max_c       = num(4L),
    thermal_max_metric  = chr(5L),
    thermal_max_error   = num(6L),
    thermal_min_c       = num(23L),
    thermal_min_metric  = chr(24L),
    thermal_min_error   = num(25L),
    taxon_class         = chr(43L),
    stringsAsFactors    = FALSE
  )
  out <- .append_all_cols(
    out, df, cname,
    used = names(df)[c(1L, 2L, 4L, 5L, 6L, 23L, 24L, 25L, 43L)]
  )
  .trait_finalize(out)
}


# ---- Madin: bacteria / archaea phenotypic + genome traits ------------------

#' Parse the Madin et al. bacteria/archaea trait database
#'
#' Reads the species-aggregated `condensed_species_NCBI.csv` (one row per
#' species). Carries the NCBI taxonomy id as a cross-reference.
#'
#' @param path Character. Path to `condensed_species_NCBI.csv`.
#' @return data.frame with canonical_name + microbial trait columns.
#' @export
parse_madin <- function(path) {
  df <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
                        na.strings = c("NA", ""))
  pick <- function(col, fn) if (col %in% names(df)) fn(df[[col]]) else NA
  chr <- function(v) {
    x <- trimws(as.character(v))
    x[x == "" | x == "NA"] <- NA_character_
    x
  }
  num <- function(v) suppressWarnings(as.numeric(v))

  cname <- chr(df$species)
  out <- data.frame(
    canonical_name   = cname,
    ncbi_tax_id      = num(df$species_tax_id),
    gram_stain       = pick("gram_stain", chr),
    metabolism       = pick("metabolism", chr),
    cell_shape       = pick("cell_shape", chr),
    motility         = pick("motility", chr),
    sporulation      = pick("sporulation", chr),
    isolation_source = pick("isolation_source", chr),
    growth_temp_c    = pick("growth_tmp", num),
    optimum_temp_c   = pick("optimum_tmp", num),
    optimum_ph       = pick("optimum_ph", num),
    genome_size_bp   = pick("genome_size", num),
    gc_content_pct   = pick("gc_content", num),
    stringsAsFactors = FALSE
  )
  out <- .append_all_cols(
    out, df, cname,
    used = c("species", "species_tax_id", "gram_stain", "metabolism",
             "cell_shape", "motility", "sporulation", "isolation_source",
             "growth_tmp", "optimum_tmp", "optimum_ph", "genome_size",
             "gc_content")
  )
  .trait_finalize(out)
}


# ---- Coral Trait Database (long -> wide) -----------------------------------

#' Parse the Coral Trait Database release
#'
#' The release is long format (one row per observation). This pivots a set of
#' high-coverage functional traits to one row per species, aggregating numeric
#' traits by median and categorical traits by mode.
#'
#' @param path Character. Path to the extracted release directory or to
#'   `ctdb_1.1.1_data.csv`.
#' @return data.frame with canonical_name + coral trait columns.
#' @export
parse_coral_traits <- function(path) {
  file <- if (dir.exists(path)) {
    f <- list.files(path, pattern = "ctdb.*_data\\.csv$", full.names = TRUE,
                    recursive = TRUE)
    if (!length(f)) {
      f <- list.files(path, pattern = "data\\.csv$", full.names = TRUE,
                      recursive = TRUE)
    }
    if (!length(f)) stop("Coral Trait data CSV not found.", call. = FALSE)
    f[1L]
  } else {
    path
  }
  df <- utils::read.csv(file, check.names = FALSE, stringsAsFactors = FALSE)

  long <- data.frame(
    name  = as.character(df$specie_name),
    trait = as.character(df$trait_name),
    value = as.character(df$value),
    stringsAsFactors = FALSE
  )

  spec <- list(
    symbiotic_state         = list(trait = "Zooxanthellate", type = "cat"),
    growth_form             = list(trait = "Growth form typical", type = "cat"),
    coloniality             = list(trait = "Coloniality", type = "cat"),
    substrate_attachment    = list(trait = "Substrate attachment", type = "cat"),
    sexual_system           = list(trait = "Sexual system", type = "cat"),
    larval_development_mode = list(trait = "Mode of larval development",
                                   type = "cat"),
    symbiont_clade          = list(trait = "Symbiodinium clade", type = "cat"),
    corallite_width_max_mm  = list(trait = "Corallite width maximum",
                                   type = "num"),
    colony_max_diameter_cm  = list(trait = "Colony maximum diameter",
                                   type = "num"),
    growth_rate_mm_yr       = list(trait = "Growth rate", type = "num"),
    depth_lower_m           = list(trait = "Depth lower", type = "num"),
    depth_upper_m           = list(trait = "Depth upper", type = "num"),
    skeletal_density_g_cm3  = list(trait = "Skeletal density", type = "num")
  )
  .trait_finalize(.pivot_species_traits(long, spec))
}


# ---- World Spider Trait Database (long -> wide) ----------------------------

#' Parse the World Spider Trait Database CSV export
#'
#' The full export is long format with ~36% access-restricted rows (value =
#' "access restricted") that are dropped. A set of high-coverage morphometric
#' and ecological traits is pivoted to one row per species.
#'
#' @param path Character. Path to the WST CSV export.
#' @return data.frame with canonical_name + spider trait columns.
#' @export
parse_spider_traits <- function(path) {
  df <- if (requireNamespace("data.table", quietly = TRUE)) {
    as.data.frame(data.table::fread(path, showProgress = FALSE),
                  stringsAsFactors = FALSE)
  } else {
    utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  }

  restricted <- !is.na(df$value) & trimws(df$value) == "access restricted"
  if ("datasetRestrictedAccess" %in% names(df)) {
    restricted <- restricted |
      (!is.na(df$datasetRestrictedAccess) &
         df$datasetRestrictedAccess == "Restricted access")
  }
  df <- df[!restricted, , drop = FALSE]

  long <- data.frame(
    name  = as.character(df$originalName),
    trait = as.character(df$trait),
    value = as.character(df$value),
    stringsAsFactors = FALSE
  )

  spec <- list(
    body_length_mm     = list(trait = "bole", type = "num"),
    prosoma_length_mm  = list(trait = "cele", type = "num"),
    prosoma_width_mm   = list(trait = "cewe", type = "num"),
    abdomen_length_mm  = list(trait = "able", type = "num"),
    leg1_length_mm     = list(trait = "l1le", type = "num"),
    ballooning         = list(trait = "balo", type = "cat"),
    web_building       = list(trait = "webb", type = "cat"),
    hunting_guild      = list(trait = "guil", type = "cat"),
    web_type           = list(trait = "webt", type = "cat"),
    circadian_activity = list(trait = "circ", type = "cat"),
    stratum            = list(trait = "strt", type = "cat")
  )
  .trait_finalize(.pivot_species_traits(long, spec))
}


# ---- Amniote Life History Database -----------------------------------------

#' Parse the Amniote Life History Database
#'
#' One row per species across birds, mammals and reptiles. The dataset codes
#' missing values as -999; these are converted to NA on every numeric trait.
#'
#' @param path Character. Path to `Amniote_Database_Aug_2015.csv`.
#' @return data.frame with canonical_name + life-history traits.
#' @export
parse_amniote <- function(path) {
  df <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  num <- function(col) {
    if (!col %in% names(df)) return(rep(NA_real_, nrow(df)))
    v <- suppressWarnings(as.numeric(df[[col]]))
    v[v == -999] <- NA_real_
    v
  }
  chr <- function(col) {
    if (!col %in% names(df)) return(rep(NA_character_, nrow(df)))
    x <- trimws(as.character(df[[col]]))
    x[x == "" | x == "NA"] <- NA_character_
    x
  }

  cname <- trimws(paste(chr("genus"), chr("species")))
  out <- data.frame(
    canonical_name        = cname,
    taxon_class           = chr("class"),
    adult_body_mass_g     = num("adult_body_mass_g"),
    no_sex_body_mass_g    = num("no_sex_body_mass_g"),
    female_body_mass_g    = num("female_body_mass_g"),
    male_body_mass_g      = num("male_body_mass_g"),
    adult_svl_cm          = num("adult_svl_cm"),
    maximum_longevity_y   = num("maximum_longevity_y"),
    litter_clutch_size    = num("litter_or_clutch_size_n"),
    clutches_per_y        = num("litters_or_clutches_per_y"),
    egg_mass_g            = num("egg_mass_g"),
    incubation_d          = num("incubation_d"),
    female_maturity_d     = num("female_maturity_d"),
    gestation_d           = num("gestation_d"),
    weaning_d             = num("weaning_d"),
    birth_hatching_wt_g   = num("birth_or_hatching_weight_g"),
    stringsAsFactors      = FALSE
  )
  # Amniote is one row per species; -999 is its missing sentinel on numerics.
  df[df == -999] <- NA
  out <- .append_all_cols(
    out, df, cname,
    used = c("genus", "species", "class", "adult_body_mass_g",
             "no_sex_body_mass_g", "female_body_mass_g", "male_body_mass_g",
             "adult_svl_cm", "maximum_longevity_y", "litter_or_clutch_size_n",
             "litters_or_clutches_per_y", "egg_mass_g", "incubation_d",
             "female_maturity_d", "gestation_d", "weaning_d",
             "birth_or_hatching_weight_g")
  )
  .trait_finalize(out)
}


# ---- COMBINE mammal traits -------------------------------------------------

#' Parse the COMBINE mammal trait database (reported values)
#'
#' Uses the reported (observed/compiled) trait table, not the phylogenetically
#' imputed one, so gap-filled numbers are never shipped as observed. Keyed on
#' the IUCN 2020 binomial.
#'
#' @param path Character. Path to `trait_data_reported.csv`.
#' @return data.frame with canonical_name + mammal traits.
#' @export
parse_combine <- function(path) {
  df <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
                        na.strings = c("NA", ""))
  num <- function(col) {
    if (!col %in% names(df)) return(rep(NA_real_, nrow(df)))
    suppressWarnings(as.numeric(df[[col]]))
  }
  chr <- function(col) {
    if (!col %in% names(df)) return(rep(NA_character_, nrow(df)))
    x <- trimws(as.character(df[[col]]))
    x[x == "" | x == "NA" | x == "Not recognised"] <- NA_character_
    x
  }
  name <- chr("iucn2020_binomial")
  fallback <- trimws(paste(chr("genus"), chr("species")))
  name[is.na(name)] <- fallback[is.na(name)]

  out <- data.frame(
    canonical_name       = name,
    adult_mass_g         = num("adult_mass_g"),
    adult_body_length_mm = num("adult_body_length_mm"),
    litter_size_n        = num("litter_size_n"),
    litters_per_year_n   = num("litters_per_year_n"),
    max_longevity_d      = num("max_longevity_d"),
    gestation_length_d   = num("gestation_length_d"),
    weaning_age_d        = num("weaning_age_d"),
    generation_length_d  = num("generation_length_d"),
    dispersal_km         = num("dispersal_km"),
    habitat_breadth_n    = num("habitat_breadth_n"),
    diet_breadth_n       = num("det_diet_breadth_n"),
    trophic_level        = num("trophic_level"),
    activity_cycle       = num("activity_cycle"),
    foraging_stratum     = chr("foraging_stratum"),
    biogeographical_realm = chr("biogeographical_realm"),
    stringsAsFactors     = FALSE
  )
  out <- .append_all_cols(
    out, df, name,
    used = c("iucn2020_binomial", "genus", "species", "adult_mass_g",
             "adult_body_length_mm", "litter_size_n", "litters_per_year_n",
             "max_longevity_d", "gestation_length_d", "weaning_age_d",
             "generation_length_d", "dispersal_km", "habitat_breadth_n",
             "det_diet_breadth_n", "trophic_level", "activity_cycle",
             "foraging_stratum", "biogeographical_realm")
  )
  .trait_finalize(out)
}


# ---- AusTraits (long -> wide) ----------------------------------------------

#' Parse the AusTraits plant trait database
#'
#' Reads the long `traits.csv` from the plain-text release and pivots a set of
#' high-coverage functional traits to one row per taxon, aggregating across all
#' records (numeric by median, categorical by mode). Numeric traits such as leaf
#' mass per area are recorded mostly at population/individual level, so records
#' of every entity type are aggregated up to the taxon.
#'
#' @param path Character. Path to the extracted release directory or to
#'   `traits.csv`.
#' @return data.frame with canonical_name + plant traits.
#' @export
parse_austraits <- function(path) {
  file <- if (dir.exists(path)) {
    f <- list.files(path, pattern = "^traits\\.csv$", full.names = TRUE,
                    recursive = TRUE)
    if (!length(f)) stop("AusTraits traits.csv not found.", call. = FALSE)
    f[1L]
  } else {
    path
  }

  cols <- c("taxon_name", "trait_name", "value")
  df <- if (requireNamespace("data.table", quietly = TRUE)) {
    as.data.frame(
      data.table::fread(file, select = cols, showProgress = FALSE),
      stringsAsFactors = FALSE
    )
  } else {
    utils::read.csv(file, check.names = FALSE, stringsAsFactors = FALSE)
  }

  long <- data.frame(
    name  = as.character(df$taxon_name),
    trait = as.character(df$trait_name),
    value = as.character(df$value),
    stringsAsFactors = FALSE
  )

  spec <- list(
    plant_growth_form     = list(trait = "plant_growth_form", type = "cat"),
    life_history          = list(trait = "life_history", type = "cat"),
    woodiness             = list(trait = "woodiness_detailed", type = "cat"),
    photosynthetic_pathway = list(trait = "photosynthetic_pathway",
                                  type = "cat"),
    dispersal_syndrome    = list(trait = "dispersal_syndrome", type = "cat"),
    resprouting_capacity  = list(trait = "resprouting_capacity", type = "cat"),
    flowering_time        = list(trait = "flowering_time", type = "cat"),
    plant_height_m        = list(trait = "plant_height", type = "num"),
    leaf_length_mm        = list(trait = "leaf_length", type = "num"),
    leaf_width_mm         = list(trait = "leaf_width", type = "num"),
    leaf_area_mm2         = list(trait = "leaf_area", type = "num"),
    leaf_mass_per_area    = list(trait = "leaf_mass_per_area", type = "num"),
    leaf_n_per_dry_mass   = list(trait = "leaf_N_per_dry_mass", type = "num"),
    leaf_p_per_dry_mass   = list(trait = "leaf_P_per_dry_mass", type = "num"),
    seed_dry_mass_mg      = list(trait = "seed_dry_mass", type = "num"),
    wood_density_g_cm3    = list(trait = "wood_density", type = "num")
  )
  .trait_finalize(.pivot_species_traits(long, spec))
}


# ---- Global Wood Density Database v2 ----------------------------------------

#' Parse the Global Wood Density Database v2 (species aggregate)
#'
#' Reads the species-aggregated file (one row per species). Wood density is
#' reported as wood specific gravity (oven-dry mass / green volume),
#' dimensionless and numerically equal to g/cm3. Bark density is not present in
#' the aggregated files and is not included.
#'
#' @param path Character. Path to `gwddagg_v2.2_species.csv`.
#' @return data.frame with canonical_name + wood density traits.
#' @export
parse_gwdd <- function(path) {
  df <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  num <- function(col) {
    if (!col %in% names(df)) return(rep(NA_real_, nrow(df)))
    suppressWarnings(as.numeric(df[[col]]))
  }
  cname <- trimws(as.character(df$species))
  out <- data.frame(
    canonical_name         = cname,
    wood_density_g_cm3      = num("wsg_est"),
    wood_density_trunk_g_cm3 = num("wsg_est_trunk"),
    wood_density_branch_g_cm3 = num("wsg_est_branch"),
    n_measurements         = num("nb"),
    stringsAsFactors       = FALSE
  )
  out <- .append_all_cols(
    out, df, cname,
    used = c("species", "wsg_est", "wsg_est_trunk", "wsg_est_branch", "nb")
  )
  .trait_finalize(out)
}
