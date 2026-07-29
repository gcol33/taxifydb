# Remaining trait parsers: FUNGuild, FishBase, FungalTraits, AlgaeTraits,
# lizard traits, LepTraits, AnimalTraits, NW European arthropod traits,
# AnAge, GloNAF, Ecoflora, FloraWeb.


# ---- shared helpers for the scrape-sourced plant trait parsers -------------

#' Is a scraped cell a "no data" sentinel?
#'
#' FloraWeb renders absent traits as German placeholders ("keine Angaben",
#' "nicht bewertet", ...). Treat those, blanks, and NA as missing.
#' @noRd
.trait_is_nodata <- function(v) {
  v2 <- tolower(trimws(v))
  is.na(v) | !nzchar(v2) |
    grepl("^(keine|kein |keiner|keinem|keine angabe)", v2) |
    grepl("nicht bewertet|nicht vergeben|keine angaben", v2)
}

#' Median of ";"-separated numeric tokens (non-numeric flags stripped)
#' @noRd
.trait_num_median <- function(x) {
  vapply(as.character(x), function(s) {
    if (is.na(s) || !nzchar(s)) return(NA_real_)
    toks <- strsplit(s, ";")[[1L]]
    nums <- suppressWarnings(as.numeric(gsub("[^0-9.+-]", "", toks)))
    nums <- nums[is.finite(nums)]
    if (!length(nums)) NA_real_ else stats::median(nums)
  }, numeric(1L), USE.NAMES = FALSE)
}

#' Collapse ";"-separated tokens to unique, NA-dropped, "; "-joined string
#' @noRd
.trait_collapse_uniq <- function(x) {
  vapply(as.character(x), function(s) {
    if (is.na(s) || !nzchar(s)) return(NA_character_)
    toks <- trimws(strsplit(s, ";")[[1L]])
    toks <- toks[nzchar(toks) & toks != "NA"]
    if (!length(toks)) NA_character_ else paste(unique(toks), collapse = "; ")
  }, character(1L), USE.NAMES = FALSE)
}

#' Drop name-less and trait-less rows, dedup by canonical_name
#' @noRd
.trait_finalize <- function(out) {
  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]
  tc <- setdiff(names(out), "canonical_name")
  has <- rowSums(!is.na(out[, tc, drop = FALSE])) > 0L
  out <- out[has, ]
  out[!duplicated(out$canonical_name), , drop = FALSE]
}


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

  # Keep every other FUNGuild field; the curated columns above stay untouched
  # and references/citation-style fields are auto-skipped.
  used_cols <- c(
    level_col[1L], "taxonLevel", "taxon", "guild",
    intersect(c("trophicMode", "trophic_mode"), names(df))[1L],
    intersect(c("growthMorphology", "growthForm", "growth_morphology"),
              names(df))[1L],
    intersect(c("confidenceRanking", "confidence_ranking", "confidence"),
              names(df))[1L]
  )
  used_cols <- used_cols[!is.na(used_cols)]
  out <- .append_all_cols(out, df, trimws(df$taxon), used = used_cols)

  out
}


#' Build a canonical_name + trait table from an rfishbase server
#'
#' Shared engine for [parse_fishbase()] and [parse_sealifebase()]. The binomial
#' `canonical_name` comes from `load_taxa()` (whose `Species` column is the full
#' binomial); the `species` table in rfishbase 5.x no longer carries a
#' species-epithet column, so the name must come from `load_taxa()` and be
#' joined to traits by `SpecCode`. Traits come from the `species` table (length,
#' mass, depth range, vulnerability, habitat, importance) and trophic level from
#' `ecology` (`DietTroph`, falling back to `FoodTroph` where diet-based values
#' are absent -- SeaLifeBase populates `FoodTroph` but rarely `DietTroph`).
#' Missing columns degrade to NA.
#'
#' @param server Either "fishbase" or "sealifebase".
#' @return data.frame keyed on `canonical_name` with eight trait columns.
#' @noRd
.rfishbase_trait_table <- function(server) {
  if (!requireNamespace("rfishbase", quietly = TRUE)) {
    stop("rfishbase is required to build the ", server,
         " enrichment from source.\n",
         "Install it with: install.packages(\"rfishbase\")", call. = FALSE)
  }

  tx <- as.data.frame(rfishbase::load_taxa(server = server),
                      stringsAsFactors = FALSE)
  bino <- data.frame(
    SpecCode       = tx$SpecCode,
    canonical_name = trimws(as.character(tx$Species)),
    stringsAsFactors = FALSE
  )
  bino <- bino[!duplicated(bino$SpecCode), , drop = FALSE]

  sp <- as.data.frame(rfishbase::species(server = server),
                      stringsAsFactors = FALSE)
  merged <- merge(sp, bino, by = "SpecCode", all.x = FALSE)

  eco <- tryCatch(
    as.data.frame(rfishbase::ecology(server = server), stringsAsFactors = FALSE),
    error = function(e) NULL
  )
  if (!is.null(eco) && "SpecCode" %in% names(eco)) {
    # Bring every ecology column (not just the two trophic-level fields);
    # SpecCode stays the join key and species-table names win on collision.
    eco_sub <- eco[!duplicated(eco$SpecCode), , drop = FALSE]
    merged <- merge(merged, eco_sub, by = "SpecCode", all.x = TRUE,
                    suffixes = c("", "_eco"))
  }

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

  # Trophic level: prefer diet-based (DietTroph), fall back to food-items-based
  # (FoodTroph). SeaLifeBase populates FoodTroph but almost never DietTroph.
  troph <- safe_num("DietTroph")
  food  <- safe_num("FoodTroph")
  troph[is.na(troph)] <- food[is.na(troph)]

  out <- data.frame(
    canonical_name  = merged$canonical_name,
    SpecCode        = as.character(merged$SpecCode),
    body_length_cm  = safe_num("Length"),
    body_mass_g     = safe_num("Weight"),
    trophic_level   = troph,
    depth_min_m     = safe_num("DepthRangeShallow"),
    depth_max_m     = safe_num("DepthRangeDeep"),
    vulnerability   = safe_num("Vulnerability"),
    habitat         = safe_chr("DemersPelag"),
    importance      = safe_chr("Importance"),
    stringsAsFactors = FALSE
  )

  # Keep binomials only (a space separates genus and epithet).
  out <- out[!is.na(out$canonical_name) & grepl(" ", out$canonical_name), ,
             drop = FALSE]
  out <- out[!duplicated(out$canonical_name), , drop = FALSE]

  # Attach one representative POPLW length-weight fit (W = a * L^b) per species,
  # so a consumer can convert the length columns to a mass. The length columns
  # cover far more species than Weight does, so the coefficients unlock them.
  # See gcol33/taxifydb#25.
  ltmax <- grep("^ltypemaxm$", names(merged), ignore.case = TRUE, value = TRUE)[1L]
  spec_ltype <- if (!is.na(ltmax)) {
    stats::setNames(trimws(as.character(merged[[ltmax]])),
                    as.character(merged$SpecCode))
  } else NULL
  lw <- .rfishbase_lw_table(server, spec_ltype)
  lw_cols <- c("lw_a", "lw_b", "lw_type", "lw_method", "lw_sex", "lw_n", "lw_r2")
  if (!is.null(lw)) {
    idx <- match(out$SpecCode, lw$SpecCode)
    for (col in lw_cols) out[[col]] <- lw[[col]][idx]
  } else {
    for (col in lw_cols) {
      out[[col]] <- if (col %in% c("lw_type", "lw_method", "lw_sex"))
        NA_character_ else NA_real_
    }
  }

  # SpecCode was only needed to join POPLW; canonical_name is the .vtr key.
  out$SpecCode <- NULL

  # Keep every other merged species/taxonomy/ecology column.
  .append_all_cols(
    out, merged, merged$canonical_name,
    used = c("Species", "canonical_name", "Length", "Weight", "DietTroph",
             "FoodTroph", "DepthRangeShallow", "DepthRangeDeep",
             "Vulnerability", "DemersPelag", "Importance", "SpecCode")
  )
}


#' One representative POPLW length-weight fit per species
#'
#' FishBase/SeaLifeBase POPLW holds the `a` and `b` of `W = a * L^b`, often
#' several rows per species (by length type, sex, method). The enrichment is
#' keyed one row per species, so this reduces POPLW to a single representative
#' fit per `SpecCode`, keeping the length convention (`Type`) beside the
#' coefficient -- a coefficient fitted on TL applied to an SL length is a silent
#' error. Among a species' fits it prefers: the length type matching the
#' species' maximum-length type (so the coefficient applies to
#' `body_length_cm`), then a general (unsexed/mixed) over a sex-specific fit,
#' then a proper regression over a weak single-L-W-pair fit, then the larger
#' sample. POPLW rows are real measured fits; FishBase's Bayesian congeneric
#' estimates live in a separate table and are not included.
#'
#' @param server "fishbase" or "sealifebase".
#' @param spec_ltype Optional named character vector (names = `SpecCode`, value
#'   = the species' maximum-length type) used to prefer a matching-type fit.
#' @return data.frame keyed on `SpecCode` with lw_a, lw_b, lw_type, lw_method,
#'   lw_sex, lw_n, lw_r2 -- one row per species -- or NULL if POPLW is absent.
#' @noRd
.rfishbase_lw_table <- function(server, spec_ltype = NULL) {
  lw <- tryCatch(
    as.data.frame(rfishbase::length_weight(server = server),
                  stringsAsFactors = FALSE),
    error = function(e) NULL)
  .rfishbase_lw_select(lw, spec_ltype)
}


#' Reduce a raw POPLW table to one representative fit per species
#'
#' The pure selection half of [.rfishbase_lw_table()], split out so the ranking
#' is testable without a network fetch. `lw` is the raw `length_weight()` frame.
#'
#' @param lw Raw POPLW data.frame (needs at least SpecCode, a, b), or NULL.
#' @param spec_ltype Optional named vector (SpecCode -> maximum-length type).
#' @return data.frame keyed on SpecCode with the lw_* columns, or NULL.
#' @noRd
.rfishbase_lw_select <- function(lw, spec_ltype = NULL) {
  if (is.null(lw) || !all(c("SpecCode", "a", "b") %in% names(lw))) return(NULL)

  get <- function(nm, default = NA) {
    if (nm %in% names(lw)) lw[[nm]] else rep(default, nrow(lw))
  }
  d <- data.frame(
    SpecCode  = as.character(get("SpecCode")),
    lw_a      = suppressWarnings(as.numeric(get("a"))),
    lw_b      = suppressWarnings(as.numeric(get("b"))),
    lw_type   = trimws(as.character(get("Type"))),
    lw_method = trimws(as.character(get("Method"))),
    lw_sex    = tolower(trimws(as.character(get("Sex")))),
    lw_n      = suppressWarnings(as.numeric(get("Number"))),
    lw_r2     = suppressWarnings(as.numeric(get("CoeffDetermination"))),
    stringsAsFactors = FALSE
  )
  d <- d[!is.na(d$lw_a) & !is.na(d$lw_b), , drop = FALSE]
  if (!nrow(d)) return(NULL)

  pref_type  <- if (!is.null(spec_ltype)) unname(spec_ltype[d$SpecCode]) else NA
  type_match <- !is.na(pref_type) & nzchar(d$lw_type) &
                toupper(pref_type) == toupper(d$lw_type)
  sex_score    <- ifelse(d$lw_sex == "unsexed", 3L,
                         ifelse(d$lw_sex == "mixed", 2L, 1L))
  method_score <- ifelse(grepl("regression", d$lw_method, ignore.case = TRUE),
                         2L, 1L)
  n_val <- ifelse(is.na(d$lw_n), 0, d$lw_n)

  ord <- order(d$SpecCode, -as.integer(type_match), -sex_score, -method_score,
               -n_val)
  d <- d[ord, , drop = FALSE]
  d[!duplicated(d$SpecCode), , drop = FALSE]
}


#' Parse FishBase species traits (via rfishbase)
#'
#' @param path Character. Not used (rfishbase fetches data directly), kept
#'   for interface consistency.
#' @return data.frame with canonical_name + body/depth/diet columns.
#' @export
parse_fishbase <- function(path) {
  .rfishbase_trait_table("fishbase")
}


#' Parse SeaLifeBase species traits (via rfishbase)
#'
#' Non-fish companion to [parse_fishbase()] (molluscs, crustaceans,
#' echinoderms, marine mammals, reptiles, etc.). Shares the same trait columns.
#'
#' @param path Ignored; rfishbase fetches data directly.
#' @return data.frame keyed on `canonical_name` with eight trait columns.
#' @export
parse_sealifebase <- function(path) {
  .rfishbase_trait_table("sealifebase")
}


#' Parse the GRooT species-aggregate root-trait table
#'
#' GRooT ships a long-format CSV (`GRooTAggregateSpeciesVersion.csv`): one row
#' per species x trait, with per-species mean, median and quartiles. This
#' pivots the nine best-populated key traits to wide format, one row per
#' species, using the per-species mean (`meanSpecies`). The species name is
#' the TNRS-resolved `genusTNRS` + `speciesTNRS`.
#'
#' @param path Character. Path to `GRooTAggregateSpeciesVersion.csv`.
#' @return data.frame keyed on `canonical_name` with nine root-trait columns.
#' @export
parse_groot <- function(path) {
  # download_groot() returns c(aggregate = ..., full = ...); older callers may
  # still pass a single aggregate-CSV path.
  agg_path  <- if (!is.null(names(path)) && "aggregate" %in% names(path)) {
    path[["aggregate"]]
  } else {
    path[[1L]]
  }
  full_path <- if (!is.null(names(path)) && "full" %in% names(path)) {
    path[["full"]]
  } else {
    NA_character_
  }

  df <- utils::read.csv(
    agg_path,
    fileEncoding = "latin1",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )

  # Nine key traits the data paper highlights keep hand-picked output names; the
  # pivot keeps every other GRooT trait too (sanitized name). The mycorrhizal-
  # colonization label carries a space, not an underscore, in the source file.
  curated <- list(
    root_diameter                 = list(trait = "Mean_Root_diameter",   type = "num"),
    specific_root_length          = list(trait = "Specific_root_length", type = "num"),
    root_tissue_density           = list(trait = "Root_tissue_density",  type = "num"),
    root_n_concentration          = list(trait = "Root_N_concentration", type = "num"),
    root_c_concentration          = list(trait = "Root_C_concentration", type = "num"),
    root_mass_fraction            = list(trait = "Root_mass_fraction",   type = "num"),
    lateral_spread                = list(trait = "Lateral_spread",       type = "num"),
    root_mycorrhizal_colonization = list(trait = "Root_mycorrhizal colonization",
                                         type = "num"),
    rooting_depth                 = list(trait = "Rooting_depth",        type = "num")
  )

  df <- df[!is.na(df$speciesTNRS) & nzchar(trimws(df$speciesTNRS)), , drop = FALSE]
  long <- data.frame(
    name  = trimws(paste(df$genusTNRS, df$speciesTNRS)),
    trait = df$traitName,
    value = df$meanSpecies,
    stringsAsFactors = FALSE
  )
  out <- .pivot_species_traits(long, curated)

  # GRooT's aggregate specific_root_area mixes incompatible scales: three source
  # papers sit ~1000x below GRooT's cm2 g-1 standard (a compilation unit error).
  # Recompute it from the full per-record version, rescaling those papers with
  # standardized sources winning per species (see .groot_fix_sra).
  if (!is.na(full_path) && nzchar(full_path) && file.exists(full_path)) {
    out <- .groot_fix_sra(out, full_path)
  }
  out
}


#' Download both GRooT files (species aggregate + full per-record version)
#'
#' The aggregate drives every trait; the full version is needed to repair
#' specific_root_area (see [parse_groot()]). The full download is best-effort:
#' if it fails, the aggregate SRA is used as-is.
#'
#' @param url Character. The aggregate ZIP URL (from the registry).
#' @param dest Character. Download directory.
#' @return Named character vector `c(aggregate = <csv>, full = <csv or NA>)`.
#' @export
download_groot <- function(url, dest) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  agg <- download_and_unzip(url, file.path(dest, "agg"),
                            "GRooTAggregateSpeciesVersion\\.csv$")
  full_url <- sub("GRooTAggregateSpeciesVersion\\.zip$", "GRooTFullVersion.zip", url)
  full <- tryCatch(
    download_and_unzip(full_url, file.path(dest, "full"),
                       "GRooTFullVersion\\.csv$"),
    error = function(e) NA_character_
  )
  c(aggregate = agg, full = full)
}


# Recompute specific_root_area from GRooT's full per-record version. Three source
# papers (Quanquan 2011, an unpublished MSc thesis; Mokany & Ash 2008; Chanteloup
# & Bonis 2013) are stored ~1000x below GRooT's cm2 g-1 standard (data paper Table
# 1/S1, median 385.8) -- a compilation unit error, not GRooT's conversion: AusTraits
# carries the identical Mokany 2008 data equally low. The x1000 is grounded, not a
# guess: Mokany & Ash 2008's own SRA-SLA regression (Fig 1B, log10 SRA = 1.019 +
# 0.024*(SLA - 18.208), SRA in m2/kg) fixes the real magnitude at ~10 m2/kg =
# ~100 cm2/g, and the stored values x1000 land in that range. The three papers are
# rescaled x1000, but standardized sources win per species: a species with any
# clean record keeps its clean median (Mokany's paper itself cautions that its
# pot-grown values differ from the field), and the rescaled papers only fill
# species that no standardized source covers.
.groot_fix_sra <- function(out, full_path,
                           rescale = c("Quanquan 2011", "Mokany and Ash 2008",
                                       "Chanteloup and Bonis 2013"),
                           factor = 1000) {
  if (!requireNamespace("data.table", quietly = TRUE)) return(out)
  cols <- c("genusTNRS", "speciesTNRS", "traitName", "traitValue",
            "referencesAbbreviated")
  fv <- tryCatch(
    as.data.frame(data.table::fread(full_path, select = cols,
                                    showProgress = FALSE)),
    error = function(e) NULL
  )
  if (is.null(fv) || !nrow(fv)) return(out)
  d <- fv[fv$traitName == "Specific_root_area", , drop = FALSE]
  if (!nrow(d)) return(out)

  v   <- suppressWarnings(as.numeric(d$traitValue))
  sp  <- trimws(paste(d$genusTNRS, d$speciesTNRS))
  flg <- d$referencesAbbreviated %in% rescale
  ok  <- is.finite(v) & nzchar(sp)

  # Standardized-source per-species median (wins where present).
  cok       <- ok & !flg
  clean_med <- if (any(cok)) tapply(v[cok], sp[cok], stats::median) else numeric(0)
  # Rescaled flagged-source per-species median (gap fill only).
  vr        <- v; vr[flg] <- vr[flg] * factor
  fok       <- ok & flg
  flag_med  <- if (any(fok)) tapply(vr[fok], sp[fok], stats::median) else numeric(0)

  final <- clean_med
  gap   <- setdiff(names(flag_med), names(clean_med))
  final[gap] <- flag_med[gap]
  if (!length(final)) return(out)

  out$specific_root_area <- as.numeric(final[out$canonical_name])
  out
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

  # Keep every other FungalTraits column. Genus is the key; canonical_name
  # mirrors it, so the shared widener treats canonical_name as the join column.
  used_cols <- c(genus_col, primary_col, secondary_col, growth_col, fruit_col,
                 decay_col, plant_path_col, animal_col, endo_col, ecto_col)
  used_cols <- used_cols[!is.na(used_cols)]
  out <- .append_all_cols(out, df, genus_clean, used = used_cols)

  rownames(out) <- NULL
  out
}


#' Standardize a raw FungalRoot per-observation mycorrhiza type label
#'
#' Collapses the FungalRoot `Mycorrhiza type` vocabulary (which uses commas and
#' free text inside single cells) to a small set of standard mycorrhizal type
#' tokens: `AM`, `EcM`, `ErM`, `OM`, `NM`, the dual forms `EcM-AM` / `ErM-EcM`
#' / `ErM-AM`, `Other`, and `uncertain`.
#'
#' @param v Character vector of raw labels.
#' @return Character vector of standardized tokens (`NA` for unrecognized).
#' @noRd
.fungalroot_std_type <- function(v) {
  v <- trimws(v)
  out <- rep(NA_character_, length(v))
  out[v == "AM"]                                    <- "AM"
  out[v == "AM-like (non-vascular plants)"]         <- "AM"
  out[v == "non-mycorrhizal"]                       <- "NM"
  out[v == "EcM, AM undetermined"]                  <- "EcM"
  out[v == "EcM, no AM colonization"]               <- "EcM"
  out[v == "EcM,AM"]                                <- "EcM-AM"
  out[v == "ErM"]                                   <- "ErM"
  out[v == "ErM,EcM"]                               <- "ErM-EcM"
  out[v == "ErM,AM"]                                <- "ErM-AM"
  out[v == "OM"]                                    <- "OM"
  out[v == "Other"]                                 <- "Other"
  out[v == "non-ectomycorrhizal (AM undetermined)"] <- "uncertain"
  out
}


#' Parse the FungalRoot database (GBIF Darwin Core Archive) to genus-level types
#'
#' Reads the FungalRoot occurrence core (`occurrences.csv`) and its
#' MeasurementOrFact extension (`measurements.csv`), keeps the per-observation
#' `Mycorrhiza type` measurements, standardizes them to a small set of type
#' tokens, and reduces them to one row per plant genus by
#' majority consensus. Mycorrhizal type is phylogenetically conserved at the
#' genus level, which is the resolution FungalRoot itself recommends for
#' inference; the per-genus value here is the most frequent standardized type
#' across that genus's observations (taxifydb's own aggregation, not
#' FungalRoot's published per-genus assignment).
#'
#' @param path Character. Directory holding the extracted FungalRoot DwC-A.
#' @return data.frame keyed on `genus` (also copied to `canonical_name`) with
#'   `mycorrhizal_type`, `mycorrhizal_status`, and `mycorrhizal_records`.
#' @export
parse_fungalroot <- function(path) {
  find_csv <- function(want_cols) {
    csvs <- list.files(path, pattern = "\\.csv$", full.names = TRUE,
                       recursive = TRUE, ignore.case = TRUE)
    for (f in csvs) {
      hdr <- names(utils::read.csv(f, nrows = 1L, check.names = FALSE,
                                   stringsAsFactors = FALSE))
      if (all(want_cols %in% hdr)) return(f)
    }
    stop(sprintf("FungalRoot: no CSV with columns %s found in %s",
                 paste(want_cols, collapse = ", "), path), call. = FALSE)
  }

  occ_file <- find_csv(c("ID", "genus", "scientificName"))
  mea_file <- find_csv(c("Core ID", "measurementType", "measurementValue"))

  occ <- utils::read.csv(occ_file, check.names = FALSE, stringsAsFactors = FALSE)
  mea <- utils::read.csv(mea_file, check.names = FALSE, stringsAsFactors = FALSE)

  myc <- mea[mea$measurementType == "Mycorrhiza type", , drop = FALSE]
  myc$genus <- occ$genus[match(myc[["Core ID"]], occ$ID)]
  myc$type  <- .fungalroot_std_type(myc$measurementValue)
  myc <- myc[!is.na(myc$genus) & nzchar(trimws(myc$genus)) & !is.na(myc$type), ,
             drop = FALSE]
  myc$genus <- trimws(myc$genus)

  if (nrow(myc) == 0L) {
    stop("FungalRoot: no usable Mycorrhiza type observations after join.",
         call. = FALSE)
  }

  # Genus-level majority consensus: per-genus most frequent standardized type
  tab   <- table(myc$genus, myc$type)
  pick  <- max.col(tab, ties.method = "first")
  n_rec <- rowSums(tab)

  genus  <- rownames(tab)
  type   <- colnames(tab)[pick]
  status <- ifelse(type == "NM", "non-mycorrhizal",
                   ifelse(type %in% c("Other", "uncertain"), "uncertain",
                          "mycorrhizal"))

  out <- data.frame(
    canonical_name      = genus,
    genus               = genus,
    mycorrhizal_type    = type,
    mycorrhizal_status  = status,
    mycorrhizal_records = as.integer(n_rec),
    stringsAsFactors    = FALSE
  )

  # Keep every other measurement type per genus. The curated block above owns
  # the standardized "Mycorrhiza type" (type / status / record count); all other
  # measurement types pivot to one column each, joined on genus.
  mea$genus <- occ$genus[match(mea[["Core ID"]], occ$ID)]
  long <- data.frame(
    name  = trimws(mea$genus),
    trait = trimws(mea$measurementType),
    value = mea$measurementValue,
    stringsAsFactors = FALSE
  )
  keep <- !is.na(long$name) & nzchar(long$name) &
          !is.na(long$trait) & long$trait != "Mycorrhiza type"
  long <- long[keep, , drop = FALSE]
  if (nrow(long) > 0L) {
    extra <- .pivot_species_traits(long, spec = list(), keep_all = TRUE)
    extra_cols <- setdiff(names(extra), "canonical_name")
    idx <- match(out$canonical_name, extra$canonical_name)
    for (cc in extra_cols) out[[cc]] <- extra[[cc]][idx]
  }

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

  # Append every trait label not folded into the eight curated buckets.
  consumed <- unlist(trait_map, use.names = FALSE)
  long <- data.frame(name = species, trait = traits, value = values,
                     stringsAsFactors = FALSE)
  long <- long[!is.na(long$trait) & !(long$trait %in% consumed), , drop = FALSE]
  if (nrow(long) > 0L) {
    extra <- .pivot_species_traits(long, spec = list(), keep_all = TRUE)
    extra_cols <- setdiff(names(extra), "canonical_name")
    idx <- match(out$canonical_name, extra$canonical_name)
    for (cc in extra_cols) out[[cc]] <- extra[[cc]][idx]
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

  out <- data.frame(
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

  # Append every other source column.
  used_cols <- c(name_col, size_col, form_col, calc_col, span_col, tide_col,
                 wave_col, env_col, sub_col)
  used_cols <- used_cols[!is.na(used_cols)]
  .append_all_cols(out, df, trimws(df[[name_col]]), used = used_cols)
}


#' Parse ReptTraits (Etard et al. 2024) reptile traits (XLSX from Figshare)
#'
#' ReptTraits is the global ecological-trait dataset for reptiles, built on the
#' Reptile Database taxonomy. The header names are mapped explicitly (not by
#' fuzzy pattern) so the distribution/environment block and the morphology block
#' resolve to the correct source columns. Columns surface a per-species range
#' signal (biogeographic realm, elevation, climate) plus body-size and
#' life-history traits across all reptiles (not lizards only).
#'
#' @param path Character. Path to the ReptTraits XLSX (or CSV/TSV).
#' @return data.frame with canonical_name + reptile trait columns.
#' @export
parse_repttraits <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") {
    df <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else if (ext %in% c("tsv", "txt")) {
    df <- utils::read.delim(path, stringsAsFactors = FALSE,
                            check.names = FALSE)
  } else {
    if (!requireNamespace("openxlsx2", quietly = TRUE)) {
      stop("Package 'openxlsx2' is required to parse ReptTraits XLSX.",
           call. = FALSE)
    }
    sheets <- openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(path))
    pick <- sheets[tolower(sheets) == "data"]
    if (length(pick) == 0L) {
      ncols <- vapply(sheets, function(s) {
        h <- tryCatch(openxlsx2::read_xlsx(path, sheet = s, rows = 1:1),
                      error = function(e) NULL)
        if (is.null(h)) 0L else ncol(h)
      }, integer(1L))
      pick <- sheets[which.max(ncols)]
    }
    df <- suppressWarnings(openxlsx2::read_xlsx(path, sheet = pick[1L]))
  }

  # Several ReptTraits headers carry trailing whitespace (e.g. "Diet ").
  names(df) <- trimws(names(df))
  hdr <- names(df)
  # Resolve a source column by exact name or, when exact = FALSE, by ASCII
  # prefix. The prefix form avoids embedding non-ASCII header text (the degree
  # sign in the temperature column) and the long quoted SVL/SCL header.
  pick_col <- function(label, exact = FALSE) {
    if (exact) {
      m <- which(hdr == label)
    } else {
      m <- which(startsWith(hdr, label))
    }
    if (length(m) == 0L) return(NULL)
    hdr[m[1L]]
  }
  safe_num <- function(label, exact = FALSE) {
    cn <- pick_col(label, exact)
    if (is.null(cn)) return(rep(NA_real_, nrow(df)))
    suppressWarnings(as.numeric(df[[cn]]))
  }
  safe_chr <- function(label, exact = FALSE) {
    cn <- pick_col(label, exact)
    if (is.null(cn)) return(rep(NA_character_, nrow(df)))
    x <- as.character(df[[cn]])
    x[x %in% c("", "NA", "No")] <- NA_character_
    trimws(x)
  }

  name_col <- pick_col("Species", exact = TRUE)
  if (is.null(name_col)) name_col <- hdr[1L]
  cname <- trimws(gsub("_", " ", df[[name_col]]))

  out <- data.frame(
    canonical_name      = cname,
    # ---- distribution / environment (the per-species range signal) ----
    biogeographic_realm = safe_chr("Main biogeographic region"),
    microhabitat        = safe_chr("Microhabitat"),
    habitat_type        = safe_chr("Habitat type"),
    elevation_min_m     = safe_num("Minimal elevation"),
    elevation_max_m     = safe_num("Maximum elevation"),
    mean_annual_temp_c  = safe_num("Mean Annual Temperature"),
    insular_endemic     = safe_chr("Insular/endemic"),
    # ---- morphology / life history (all reptiles) ----
    body_mass_g         = safe_num("Maximum body mass"),
    svl_mm              = safe_num("Maximum length"),
    total_length_mm     = safe_num("Maximum total length"),
    longevity_yr        = safe_num("Maximum Longevity"),
    diet                = safe_chr("Diet", exact = TRUE),
    reproductive_mode   = safe_chr("Reproductive mode"),
    clutch_size         = safe_num("Mean number of offspring"),
    active_time         = safe_chr("Active time"),
    foraging_mode       = safe_chr("Foraging mode"),
    stringsAsFactors = FALSE
  )

  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]
  out <- out[!duplicated(out$canonical_name), ]

  # Keep every other ReptTraits column (the curated 16 are resolved by header).
  used_cols <- c(
    name_col,
    pick_col("Main biogeographic region"), pick_col("Microhabitat"),
    pick_col("Habitat type"), pick_col("Minimal elevation"),
    pick_col("Maximum elevation"), pick_col("Mean Annual Temperature"),
    pick_col("Insular/endemic"), pick_col("Maximum body mass"),
    pick_col("Maximum length"), pick_col("Maximum total length"),
    pick_col("Maximum Longevity"), pick_col("Diet", exact = TRUE),
    pick_col("Reproductive mode"), pick_col("Mean number of offspring"),
    pick_col("Active time"), pick_col("Foraging mode")
  )
  used_cols <- unlist(used_cols)
  used_cols <- used_cols[!is.na(used_cols)]
  .append_all_cols(out, df, cname, used = used_cols)
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

  cname <- trimws(df[[name_col]])
  out <- data.frame(
    canonical_name       = cname,
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

  # Keep every other consensus column, including the raw month columns
  # (flight_months is derived from them and kept above).
  out <- .append_all_cols(
    out, df, cname,
    used = c(name_col, "WS_L", "WS_U", "Voltinism", "DiapauseStage",
             "CanopyAffinity", "EdgeAffinity", "MoistureAffinity",
             "DisturbanceAffinity", "NumberOfHostplantFamilies",
             "FlightDuration")
  )
  .trait_finalize(out)
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

  # Non-positive masses/rates are invalid; drop them before collapsing so the
  # per-species median (and its within-species range) rest only on valid records.
  obs$body_mass_kg[is.na(obs$body_mass_kg) | obs$body_mass_kg <= 0] <- NA_real_
  obs$metabolic_rate_w[is.na(obs$metabolic_rate_w) | obs$metabolic_rate_w <= 0] <- NA_real_
  out <- .aggregate_spread(obs, c("body_mass_kg", "metabolic_rate_w"))

  # Aggregate every other observation column to species level (numeric ->
  # median, categorical -> mode) alongside the two curated columns.
  used_cols <- c(name_col,
                 if (length(bm_col) > 0L) bm_col[1L],
                 if (length(mr_col) > 0L) mr_col[1L])
  out <- .append_all_cols(out, df, canonical, used = used_cols)
  .trait_finalize(out)
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

  # One long (name, trait, value) table from BOTH the MoF measurement rows and
  # the description extension rows, keyed to the species name via taxon_id.
  long_parts <- list()

  if (!is.null(mof_file)) {
    mof <- utils::read.delim(mof_file, stringsAsFactors = FALSE, quote = "")
    mof_id <- intersect(names(mof), c("id", "taxonID", "coreid"))
    if (length(mof_id) == 0L) mof_id <- names(mof)[1L] else mof_id <- mof_id[1L]
    type_col <- intersect(names(mof), c("measurementType", "type"))
    if (length(type_col) == 0L) type_col <- names(mof)[2L] else type_col <- type_col[1L]
    val_col <- intersect(names(mof), c("measurementValue", "value"))
    if (length(val_col) == 0L) val_col <- names(mof)[3L] else val_col <- val_col[1L]
    long_parts[[length(long_parts) + 1L]] <- data.frame(
      name  = out$canonical_name[match(mof[[mof_id]], out$taxon_id)],
      trait = trimws(as.character(mof[[type_col]])),
      value = as.character(mof[[val_col]]),
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(desc_file)) {
    desc <- utils::read.delim(desc_file, stringsAsFactors = FALSE, quote = "")
    desc_id <- intersect(names(desc), c("id", "taxonID", "coreid"))
    if (length(desc_id) == 0L) desc_id <- names(desc)[1L] else desc_id <- desc_id[1L]
    desc_col <- intersect(names(desc), c("description", "value"))
    if (length(desc_col) == 0L) desc_col <- names(desc)[2L] else desc_col <- desc_col[1L]
    type_col2 <- intersect(names(desc), c("type", "measurementType"))
    if (length(type_col2) == 0L) type_col2 <- names(desc)[3L] else type_col2 <- type_col2[1L]
    long_parts[[length(long_parts) + 1L]] <- data.frame(
      name  = out$canonical_name[match(desc[[desc_id]], out$taxon_id)],
      trait = trimws(as.character(desc[[type_col2]])),
      value = as.character(desc[[desc_col]]),
      stringsAsFactors = FALSE
    )
  }

  # Curated nice names (7 quantitative + 3 categorical); every other trait is
  # kept too via keep_all.
  curated <- list(
    body_size_mm  = list(trait = "Body_size",           type = "num"),
    dispersal     = list(trait = "Dispersal_ability",   type = "num"),
    voltinism     = list(trait = "Voltinism_mean",      type = "num"),
    fecundity     = list(trait = "Fecundity",           type = "num"),
    development_d = list(trait = "Development_time",     type = "num"),
    lifespan_d    = list(trait = "Lifespan",            type = "num"),
    thermal_mean  = list(trait = "Thermal_mean",        type = "num"),
    diurnality    = list(trait = "Diurnality",          type = "cat"),
    feeding_guild = list(trait = "Feeding_guild_adult", type = "cat"),
    trophic_range = list(trait = "Trophic_range_adult", type = "cat")
  )

  long <- if (length(long_parts)) {
    do.call(rbind, long_parts)
  } else {
    data.frame(name = character(0), trait = character(0),
               value = character(0), stringsAsFactors = FALSE)
  }
  .pivot_species_traits(long, curated)
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
    name_used <- c(genus_col[1L], sp_col[1L])
  } else {
    name_col <- intersect(
      names(df),
      c("Scientific_name", "ScientificName", "scientific_name",
        "Common_name", "Binomial")
    )
    if (length(name_col) == 0L) name_col <- names(df)[1L] else name_col <- name_col[1L]
    canonical <- trimws(df[[name_col]])
    name_used <- name_col
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
  body_mass_col <- find_col(c("Body.mass..g.", "Body_mass_g", "Adult.weight..g.",
                              "AdultWeight", "body_mass"))
  metab_col <- find_col(c("Metabolic.rate..W.", "Metabolic_rate_W",
                          "MetabolicRate", "metabolic_rate"))
  fem_mat_col <- find_col(c("Female.maturity..days.", "Female_maturity_days",
                            "FemaleMaturity", "female_maturity"))
  male_mat_col <- find_col(c("Male.maturity..days.", "Male_maturity_days",
                             "MaleMaturity", "male_maturity"))
  gest_col <- find_col(c("Gestation.Incubation..days.",
                         "Gestation_Incubation_days", "GestationIncubation",
                         "gestation_incubation"))
  litter_col <- find_col(c("Litter.Clutch.size", "Litter_Clutch_size",
                           "LitterClutchSize", "litter_clutch_size"))
  birth_col <- find_col(c("Birth.weight..g.", "Birth_weight_g", "BirthWeight",
                          "birth_weight"))
  growth_col <- find_col(c("Growth.rate..1.days.", "Growth_rate", "GrowthRate",
                           "growth_rate"))
  temp_col <- find_col(c("Temperature..K.", "Temperature_K", "Temperature",
                         "temperature"))

  out <- data.frame(
    canonical_name         = canonical,
    max_longevity_yr       = safe_num(longevity_col),
    body_mass_g            = safe_num(body_mass_col),
    metabolic_rate_w       = safe_num(metab_col),
    female_maturity_d      = safe_num(fem_mat_col),
    male_maturity_d        = safe_num(male_mat_col),
    gestation_incubation_d = safe_num(gest_col),
    litter_size            = safe_num(litter_col),
    birth_mass_g           = safe_num(birth_col),
    growth_rate            = safe_num(growth_col),
    temperature_k          = safe_num(temp_col),
    stringsAsFactors = FALSE
  )

  # Keep every other AnAge column; blanks are the source's missing marker.
  used_cols <- c(name_used, longevity_col, body_mass_col, metab_col,
                 fem_mat_col, male_mat_col, gest_col, litter_col, birth_col,
                 growth_col, temp_col)
  out <- .append_all_cols(out, df, canonical, used = used_cols)
  .trait_finalize(out)
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
  out <- out[!duplicated(paste(out$canonical_name, out$region_id)), ]

  # Carry the rest of the GloNAF flora record per (species, region); the merge
  # helper keys are dropped, canonical_name / region_id / naturalized are kept.
  .append_all_cols(
    out, flora, trimws(flora$canonical_name),
    group = "region_id", group_row = as.character(flora$region_id),
    used = c("join_key", "region_join", "region_code_resolved", "OBJIDsic")
  )
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

  # Keep every other sheet column (raw French header, sanitized). The curated
  # and recoded columns above are consumed and stay untouched.
  used_cols <- cn[cn_key %in% c("nomh", "nomb", "noma", "floraison",
                                "pollinisation", "dissemination", "sexualite",
                                "couleur_fleur", "fruit", "type_ligneux",
                                "continentalite", "salinite")]
  out <- .append_all_cols(out, df, nm, used = used_cols)
  .trait_finalize(out)
}


#' Parse the Ecoflora scrape snapshot (British Isles plant traits)
#'
#' Reads the per-species Ecoflora scrape (`results.csv`, one row per species,
#' trait columns named by their Ecoflora short codes) and returns a clean wide
#' data.frame keyed on `canonical_name`. Numeric fields (heights, seed weight,
#' flowering months) take the median of any multiple scraped values;
#' categorical fields keep the unique values joined with "; ". Every trait
#' column carries a `_uk` suffix to mark the British-flora calibration and to
#' avoid collisions when chained with other plant-trait enrichments.
#'
#' Ecoflora (Fitter & Peat 1994) has no bulk download or API; the snapshot was
#' collected one species at a time and is redistributed under the source
#' licence (CC BY-NC-SA 4.0).
#'
#' @param path Character. Path to the Ecoflora `results.csv` snapshot.
#' @return data.frame with `canonical_name` + 18 `_uk` trait columns.
#' @export
parse_ecoflora <- function(path) {
  df <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "NA",
                        colClasses = "character")
  chr <- function(col) {
    if (!col %in% names(df)) return(rep(NA_character_, nrow(df)))
    .trait_collapse_uniq(df[[col]])
  }
  num <- function(col) {
    if (!col %in% names(df)) return(rep(NA_real_, nrow(df)))
    .trait_num_median(df[[col]])
  }
  int_month <- function(col) {
    v <- num(col)
    v[is.na(v) | v < 1 | v > 12] <- NA_real_
    as.integer(round(v))
  }

  out <- data.frame(
    canonical_name            = trimws(df$species),
    height_max_mm_uk          = num("h_max"),
    height_min_mm_uk          = num("h_min"),
    leaf_area_uk              = chr("le_area"),
    leaf_longevity_uk         = chr("le_long"),
    root_system_uk            = chr("root_system"),
    photosynthetic_pathway_uk = chr("phot_path"),
    life_form_uk              = chr("li_form"),
    reproduction_uk           = chr("reprod_meth"),
    flower_begin_month_uk     = int_month("flw_early"),
    flower_end_month_uk       = int_month("flw_late"),
    pollination_vector_uk     = chr("poll_vect"),
    seed_weight_mg_uk         = num("seed_wght"),
    propagule_uk              = chr("propag"),
    ell_light_uk              = chr("ell_light_uk"),
    ell_moisture_uk           = chr("ell_moist_uk"),
    ell_reaction_uk           = chr("ell_pH_uk"),
    ell_nitrogen_uk           = chr("ell_N"),
    ell_salt_uk               = chr("ell_S"),
    stringsAsFactors          = FALSE
  )

  # Keep every other scraped column (raw short-code name, sanitized).
  out <- .append_all_cols(
    out, df, trimws(df$species),
    used = c("species", "h_max", "h_min", "le_area", "le_long", "root_system",
             "phot_path", "li_form", "reprod_meth", "flw_early", "flw_late",
             "poll_vect", "seed_wght", "propag", "ell_light_uk", "ell_moist_uk",
             "ell_pH_uk", "ell_N", "ell_S")
  )
  .trait_finalize(out)
}


# FloraWeb label -> output column map. Curated from the four scraped trait
# pages (biologie / morphologie / oekologie / verbreitung), covering the full
# set of per-species scalar traits. The latitudinal-zone areal-matrix rows,
# the numeric chromosome-count distribution rows, and the per-subspecies
# "Chromosomen Anz. Nachweise" rows are intentionally excluded (cross-tab
# noise, not per-species traits). All output columns carry a `_de` suffix.
.floraweb_label_map <- c(
  # morphology
  "Wuchshöhe (Rothmaler)"                          = "height_de",
  "Lebensform – jew. Lebensdauer"                  = "life_form_de",
  "Blattform"                                           = "leaf_shape_de",
  "Blattanatomie"                                       = "leaf_anatomy_de",
  "Blattausdauer"                                       = "leaf_persistence_de",
  "Speicherorgane, Spross- und Wurzelmetamorphosen"     = "storage_organs_de",
  "Blühmonate (Rothmaler)"                         = "flowering_months_de",
  "Blühmonate (BiolFlor)"                          = "flowering_months_biolflor_de",
  "Blühphase"                                      = "flowering_phase_de",
  "Phänologische Jahreszeit"                       = "phenological_season_de",
  "Beschreibung"                                        = "description_de",
  # biology
  "Bestäubung (Pollenvektoren)"                    = "pollination_vector_de",
  "Bestäuber"                                      = "pollinator_de",
  "Belohnung für Bestäuber"                   = "pollinator_reward_de",
  "Blumentyp (nach Kugler 1970)"                        = "flower_type_de",
  "Blumenklasse (nach Müller 1881)"                = "flower_class_de",
  "Ausbreitungstyp"                                     = "dispersal_type_de",
  "Diasporentyp (Ausbreitungseinheit)"                  = "diaspore_type_de",
  "Germinulentyp (Keimungsheinheit)"                    = "germinule_type_de",
  "Reproduktionstyp"                                    = "reproduction_type_de",
  "Vegetative Ausbreitung"                              = "vegetative_spread_de",
  "Befruchtungstyp"                                     = "fertilization_type_de",
  "Apomixis"                                            = "apomixis_de",
  "Diklinie (räumliche Geschlechtertrennung)"      = "dicliny_de",
  "Dichogamie (zeitliche Geschlechtertrennung)"         = "dichogamy_de",
  "SI-Reaktion"                                         = "self_incompatibility_de",
  "SI-Mechanismus"                                      = "si_mechanism_de",
  "Ploidiegrad"                                         = "ploidy_de",
  "Chromosomenzahl (BiolFlor)"                          = "chromosome_number_de",
  "Häufigkeitsverteilung der Chromosomenzahlen"    = "chromosome_freq_de",
  "Chromosomen"                                         = "chromosomes_de",
  # ecology (Ellenberg indicator values + community / hemeroby bindings)
  "Lichtzahl"                                           = "ell_light_de",
  "Temperaturzahl"                                      = "ell_temperature_de",
  "Kontinentalitätszahl"                           = "ell_continentality_de",
  "Feuchtezahl"                                         = "ell_moisture_de",
  "Feuchtewechsel"                                      = "ell_moisture_variability_de",
  "Reaktionszahl"                                       = "ell_reaction_de",
  "Stickstoffzahl"                                      = "ell_nitrogen_de",
  "Salzzahl"                                            = "ell_salt_de",
  "Schwermetallresistenz"                               = "heavy_metal_resistance_de",
  "Ökologischer Strategietyp"                      = "strategy_type_de",
  "Standort"                                            = "habitat_site_de",
  "Formation"                                           = "formation_de",
  "Bindung an Pflanzengesellschaften"                   = "plant_community_de",
  "Biotoptyp"                                           = "biotope_type_de",
  "Bindung an Wald"                                     = "forest_binding_de",
  "Hemerobie (menschlicher Einfluss)"                   = "hemeroby_de",
  "Urbanität (Bindung an Siedlungen)"              = "urbanity_de",
  # distribution
  "Florengebiete"                                       = "floristic_zones_de",
  "Arealformel"                                         = "areal_formula_de",
  "Arealtyp (Oberdorfer, 1983)"                         = "areal_type_de",
  "Ozeanität"                                      = "oceanity_de",
  "Arealzentrum"                                        = "range_centre_de",
  "Größe des Weltareals"                      = "world_range_size_de",
  "Häufigkeit im Weltareal"                        = "world_range_frequency_de",
  "Lage im Weltareal"                                   = "world_range_position_de",
  "Gefährdung im Weltareal"                        = "world_range_hazard_de",
  "Arealanteil Deutschlands"                            = "germany_range_share_de",
  "Verantwortlichkeit Deutschlands"                     = "germany_responsibility_de",
  "Höhenstufen (Florenzonen)"                      = "altitude_belts_de"
)


#' Parse the FloraWeb scrape snapshot (German-flora plant traits)
#'
#' Reads the long-format FloraWeb scrape (`results_long.csv`, one row per
#' species/page/label/value) and pivots it to a clean wide data.frame keyed on
#' `canonical_name`. German "no data" placeholders are dropped, multiple values
#' for one trait are collapsed to unique values joined with "; ", and the
#' German trait labels are mapped to English column names (each with a `_de`
#' suffix). Covers morphology, reproductive biology, the nine Ellenberg
#' indicator values, ploidy and chromosome number, and chorological
#' distribution.
#'
#' FloraWeb (Bundesamt fuer Naturschutz) is the live portal carrying the
#' BiolFlor trait data (Klotz, Kuehn & Durka 2002) plus Rothmaler morphology
#' and Ellenberg indicator values. It has no bulk export or API, so the
#' snapshot was scraped per species; the access date is its version.
#'
#' @param path Character. Path to the FloraWeb `results_long.csv` snapshot.
#' @return data.frame with `canonical_name` + the mapped `_de` trait columns.
#' @export
parse_floraweb <- function(path) {
  d <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                       encoding = "UTF-8")
  d$col <- .floraweb_label_map[d$label]

  # Labels the curated map does not cover. Many FloraWeb "labels" are
  # value-encoded rows (latitudinal areal matrix, chromosome-count
  # distributions, per-subspecies counts) that would explode into hundreds of
  # degenerate columns; only fold the unmapped labels back in when there are
  # few enough to be genuine per-species traits.
  unmapped <- unique(d$label[is.na(d$col) & !is.na(d$label) & nzchar(d$label)])
  keep_unmapped <- length(unmapped) > 0L && length(unmapped) < 60L
  d_extra <- if (keep_unmapped) d[d$label %in% unmapped, , drop = FALSE] else NULL

  d <- d[!is.na(d$col), , drop = FALSE]
  d <- d[!.trait_is_nodata(d$value), , drop = FALSE]
  d$value <- trimws(gsub("\\s+", " ", d$value))
  d <- d[nzchar(d$value), , drop = FALSE]

  ids   <- unique(d$name_usage_id)
  canon <- d$canonical_name[match(ids, d$name_usage_id)]
  id_idx <- match(d$name_usage_id, ids)

  out <- data.frame(canonical_name = trimws(canon), stringsAsFactors = FALSE)
  for (cc in intersect(unname(.floraweb_label_map), unique(d$col))) {
    sel <- d$col == cc
    agg <- tapply(d$value[sel], id_idx[sel],
                  function(v) paste(unique(v), collapse = "; "))
    vec <- rep(NA_character_, length(ids))
    vec[as.integer(names(agg))] <- agg
    out[[cc]] <- vec
  }

  # The BiolFlor chromosome-number cell stacks base ("1n = 9") and somatic
  # ("2n = 18") counts; whitespace collapse fuses them ("1n = 92n = 18"). Split
  # them back apart, and strip the boilerplate prefix from the count tables.
  if ("chromosome_number_de" %in% names(out)) {
    out$chromosome_number_de <- gsub("([0-9])\\s*([12]n\\s*=)", "\\1; \\2",
                                     out$chromosome_number_de)
  }
  for (cc in c("chromosome_freq_de", "chromosomes_de")) {
    if (cc %in% names(out)) {
      out[[cc]] <- trimws(sub("^Chromosomen Anz\\. Nachweise\\s*", "",
                              out[[cc]]))
    }
  }

  # Fold the unmapped labels back in only when the guard above deemed them few
  # enough to be genuine per-species traits (not value-encoded matrix rows).
  if (keep_unmapped && !is.null(d_extra) && nrow(d_extra) > 0L) {
    d_extra <- d_extra[!.trait_is_nodata(d_extra$value), , drop = FALSE]
    d_extra$value <- trimws(gsub("\\s+", " ", d_extra$value))
    d_extra <- d_extra[nzchar(d_extra$value), , drop = FALSE]
    long <- data.frame(
      name  = trimws(d_extra$canonical_name),
      trait = d_extra$label,
      value = d_extra$value,
      stringsAsFactors = FALSE
    )
    extra <- .pivot_species_traits(long, spec = list(), keep_all = TRUE)
    extra_cols <- setdiff(names(extra), "canonical_name")
    idx <- match(trimws(out$canonical_name), extra$canonical_name)
    for (cc in extra_cols) out[[cc]] <- extra[[cc]][idx]
  }

  .trait_finalize(out)
}
