# BETSI recovery: published BETSI-derived trait matrices, re-assembled.
#
# BETSI (Biological and Ecological Traits of Soil Invertebrates; Hedde et al.,
# portail.betsi.cnrs.fr) is a European soil-fauna trait database whose live
# portal is offline. No complete export is recoverable; what is recoverable are
# the species-level matrices individual studies downloaded from BETSI and then
# printed or deposited. This module rebuilds those into per-taxon enrichment
# assets (see gcol33/taxifydb#42).
#
# Two matrix shapes, one dispatcher:
#   * "fuzzy" -- for each (species, trait) the affinity is split as percentages
#     across that trait's modality bins, summing to 100 (a self-validating
#     invariant, checked on ingest). Kept in full as one numeric column per
#     (trait, modality) bin, 0-100, named `<trait>__<modality>`; nothing the
#     matrix recorded is collapsed away. (Pelosi 2014 earthworms.)
#   * "hard" -- one scalar or categorical value per (species, trait), read into
#     one column per trait. (Lu 2025 Collembola.)
# Code-keyed matrices (the INRAE Collembola deposits, Bonfanti 2022) key traits
# on a 6-letter GEN_SPE species code with no bundled legend, which gen_spe()
# decodes.
#
# Provenance is per column, not per row: a source can carry some BETSI-derived
# trait columns beside its own primary columns (Lu 2025 draws 6 traits from BETSI
# but measured trophic guild, vertical distribution and life form itself). Each
# built column's tier is recorded in the `.vtr`'s meta.json `provenance` block
# via .betsi_recovery_provenance() -- never flattened into one label, never faked
# onto rows it does not describe. [Data Prep Labels, Never Decides] applied to
# provenance. A per-row source column is added only when several sources merge
# into one per-taxon asset.
#
# Trait axes with a faithful T-SITA concept are crosswalked
# (inst/extdata/tsita_crosswalk.csv); the axes T-SITA does not cover (the
# earthworm-specific epithelium / typhlosolis / cocoon / mass:length / vertical,
# and Collembola pigmentation / antenna:body ratio / trophic guild / life form)
# keep their own names, unmapped, by design.


# The provenance tiers, never flattened into one undocumented table.
# `source_study` marks a value the source study measured itself, outside BETSI.
.betsi_provenance_tiers <- c("betsi_export", "betsi_derived",
                             "literature_reconstruction", "source_study")


# In-hand recovered matrices, one descriptor per source, grouped by the
# per-taxon enrichment each source feeds.
#
#   shape              "fuzzy" | "hard".
#   traits             fuzzy: trait -> ordered modality bins (the wide-column
#                      template AND a guard the matrix carries exactly these);
#                      hard:  trait -> value type ("numeric" | "categorical").
#   provenance_default tier for every trait not named in `provenance`.
#   provenance         per-trait overrides of the default.
.betsi_recovery_sources <- list(
  pelosi2014_earthworm = list(
    enrichment         = "betsi_earthworm_traits",
    taxon              = "earthworm",
    key_type           = "binomial",
    shape              = "fuzzy",
    file               = "pelosi2014_earthworm.csv",
    provenance_default = "betsi_derived",
    traits = list(
      body_length_mm           = c("20-50", "50-100", "100-150", "150-200", "200-400"),
      body_mass_length_ratio   = c("1-7", "7-15", ">15"),
      cocoon_diameter_mm       = c("1-2", "2-4", "4-6"),
      epithelium               = c("supple", "rigid"),
      typhlosolis              = c("simple", "large_feather"),
      carbon_pref_mgkg         = c("<20", "20-33.3", "33.3-60", ">60"),
      vertical_distribution_cm = c("0-5", "5-20", ">20")
    )
  ),

  lu2025_collembola = list(
    enrichment         = "betsi_collembola_traits",
    taxon              = "collembola",
    key_type           = "binomial",
    shape              = "hard",
    file               = "lu2025_collembola.csv",
    provenance_default = "betsi_derived",
    # Lu 2025 states trait values are "derived from the BETSI databases ... except
    # for trophic guilds and vertical distribution which were measured in this
    # study"; life form is assigned per Potapov et al. (2016), not BETSI.
    provenance = list(
      stratification_scaled = "source_study",
      trophic_position      = "source_study",
      life_form             = "source_study"
    ),
    traits = list(
      antenna_body_ratio    = "numeric",
      body_length_mm        = "numeric",
      pigment_scaled        = "numeric",
      ocelli_number         = "numeric",
      furca                 = "numeric",
      reproduction          = "numeric",
      stratification_scaled = "numeric",
      trophic_position      = "categorical",
      life_form             = "categorical"
    )
  )
)


#' Six-letter GEN_SPE species code
#'
#' The BETSI-derived code-keyed matrices (INRAE UU2FQT / UCYSLH, Bonfanti 2022)
#' encode a species as the first three letters of the genus and the first three
#' of the specific epithet, uppercased and joined by an underscore:
#' `"Brachystomella parvula"` becomes `"BRA_PAR"`. Hyphens in the epithet are
#' dropped before the first three letters are taken. Returns `NA` for anything
#' that is not a capitalised `Genus species` binomial.
#'
#' @param binomial Character vector of `"Genus species"` names.
#' @return Character vector of `GEN_SPE` codes, `NA` where the input is not a
#'   capitalised `Genus species` binomial.
#' @examples
#' gen_spe(c("Brachystomella parvula", "Allacma gallica", "Onychiurus"))
#' @export
gen_spe <- function(binomial) {
  x <- trimws(as.character(binomial))
  pat <- "^([A-Z][a-z]+)\\s+([a-z][a-z-]*).*$"
  ok  <- grepl(pat, x)
  genus <- toupper(sub(pat, "\\1", x))
  epi   <- toupper(gsub("-", "", sub(pat, "\\2", x)))
  ifelse(ok, paste0(substr(genus, 1L, 3L), "_", substr(epi, 1L, 3L)),
         NA_character_)
}


#' List the BETSI-recovery enrichments
#'
#' The per-taxon assets rebuilt from published BETSI-derived matrices. Each is a
#' normal entry in the enrichment build registry; this lists the recovery subset
#' [build_betsi_recovery()] accepts.
#'
#' @return Character vector of recovery enrichment names.
#' @seealso [build_betsi_recovery()], [parse_betsi_recovery()]
#' @export
list_betsi_recovery <- function() {
  unique(unname(vapply(.betsi_recovery_sources, `[[`, "", "enrichment")))
}


#' Build a BETSI-recovery enrichment `.vtr` from its frozen matrices
#'
#' Convenience wrapper over [build_enrichment()] restricted to the per-taxon
#' assets rebuilt from published BETSI-derived matrices (see
#' [list_betsi_recovery()]). The heavy lifting -- staging the frozen matrices,
#' the parse, cross-backbone name resolution, T-SITA + provenance metadata, the
#' `.vtr` write -- is the shared enrichment pipeline; this only guards the name.
#'
#' @param name Character. A recovery enrichment name.
#' @param ... Passed to [build_enrichment()] (`output_dir`, `version`, ...).
#' @return Path to the built `.vtr` (invisibly).
#' @seealso [list_betsi_recovery()]
#' @export
build_betsi_recovery <- function(name, ...) {
  if (!name %in% list_betsi_recovery()) {
    stop(sprintf("'%s' is not a BETSI-recovery enrichment. Available: %s",
                 name, paste(list_betsi_recovery(), collapse = ", ")),
         call. = FALSE)
  }
  build_enrichment(name, ...)
}


#' Parse a per-taxon BETSI-recovery matrix into per-species trait columns
#'
#' Reads the frozen matrices for the sources feeding `enrichment` and returns one
#' row per species. Fuzzy sources (`species,trait,class,pct`) are checked for the
#' fuzzy-coding invariant (each species-by-trait affinity block sums to 100,
#' tolerating +-1 from integer rounding) and pivoted to one numeric column per
#' `<trait>__<modality>` bin (0-100). Hard sources (one value per trait) are read
#' into one column per trait. Provenance is not a data column here; it is written
#' to `meta.json` from the descriptor by the build.
#'
#' @param enrichment Character. A per-taxon recovery enrichment name (see
#'   [list_betsi_recovery()]).
#' @param path Character. Directory holding the source `.csv` files (staged from
#'   `inst/extdata/betsi/` by the build), or a single `.csv` when the enrichment
#'   has one source.
#' @return data.frame with `canonical_name` and the trait columns, one row per
#'   species.
#' @seealso [build_betsi_recovery()]
#' @export
parse_betsi_recovery <- function(enrichment, path) {
  srcs <- Filter(function(s) identical(s$enrichment, enrichment),
                 .betsi_recovery_sources)
  if (!length(srcs)) {
    stop(sprintf("No BETSI-recovery source feeds enrichment '%s'.", enrichment),
         call. = FALSE)
  }
  parts <- lapply(names(srcs), function(key) {
    s <- srcs[[key]]
    f <- if (dir.exists(path)) file.path(path, s$file) else path
    if (!file.exists(f)) {
      stop(sprintf("BETSI-recovery source '%s' not found at: %s", key, f),
           call. = FALSE)
    }
    tab <- utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
    if (identical(s$shape, "fuzzy")) {
      .betsi_pivot_matrix(tab, s, key)
    } else if (identical(s$shape, "hard")) {
      .betsi_read_hard(tab, s, key)
    } else {
      stop(sprintf("BETSI-recovery source '%s' has unknown shape '%s'.",
                   key, s$shape), call. = FALSE)
    }
  })
  out <- .rbind_fill(parts)
  out <- out[!is.na(out$canonical_name) & nzchar(out$canonical_name), ,
             drop = FALSE]
  out <- out[.is_binomial(out$canonical_name), , drop = FALSE]
  out[order(out$canonical_name), , drop = FALSE]
}


#' Per-column provenance map for a recovery enrichment's built columns
#'
#' Maps each built trait column of `enrichment` to its provenance tier, resolving
#' a fuzzy modality column (`<trait>__<bin>`) back to its logical trait. Returned
#' to [build_enrichment()] and written to the `.vtr`'s `meta.json` `provenance`
#' block. Non-trait columns (`canonical_name`) are skipped.
#'
#' @param enrichment Character. A recovery enrichment name.
#' @param cols Character. The built data.frame's column names.
#' @return A named list mapping column -> tier, or `NULL` if none map.
#' @noRd
.betsi_recovery_provenance <- function(enrichment, cols) {
  srcs <- Filter(function(s) identical(s$enrichment, enrichment),
                 .betsi_recovery_sources)
  tier_of <- list()
  for (s in srcs) {
    # `[[` not `$`: "provenance" is a prefix of "provenance_default", so `$`
    # partial-matches the default when a source has no per-trait overrides.
    prov    <- s[["provenance"]]
    default <- s[["provenance_default"]]
    for (tr in names(s[["traits"]])) {
      tier_of[[tr]] <- if (!is.null(prov[[tr]])) prov[[tr]] else default
    }
  }
  out <- list()
  for (col in cols) {
    if (identical(col, "canonical_name")) next
    tier <- tier_of[[sub("__.*$", "", col)]]
    if (!is.null(tier)) out[[col]] <- tier
  }
  if (!length(out)) NULL else out
}


# Sanitise a modality label to a field-name token: `">15"` -> `"gt15"`, `"<20"`
# -> `"lt20"`, `"33.3-60"` -> `"33_3_60"`. Keeps the mapping reversible enough
# to stay readable while producing a valid, stable column name.
.betsi_bin_token <- function(class) {
  x <- as.character(class)
  x <- gsub(">", "gt", x, fixed = TRUE)
  x <- gsub("<", "lt", x, fixed = TRUE)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  sub("^_|_$", "", x)
}


# Validate one fuzzy long-format matrix against its descriptor and pivot it to a
# per-species wide frame of `<trait>__<modality>` affinity columns.
.betsi_pivot_matrix <- function(long, s, key) {
  need <- c("species", "trait", "class", "pct")
  miss <- setdiff(need, names(long))
  if (length(miss)) {
    stop(sprintf("BETSI-recovery matrix '%s' missing column(s): %s",
                 key, paste(miss, collapse = ", ")), call. = FALSE)
  }
  long$species <- trimws(as.character(long$species))
  long$trait   <- trimws(as.character(long$trait))
  long$class   <- trimws(as.character(long$class))
  long$pct     <- suppressWarnings(as.numeric(as.character(long$pct)))
  if (any(is.na(long$pct))) {
    stop(sprintf("BETSI-recovery matrix '%s' has non-numeric affinities.", key),
         call. = FALSE)
  }

  # The matrix must carry exactly the descriptor's traits and modalities: a
  # missing or stray trait/class is an extraction drift, not something to paper
  # over. This is [Sanity-Check Input Totals in Data Scripts] for a trait table.
  got_traits <- sort(unique(long$trait))
  want_traits <- sort(names(s$traits))
  if (!identical(got_traits, want_traits)) {
    stop(sprintf(paste0("BETSI-recovery matrix '%s' trait set does not match ",
                        "its descriptor.\n  extra: %s\n  missing: %s"),
                 key,
                 paste(setdiff(got_traits, want_traits), collapse = ", "),
                 paste(setdiff(want_traits, got_traits), collapse = ", ")),
         call. = FALSE)
  }
  for (tr in names(s$traits)) {
    got_cls  <- sort(unique(long$class[long$trait == tr]))
    want_cls <- sort(s$traits[[tr]])
    if (!identical(got_cls, want_cls)) {
      stop(sprintf(paste0("BETSI-recovery matrix '%s', trait '%s' modalities ",
                          "do not match the descriptor.\n  extra: %s\n  ",
                          "missing: %s"),
                   key, tr,
                   paste(setdiff(got_cls, want_cls), collapse = ", "),
                   paste(setdiff(want_cls, got_cls), collapse = ", ")),
           call. = FALSE)
    }
  }

  species <- sort(unique(long$species))
  lut <- stats::setNames(long$pct,
                         paste(long$species, long$trait, long$class,
                               sep = "\r"))
  out <- data.frame(canonical_name = species, stringsAsFactors = FALSE)
  for (tr in names(s$traits)) {
    block_cols <- character(0)
    for (cls in s$traits[[tr]]) {
      col <- paste0(tr, "__", .betsi_bin_token(cls))
      out[[col]] <- unname(lut[paste(species, tr, cls, sep = "\r")])
      block_cols <- c(block_cols, col)
    }
    block <- out[block_cols]
    if (anyNA(block)) {
      i <- which(rowSums(is.na(block)) > 0L)[1L]
      stop(sprintf(paste0("BETSI-recovery matrix '%s' has no value for '%s', ",
                          "trait '%s' -- the frozen matrix is incomplete."),
                   key, species[i], tr), call. = FALSE)
    }
    block_sum <- rowSums(block)
    bad <- which(abs(block_sum - 100) > 1.5)
    if (length(bad)) {
      stop(sprintf(paste0("BETSI-recovery matrix '%s' fails the fuzzy-coding ",
                          "invariant: '%s' trait '%s' affinities sum to %.1f, ",
                          "not 100."),
                   key, species[bad[1L]], tr, block_sum[bad[1L]]),
           call. = FALSE)
    }
  }
  out
}


# Read one hard-value wide matrix (`species` + one column per trait) against its
# descriptor, coercing each trait to its declared type.
.betsi_read_hard <- function(wide, s, key) {
  if (!"species" %in% names(wide)) {
    stop(sprintf("BETSI-recovery matrix '%s' has no 'species' column.", key),
         call. = FALSE)
  }
  want <- names(s$traits)
  got  <- setdiff(names(wide), "species")
  if (!identical(sort(got), sort(want))) {
    stop(sprintf(paste0("BETSI-recovery matrix '%s' columns do not match its ",
                        "descriptor.\n  extra: %s\n  missing: %s"),
                 key,
                 paste(setdiff(got, want), collapse = ", "),
                 paste(setdiff(want, got), collapse = ", ")), call. = FALSE)
  }
  out <- data.frame(canonical_name = trimws(as.character(wide$species)),
                    stringsAsFactors = FALSE)
  for (tr in want) {
    if (identical(s$traits[[tr]], "numeric")) {
      v <- suppressWarnings(as.numeric(as.character(wide[[tr]])))
      if (any(is.na(v) & !is.na(wide[[tr]]) & nzchar(trimws(as.character(wide[[tr]]))))) {
        stop(sprintf(paste0("BETSI-recovery matrix '%s', numeric trait '%s' has ",
                            "a non-numeric value."), key, tr), call. = FALSE)
      }
      out[[tr]] <- v
    } else {
      out[[tr]] <- trimws(as.character(wide[[tr]]))
    }
  }
  out
}


# Row-bind data frames with differing columns, filling absents with NA, so
# matrices with different trait systems merge into one per-taxon asset.
.rbind_fill <- function(dfs) {
  dfs <- Filter(function(d) !is.null(d) && nrow(d) > 0L, dfs)
  if (!length(dfs)) return(data.frame())
  cols <- unique(unlist(lapply(dfs, names)))
  dfs <- lapply(dfs, function(d) {
    for (m in setdiff(cols, names(d))) d[[m]] <- NA
    d[cols]
  })
  do.call(rbind, dfs)
}


# Copy a recovery enrichment's frozen matrices out of the installed package into
# a build directory, so the shared pipeline's file-based parser reads them the
# same way it reads a downloaded source.
.betsi_recovery_stage <- function(enrichment, dest) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  srcs <- Filter(function(s) identical(s$enrichment, enrichment),
                 .betsi_recovery_sources)
  for (s in srcs) {
    src <- system.file("extdata", "betsi", s$file, package = "taxifydb")
    if (!nzchar(src)) {
      stop(sprintf(paste0("Frozen BETSI-recovery matrix '%s' not found in the ",
                          "installed package. Run data-raw/betsi_recovery.R."),
                   s$file), call. = FALSE)
    }
    file.copy(src, file.path(dest, s$file), overwrite = TRUE)
  }
  dest
}
