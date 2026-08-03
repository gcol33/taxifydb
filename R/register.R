# ---- Unified genus register: build pipeline ----
#
# Cross-backbone genus-level index, unioned from a fixed backbone set (see
# register_backbones()). Two artifacts:
#
#   genus_register.vtr   -- one row per genus, with classification + life_form
#   backend_coverage.vtr -- long format: one row per (genus x backend)
#
# Ported from taxify's runtime R/register.R (#23). There, build_genus_
# register() unioned whichever backbones a given user happened to have
# installed, so taxify()'s kingdom_group/taxon_group/life_form output and
# inspect() depended on the caller's local install rather than being
# reproducible. Here the build always unions the same fixed backbone set --
# each backbone's `.vtr` resolved from a local `output/<name>/<name>.vtr`
# build or downloaded from the published manifest entry -- and the two
# results are published like any other taxifydb asset, so every user gets the
# same genus_register.vtr / backend_coverage.vtr regardless of which
# backbones they have installed locally. taxify keeps a thin
# `ensure_register()` that downloads the published `.vtr` the same way it
# downloads a backbone or enrichment (gcol33/taxify#21).


# ---- Backbone genus extraction ----
#
# Each extractor reads genus-rank rows (or, for species-only backbones,
# derives genera from accepted species) from a built backbone `.vtr` and
# returns a data.frame with columns genus, kingdom, phylum, class, order,
# family.

#' Extract genus rows from WFO backbone
#'
#' @param bb_path Character. Path to WFO .vtr file.
#' @return data.frame with columns: genus, kingdom, phylum, class, order,
#'   family (kingdom/phylum/class/order are NA for WFO -- not stored in
#'   backbone).
#' @noRd
extract_wfo_genera <- function(bb_path) {
  df <- vectra::tbl(bb_path) |>
    vectra::filter(taxon_rank == "GENUS") |>
    vectra::select(canonical_name, family, genus) |>
    vectra::collect()

  if (nrow(df) == 0L) return(empty_genus_df())

  data.frame(
    genus   = df$canonical_name,
    kingdom = NA_character_,
    phylum  = NA_character_,
    class   = NA_character_,
    order   = NA_character_,
    family  = df$family,
    stringsAsFactors = FALSE
  )
}


#' Extract genus rows from COL backbone
#'
#' COL stores kingdom/phylum/class/order as direct columns.
#'
#' @param bb_path Character. Path to COL .vtr file.
#' @return data.frame with columns: genus, kingdom, phylum, class, order, family.
#' @noRd
extract_col_genera <- function(bb_path) {
  # Collect genus rows -- vectra select() uses bare names; collect all columns
  # then subset in R to handle the optionally-present higher-classification cols.
  df <- vectra::tbl(bb_path) |>
    vectra::filter(taxon_rank == "GENUS") |>
    vectra::collect()

  if (nrow(df) == 0L) return(empty_genus_df())

  result <- data.frame(
    genus  = df$canonical_name,
    family = if ("family" %in% names(df)) df$family else NA_character_,
    stringsAsFactors = FALSE
  )
  for (col in c("kingdom", "phylum", "class", "order")) {
    result[[col]] <- if (col %in% names(df)) df[[col]] else NA_character_
  }
  result[, c("genus", "kingdom", "phylum", "class", "order", "family"),
         drop = FALSE]
}


#' Extract genus rows from GBIF backbone
#'
#' GBIF backbone stores kingdom/phylum/class/order as separate taxonomy keys
#' that are not present in the converted .vtr. We do have `genus` and
#' `family`. Higher classification columns are absent; they need to be
#' provided via the GBIF hierarchy (see `resolve_kingdom_via_gbif()`).
#'
#' @param bb_path Character. Path to GBIF .vtr file.
#' @return data.frame with columns: genus, kingdom, phylum, class, order, family.
#' @noRd
extract_gbif_genera <- function(bb_path) {
  df <- vectra::tbl(bb_path) |>
    vectra::filter(taxon_rank == "GENUS") |>
    vectra::select(canonical_name, family, genus) |>
    vectra::collect()

  if (nrow(df) == 0L) return(empty_genus_df())

  data.frame(
    genus   = df$canonical_name,
    kingdom = NA_character_,
    phylum  = NA_character_,
    class   = NA_character_,
    order   = NA_character_,
    family  = df$family,
    stringsAsFactors = FALSE
  )
}


#' Extract unique genera from a Euro+Med backbone
#'
#' Euro+Med uses the unified backbone schema (canonical_name, taxon_rank, genus).
#' Plants-only, so kingdom is always "Plantae".
#'
#' @param bb_path Character. Path to Euro+Med .vtr file.
#' @return data.frame with columns: genus, kingdom, phylum, class, order, family.
#' @noRd
extract_euromed_genera <- function(bb_path) {
  df <- vectra::tbl(bb_path) |>
    vectra::filter(taxon_rank == "GENUS") |>
    vectra::select(canonical_name, family, genus) |>
    vectra::collect()

  if (nrow(df) == 0L) return(empty_genus_df())

  data.frame(
    genus   = df$canonical_name,
    kingdom = "Plantae",
    phylum  = NA_character_,
    class   = NA_character_,
    order   = NA_character_,
    family  = df$family,
    stringsAsFactors = FALSE
  )
}


#' Extract genus rows from ITIS backbone
#'
#' ITIS uses unified schema. No kingdom column.
#'
#' @param bb_path Character. Path to ITIS .vtr file.
#' @return data.frame with columns: genus, kingdom, phylum, class, order, family.
#' @noRd
extract_itis_genera <- function(bb_path) {
  df <- vectra::tbl(bb_path) |>
    vectra::filter(taxon_rank == "GENUS") |>
    vectra::select(canonical_name, family, genus) |>
    vectra::collect()

  if (nrow(df) == 0L) return(empty_genus_df())

  data.frame(
    genus   = df$canonical_name,
    kingdom = NA_character_,
    phylum  = NA_character_,
    class   = NA_character_,
    order   = NA_character_,
    family  = df$family,
    stringsAsFactors = FALSE
  )
}


#' Extract genus rows from NCBI backbone
#'
#' NCBI uses unified schema. Has kingdom column but values are NCBI-specific
#' (e.g. "Pseudomonadati") -- not standard kingdom names, so treated as NA
#' here and normalized later via `normalize_kingdom_names()`.
#'
#' @param bb_path Character. Path to NCBI .vtr file.
#' @return data.frame with columns: genus, kingdom, phylum, class, order, family.
#' @noRd
extract_ncbi_genera <- function(bb_path) {
  # Collect genus rows; runtime build may omit kingdom/phylum/class/order.
  df <- vectra::tbl(bb_path) |>
    vectra::filter(taxon_rank == "GENUS") |>
    vectra::collect()

  if (nrow(df) == 0L) return(empty_genus_df())

  data.frame(
    genus   = df$canonical_name,
    kingdom = if ("kingdom" %in% names(df)) df$kingdom else NA_character_,
    phylum  = NA_character_,
    class   = NA_character_,
    order   = NA_character_,
    family  = if ("family" %in% names(df)) df$family else NA_character_,
    stringsAsFactors = FALSE
  )
}


#' Extract genus rows from OTT backbone
#'
#' OTT uses unified schema. Kingdom column exists, populated for ~2% of genera.
#'
#' @param bb_path Character. Path to OTT .vtr file.
#' @return data.frame with columns: genus, kingdom, phylum, class, order, family.
#' @noRd
extract_ott_genera <- function(bb_path) {
  df <- vectra::tbl(bb_path) |>
    vectra::filter(taxon_rank == "GENUS") |>
    vectra::collect()

  if (nrow(df) == 0L) return(empty_genus_df())

  data.frame(
    genus   = df$canonical_name,
    kingdom = if ("kingdom" %in% names(df)) df$kingdom else NA_character_,
    phylum  = NA_character_,
    class   = NA_character_,
    order   = NA_character_,
    family  = if ("family" %in% names(df)) df$family else NA_character_,
    stringsAsFactors = FALSE
  )
}


#' Extract genus rows from WoRMS backbone
#'
#' WoRMS has fully denormalized classification: kingdom, phylum, class, order.
#' Most valuable source for higher-taxonomy resolution.
#'
#' @param bb_path Character. Path to WoRMS .vtr file.
#' @return data.frame with columns: genus, kingdom, phylum, class, order, family.
#' @noRd
extract_worms_genera <- function(bb_path) {
  df <- vectra::tbl(bb_path) |>
    vectra::filter(taxon_rank == "GENUS") |>
    vectra::collect()

  if (nrow(df) == 0L) return(empty_genus_df())

  pick <- function(col) if (col %in% names(df)) df[[col]] else NA_character_
  data.frame(
    genus   = df$canonical_name,
    kingdom = pick("kingdom"),
    phylum  = pick("phylum"),
    class   = pick("class"),
    order   = pick("order"),
    family  = pick("family"),
    stringsAsFactors = FALSE
  )
}


#' Empty genus data.frame (zero rows, correct schema)
#' @noRd
empty_genus_df <- function() {
  data.frame(
    genus   = character(0L),
    kingdom = character(0L),
    phylum  = character(0L),
    class   = character(0L),
    order   = character(0L),
    family  = character(0L),
    stringsAsFactors = FALSE
  )
}


#' Derive genus rows from a species-only backbone
#'
#' FishBase and SeaLifeBase backbones carry only species rows (no genus-rank
#' records), so genera are derived as the distinct genus + classification of
#' the accepted species. Classification is constant within a genus in these
#' backbones, so the first occurrence is representative.
#' @noRd
.extract_species_derived_genera <- function(bb_path) {
  df <- vectra::tbl(bb_path) |>
    vectra::filter(taxon_rank == "SPECIES" &
                     taxonomic_status == "ACCEPTED") |>
    vectra::collect()

  if (nrow(df) == 0L) return(empty_genus_df())

  pick <- function(col) if (col %in% names(df)) df[[col]] else NA_character_
  out <- data.frame(
    genus   = df$genus,
    kingdom = pick("kingdom"),
    phylum  = pick("phylum"),
    class   = pick("class"),
    order   = pick("order"),
    family  = df$family,
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$genus) & nzchar(out$genus), , drop = FALSE]
  out[!duplicated(out$genus), , drop = FALSE]
}

extract_fishbase_genera <- function(bb_path) {
  .extract_species_derived_genera(bb_path)
}

extract_sealifebase_genera <- function(bb_path) {
  .extract_species_derived_genera(bb_path)
}

#' Extract genus rows from the Reptile Database backbone
#'
#' Reptile Database is species-only (no genus-rank records); genera are derived
#' from accepted species. The backbone stamps a fixed higher classification
#' (Animalia / Chordata / Reptilia) plus order and family, so reptile genera
#' carry full classification.
#' @noRd
extract_reptiledb_genera <- function(bb_path) {
  .extract_species_derived_genera(bb_path)
}


#' Extract genera as the union of genus-rank rows and accepted species
#'
#' Covers any backbone that carries genus-rank records, whether or not it also
#' needs the species-derived fill: the two sets are stacked with the genus-rank
#' rows first, so their classification wins for a genus present at both ranks,
#' and a backbone carrying no genus rows degrades to the species-derived set.
#' Classification columns absent from the backbone come back `NA`.
#' @noRd
.extract_union_genera <- function(bb_path) {
  pick <- function(df, col) if (col %in% names(df)) df[[col]] else NA_character_

  gr <- tryCatch(
    vectra::tbl(bb_path) |>
      vectra::filter(taxon_rank == "GENUS") |>
      vectra::collect(),
    error = function(e) NULL
  )
  sp <- tryCatch(
    vectra::tbl(bb_path) |>
      vectra::filter(taxon_rank == "SPECIES" & taxonomic_status == "ACCEPTED") |>
      vectra::collect(),
    error = function(e) NULL
  )

  as_rows <- function(df, genus_col) {
    data.frame(
      genus   = df[[genus_col]],
      kingdom = pick(df, "kingdom"),
      phylum  = pick(df, "phylum"),
      class   = pick(df, "class"),
      order   = pick(df, "order"),
      family  = pick(df, "family"),
      stringsAsFactors = FALSE
    )
  }

  rows <- list()
  if (!is.null(gr) && nrow(gr) > 0L) {
    rows$genus_rank <- as_rows(gr, "canonical_name")
  }
  if (!is.null(sp) && nrow(sp) > 0L) {
    rows$species <- as_rows(sp, "genus")
  }
  if (length(rows) == 0L) return(empty_genus_df())

  combined <- do.call(rbind, rows)
  combined <- combined[!is.na(combined$genus) & nzchar(combined$genus), ,
                       drop = FALSE]
  if (nrow(combined) == 0L) return(empty_genus_df())

  combined[!duplicated(combined$genus), , drop = FALSE]
}


#' Extract genera from a vascular-plant backbone (LCVP, WCVP)
#'
#' Both are vascular-plant-only, so kingdom is always "Plantae" and neither
#' source records phylum or class. Genera are the union of any genus-rank rows
#' (WCVP carries them; LCVP is species-and-below only) and the genera of
#' accepted species.
#' @noRd
.extract_plant_genera <- function(bb_path) {
  out <- .extract_union_genera(bb_path)
  if (nrow(out) == 0L) return(out)

  out$kingdom <- "Plantae"
  out$phylum  <- NA_character_
  out$class   <- NA_character_
  out
}

extract_lcvp_genera <- function(bb_path) .extract_plant_genera(bb_path)
extract_wcvp_genera <- function(bb_path) .extract_plant_genera(bb_path)


#' Extract genus rows from the Mammal Diversity Database backbone
#'
#' MDD is species-and-subspecies only (no genus-rank records), so genera are
#' derived from accepted species. The backbone stamps a fixed higher
#' classification (Animalia / Chordata / Mammalia) plus order and family.
#' @noRd
extract_mdd_genera <- function(bb_path) {
  .extract_species_derived_genera(bb_path)
}


#' Extract genus rows from the AviList backbone
#'
#' AviList carries genus-rank rows alongside species and subspecies, with the
#' higher classification denormalized onto every row and a fixed
#' Animalia / Chordata / Aves stamp.
#' @noRd
extract_avilist_genera <- function(bb_path) {
  .extract_union_genera(bb_path)
}


#' Extract genus rows from the LPSN backbone
#'
#' LPSN carries more genus-rank rows than the accepted species imply, since a
#' validly published genus need not have an accepted species in the list, so
#' the union keeps both. Kingdom is LPSN's domain (Bacteria / Archaea), which
#' is already the form [normalize_kingdom_names()] produces for prokaryotes.
#' @noRd
extract_lpsn_genera <- function(bb_path) {
  .extract_union_genera(bb_path)
}


#' Extract genus rows from the Index Fungorum backbone
#'
#' Fungorum is species-and-below only and the one classification column it
#' carries is `family`, so its genera arrive with family alone. No kingdom is
#' stamped: Index Fungorum covers slime moulds and oomycetes alongside true
#' fungi, so a blanket "Fungi" would mislabel them.
#' @noRd
extract_fungorum_genera <- function(bb_path) {
  .extract_union_genera(bb_path)
}


#' Extract genus rows from the AlgaeBase backbone
#'
#' AlgaeBase carries genus-rank rows but only `family` beyond them. No kingdom
#' is stamped: "algae" spans Plantae, Chromista and the cyanobacteria, so there
#' is no single correct value.
#' @noRd
extract_algaebase_genera <- function(bb_path) {
  .extract_union_genera(bb_path)
}


# ---- Backbones the register unions ----

#' Genus extractor registry
#'
#' Maps each backend the register unions to its genus-row extractor. Single
#' source of truth: [build_genus_register()] and [build_backend_coverage()]
#' both read this instead of each keeping its own backend list.
#'
#' @noRd
.register_extractors <- list(
  wfo         = extract_wfo_genera,
  col         = extract_col_genera,
  gbif        = extract_gbif_genera,
  itis        = extract_itis_genera,
  ncbi        = extract_ncbi_genera,
  ott         = extract_ott_genera,
  worms       = extract_worms_genera,
  euromed     = extract_euromed_genera,
  fishbase    = extract_fishbase_genera,
  sealifebase = extract_sealifebase_genera,
  reptiledb   = extract_reptiledb_genera,
  lcvp        = extract_lcvp_genera,
  wcvp        = extract_wcvp_genera,
  mdd         = extract_mdd_genera,
  avilist     = extract_avilist_genera,
  lpsn        = extract_lpsn_genera,
  fungorum    = extract_fungorum_genera,
  algaebase   = extract_algaebase_genera
)


#' Backbone order used to resolve a genus's classification
#'
#' A genus is usually carried by several backbones. This is the order in which
#' their classifications are consulted: for each of kingdom, phylum, class,
#' order and family independently, the first backbone here that supplies a
#' value wins, so a lower-priority source can still fill a rank the higher one
#' leaves empty.
#'
#' Taxon authorities sit above the broad aggregators, since the point of
#' carrying MDD, AviList, LPSN or the Reptile Database is to prefer their
#' treatment over a general checklist's. Fungorum and AlgaeBase come last:
#' family is the only rank they record, and neither has a single correct
#' kingdom to offer.
#'
#' Must name every backbone in [.register_extractors]; the guard in
#' [resolve_genus_classification()] fails loudly rather than let a newly
#' registered extractor be dropped in silence.
#' @noRd
.register_priority <- function() {
  c("worms", "col", "wcvp", "reptiledb", "mdd", "avilist", "lpsn",
    "gbif", "euromed", "lcvp", "itis", "ncbi", "ott", "wfo",
    "fishbase", "sealifebase", "fungorum", "algaebase")
}


#' Names of the backbones the genus register and backend coverage union
#'
#' @return Character vector of backend identifiers.
#' @export
register_backbones <- function() {
  names(.register_extractors)
}


# ---- Kingdom name normalization ----

#' Normalize non-standard kingdom names to standard taxonomy
#'
#' NCBI uses clade-based names (Pseudomonadati, Bacillati, etc.) and viral
#' realm names (*virae). OTT uses names like Archaeplastida, Chloroplastida.
#' Maps these to standard kingdom names: Plantae, Animalia, Fungi, Bacteria,
#' Archaea, Chromista, Protozoa, Viruses.
#'
#' @param kingdom Character vector of kingdom names.
#' @return Character vector with normalized kingdom names.
#' @noRd
normalize_kingdom_names <- function(kingdom) {
  # NCBI clade -> standard kingdom
  ncbi_map <- c(
    # Bacteria (various phyla-level clades)
    Pseudomonadati   = "Bacteria",
    Bacillati        = "Bacteria",
    Fusobacteriati   = "Bacteria",
    Nanobdellati     = "Bacteria",
    Thermotogati     = "Bacteria",
    # Archaea
    Methanobacteriati  = "Archaea",
    Promethearchaeati  = "Archaea",
    Thermoproteati     = "Archaea",
    # Eukaryotes
    Metazoa         = "Animalia",
    Viridiplantae   = "Plantae"
  )

  # OTT -> standard kingdom
  ott_map <- c(
    Archaeplastida  = "Plantae",
    Chloroplastida  = "Plantae",
    Fungi           = "Fungi",
    Metazoa         = "Animalia"
  )

  # NCBI viral realms (all end in "virae")
  is_virus <- !is.na(kingdom) & grepl("virae$", kingdom, ignore.case = TRUE)

  # Apply maps
  m_ncbi <- match(kingdom, names(ncbi_map))
  hit_ncbi <- !is.na(m_ncbi)
  kingdom[hit_ncbi] <- ncbi_map[m_ncbi[hit_ncbi]]

  m_ott <- match(kingdom, names(ott_map))
  hit_ott <- !is.na(m_ott)
  kingdom[hit_ott] <- ott_map[m_ott[hit_ott]]

  kingdom[is_virus] <- "Viruses"

  kingdom
}


#' Infer kingdom from family membership
#'
#' For genera with a known family but no kingdom, look up the most common
#' kingdom among other genera in the same family that do have a kingdom.
#'
#' @param resolved data.frame with genus, kingdom, family columns.
#' @return Updated data.frame with kingdom filled where possible.
#' @noRd
infer_kingdom_from_family <- function(resolved) {
  has_kingdom <- !is.na(resolved$kingdom) & nzchar(resolved$kingdom)
  has_family  <- !is.na(resolved$family) & nzchar(resolved$family)
  needs_fill  <- !has_kingdom & has_family

  if (!any(needs_fill) || !any(has_kingdom & has_family)) return(resolved)

  # Build family -> kingdom lookup from genera that have both
  ref <- resolved[has_kingdom & has_family, , drop = FALSE]
  # For each family, take the most common kingdom (majority vote)
  fam_split <- split(ref$kingdom, ref$family)
  family_kingdom <- vapply(fam_split, function(k) {
    tab <- table(k)
    names(which.max(tab))
  }, character(1L))

  # Apply to genera that need it
  m <- match(resolved$family[needs_fill], names(family_kingdom))
  hit <- !is.na(m)
  if (any(hit)) {
    fill_idx <- which(needs_fill)[hit]
    resolved$kingdom[fill_idx] <- family_kingdom[m[hit]]
  }

  resolved
}


# ---- Classification conflict resolution ----

#' Resolve classification conflicts across backends
#'
#' Merges genera from multiple backends, preferring WoRMS > COL > WCVP >
#' Reptile DB > GBIF > Euro+Med > LCVP > ITIS > NCBI > OTT > WFO > FishBase >
#' SeaLifeBase for each classification column.
#' When the same genus appears in multiple backends, the first non-NA value
#' in priority order is used.
#'
#' @param genera_list Named list of data.frames, each with columns
#'   genus, kingdom, phylum, class, order, family.
#'   Names should be backend identifiers (e.g., "col", "gbif", "wfo").
#' @return data.frame with deduplicated genera and resolved classification.
#' @noRd
resolve_genus_classification <- function(genera_list) {
  priority <- .register_priority()

  # A registered extractor missing from the priority order would have its
  # genera dropped here without a word, so drift is an error, not a silence.
  missing <- setdiff(names(.register_extractors), priority)
  if (length(missing) > 0L) {
    stop(sprintf(
      "Backbone(s) registered in .register_extractors but absent from .register_priority(): %s",
      paste(missing, collapse = ", ")), call. = FALSE)
  }

  # Combine all genera, tagging each with its source backend
  all_rows <- lapply(priority, function(be) {
    df <- genera_list[[be]]
    if (is.null(df) || nrow(df) == 0L) return(NULL)
    df$source_backend <- be
    df
  })
  all_rows <- Filter(Negate(is.null), all_rows)
  if (length(all_rows) == 0L) return(empty_genus_df())

  combined <- do.call(rbind, all_rows)

  # Drop rows with NA or empty genus
  valid <- !is.na(combined$genus) & nzchar(combined$genus)
  combined <- combined[valid, , drop = FALSE]

  # Fold clade spellings to standard kingdoms up front. The caller normalizes
  # again afterwards and the map is idempotent, but the coherence gate below
  # compares kingdoms, and "Bacillati" and "Bacteria" are the same kingdom
  # under two names -- comparing raw strings would treat them as a conflict.
  combined$kingdom <- normalize_kingdom_names(combined$kingdom)

  # A source can contradict itself: COL files 2012 genera under two kingdoms
  # and WoRMS 1432, because a genus name may be occupied twice and a backbone
  # carries both occupants. Ordering by priority alone left the winner to
  # whichever of that source's rows happened to sort first, which handed 93
  # genera their own source's minority reading -- Pteropus, 66 species of
  # flying fox, resolved to Fungi on a single COL row against two Animalia
  # ones. Rank each row by how often its source repeats that kingdom for that
  # genus so the source's own majority speaks for it; an even split keeps the
  # incoming order, and rows recording no kingdom stay where they are.
  kg <- paste(combined$genus, combined$source_backend, combined$kingdom,
              sep = "\r")
  kg_tab <- table(kg)
  support <- as.integer(kg_tab[match(kg, names(kg_tab))])
  support[is.na(combined$kingdom) | !nzchar(combined$kingdom)] <- 0L

  # Order by genus, then priority, then within-source kingdom support, so the
  # first non-NA per genus wins via match()
  combined$priority_rank <- match(combined$source_backend, priority)
  combined <- combined[order(combined$genus, combined$priority_rank,
                             -support), ]

  genera_all <- combined$genus

  # Start with first row per genus (highest-priority backend)
  first_idx <- which(!duplicated(genera_all))
  result <- data.frame(
    genus = genera_all[first_idx],
    stringsAsFactors = FALSE
  )

  # For each classification column, take the first usable value per genus.
  fill_col <- function(vals, usable) {
    sub_genus <- genera_all[usable]
    sub_vals  <- vals[usable]
    first_hit <- which(!duplicated(sub_genus))
    sub_vals[first_hit][match(result$genus, sub_genus[first_hit])]
  }
  is_val <- function(v) !is.na(v) & nzchar(v)

  # Kingdom first: it decides which rows may speak for the ranks below it.
  result$kingdom <- fill_col(combined$kingdom, is_val(combined$kingdom))

  # A genus name can belong to two kingdoms at once -- Goodfellowia is a
  # starling and a bacterium, Verreauxia a plant and a piculet -- and a flat
  # genus index holds one answer. Resolving each rank independently let the
  # losing kingdom still fill the ranks the winner left empty, which produced
  # rows like a Plantae genus in the bird order Piciformes. A source may now
  # only fill a rank if it agrees with the resolved kingdom, or records no
  # kingdom at all (Fungorum, AlgaeBase and much of GBIF record none, and they
  # stay eligible so the fill they provide is not lost).
  win <- result$kingdom[match(genera_all, result$genus)]
  coherent <- !is_val(combined$kingdom) | is.na(win) | combined$kingdom == win

  for (col in c("phylum", "class", "order", "family")) {
    result[[col]] <- fill_col(combined[[col]], is_val(combined[[col]]) & coherent)
  }

  result
}


# ---- GBIF hierarchy walk for unresolved kingdoms ----

#' Session cache for the GBIF parent-key hierarchy walk
#'
#' Holds `gbif_hierarchy_cache` across repeated `resolve_kingdom_via_gbif()`
#' calls within one build session (loading the full GBIF taxon_id/parent_key
#' table costs several seconds).
#' @noRd
.register_env <- new.env(parent = emptyenv())


#' Resolve unknown genera to kingdom_group via GBIF parent_key traversal
#'
#' For genera where taxon_group is "unknown", walks the GBIF backbone
#' parent_key chain upward until a KINGDOM-rank row is found, then maps
#' the kingdom name to kingdom_group and taxon_group.
#'
#' This runs only during build_genus_register() -- one-time build cost.
#'
#' @param resolved data.frame with genus/kingdom_group/taxon_group columns.
#' @param gbif_path Character. Path to GBIF .vtr file.
#' @return Updated resolved data.frame.
#' @noRd
resolve_kingdom_via_gbif <- function(resolved, gbif_path) {
  if (is.null(gbif_path) || is.na(gbif_path) || !file.exists(gbif_path)) {
    return(resolved)
  }

  unknown_idx <- which(resolved$taxon_group == "unknown" |
                       resolved$kingdom_group == "unknown")
  if (length(unknown_idx) == 0L) return(resolved)

  unknown_genera <- resolved$genus[unknown_idx]
  if (length(unknown_genera) == 0L) return(resolved)

  # Load the GBIF backbone columns needed for traversal
  # taxon_id, parent_key, taxon_rank, canonical_name -- subset to minimize memory
  if (is.null(.register_env$gbif_hierarchy_cache)) {
    gbif_df <- tryCatch({
      vectra::tbl(gbif_path) |>
        vectra::select(taxon_id, parent_key, taxon_rank, canonical_name) |>
        vectra::collect()
    }, error = function(e) NULL)
    if (is.null(gbif_df) || nrow(gbif_df) == 0L) return(resolved)
    .register_env$gbif_hierarchy_cache <- gbif_df
  } else {
    gbif_df <- .register_env$gbif_hierarchy_cache
  }

  # Build hash maps for fast traversal
  id_to_parent    <- stats::setNames(gbif_df$parent_key,    gbif_df$taxon_id)
  id_to_rank      <- stats::setNames(gbif_df$taxon_rank,    gbif_df$taxon_id)
  id_to_canonical <- stats::setNames(gbif_df$canonical_name, gbif_df$taxon_id)

  # Kingdom name -> kingdom_group mapping
  kingdom_group_map <- c(
    "Plantae"   = "plantae",
    "Fungi"     = "fungi",
    "Animalia"  = "animalia",
    "Chromista" = "chromista",
    "Protozoa"  = "protozoa",
    "Bacteria"  = "bacteria",
    "Archaea"   = "archaea",
    "Viruses"   = "viruses"
  )
  kingdom_taxon_map <- c(
    "Plantae"   = "unknown",
    "Fungi"     = "fungus",
    "Animalia"  = "animal",
    "Chromista" = "unknown",
    "Protozoa"  = "unknown",
    "Bacteria"  = "unknown",
    "Archaea"   = "unknown",
    "Viruses"   = "unknown"
  )

  # Vectorized parent_key traversal -- repeated joins instead of a per-genus loop.
  # Start: match each unknown genus name to its GBIF taxon_id.
  genus_rows <- gbif_df[!is.na(gbif_df$taxon_rank) & gbif_df$taxon_rank == "GENUS" &
                          gbif_df$canonical_name %in% unknown_genera, ,
                        drop = FALSE]

  if (nrow(genus_rows) == 0L) return(resolved)

  # Walk ALL genus entries (including duplicates across kingdoms).
  # After the walk, pick the most common kingdom per genus name to avoid
  # misclassification from homonymous genera (e.g., Escherichia in both
  # Bacteria and Animalia).
  work <- data.frame(
    genus_name   = genus_rows$canonical_name,
    current_id   = genus_rows$taxon_id,
    kingdom_name = NA_character_,
    stringsAsFactors = FALSE
  )

  # pre-build lookup vectors once
  id_to_parent    <- stats::setNames(gbif_df$parent_key,    gbif_df$taxon_id)
  id_to_rank      <- stats::setNames(gbif_df$taxon_rank,    gbif_df$taxon_id)
  id_to_canonical <- stats::setNames(gbif_df$canonical_name, gbif_df$taxon_id)

  # iteratively hop to parent until all rows hit KINGDOM or exhaust depth
  for (step in seq_len(20L)) {
    pending <- is.na(work$kingdom_name)
    if (!any(pending)) break

    cur_ids  <- work$current_id[pending]
    cur_rank <- id_to_rank[cur_ids]

    # rows that reached KINGDOM this step
    at_kingdom <- !is.na(cur_rank) & cur_rank == "KINGDOM"
    if (any(at_kingdom)) {
      idx <- which(pending)[at_kingdom]
      work$kingdom_name[idx] <- id_to_canonical[work$current_id[idx]]
    }

    # rows still pending: hop to parent
    still_pending <- pending & is.na(work$kingdom_name)
    if (!any(still_pending)) break
    parents <- id_to_parent[work$current_id[still_pending]]
    # stop rows that hit NA parent or self-loop
    dead <- is.na(parents) | parents == work$current_id[still_pending]
    if (any(dead)) work$kingdom_name[which(still_pending)[dead]] <- "unknown_stop"
    work$current_id[still_pending] <- parents
  }

  # For genus names with multiple GBIF entries (homonyms across kingdoms),
  # pick the most common resolved kingdom per genus name.
  work$kg <- kingdom_group_map[work$kingdom_name]
  work$kg[is.na(work$kg)] <- "unknown"
  # Aggregate: for each genus, pick kingdom with most GBIF entries
  genus_split <- split(work, work$genus_name)
  best <- vapply(genus_split, function(sub) {
    tab <- table(sub$kg)
    tab <- tab[names(tab) != "unknown"]
    if (length(tab) == 0L) return("unknown")
    names(which.max(tab))
  }, character(1L))
  work <- data.frame(
    genus_name   = names(best),
    kingdom_name = NA_character_,
    stringsAsFactors = FALSE
  )
  # Map best kingdom_group back to kingdom_name for taxon_map lookup
  best_kg <- unname(best)
  kg_to_kingdom <- stats::setNames(names(kingdom_group_map), kingdom_group_map)
  work$kingdom_name <- kg_to_kingdom[best_kg]
  work$kingdom_name[is.na(work$kingdom_name)] <- "unknown_stop"

  # map kingdom names to kingdom_group / taxon_group
  kg_vec <- kingdom_group_map[work$kingdom_name]
  tg_vec <- kingdom_taxon_map[work$kingdom_name]
  kg_vec[is.na(kg_vec)] <- "unknown"
  tg_vec[is.na(tg_vec)] <- "unknown"

  # apply to resolved data.frame via match (vectorized)
  m <- match(resolved$genus[unknown_idx], work$genus_name)
  hit <- !is.na(m)
  if (any(hit)) {
    update_idx <- unknown_idx[hit]
    resolved$kingdom_group[update_idx] <- unname(kg_vec[m[hit]])
    resolved$taxon_group[update_idx]   <- unname(tg_vec[m[hit]])
    resolved$life_form[update_idx]     <-
      gsub("_", " ", unname(tg_vec[m[hit]]), fixed = TRUE)
  }

  resolved
}


# ---- Backbone .vtr path resolution ----

#' Local cache directory for backbones downloaded to build the register
#' @noRd
.register_cache_dir <- function(output_dir) file.path(output_dir, "_cache")


#' Resolve one backbone's `.vtr` path for a register build
#'
#' Preference order: (1) an explicit path in `backbone_paths`, (2) the
#' standard local build output `output/<name>/<name>.vtr` (from a prior
#' [build_backend()] run), (3) the version published in `manifest.json`,
#' downloaded into `<output_dir>/_cache/<name>.vtr`.
#'
#' @param name Character. Backend identifier.
#' @param backbone_paths Named list/character vector of explicit overrides.
#' @param output_dir Character. The register/coverage build's own output dir.
#' @param manifest Parsed manifest.json (as a list).
#' @param verbose Logical.
#' @return Character path, or `NULL` if the backbone could not be resolved.
#' @noRd
.resolve_one_backbone_path <- function(name, backbone_paths, output_dir,
                                       manifest, verbose) {
  # `[[` on a missing name errors for an atomic vector (unlike a list, where it
  # returns NULL) -- backbone_paths may be either, since build_register() feeds
  # its own resolve_register_backbone_paths() output (a named character
  # vector) straight back in as `backbone_paths` for both sub-builds. Guard
  # explicitly so a name absent from either representation is a clean miss.
  explicit <- if (name %in% names(backbone_paths)) backbone_paths[[name]] else NULL
  if (!is.null(explicit) && nzchar(explicit)) {
    if (!file.exists(explicit)) {
      stop(sprintf("backbone_paths[['%s']] does not exist: %s",
                   name, explicit), call. = FALSE)
    }
    if (verbose) message(sprintf("  [%s] Using supplied path: %s", name, explicit))
    return(explicit)
  }

  local_path <- file.path("output", name, paste0(name, ".vtr"))
  if (file.exists(local_path)) {
    if (verbose) message(sprintf("  [%s] Using local build: %s", name, local_path))
    return(local_path)
  }

  entry <- manifest$backends[[name]]
  if (is.null(entry) || is.null(entry$full_url)) {
    if (verbose) {
      message(sprintf(
        "  [%s] Not found locally (output/%s/%s.vtr) or in manifest, skipping.",
        name, name, name))
    }
    return(NULL)
  }

  cache_dir <- .register_cache_dir(output_dir)
  dest_name <- paste0(name, ".vtr")
  cache_path <- file.path(cache_dir, dest_name)
  # Mirrors download_curl_file()'s own cache-hit test so the log message
  # reflects what it will actually do (skip vs. fetch).
  cached_already <- file.exists(cache_path) && file.size(cache_path) > 100L
  if (verbose) {
    message(if (cached_already) {
      sprintf("  [%s] Using cached download: %s", name, cache_path)
    } else {
      sprintf("  [%s] Downloading v%s (%.0f MB) from manifest...",
             name, entry$latest %||% "?", (entry$full_size %||% 0) / 1048576)
    })
  }
  dest <- download_curl_file(entry$full_url, cache_dir, dest_name)

  meta_dest <- paste0(tools::file_path_sans_ext(dest), ".meta")
  if (!file.exists(meta_dest)) {
    writeLines(c(
      paste0("backend=", name),
      paste0("version=", entry$latest %||% ""),
      paste0("download_date=", format(Sys.Date())),
      paste0("url=", entry$source_url %||% entry$full_url),
      paste0("nrow=", entry$nrow %||% "")
    ), meta_dest)
  }
  dest
}


#' Resolve `.vtr` paths for every backbone the register unions
#'
#' @param backbone_paths Named list/character vector or `NULL`. Explicit
#'   `.vtr` path overrides (`name = path`) for one or more of
#'   [register_backbones()]. Backbones not named here resolve from
#'   `output/<name>/<name>.vtr` or `manifest.json`.
#' @param output_dir Character. The register/coverage build's own output
#'   directory (used for the download cache subdirectory).
#' @param manifest_path Character. Path to `manifest.json`.
#' @param verbose Logical.
#' @return Named character vector of resolved paths, one per backbone that
#'   could be resolved (possibly not all of [register_backbones()]).
#' @noRd
resolve_register_backbone_paths <- function(backbone_paths = NULL,
                                            output_dir = "output/register",
                                            manifest_path = "manifest/manifest.json",
                                            verbose = TRUE) {
  manifest <- if (file.exists(manifest_path)) {
    jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  } else {
    list(backends = list())
  }
  backbone_paths <- backbone_paths %||% list()

  names_needed <- register_backbones()
  paths <- stats::setNames(vector("list", length(names_needed)), names_needed)
  for (nm in names_needed) {
    paths[[nm]] <- .resolve_one_backbone_path(nm, backbone_paths, output_dir,
                                              manifest, verbose)
  }
  paths <- paths[!vapply(paths, is.null, logical(1L))]
  unlist(paths, use.names = TRUE)
}


# ---- Build functions ----

#' Build the genus register from a fixed backbone set
#'
#' Reads genus-rank rows from each of [register_backbones()] (resolved via
#' `resolve_register_backbone_paths()`), unions them, resolves classification
#' conflicts, normalizes non-standard kingdom names, assigns `kingdom_group` /
#' `taxon_group` / `life_form` (via `assign_life_form()`, falling back to a
#' GBIF parent-key hierarchy walk for genera still unresolved), and writes
#' `genus_register.vtr`.
#'
#' Every call unions the same fixed backbone set, so the built register is
#' reproducible: it does not depend on which backbones happen to be installed
#' on the machine running the build (contrast taxify's runtime
#' `taxify_build_register()`, which unions whichever backbones the caller has
#' installed).
#'
#' @param backbone_paths Named list/character vector or `NULL`. Explicit
#'   `.vtr` path overrides for one or more of [register_backbones()].
#'   Backbones not named here resolve from `output/<name>/<name>.vtr` (a
#'   prior [build_backend()] run) or from the version published in
#'   `manifest.json`.
#' @param output_dir Character or `NULL`. Output directory. Default
#'   `output/genus_register`.
#' @param version Character or `NULL`. Version string for the build. If
#'   `NULL`, defaults to the current `YYYY.MM`.
#' @param manifest_path Character. Path to `manifest.json`, used to resolve
#'   backbones not present locally.
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#' @return Path to `genus_register.vtr` (invisibly).
#' @export
build_genus_register <- function(backbone_paths = NULL, output_dir = NULL,
                                 version = NULL,
                                 manifest_path = "manifest/manifest.json",
                                 verbose = TRUE) {
  output_dir <- output_dir %||% file.path("output", "genus_register")
  version <- version %||% format(Sys.Date(), "%Y.%m")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  paths <- resolve_register_backbone_paths(backbone_paths, output_dir,
                                           manifest_path, verbose)
  if (length(paths) == 0L) {
    stop(paste0(
      "No backbone .vtr files resolved (checked output/<name>/<name>.vtr, ",
      manifest_path, ", and backbone_paths)."
    ), call. = FALSE)
  }

  genera_list <- list()
  for (nm in names(paths)) {
    if (verbose) message(sprintf("  [%s] Extracting genus rows...", nm))
    genera_list[[nm]] <- .register_extractors[[nm]](paths[[nm]])
    if (verbose) {
      message(sprintf("  [%s] %d genera found.", nm, nrow(genera_list[[nm]])))
    }
  }

  if (verbose) {
    message("Resolving classification conflicts (WoRMS > COL > WCVP > ...)...")
  }
  resolved <- resolve_genus_classification(genera_list)

  # Normalize non-standard kingdom names from NCBI and OTT
  resolved$kingdom <- normalize_kingdom_names(resolved$kingdom)

  if (verbose) message("Assigning life forms...")
  lf <- assign_life_form(resolved$family, resolved$kingdom)
  resolved$kingdom_group <- lf$kingdom_group
  resolved$taxon_group   <- lf$taxon_group
  resolved$life_form     <- lf$life_form

  # Second pass: use GBIF parent_key traversal to resolve remaining unknowns
  gbif_path <- unname(paths["gbif"])
  n_unknown_before <- sum(resolved$taxon_group == "unknown", na.rm = TRUE)
  if (n_unknown_before > 0L && length(gbif_path) > 0L && !is.na(gbif_path)) {
    if (verbose) {
      message(sprintf(
        "  Resolving %d unknown genera via GBIF hierarchy...", n_unknown_before
      ))
    }
    resolved <- resolve_kingdom_via_gbif(resolved, gbif_path)
    n_unknown_after <- sum(resolved$taxon_group == "unknown", na.rm = TRUE)
    if (verbose) {
      message(sprintf(
        "  %d resolved; %d still unknown.",
        n_unknown_before - n_unknown_after, n_unknown_after
      ))
    }
  }

  # Reconcile kingdom <-> kingdom_group:
  # 1. Where kingdom is set (from WoRMS/COL), override kingdom_group/taxon_group
  # 2. Where kingdom is NA, backfill from kingdom_group
  kingdom_to_group <- c(
    Plantae = "plantae", Animalia = "animalia", Fungi = "fungi",
    Chromista = "chromista", Protozoa = "protozoa", Bacteria = "bacteria",
    Archaea = "archaea", Viruses = "viruses"
  )
  kingdom_to_taxon <- c(
    Plantae = "unknown", Animalia = "animal", Fungi = "fungus",
    Chromista = "unknown", Protozoa = "unknown", Bacteria = "unknown",
    Archaea = "unknown", Viruses = "unknown"
  )
  has_kingdom <- !is.na(resolved$kingdom) & resolved$kingdom %in% names(kingdom_to_group)
  # Override kingdom_group from authoritative kingdom (WoRMS/COL win over GBIF walk)
  resolved$kingdom_group[has_kingdom] <- kingdom_to_group[resolved$kingdom[has_kingdom]]
  # Only override taxon_group if it was wrongly set (not from life_form assignment)
  wrong_taxon <- has_kingdom & resolved$taxon_group != kingdom_to_taxon[resolved$kingdom]
  # But don't override specific taxon_groups (angiosperm, fern, etc.) with generic "unknown"
  wrong_taxon <- wrong_taxon &
    !(resolved$taxon_group %in% c("angiosperm", "gymnosperm", "fern", "lycophyte",
                                   "moss", "liverwort", "hornwort", "green_alga",
                                   "red_alga", "brown_alga", "diatom", "lichen",
                                   "oomycete", "slime_mould", "fungus"))
  resolved$taxon_group[wrong_taxon] <- kingdom_to_taxon[resolved$kingdom[wrong_taxon]]

  # Backfill kingdom from kingdom_group where still NA
  kingdom_from_group <- c(
    plantae = "Plantae", animalia = "Animalia", fungi = "Fungi",
    chromista = "Chromista", protozoa = "Protozoa", bacteria = "Bacteria",
    archaea = "Archaea", viruses = "Viruses"
  )
  needs_kingdom <- is.na(resolved$kingdom) & resolved$kingdom_group %in% names(kingdom_from_group)
  resolved$kingdom[needs_kingdom] <- kingdom_from_group[resolved$kingdom_group[needs_kingdom]]

  # Family-based kingdom inference: for genera with known family but no kingdom,
  # inherit kingdom from other genera in the same family (runs last, after GBIF
  # walk and kingdom backfill have maximized the number of known kingdoms)
  n_no_kingdom <- sum(is.na(resolved$kingdom))
  if (n_no_kingdom > 0L) {
    resolved <- infer_kingdom_from_family(resolved)
    n_filled_fam <- n_no_kingdom - sum(is.na(resolved$kingdom))
    if (verbose && n_filled_fam > 0L) {
      message(sprintf("  Family-based kingdom inference filled %d genera.", n_filled_fam))
    }
  }

  # Pattern-based kingdom inference for remaining unknowns:
  # - Viral families (*viridae, *satellitidae, *viricetes) -> Viruses
  # - Candidatus prefix -> Bacteria (provisional prokaryote names)
  still_na <- is.na(resolved$kingdom)
  has_fam <- still_na & !is.na(resolved$family) & nzchar(resolved$family)
  viral_fam <- has_fam & grepl("viridae$|satellitidae$|viricetes$", resolved$family)
  resolved$kingdom[viral_fam] <- "Viruses"

  # Genus names containing "virus" (common ICTV naming) -> Viruses
  viral_name <- still_na & !viral_fam & !is.na(resolved$genus) &
    grepl("virus$|virus ", resolved$genus, ignore.case = TRUE)
  resolved$kingdom[viral_name] <- "Viruses"

  candidatus <- still_na & !is.na(resolved$genus) &
    grepl("^Candidatus ", resolved$genus)
  resolved$kingdom[candidatus] <- "Bacteria"

  n_pattern <- sum(viral_fam | viral_name | candidatus)
  if (verbose && n_pattern > 0L) {
    message(sprintf("  Pattern-based kingdom inference filled %d genera.", n_pattern))
  }

  # Sync kingdom_group/taxon_group for all newly filled genera
  newly_filled <- !is.na(resolved$kingdom) & resolved$kingdom_group == "unknown"
  if (any(newly_filled)) {
    kg_mapped <- kingdom_to_group[resolved$kingdom[newly_filled]]
    resolved$kingdom_group[newly_filled] <- ifelse(is.na(kg_mapped), "unknown", kg_mapped)
  }

  # Reorder columns
  resolved <- resolved[, c("genus", "kingdom", "phylum", "class", "order",
                            "family", "kingdom_group", "taxon_group",
                            "life_form"), drop = FALSE]
  resolved <- resolved[order(resolved$genus), , drop = FALSE]
  rownames(resolved) <- NULL

  vtr_path <- file.path(output_dir, "genus_register.vtr")
  vectra::write_vtr(resolved, vtr_path)
  vectra::create_index(vtr_path, "genus")

  meta_path <- paste0(tools::file_path_sans_ext(vtr_path), ".meta")
  writeLines(c(
    "backend=genus_register",
    paste0("version=", version),
    paste0("download_date=", format(Sys.time(), "%Y-%m-%d")),
    paste0("download_timestamp=", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
    paste0("url=derived from: ", paste(names(paths), collapse = ", ")),
    paste0("nrow=", nrow(resolved))
  ), meta_path)

  if (verbose) {
    message(sprintf("Genus register written: %s (%d genera)", vtr_path,
                    nrow(resolved)))
  }
  invisible(vtr_path)
}


#' Build the backend coverage table
#'
#' For each backbone in [register_backbones()] (resolved via
#' `resolve_register_backbone_paths()`), extracts the genus list and writes a
#' long-format `backend_coverage.vtr` recording which genera are covered by
#' which backend and at what version/date.
#'
#' @inheritParams build_genus_register
#' @param output_dir Character or `NULL`. Output directory. Default
#'   `output/backend_coverage`.
#' @return Path to `backend_coverage.vtr` (invisibly).
#' @export
build_backend_coverage <- function(backbone_paths = NULL, output_dir = NULL,
                                   version = NULL,
                                   manifest_path = "manifest/manifest.json",
                                   verbose = TRUE) {
  output_dir <- output_dir %||% file.path("output", "backend_coverage")
  version <- version %||% format(Sys.Date(), "%Y.%m")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  paths <- resolve_register_backbone_paths(backbone_paths, output_dir,
                                           manifest_path, verbose)
  if (length(paths) == 0L) {
    stop(paste0(
      "No backbone .vtr files resolved (checked output/<name>/<name>.vtr, ",
      manifest_path, ", and backbone_paths)."
    ), call. = FALSE)
  }

  coverage_rows <- list()
  for (nm in names(paths)) {
    meta <- read_meta(paste0(tools::file_path_sans_ext(paths[[nm]]), ".meta"))
    be_version <- if (!is.null(meta) && "version" %in% names(meta) &&
                      nzchar(meta[["version"]])) meta[["version"]] else NA_character_
    date_added <- if (!is.null(meta) && "download_date" %in% names(meta)) {
      meta[["download_date"]]
    } else {
      NA_character_
    }

    if (verbose) {
      message(sprintf("  [%s] Building coverage (v%s)...", nm, be_version %||% "?"))
    }
    genera_df <- .register_extractors[[nm]](paths[[nm]])
    genera <- unique(genera_df$genus)
    genera <- genera[!is.na(genera) & nzchar(genera)]

    if (length(genera) == 0L) next

    coverage_rows[[nm]] <- data.frame(
      genus      = genera,
      backend    = nm,
      version    = be_version,
      date_added = date_added,
      stringsAsFactors = FALSE
    )
    if (verbose) message(sprintf("  [%s] %d genera.", nm, length(genera)))
  }

  if (length(coverage_rows) == 0L) {
    stop("No coverage rows produced from any resolved backbone.", call. = FALSE)
  }

  coverage <- do.call(rbind, coverage_rows)
  coverage <- coverage[order(coverage$genus, coverage$backend), , drop = FALSE]
  rownames(coverage) <- NULL

  vtr_path <- file.path(output_dir, "backend_coverage.vtr")
  vectra::write_vtr(coverage, vtr_path)
  vectra::create_index(vtr_path, "genus")

  meta_path <- paste0(tools::file_path_sans_ext(vtr_path), ".meta")
  writeLines(c(
    "backend=backend_coverage",
    paste0("version=", version),
    paste0("download_date=", format(Sys.time(), "%Y-%m-%d")),
    paste0("download_timestamp=", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
    paste0("url=derived from: ", paste(names(paths), collapse = ", ")),
    paste0("nrow=", nrow(coverage))
  ), meta_path)

  if (verbose) {
    message(sprintf("Backend coverage written: %s (%d rows)",
                    vtr_path, nrow(coverage)))
  }
  invisible(vtr_path)
}


#' Build both genus_register.vtr and backend_coverage.vtr
#'
#' Convenience wrapper that calls [build_genus_register()] and
#' [build_backend_coverage()] in sequence. Resolves every backbone's `.vtr`
#' exactly once (via `resolve_register_backbone_paths()`, downloading into
#' `output/genus_register/_cache` when needed) and passes the fully resolved
#' paths to both builders as `backbone_paths`, so a manifest-downloaded
#' backbone (several hundred MB each, ~5 GB across the set) is fetched once
#' for both artifacts rather than once per builder.
#'
#' @inheritParams build_genus_register
#' @return Named list with paths to `genus_register.vtr` and
#'   `backend_coverage.vtr` (invisibly).
#' @export
build_register <- function(backbone_paths = NULL, version = NULL,
                           manifest_path = "manifest/manifest.json",
                           verbose = TRUE) {
  version <- version %||% format(Sys.Date(), "%Y.%m")

  resolved <- resolve_register_backbone_paths(
    backbone_paths, file.path("output", "genus_register"), manifest_path,
    verbose
  )

  if (verbose) message("=== Building genus register ===")
  reg_path <- build_genus_register(backbone_paths = resolved,
                                   version = version,
                                   manifest_path = manifest_path,
                                   verbose = verbose)

  if (verbose) message("\n=== Building backend coverage ===")
  cov_path <- build_backend_coverage(backbone_paths = resolved,
                                     version = version,
                                     manifest_path = manifest_path,
                                     verbose = verbose)

  invisible(list(register = reg_path, coverage = cov_path))
}
