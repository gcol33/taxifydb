# Trait parsers (wave 2 heavier: Tree of Sex, World Checklist of Useful Plant
# Species, BIEN). Each returns a data.frame with canonical_name + trait columns.


# ---- Tree of Sex -----------------------------------------------------------

#' Pick a Tree of Sex column by name pattern, excluding source/notes columns
#' @noRd
.tos_pick <- function(df, pattern) {
  nm <- names(df)
  cand <- nm[grepl(pattern, nm, ignore.case = TRUE) &
               !grepl("^\\s*(source|notes)", nm, ignore.case = TRUE)]
  if (length(cand)) {
    x <- trimws(as.character(df[[cand[1L]]]))
    x[x == "" | x == "NA"] <- NA_character_
    x
  } else {
    rep(NA_character_, nrow(df))
  }
}

#' Raw Tree of Sex column names consumed by the curated block
#'
#' The same name-pattern selection `.tos_pick()` uses, but returning the picked
#' raw column names (plus the Genus/species pair folded into `canonical_name`).
#' Passed as `used=` to `.append_all_cols()` so a curated column whose raw source
#' name differs from its output name (e.g. `environmental sex determination`
#' -> `environmental_sd`) is not re-appended under its raw name.
#' @noRd
.tos_used <- function(df) {
  patterns <- c("sexual.?system", "karyotype", "genotypic", "molecular.?basis",
                "selfing", "environmental", "haplodiploidy")
  nm <- names(df)
  picked <- vapply(patterns, function(p) {
    cand <- nm[grepl(p, nm, ignore.case = TRUE) &
                 !grepl("^\\s*(source|notes)", nm, ignore.case = TRUE)]
    if (length(cand)) cand[1L] else NA_character_
  }, character(1L))
  c("Genus", "species", picked[!is.na(picked)])
}

#' Parse the Tree of Sex database (plants + vertebrates + invertebrates)
#'
#' Row-binds the three sub-tables on a common core keyed on `Genus species`,
#' adding a `taxon_group` discriminator. Group-specific traits (selfing for
#' plants, environmental sex determination for vertebrates, haplodiploidy for
#' invertebrates) are kept and are NA for the other groups.
#'
#' @param path Character. Directory holding the three Tree of Sex CSVs.
#' @return data.frame with canonical_name + sex-determination traits.
#' @export
parse_tree_of_sex <- function(path) {
  files <- list.files(path, pattern = "\\.csv$", full.names = TRUE,
                      recursive = TRUE)
  grp_of <- function(f) {
    b <- tolower(basename(f))
    if (grepl("^plant", b)) "plants"
    else if (grepl("^vert", b)) "vertebrates"
    else if (grepl("^invert", b)) "invertebrates"
    else NA_character_
  }
  cols <- c("canonical_name", "taxon_group", "sexual_system", "karyotype",
            "genotypic", "molecular_basis", "selfing", "environmental_sd",
            "haplodiploidy")
  parts <- list()
  for (f in files) {
    g <- grp_of(f)
    if (is.na(g)) next
    df <- utils::read.csv(f, check.names = FALSE, stringsAsFactors = FALSE,
                          na.strings = c("NA", ""), fileEncoding = "UTF-8-BOM")
    if (!all(c("Genus", "species") %in% names(df))) next
    out <- data.frame(
      canonical_name  = trimws(paste(df$Genus, df$species)),
      taxon_group     = g,
      sexual_system   = .tos_pick(df, "sexual.?system"),
      karyotype       = .tos_pick(df, "karyotype"),
      genotypic       = .tos_pick(df, "genotypic"),
      molecular_basis = .tos_pick(df, "molecular.?basis"),
      selfing         = .tos_pick(df, "selfing"),
      environmental_sd = .tos_pick(df, "environmental"),
      haplodiploidy   = .tos_pick(df, "haplodiploidy"),
      stringsAsFactors = FALSE
    )
    out <- out[cols]
    out <- .append_all_cols(out, df, out$canonical_name, used = .tos_used(df))
    parts[[length(parts) + 1L]] <- out
  }
  if (!length(parts)) stop("No Tree of Sex CSVs found.", call. = FALSE)
  # Each sub-table now carries its own group-specific extra columns, so bind on
  # the union of columns, filling absent ones with NA.
  all_cols <- unique(unlist(lapply(parts, names)))
  parts <- lapply(parts, function(p) {
    miss <- setdiff(all_cols, names(p))
    for (m in miss) p[[m]] <- NA
    p[all_cols]
  })
  combined <- do.call(rbind, parts)
  .trait_finalize(combined)
}


# ---- World Checklist of Useful Plant Species (PDF) -------------------------

#' Reflow a two-column PDF into reading-order lines
#'
#' `pdftools::pdf_text` reads two-column pages across both columns, scrambling
#' record order. This rebuilds reading order from token coordinates
#' (`pdf_data`): split each page at its horizontal midpoint, then read the left
#' column top-to-bottom followed by the right column, grouping tokens into lines
#' by their y-position.
#' @noRd
.pdf_reflow_2col <- function(path) {
  pages <- pdftools::pdf_data(path)
  out <- character(0)
  for (d in pages) {
    if (is.null(d) || nrow(d) == 0L) next
    mid <- min(d$x) + (max(d$x + d$width) - min(d$x)) / 2
    for (side in c("L", "R")) {
      dd <- d[if (side == "L") d$x < mid else d$x >= mid, , drop = FALSE]
      if (nrow(dd) == 0L) next
      grp <- round(dd$y / 3)
      ord <- order(grp, dd$x)
      dd <- dd[ord, , drop = FALSE]
      grp <- grp[ord]
      for (g in unique(grp)) {
        out <- c(out, trimws(paste(dd$text[grp == g], collapse = " ")))
      }
    }
  }
  out
}

#' Parse the World Checklist of Useful Plant Species PDF
#'
#' The checklist is published only as a typeset PDF. Each species is a name line
#' followed by a data line `<IPNI LSID> | <space-separated use codes> | [CWR] |
#' [sources]`. The ten Level-1 use codes are exploded into boolean columns and a
#' crop-wild-relative flag is set from the optional `CWR` token.
#'
#' @param path Character. Path to the WCUP PDF.
#' @return data.frame with canonical_name + use-category booleans.
#' @export
parse_useful_plants <- function(path) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Package 'pdftools' is required to parse the Useful Plants PDF.",
         call. = FALSE)
  }
  lines <- .pdf_reflow_2col(path)

  use_codes <- c(AF = "animal_food", EU = "environmental_uses",
                 FU = "fuels", GS = "gene_sources", HF = "human_food",
                 IF = "invertebrate_food", MA = "materials",
                 ME = "medicines", PO = "poisons", SU = "social_uses")

  is_data <- grepl("^[0-9]+-[0-9]+\\s*\\|", lines)
  idx <- which(is_data)

  n <- length(idx)
  canonical <- character(n)
  cwr <- integer(n)

  prev_nonempty <- function(i) {
    j <- i - 1L
    while (j >= 1L && !nzchar(lines[j])) j <- j - 1L
    if (j >= 1L) lines[j] else ""
  }

  # First pass: extract each record's name, CWR flag, and use codes, collecting
  # the union of codes actually present. The ten curated codes are the full
  # documented WCUP Level-1 scheme, but any further code-shaped token present is
  # kept as its own boolean column so no source code is dropped.
  rec_codes <- vector("list", n)
  for (k in seq_len(n)) {
    i <- idx[k]
    sp_line <- prev_nonempty(i)
    toks <- strsplit(sp_line, "\\s+")[[1L]]
    toks <- toks[nzchar(toks)]
    canonical[k] <- if (length(toks) >= 2L) paste(toks[1L], toks[2L]) else NA
    fields <- trimws(strsplit(lines[i], "|", fixed = TRUE)[[1L]])
    cwr[k] <- as.integer(any(toupper(fields) == "CWR"))
    codes_field <- if (length(fields) >= 2L) fields[2L] else ""
    codes <- toupper(strsplit(codes_field, "\\s+")[[1L]])
    codes <- codes[grepl("^[A-Z]{2,}$", codes) & codes != "CWR"]
    rec_codes[[k]] <- codes
  }

  present <- unique(unlist(rec_codes))
  extra_codes <- sort(setdiff(present, names(use_codes)))
  ordered_codes <- c(names(use_codes), extra_codes)

  # Curated codes keep their documented names; extra codes become their own
  # (sanitized) boolean column, de-duplicated against the curated names.
  col_names <- character(length(ordered_codes))
  taken <- "canonical_name"
  for (ci in seq_along(ordered_codes)) {
    code <- ordered_codes[ci]
    base <- if (code %in% names(use_codes)) use_codes[[code]]
            else .sanitize_col(code)
    nm <- .uniq_colname(base, taken)
    col_names[ci] <- nm
    taken <- c(taken, nm)
  }

  code_mat <- matrix(0L, nrow = n, ncol = length(ordered_codes),
                     dimnames = list(NULL, ordered_codes))
  for (k in seq_len(n)) {
    hit <- intersect(rec_codes[[k]], ordered_codes)
    if (length(hit)) code_mat[k, hit] <- 1L
  }

  out <- data.frame(canonical_name = canonical, stringsAsFactors = FALSE)
  for (ci in seq_along(ordered_codes)) {
    out[[col_names[ci]]] <- code_mat[, ordered_codes[ci]]
  }
  out$crop_wild_relative <- cwr
  out <- out[!is.na(out$canonical_name) & nzchar(out$canonical_name), ]
  out[!duplicated(out$canonical_name), , drop = FALSE]
}


# ---- BIEN (built via the BIEN R package at build time) ---------------------

#' Parse BIEN trait data (queried via the BIEN R package)
#'
#' Like fishbase via rfishbase, BIEN is queried directly: a single bulk pull of
#' the selected traits across all species (one query per trait list), filtered
#' to public-access records, then pivoted to one row per species (numeric by
#' median, categorical by mode). The `path` argument is unused (kept for a
#' uniform parser interface).
#'
#' @param path Unused.
#' @return data.frame with canonical_name + plant traits.
#' @export
parse_bien <- function(path) {
  if (!requireNamespace("BIEN", quietly = TRUE)) {
    stop("Package 'BIEN' is required to build the BIEN enrichment.",
         call. = FALSE)
  }
  spec <- list(
    plant_height_m       = list(trait = "whole plant height", type = "num"),
    max_plant_height_m   = list(trait = "maximum whole plant height",
                                type = "num"),
    dbh_cm               = list(trait = "diameter at breast height (1.3 m)",
                                type = "num"),
    sla_mm2_mg           = list(trait = "leaf area per leaf dry mass",
                                type = "num"),
    leaf_area_mm2        = list(trait = "leaf area", type = "num"),
    leaf_dry_mass_mg     = list(trait = "leaf dry mass", type = "num"),
    leaf_n_per_dry_mass  = list(trait = "leaf nitrogen content per leaf dry mass",
                                type = "num"),
    leaf_p_per_dry_mass  = list(
      trait = "leaf phosphorus content per leaf dry mass", type = "num"),
    leaf_thickness_mm    = list(trait = "leaf thickness", type = "num"),
    seed_mass_mg         = list(trait = "seed mass", type = "num"),
    wood_density_g_cm3   = list(trait = "stem wood density", type = "num"),
    leaf_lifespan        = list(trait = "leaf life span", type = "num"),
    growth_form          = list(trait = "whole plant growth form", type = "cat"),
    woodiness            = list(trait = "whole plant woodiness", type = "cat"),
    dispersal_syndrome   = list(trait = "whole plant dispersal syndrome",
                                type = "cat"),
    flower_color         = list(trait = "flower color", type = "cat")
  )
  # Enumerate every BIEN trait so none is dropped; the curated spec above only
  # renames/types the 16 it references, and .pivot_species_traits(keep_all) adds
  # a column for each remaining fetched trait. If the trait catalogue is
  # unavailable, fall back to fetching just the curated 16.
  curated_traits <- unname(vapply(spec, function(s) s$trait, character(1L)))
  all_traits <- tryCatch(BIEN::BIEN_trait_list(), error = function(e) NULL)
  traits <- if (is.data.frame(all_traits)) {
    tcol <- intersect(c("trait_name", "trait"), names(all_traits))
    if (length(tcol)) unique(as.character(all_traits[[tcol[1L]]])) else character(0)
  } else if (is.character(all_traits)) {
    unique(all_traits)
  } else {
    character(0)
  }
  traits <- traits[!is.na(traits) & nzchar(trimws(traits))]
  if (!length(traits)) {
    message("  [bien] BIEN_trait_list() unavailable; fetching curated 16 only.")
    traits <- curated_traits
  } else {
    # Guarantee the curated traits are fetched even if absent from the catalogue.
    traits <- union(traits, curated_traits)
  }

  # Query one trait at a time and immediately reduce each pull to the three
  # columns we keep, freeing the full (many-column, multi-million-row) result
  # before the next trait. A single all-traits pull holds every BIEN column for
  # every record at once and exhausts memory; per-trait reduction bounds the
  # peak to one trait's records.
  long_list <- vector("list", length(traits))
  for (i in seq_along(traits)) {
    raw <- BIEN::BIEN_trait_trait(trait = traits[i])
    if (is.data.frame(raw) && nrow(raw) > 0L) {
      if ("access" %in% names(raw)) {
        raw <- raw[!is.na(raw$access) & raw$access == "public", , drop = FALSE]
      }
      if (nrow(raw) > 0L) {
        long_list[[i]] <- data.frame(
          name  = as.character(raw$scrubbed_species_binomial),
          trait = as.character(raw$trait_name),
          value = as.character(raw$trait_value),
          stringsAsFactors = FALSE
        )
      }
    }
    rm(raw); gc(FALSE)
    message(sprintf("  [bien] %d/%d %s: %s records", i, length(traits),
                    traits[i],
                    format(if (is.null(long_list[[i]])) 0L
                           else nrow(long_list[[i]]), big.mark = ",")))
  }

  long <- do.call(rbind, long_list)
  if (is.null(long) || nrow(long) == 0L) {
    stop("BIEN_trait_trait returned no data.", call. = FALSE)
  }
  .trait_finalize(.pivot_species_traits(long, spec))
}
