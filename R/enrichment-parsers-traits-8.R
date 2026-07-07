# Wave-B trait parsers (citation-only / unstated-licence tier, built with the
# maintainer's explicit go-ahead): plant chromosome counts (CCDB), mammal
# host-parasite summaries (GMPD 2.0), British & Irish plant / bryophyte
# attributes (PLANTATT, BRYOATT), and Central European clonal / bud-bank traits
# (CLO-PLA). Coded categorical source fields are kept verbatim rather than
# decoded, so no trait label is invented.


#' Parse the Chromosome Counts Database (CCDB)
#'
#' Somatic chromosome numbers for plants, taken from the CCDB `statistics`
#' service (one server-side per-taxon reduction per major plant group). Names
#' are collapsed to the binomial (infraspecific and authorship qualifiers are
#' dropped) and reduced to one row per species: the median chromosome number
#' (2n) with its minimum and maximum across the aggregated records.
#'
#' @param path Directory of per-group CCDB statistics CSVs (or a single CSV).
#' @return data.frame with canonical_name + chromosome-number traits.
#' @export
parse_ccdb <- function(path) {
  files <- if (dir.exists(path)) {
    list.files(path, pattern = "\\.csv$", full.names = TRUE)
  } else path
  if (!length(files)) stop("CCDB: no statistics CSVs found.", call. = FALSE)
  d <- do.call(rbind, lapply(files, function(f) {
    utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  }))
  # Collapse "Genus species subsp. x (Author)" to the binomial; keep the hybrid
  # epithet when the second token is a hybrid marker ("Genus x epithet").
  collapse_name <- function(z) {
    z <- z[nzchar(z)]
    if (length(z) < 2L) return(NA_character_)
    # Hybrid marker: an "x"/"X" or a single non-ASCII glyph (the multiply sign).
    hy <- z[2L] %in% c("x", "X") || grepl("[^ -~]", z[2L])
    if (hy) {
      if (length(z) >= 3L) paste(z[1:3], collapse = " ") else NA_character_
    } else {
      paste(z[1:2], collapse = " ")
    }
  }
  name <- vapply(strsplit(trimws(as.character(d$resolved_name)), "\\s+"),
                 collapse_name, character(1L))
  long <- do.call(rbind, list(
    data.frame(name = name, trait = "chromosome_number_2n",
               value = as.character(d$median),  stringsAsFactors = FALSE),
    data.frame(name = name, trait = "chromosome_2n_min",
               value = as.character(d$minimum), stringsAsFactors = FALSE),
    data.frame(name = name, trait = "chromosome_2n_max",
               value = as.character(d$max),     stringsAsFactors = FALSE)
  ))
  long <- long[nzchar(trimws(long$name)) &
                 !is.na(suppressWarnings(as.numeric(long$value))), , drop = FALSE]
  spec <- list(
    chromosome_number_2n = list(trait = "chromosome_number_2n", type = "num"),
    chromosome_2n_min    = list(trait = "chromosome_2n_min",    type = "num"),
    chromosome_2n_max    = list(trait = "chromosome_2n_max",    type = "num")
  )
  .trait_finalize(.pivot_species_traits(long, spec, keep_all = FALSE))
}


#' Parse the Global Mammal Parasite Database (GMPD 2.0)
#'
#' Host-parasite association records for wild ungulates, carnivores and
#' primates, aggregated to one row per host species. Reported traits are the
#' parasite richness (distinct parasite species), the distinct-parasite count by
#' parasite type (helminth, virus, bacteria, protozoan, arthropod, fungus,
#' prion), the mean reported prevalence and the host group.
#'
#' @param path Path to `GMPD_main.csv` (or a directory containing it).
#' @return data.frame with canonical_name (host) + parasite-summary traits.
#' @export
parse_gmpd <- function(path) {
  f <- if (dir.exists(path)) {
    list.files(path, pattern = "\\.csv$", full.names = TRUE)[1L]
  } else path
  d <- utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  # The source carries some invalid UTF-8 bytes; sanitize before any string op.
  clean <- function(x) trimws(.to_utf8(as.character(x)))
  d <- d[nzchar(clean(d$HostCorrectedName)), , drop = FALSE]

  host  <- clean(d$HostCorrectedName)
  par   <- clean(d$ParasiteCorrectedName)
  ptype <- clean(d$ParType)
  prev  <- suppressWarnings(as.numeric(d$Prevalence))
  grp   <- clean(d$Group)

  hosts <- sort(unique(host))
  idxs  <- split(seq_along(host), host)[hosts]
  n_distinct_par <- function(i, type = NULL) {
    p <- par[i]; if (!is.null(type)) p <- p[ptype[i] == type]
    length(unique(p[nzchar(p)]))
  }
  types <- c(helminth = "Helminth", virus = "Virus", bacteria = "Bacteria",
             protozoa = "Protozoa", arthropod = "Arthropod",
             fungus = "Fungus", prion = "Prion")

  out <- data.frame(canonical_name = hosts, stringsAsFactors = FALSE)
  out$parasite_richness <- vapply(idxs, n_distinct_par, integer(1L))
  for (nm in names(types)) {
    out[[paste0("n_", nm)]] <-
      vapply(idxs, function(i) n_distinct_par(i, types[[nm]]), integer(1L))
  }
  out$mean_prevalence <- vapply(idxs, function(i) {
    v <- prev[i]; v <- v[is.finite(v)]
    if (length(v)) round(mean(v), 3L) else NA_real_
  }, numeric(1L))
  out$host_group <- vapply(idxs, function(i) .cat_mode(grp[i]), character(1L))
  .trait_finalize(out)
}


#' Parse PLANTATT (attributes of British and Irish plants)
#'
#' One row per vascular-plant taxon. Retained traits are the five Ellenberg
#' indicator values (light, moisture, reaction, nitrogen, salt), maximum height
#' (cm), and the source's life-form, woodiness and native-status codes (kept
#' verbatim).
#'
#' @param path Path to `PLANTATT_19_Nov_08.xls` (or its directory).
#' @return data.frame with canonical_name + British/Irish plant attributes.
#' @export
parse_plantatt <- function(path) {
  f <- if (dir.exists(path)) {
    list.files(path, pattern = "PLANTATT.*\\.xls$", full.names = TRUE,
               ignore.case = TRUE)[1L]
  } else path
  d <- as.data.frame(
    readxl::read_excel(f, sheet = "Data", .name_repair = "minimal"),
    check.names = FALSE, stringsAsFactors = FALSE)
  if (!"Taxon name" %in% names(d)) {
    stop("PLANTATT: missing 'Taxon name' column.", call. = FALSE)
  }
  num <- function(c) suppressWarnings(as.numeric(d[[c]]))
  chr <- function(c) {
    x <- trimws(as.character(d[[c]])); x[x == "" | x == "NA"] <- NA_character_; x
  }
  out <- data.frame(
    canonical_name     = trimws(as.character(d[["Taxon name"]])),
    ellenberg_light    = num("L"),
    ellenberg_moisture = num("F"),
    ellenberg_reaction = num("R"),
    ellenberg_nitrogen = num("N"),
    ellenberg_salt     = num("S"),
    max_height_cm      = num("Hght"),
    life_form          = chr("LF1"),
    woodiness          = chr("W"),
    native_status      = chr("NS"),
    stringsAsFactors   = FALSE
  )
  .trait_finalize(out)
}


#' Parse BRYOATT (attributes of British and Irish bryophytes)
#'
#' One row per moss, liverwort or hornwort taxon. Retained traits are the five
#' Ellenberg indicator values (light, moisture, reaction, nitrogen, salt), and
#' the source's life-form, plant-group (moss / liverwort / hornwort) and status
#' codes (kept verbatim).
#'
#' @param path Path to `Bryoatt_updated_2017.xls` (or its directory).
#' @return data.frame with canonical_name + British/Irish bryophyte attributes.
#' @export
parse_bryoatt <- function(path) {
  f <- if (dir.exists(path)) {
    list.files(path, pattern = "Bryoatt.*\\.xls$", full.names = TRUE,
               recursive = TRUE, ignore.case = TRUE)[1L]
  } else path
  d <- as.data.frame(
    readxl::read_excel(f, sheet = "BRYOATT", .name_repair = "minimal"),
    check.names = FALSE, stringsAsFactors = FALSE)
  if (!"Taxon name" %in% names(d)) {
    stop("BRYOATT: missing 'Taxon name' column.", call. = FALSE)
  }
  num <- function(c) suppressWarnings(as.numeric(d[[c]]))
  chr <- function(c) {
    x <- trimws(as.character(d[[c]])); x[x == "" | x == "NA"] <- NA_character_; x
  }
  out <- data.frame(
    canonical_name     = trimws(as.character(d[["Taxon name"]])),
    ellenberg_light    = num("L"),
    ellenberg_moisture = num("F"),
    ellenberg_reaction = num("R"),
    ellenberg_nitrogen = num("N"),
    ellenberg_salt     = num("S"),
    life_form          = chr("LF1"),
    plant_group        = chr("ML"),
    status             = chr("Stat"),
    stringsAsFactors   = FALSE
  )
  .trait_finalize(out)
}


#' Parse CLO-PLA (clonal and bud-bank traits of the Central European flora)
#'
#' One row per species for 29 traits of clonal growth, bud banks and lifespan.
#' Names are collapsed to the binomial (authorship and infraspecific rank are
#' dropped, hybrid markers kept), so infraspecific records are aggregated to the
#' species. Numeric traits (growth-form flags, bud-bank sizes/depths,
#' persistence, offspring, spread, clonal index) are reduced by median; the
#' nominal coded traits (branching, cyclicity, clonal-growth-organ type,
#' regenerative-bud position and role) by mode. Coded fields are kept verbatim
#' as in the source; consult the CLO-PLA metadata to decode them.
#'
#' @param path Path to `CLO-PLA-traits.txt` (or a directory containing it).
#' @return data.frame with canonical_name + CLO-PLA clonal / bud-bank traits.
#' @export
parse_clopla <- function(path) {
  f <- if (dir.exists(path)) {
    list.files(path, pattern = "CLO-PLA-traits\\.txt$", full.names = TRUE,
               recursive = TRUE)[1L]
  } else path
  if (is.na(f) || !nzchar(f)) {
    stop("CLO-PLA: 'CLO-PLA-traits.txt' not found.", call. = FALSE)
  }
  # The source is Windows-1252 encoded and wraps Species_name in literal double
  # quotes (a doubled-quote artefact); read as latin1, strip the wrapping quote.
  d <- utils::read.delim(f, sep = "\t", quote = "\"", stringsAsFactors = FALSE,
                         check.names = FALSE, fileEncoding = "latin1",
                         na.strings = c("", "NA"))
  raw <- .to_utf8(trimws(gsub('^"+|"+$', "", trimws(as.character(d$Species_name)))))

  # Collapse "Genus species (Author)" to the binomial; keep a hybrid epithet
  # ("Genus x epithet"); drop infrageneric aggregates ("Genus sect. X").
  collapse_name <- function(z) {
    z <- z[nzchar(z)]
    if (length(z) < 2L) return(NA_character_)
    if (grepl("^(sect|subsect|ser|subser|agg)\\.$", z[2L])) return(NA_character_)
    hy <- z[2L] %in% c("x", "X") ||
      (nchar(z[2L]) == 1L && grepl("[^ -~]", z[2L]))
    if (hy && length(z) >= 3L) paste(z[1:3], collapse = " ")
    else paste(z[1:2], collapse = " ")
  }
  name <- vapply(strsplit(raw, "\\s+"), collapse_name, character(1L))

  num_cols <- c("woody", "annual", "perennialnonclonal", "monocarpic",
                "polycarpic", "clonal", "Primaryroot", "BB0", "BB0_mn10",
                "BB_gtmn10", "BB0R", "BB0_mn10R", "BB_gtmn10R", "BBsize",
                "BBdepth", "BBRsize", "BBRdepth", "persistence", "offspring",
                "offspring_wsmall", "spread", "clonalindex", "dispersibility",
                "Rsprouter")
  cat_cols <- c("branching", "cyclicity", "finalCGO", "PositionRB", "RoleRB")

  mk <- function(cols) do.call(rbind, lapply(cols, function(cn) {
    data.frame(name = name, trait = cn, value = as.character(d[[cn]]),
               stringsAsFactors = FALSE)
  }))
  long <- rbind(mk(num_cols), mk(cat_cols))
  long <- long[nzchar(trimws(long$name)) & !is.na(long$value) &
                 nzchar(trimws(long$value)), , drop = FALSE]

  spec <- c(
    stats::setNames(
      lapply(num_cols, function(c) list(trait = c, type = "num")), num_cols),
    stats::setNames(
      lapply(cat_cols, function(c) list(trait = c, type = "cat")), cat_cols)
  )
  .trait_finalize(.pivot_species_traits(long, spec, keep_all = FALSE))
}
