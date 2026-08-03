# Carabid (ground beetle) trait sources.
#
# Ground beetles are among the most intensively sampled insect groups in
# ecology, and until now taxify's only beetle source was `saproxylic`, which is
# restricted to the deadwood guild. The obvious source, carabids.org (Homburg
# et al. 2014), covers 3,400+ western Palaearctic species but has been down for
# maintenance and carries no licence beyond a bare copyright line, so it cannot
# be built or redistributed. These parsers cover what is lawfully reachable.
#
# A genealogy warning that matters for the trait registry: several European
# carabid trait compilations draw their body-size and wing values FROM
# carabids.org. Chowdhury, the Alpine Dryad compilation and the Eberswalde
# monitoring file are therefore not independent of one another, and agreement
# between them is not corroboration. Only specimen-measured sources (the
# Finnish Finand & Kotze deposit) provide an independent calibration.


#' Parse the Chowdhury et al. 2025 German carabid trend + trait table
#'
#' Supporting Information Data S1 of "Widespread Decline of Ground Beetles in
#' Germany" (Diversity and Distributions, doi:10.1111/ddi.70112). One row per
#' species: 383 rows, of which 382 carry trait values (*Ocys tachysoides* has
#' none), which makes this by a wide margin the largest lawfully reachable
#' carabid trait block.
#'
#' Trait values are kept verbatim rather than normalized here. The source
#' vocabularies are small and self-describing, and taxify's cross-source trait
#' registry is where a vocabulary is mapped, so the `.vtr` keeps what the
#' authors wrote and the registry decides what it means.
#'
#' Read off the file rather than the paper: `trophicLevel` has three values
#' (Predator 274, Herbivore 80, Omnivore 28) and no mycetophage category, and
#' `habitatPref` has seven classes, not eight.
#'
#' Alongside the traits the table carries the paper's own result, a modelled
#' two-year occupancy trend per species. Nothing else in taxify holds a
#' national population trend, so those columns are kept.
#'
#' @param path Character. Path to the downloaded `.xlsx`.
#' @return data.frame with `canonical_name` plus trait and trend columns.
#' @export
parse_chowdhury <- function(path) {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop("Parsing the Chowdhury table requires the 'openxlsx2' package.",
         call. = FALSE)
  }
  d <- openxlsx2::wb_to_df(path, sheet = 1)

  need <- c("species", "wings", "trophicLevel", "meanSize", "habitatPref")
  miss <- setdiff(need, names(d))
  if (length(miss) > 0L) {
    stop(sprintf("Chowdhury table missing expected column(s): %s",
                 paste(miss, collapse = ", ")), call. = FALSE)
  }

  chr <- function(cn) {
    if (!cn %in% names(d)) return(NA_character_)
    v <- trimws(as.character(d[[cn]]))
    v[!nzchar(v) | v == "NA"] <- NA_character_
    v
  }
  num <- function(cn) {
    if (!cn %in% names(d)) return(NA_real_)
    suppressWarnings(as.numeric(trimws(as.character(d[[cn]]))))
  }

  out <- data.frame(
    canonical_name     = .to_utf8(trimws(as.character(d$species))),
    body_length_mm     = num("meanSize"),
    wing_morph         = chr("wings"),
    trophic_level      = chr("trophicLevel"),
    habitat_pref       = chr("habitatPref"),
    red_list_germany   = chr("RL_D"),
    red_list_iucn      = chr("RL_IUCN"),
    threat_status      = chr("thrt_status"),
    occupancy_trend    = num("mean_trend"),
    trend_status       = chr("trend_status"),
    trend_significance = chr("significance_status"),
    stringsAsFactors   = FALSE
  )

  .trait_finalize(out)
}


#' Parse the Finand & Kotze Helsinki urban-forest carabid trait table
#'
#' `Species_traits_zenodo.xlsx` from the Zenodo deposit behind Finand & Kotze's
#' urban forest fragmentation study: 34 species from 25 remnant forests in
#' Helsinki, with body length measured from the specimens the authors caught
#' rather than compiled from a database.
#'
#' That independence is the reason to carry a table this small. Every other
#' reachable carabid trait source traces back to carabids.org (Homburg et al.
#' 2014), so none of them can check another. This one can: its body lengths run
#' at a median ratio of 0.9946 against arthropod_traits over 28 shared species
#' (IQR 0.985-1.008) with only 7.1 percent exactly equal, and at ~1.00 against
#' the Chowdhury table over 14. Values such as 6.066667 mm are means over
#' individuals, not a rounded lookup.
#'
#' `Body_length` is a whole-body length, not the elytra length the name might
#' suggest: elytra would sit near 0.6 of these values, and the published body
#' lengths agree directly (Cychrus caraboides 16.9 mm against 14-19 in the
#' literature, Carabus glabratus 28.6 against 22-34).
#'
#' The wing column is the same three-state trait the German table carries. On
#' the 25 species they share there is no long-versus-short disagreement at all,
#' but seven species the German table calls dimorphic get a definite morph
#' here, because this records the morph of the beetles actually caught in
#' Helsinki while the German table states the species' capacity.
#'
#' @param path Character. Path to `Species_traits_zenodo.xlsx`.
#' @return data.frame with `canonical_name` plus trait columns.
#' @export
parse_finand <- function(path) {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop("Parsing the Finand table requires the 'openxlsx2' package.",
         call. = FALSE)
  }
  d <- openxlsx2::wb_to_df(path, sheet = 1)

  need <- c("SpeciesBIS", "Body_length", "Feeding", "Habitat", "Wings")
  miss <- setdiff(need, names(d))
  if (length(miss) > 0L) {
    stop(sprintf("Finand table missing expected column(s): %s",
                 paste(miss, collapse = ", ")), call. = FALSE)
  }

  chr <- function(cn) {
    if (!cn %in% names(d)) return(NA_character_)
    v <- trimws(as.character(d[[cn]]))
    v[!nzchar(v) | v == "NA"] <- NA_character_
    v
  }

  # The single-letter codes are expanded here rather than left raw, because
  # unlike the German table's words these are opaque on their own. The
  # expansions follow the paper's trait description, and the wing codes are
  # confirmed against the German table over the 25 shared species.
  wings <- c(LW = "Long-winged", SW = "Short-winged", DM = "Dimorphic")
  feed  <- c(C = "Carnivore", O = "Omnivore")
  hab   <- c(F = "Forest", G = "Generalist", O = "Open")
  drou  <- c(M = "Mesic", H = "Humid", X = "Dry")

  out <- data.frame(
    canonical_name   = .to_utf8(trimws(as.character(d$SpeciesBIS))),
    body_length_mm   = suppressWarnings(as.numeric(d$Body_length)),
    wing_morph       = unname(wings[chr("Wings")]),
    feeding_type     = unname(feed[chr("Feeding")]),
    habitat_pref     = unname(hab[chr("Habitat")]),
    moisture_pref    = unname(drou[chr("Drought")]),
    stringsAsFactors = FALSE
  )

  .trait_finalize(out)
}


#' Parse the Eberswalde long-term carabid monitoring trait table
#'
#' `EWcarabids1999-2022_species_trends_traits.csv` from the Leuphana deposit
#' behind the 24-year drought study of Weiss, von Wehrden and Linde: 27 species
#' from 13 forest plots near Eberswalde, semicolon-delimited with German
#' decimal commas.
#'
#' Two of its columns are carabids.org verbatim, which the deposit's own README
#' states outright ("size = mean size (body length, mm) ... Source: Homburg et
#' al. (2014) / carabids.org", likewise wings and latitude). The data confirm
#' it: over the 19 species shared with the Chowdhury table every single size is
#' exactly equal and every wing class agrees. They are kept because a door
#' should surface what its source carries, and they are excluded from the
#' cross-source trait registry, where they would double-count one lineage.
#'
#' What belongs to this deposit alone: a 24-year local abundance trend, a
#' sensitivity to the 72-month SPEI drought index, and a feeding guild refined
#' by the authors' field observations to name the prey (Gastropoda,
#' Collembola). The humidity preference is Sustek's (2004) 1-8 scale, an
#' independent source.
#'
#' @param path Character. Path to the downloaded `.csv`.
#' @return data.frame with `canonical_name` plus trait columns.
#' @export
parse_eberswalde <- function(path) {
  d <- utils::read.csv(path, sep = ";", dec = ",", stringsAsFactors = FALSE,
                       fileEncoding = "UTF-8-BOM")
  if (!"species" %in% names(d)) {
    d <- utils::read.csv(path, sep = ";", dec = ",", stringsAsFactors = FALSE)
  }
  need <- c("species", "wings", "size", "trend2")
  miss <- setdiff(need, names(d))
  if (length(miss) > 0L) {
    stop(sprintf("Eberswalde table missing expected column(s): %s",
                 paste(miss, collapse = ", ")), call. = FALSE)
  }

  chr <- function(cn) {
    if (!cn %in% names(d)) return(NA_character_)
    v <- trimws(as.character(d[[cn]]))
    v[!nzchar(v) | v == "NA"] <- NA_character_
    v
  }
  num <- function(cn) {
    if (!cn %in% names(d)) return(NA_real_)
    suppressWarnings(as.numeric(trimws(as.character(d[[cn]]))))
  }

  out <- data.frame(
    canonical_name   = .to_utf8(trimws(as.character(d$species))),
    body_length_mm   = num("size"),
    wing_morph       = chr("wings"),
    feeding_guild    = chr("feeding_guild"),
    humidity_pref    = num("humidity_preference_SUSTEK"),
    range_centre_lat = num("latitude"),
    abundance_total  = num("sum"),
    abundance_trend  = chr("trend2"),
    drought_effect   = chr("spei_eff2"),
    stringsAsFactors = FALSE
  )

  .trait_finalize(out)
}


#' Parse the Imageomics NEON ground-beetle image measurements
#'
#' `BeetleMeasurements.csv` from the 2018-NEON-beetles deposit: 39,064
#' measurements taken from images of pinned individuals across 86 species at 30
#' NEON sites, reduced here to a per-species median.
#'
#' These are North American species measured from specimens, so they are
#' independent of the carabids.org lineage every European carabid source
#' shares, and they reach a fauna the European sources do not cover at all.
#'
#' The source measures the elytron rather than the whole animal and stores
#' centimetres in `dist_cm`, so values are converted to the millimetres the
#' elytra_length trait uses. Sanity holds within the source and against it: the
#' length-to-width ratio has a median of 1.79 (range 1.34-2.25), the shape of a
#' carabid elytron, and Carabus nemoralis reads 14.87 mm against the 23.4 mm
#' body length the Finnish specimens give, a ratio of 0.64.
#'
#' `lying_flat = "No"` marks a specimen photographed at an angle, where a
#' projected length is foreshortened; those measurement pairs are dropped.
#'
#' @param path Character. Path to `BeetleMeasurements.csv`.
#' @return data.frame with `canonical_name` plus elytra measurements.
#' @export
parse_imageomics_neon <- function(path) {
  d <- utils::read.csv(path, stringsAsFactors = FALSE)
  need <- c("scientificName", "structure", "dist_cm", "lying_flat")
  miss <- setdiff(need, names(d))
  if (length(miss) > 0L) {
    stop(sprintf("NEON measurements missing expected column(s): %s",
                 paste(miss, collapse = ", ")), call. = FALSE)
  }

  d <- d[!is.na(d$dist_cm) & d$dist_cm > 0, , drop = FALSE]
  d <- d[trimws(d$lying_flat) == "Yes", , drop = FALSE]
  d$name <- .to_utf8(trimws(as.character(d$scientificName)))
  # Unidentified material carries a placeholder rather than a binomial.
  d <- d[nzchar(d$name) & !grepl("sp\\.$|^Carabidae", d$name), , drop = FALSE]

  per <- function(what) {
    s <- d[trimws(d$structure) == what, , drop = FALSE]
    if (nrow(s) == 0L) return(NULL)
    a <- stats::aggregate(list(v = s$dist_cm * 10),
                          by = list(canonical_name = s$name), FUN = stats::median)
    a$n <- stats::aggregate(list(n = s$dist_cm),
                            by = list(canonical_name = s$name), FUN = length)$n
    a
  }
  el <- per("ElytraLength")
  ew <- per("ElytraWidth")

  out <- data.frame(canonical_name = sort(unique(d$name)), stringsAsFactors = FALSE)
  out$elytra_length_mm <- if (is.null(el)) NA_real_ else
    el$v[match(out$canonical_name, el$canonical_name)]
  out$elytra_width_mm <- if (is.null(ew)) NA_real_ else
    ew$v[match(out$canonical_name, ew$canonical_name)]
  out$measurement_n <- if (is.null(el)) NA_integer_ else
    el$n[match(out$canonical_name, el$canonical_name)]

  .trait_finalize(out)
}
