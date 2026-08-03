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
