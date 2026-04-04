# ---- PanTHERIA enrichment: Jones et al. 2009 (Ecological Archives, CC0) ----
#
# Source: PanTHERIA — species-level database of life history, ecology,
#         and geography of extant and recently extinct mammals
# DOI: 10.1890/08-1494.1
# ~5416 mammal species

# ESA Ecological Archives direct download
.pantheria_url <- "https://esapubs.org/archive/ecol/E090/184/PanTHERIA_1-0_WR05_Aug2008.txt"

download_pantheria <- function(dest = tempdir()) {
  path <- file.path(dest, "PanTHERIA.txt")
  if (!file.exists(path)) {
    message("Downloading PanTHERIA...")
    curl::curl_download(.pantheria_url, path, quiet = FALSE)
  }
  path
}

build_pantheria <- function(output_dir) {
  txt_path <- download_pantheria()

  df <- read.delim(txt_path, stringsAsFactors = FALSE, na.strings = c("-999", "-999.00"))

  # PanTHERIA uses MSW05_Binomial for species name
  name_col <- intersect(
    names(df),
    c("MSW05_Binomial", "MSW93_Binomial", "Scientific_Name")
  )
  if (length(name_col) == 0L) name_col <- names(df)[1]
  else name_col <- name_col[1]

  # Map columns — PanTHERIA uses long descriptive names with numbers
  find_col <- function(patterns) {
    for (p in patterns) {
      m <- grep(p, names(df), ignore.case = TRUE, value = TRUE)
      if (length(m) > 0L) return(m[1])
    }
    NULL
  }

  safe_num <- function(col_name) {
    if (is.null(col_name)) return(rep(NA_real_, nrow(df)))
    x <- suppressWarnings(as.numeric(df[[col_name]]))
    x[x == -999] <- NA_real_  # extra safety
    x
  }

  out <- data.frame(
    canonical_name  = trimws(df[[name_col]]),
    body_mass_g     = safe_num(find_col(c("AdultBodyMass_g", "X5.1_AdultBodyMass",
                                          "BodyMass"))),
    longevity_mo    = safe_num(find_col(c("MaxLongevity_m", "X17.1_MaxLongevity"))),
    litter_size     = safe_num(find_col(c("LitterSize", "X15.1_LitterSize"))),
    gestation_d     = safe_num(find_col(c("GestationLen_d", "X9.1_GestationLen"))),
    weaning_d       = safe_num(find_col(c("WeaningAge_d", "X25.1_WeaningAge"))),
    home_range_km2  = safe_num(find_col(c("HomeRange_km2", "X22.1_HomeRange",
                                          "HomeRange_Indiv_km2"))),
    diet_breadth    = safe_num(find_col(c("DietBreadth", "X6.2_TrophicLevel",
                                          "diet_breadth"))),
    habitat_breadth = safe_num(find_col(c("HabitatBreadth", "X12.2_HabitatBreadth",
                                          "habitat_breadth"))),
    stringsAsFactors = FALSE
  )

  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0, ]
  out <- out[!duplicated(out$canonical_name), ]

  # Resolve source names against all 7 backends
  out <- resolve_enrichment_names(out)

  vtr_path <- file.path(output_dir, "pantheria.vtr")
  build_enrichment_vtr(
    out, vtr_path,
    name       = "pantheria",
    version    = "1.0",
    source_url = .pantheria_url,
    source_doi = "10.1890/08-1494.1",
    license    = "CC0",
    attribution = "Jones KE et al. (2009) PanTHERIA: a species-level database of life history, ecology, and geography of extant and recently extinct mammals. Ecology 90:2648."
  )
}
