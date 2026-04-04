# ---- GRIIS enrichment: Global Register of Introduced and Invasive Species ----
#
# Source: GRIIS combined checklist (GBIF hosted dataset)
# DOI: 10.15468/6jcu0q
# License: CC BY 4.0
# ~23k name x country combinations across 196 countries
#
# GRIIS data is available as a DwC-A from GBIF:
# https://www.gbif.org/dataset/b351a324-77c4-41c9-a909-f30b77c4571b

# GRIIS Country Compendium V1.0 from Zenodo (single CSV, all 196 countries)
.griis_url <- "https://zenodo.org/records/6348164/files/GRIIS%20-%20Country%20Compendium%20V1_0.csv?download=1"

download_griis <- function(dest = tempdir()) {
  csv_path <- file.path(dest, "GRIIS_Country_Compendium_V1_0.csv")
  if (!file.exists(csv_path)) {
    message("Downloading GRIIS Country Compendium V1.0...")
    curl::curl_download(.griis_url, csv_path, quiet = FALSE)
  }
  csv_path
}

build_griis <- function(output_dir) {
  csv_path <- download_griis()

  message("Reading GRIIS data...")
  df <- read.csv(csv_path, stringsAsFactors = FALSE)

  # Find species name column
  name_col <- intersect(
    names(df),
    c("scientificName", "canonicalName", "species", "taxonName",
      "Scientific.Name", "accepted_name")
  )
  if (length(name_col) == 0L) {
    name_col <- grep("scien|canon|species|taxon|name", names(df),
                     ignore.case = TRUE, value = TRUE)
    if (length(name_col) == 0L) name_col <- names(df)[1]
  }
  # Prefer "species" column (canonical binomial) over scientificName (has authorship)
  if ("species" %in% names(df)) {
    name_col <- "species"
  } else {
    name_col <- name_col[1]
  }

  # GRIIS Compendium columns:
  #   countryCode_alpha2 — 2-letter ISO country code
  #   isInvasive — "Invasive", "NULL", etc.
  #   establishmentMeans — "ALIEN", "NATIVE", etc.
  cc_col <- if ("countryCode_alpha2" %in% names(df)) {
    "countryCode_alpha2"
  } else {
    cc <- grep("countryCode|country_code", names(df), ignore.case = TRUE, value = TRUE)
    if (length(cc) > 0L) cc[1] else NULL
  }

  country_codes <- if (!is.null(cc_col)) {
    toupper(trimws(df[[cc_col]]))
  } else {
    rep(NA_character_, nrow(df))
  }

  # Derive invasive status from isInvasive + establishmentMeans
  is_inv <- if ("isInvasive" %in% names(df)) tolower(trimws(df$isInvasive)) else rep("null", nrow(df))
  estab <- if ("establishmentMeans" %in% names(df)) tolower(trimws(df$establishmentMeans)) else rep("", nrow(df))

  invasive_status <- ifelse(
    is_inv == "invasive", "invasive",
    ifelse(estab %in% c("alien", "introduced"), "introduced",
    ifelse(estab == "native", "native", "introduced"))
  )
  # GRIIS is a register of introduced/invasive species — default to "introduced"
  # if only establishmentMeans == "ALIEN" and isInvasive == "NULL"

  out <- data.frame(
    canonical_name  = trimws(df[[name_col]]),
    country_code    = country_codes,
    invasive_status = invasive_status,
    stringsAsFactors = FALSE
  )

  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0, ]
  out <- out[!is.na(out$country_code) & nchar(out$country_code) == 2L, ]
  # Deduplicate per name x country (keep first)
  out <- out[!duplicated(paste(out$canonical_name, out$country_code)), ]

  # Resolve source names against all 7 backends
  out <- resolve_enrichment_names(out, group_cols = "country_code")

  vtr_path <- file.path(output_dir, "griis.vtr")
  build_enrichment_vtr(
    out, vtr_path,
    name       = "griis",
    version    = "2026.04",
    source_url = .griis_url,
    source_doi = "10.15468/6jcu0q",
    license    = "CC BY 4.0",
    attribution = "Pagad S et al. GRIIS - Global Register of Introduced and Invasive Species.",
    group_col  = "country_code"
  )
}
