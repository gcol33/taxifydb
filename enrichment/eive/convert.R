# ---- EIVE enrichment: Dengler et al. 2023 (Zenodo, CC BY 4.0) ----
#
# Source: EIVE 1.0 — Ecological Indicator Values for Europe
# DOI: 10.3897/VCS.98324
# Zenodo: https://zenodo.org/records/7525670
# ~14.5k European vascular plant species

# Zenodo record 7534792 hosts the EIVE 1.0 supplementary data (XLSX)
.eive_url <- "https://zenodo.org/records/7534792/files/EIVE_Paper_1.0_SM_08.xlsx?download=1"

download_eive <- function(dest = tempdir()) {
  path <- file.path(dest, "EIVE_1.0.xlsx")
  if (!file.exists(path)) {
    message("Downloading EIVE 1.0...")
    curl::curl_download(.eive_url, path, quiet = FALSE)
  }
  path
}

build_eive <- function(output_dir) {
  xlsx_path <- download_eive()

  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("readxl is required to read EIVE xlsx. Install with: install.packages('readxl')")
  }

  # Data is in sheet "mainTable"; sheet 1 is Readme
  df <- as.data.frame(
    readxl::read_excel(xlsx_path, sheet = "mainTable"),
    stringsAsFactors = FALSE
  )

  # Columns: TaxonConcept, UUID, TaxonRank, AccordingTo,
  #   EIVEres-M, EIVEres-M.nw3, EIVEres-M.n,
  #   EIVEres-N, ..., EIVEres-R, ..., EIVEres-L, ..., EIVEres-T, ...
  # We want TaxonConcept as name and EIVEres-{L,T,M,R,N} as indicators

  name_col <- "TaxonConcept"
  if (!name_col %in% names(df)) {
    name_col <- names(df)[1]
  }

  # Find the main indicator columns (not .nw3 or .n variants)
  find_col <- function(patterns) {
    for (p in patterns) {
      m <- grep(p, names(df), value = TRUE)
      if (length(m) > 0L) return(m[1])
    }
    NA_character_
  }

  light_col <- find_col(c("^EIVEres-L$", "^EIVEres.L$"))
  temp_col  <- find_col(c("^EIVEres-T$", "^EIVEres.T$"))
  moist_col <- find_col(c("^EIVEres-M$", "^EIVEres.M$"))
  react_col <- find_col(c("^EIVEres-R$", "^EIVEres.R$"))
  nutr_col  <- find_col(c("^EIVEres-N$", "^EIVEres.N$"))

  indicator_cols <- c(light_col, temp_col, moist_col, react_col, nutr_col)
  missing <- is.na(indicator_cols)
  if (any(missing)) {
    message("Warning: Could not find indicator columns: ",
            paste(c("light", "temperature", "moisture", "reaction", "nutrients")[missing],
                  collapse = ", "))
    message("Available columns: ", paste(names(df), collapse = ", "))
  }

  out <- data.frame(
    canonical_name = trimws(df[[name_col]]),
    stringsAsFactors = FALSE
  )

  # Add each indicator if found, coercing to numeric
  safe_num <- function(x) suppressWarnings(as.numeric(x))

  if (!is.na(light_col)) out$light       <- safe_num(df[[light_col]])
  if (!is.na(temp_col))  out$temperature  <- safe_num(df[[temp_col]])
  if (!is.na(moist_col)) out$moisture     <- safe_num(df[[moist_col]])
  if (!is.na(react_col)) out$reaction     <- safe_num(df[[react_col]])
  if (!is.na(nutr_col))  out$nutrients    <- safe_num(df[[nutr_col]])

  # Drop NA names and duplicates
  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0, ]
  out <- out[!duplicated(out$canonical_name), ]

  # Resolve source names against all 7 backends
  out <- resolve_enrichment_names(out)

  vtr_path <- file.path(output_dir, "eive.vtr")
  build_enrichment_vtr(
    out, vtr_path,
    name       = "eive",
    version    = "1.0",
    source_url = .eive_url,
    source_doi = "10.3897/VCS.98324",
    license    = "CC BY 4.0",
    attribution = "Dengler J et al. (2023) EIVE 1.0 -- a standardized set of Ecological Indicator Values for Europe. Vegetation Classification and Survey 4:7-29."
  )
}
