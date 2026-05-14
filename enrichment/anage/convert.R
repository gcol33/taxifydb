# ---- AnAge enrichment: Animal Ageing and Longevity Database ----
#
# Source: Human Ageing Genomic Resources, AnAge build 15
# DOI: 10.1111/j.1420-9101.2009.01783.x
# License: CC BY (per HAGR terms)
# Reference: Tacutu R et al. (2018) Human Ageing Genomic Resources:
# new and updated databases. Nucleic Acids Research 46:D1083-D1090.

.anage_url <- "https://genomics.senescence.info/species/dataset.zip"

download_anage <- function(dest = tempdir()) {
  zip_path <- file.path(dest, "anage.zip")
  if (!file.exists(zip_path) || file.size(zip_path) < 1000L) {
    message("Downloading AnAge dataset...")
    h <- curl::new_handle()
    curl::handle_setopt(h, followlocation = TRUE, maxredirs = 10L)
    curl::handle_setheaders(h, "User-Agent" = "taxify-backbones/1.0")
    curl::curl_download(.anage_url, zip_path, handle = h)
  }
  ext_dir <- file.path(dest, "anage_extracted")
  if (!dir.exists(ext_dir)) {
    dir.create(ext_dir, recursive = TRUE)
    utils::unzip(zip_path, exdir = ext_dir)
  }
  files <- list.files(ext_dir, pattern = "(?i)anage.*\\.txt$",
                      full.names = TRUE, recursive = TRUE)
  if (!length(files)) {
    stop(sprintf("No anage*.txt found in %s. Contents: %s",
                 ext_dir,
                 paste(list.files(ext_dir, recursive = TRUE),
                       collapse = ", ")), call. = FALSE)
  }
  files[1L]
}

build_anage <- function(output_dir) {
  path <- download_anage()

  message("Reading AnAge data table...")
  df <- read.delim(path, stringsAsFactors = FALSE, quote = "")

  # Build canonical_name from Genus + Species, falling back to a combined column
  genus_col <- intersect(names(df), c("Genus", "genus"))
  sp_col    <- intersect(names(df), c("Species", "species"))
  if (length(genus_col) && length(sp_col)) {
    canonical <- trimws(paste(df[[genus_col[1L]]], df[[sp_col[1L]]]))
  } else {
    name_col <- intersect(names(df),
                          c("Scientific_name", "ScientificName",
                            "scientific_name", "Common_name", "Binomial"))
    if (!length(name_col)) name_col <- names(df)[1L]
    canonical <- trimws(df[[name_col[1L]]])
  }

  find_col <- function(...) {
    for (p in c(...)) {
      m <- grep(p, names(df), ignore.case = TRUE, value = TRUE)
      if (length(m)) return(m[1L])
    }
    NULL
  }
  safe_num <- function(col) {
    if (is.null(col)) return(rep(NA_real_, nrow(df)))
    x <- suppressWarnings(as.numeric(df[[col]]))
    x[x < 0] <- NA_real_
    x
  }

  out <- data.frame(
    canonical_name         = canonical,
    max_longevity_yr       = safe_num(find_col(
      "Maximum.longevity..yrs.", "Maximum_longevity_yrs",
      "Maximum.longevity", "MaxLongevity", "max_longevity")),
    body_mass_g            = safe_num(find_col(
      "Body.mass..g.", "Body_mass_g", "Adult.weight..g.",
      "AdultWeight", "body_mass")),
    metabolic_rate_w       = safe_num(find_col(
      "Metabolic.rate..W.", "Metabolic_rate_W", "MetabolicRate",
      "metabolic_rate")),
    female_maturity_d      = safe_num(find_col(
      "Female.maturity..days.", "Female_maturity_days",
      "FemaleMaturity", "female_maturity")),
    male_maturity_d        = safe_num(find_col(
      "Male.maturity..days.", "Male_maturity_days",
      "MaleMaturity", "male_maturity")),
    gestation_incubation_d = safe_num(find_col(
      "Gestation.Incubation..days.", "Gestation_Incubation_days",
      "GestationIncubation", "gestation_incubation")),
    litter_size            = safe_num(find_col(
      "Litter.Clutch.size", "Litter_Clutch_size",
      "LitterClutchSize", "litter_clutch_size")),
    birth_mass_g           = safe_num(find_col(
      "Birth.weight..g.", "Birth_weight_g",
      "BirthWeight", "birth_weight")),
    growth_rate            = safe_num(find_col(
      "Growth.rate..1.days.", "Growth_rate",
      "GrowthRate", "growth_rate")),
    temperature_k          = safe_num(find_col(
      "Temperature..K.", "Temperature_K",
      "Temperature", "temperature")),
    stringsAsFactors = FALSE
  )

  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]
  trait_cols <- setdiff(names(out), "canonical_name")
  has_data <- rowSums(!is.na(out[, trait_cols, drop = FALSE])) > 0L
  out <- out[has_data, ]
  out <- out[!duplicated(out$canonical_name), ]

  message(sprintf("  %d AnAge species with trait data", nrow(out)))

  # Resolve source names against all 7 backends
  out <- resolve_enrichment_names(out)

  vtr_path <- file.path(output_dir, "anage.vtr")
  build_enrichment_vtr(
    out, vtr_path,
    name        = "anage",
    version     = "15.0",
    source_url  = .anage_url,
    source_doi  = "10.1111/j.1420-9101.2009.01783.x",
    license     = "CC BY",
    attribution = paste0(
      "Tacutu R et al. (2018) Human Ageing Genomic Resources: ",
      "new and updated databases. Nucleic Acids Research 46:D1083-D1090."
    )
  )
}
