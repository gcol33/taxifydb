# ---- Diaz traits enrichment: Diaz et al. 2022 (TRY, CC BY 3.0) ----
#
# Source: The global spectrum of plant form and function (TRY File Archive)
# DOI: 10.1038/s41586-022-05606-z (paper), data from TRY
#
# This uses the openly accessible species means from the paper's
# supplementary material, which contains seed mass and plant height
# for ~46k species.
#
# The supplementary data is available from the Nature paper or TRY.
# If the direct URL changes, the file can be obtained from:
#   https://www.try-db.org/TryWeb/Data.php (File Archive, open datasets)

# Direct download from Nature supplementary (Excel file)
.diaz_url <- "https://static-content.springer.com/esm/art%3A10.1038%2Fs41586-022-05606-z/MediaObjects/41586_2022_5606_MOESM3_ESM.xlsx"

download_diaz <- function(dest = tempdir()) {
  xlsx_path <- file.path(dest, "Diaz_2022_traits.xlsx")
  if (!file.exists(xlsx_path)) {
    message("Downloading Diaz et al. 2022 trait data...")
    curl::curl_download(.diaz_url, xlsx_path, quiet = FALSE)
  }
  xlsx_path
}

build_diaz_traits <- function(output_dir) {
  path <- download_diaz()

  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("readxl is required to read Diaz supplementary xlsx. ",
         "Install with: install.packages('readxl')")
  }

  # The supplementary file may have multiple sheets
  sheets <- readxl::excel_sheets(path)
  message("Available sheets: ", paste(sheets, collapse = ", "))

  # Read the main data sheet (usually the first with data)
  df <- readxl::read_excel(path, sheet = 1L)
  df <- as.data.frame(df, stringsAsFactors = FALSE)

  # Find species name column
  name_col <- intersect(
    names(df),
    c("Species", "species", "SpecName", "Taxon", "Scientific_name",
      "AccSpeciesName")
  )
  if (length(name_col) == 0L) {
    # Try partial matching
    name_col <- grep("spec|taxon|name", names(df), ignore.case = TRUE,
                     value = TRUE)
    if (length(name_col) == 0L) name_col <- names(df)[1]
  }
  name_col <- name_col[1]

  # Find seed mass and height columns
  find_col <- function(patterns) {
    for (p in patterns) {
      m <- grep(p, names(df), ignore.case = TRUE, value = TRUE)
      if (length(m) > 0L) return(m[1])
    }
    NULL
  }

  seed_col <- find_col(c("seed.*mass", "Seed.mass", "sm_", "SeedMass",
                         "Diaspore.mass"))
  height_col <- find_col(c("plant.*height", "Height", "PlantHeight",
                           "Hmax", "height_m"))

  safe_num <- function(col_name) {
    if (is.null(col_name)) return(rep(NA_real_, nrow(df)))
    suppressWarnings(as.numeric(df[[col_name]]))
  }

  out <- data.frame(
    canonical_name = trimws(df[[name_col]]),
    seed_mass_mg   = safe_num(seed_col),
    plant_height_m = safe_num(height_col),
    stringsAsFactors = FALSE
  )

  # If seed mass is in grams, convert to mg
  if (!is.null(seed_col) && !all(is.na(out$seed_mass_mg))) {
    median_val <- median(out$seed_mass_mg, na.rm = TRUE)
    if (median_val < 1) {
      # Likely in grams, convert to mg
      message("Seed mass appears to be in grams, converting to mg")
      out$seed_mass_mg <- out$seed_mass_mg * 1000
    }
  }

  # If height is in cm, convert to m
  if (!is.null(height_col) && !all(is.na(out$plant_height_m))) {
    median_val <- median(out$plant_height_m, na.rm = TRUE)
    if (median_val > 100) {
      # Likely in cm, convert to m
      message("Height appears to be in cm, converting to m")
      out$plant_height_m <- out$plant_height_m / 100
    }
  }

  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0, ]
  # Keep rows that have at least one non-NA trait
  has_data <- !is.na(out$seed_mass_mg) | !is.na(out$plant_height_m)
  out <- out[has_data, ]
  out <- out[!duplicated(out$canonical_name), ]

  # Resolve source names against all 7 backends
  out <- resolve_enrichment_names(out)

  vtr_path <- file.path(output_dir, "diaz_traits.vtr")
  build_enrichment_vtr(
    out, vtr_path,
    name       = "diaz_traits",
    version    = "2022.1",
    source_url = .diaz_url,
    source_doi = "10.1038/s41586-022-05606-z",
    license    = "CC BY 3.0",
    attribution = "Diaz S et al. (2022) The global spectrum of plant form and function: enhanced species-level trait data. Nature."
  )
}
