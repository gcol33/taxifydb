# ---- Woodiness enrichment: Zanne et al. 2014 (Dryad, CC0) ----
#
# Source: GlobalWoodinessDatabase.csv from Dryad
# DOI: 10.5061/dryad.63q27
# ~50k species: woody, herbaceous, or variable

.woodiness_url <- "https://datadryad.org/api/v2/datasets/doi%3A10.5061%2Fdryad.63q27"

download_woodiness <- function(dest = tempdir()) {
  path <- file.path(dest, "GlobalWoodinessDatabase.csv")
  if (file.exists(path)) return(path)

  message("Downloading Zanne et al. 2014 woodiness database...")

  # Dryad blocks plain curl. Use rdryad if available, else download.file
  # with browser-like headers.
  zip_path <- file.path(dest, "dryad_woodiness.zip")

  if (requireNamespace("rdryad", quietly = TRUE)) {
    files <- rdryad::dryad_download("doi:10.5061/dryad.63q27", dest = dest)
    csv <- list.files(dest, pattern = "(?i)woodiness.*\\.csv$",
                      full.names = TRUE, recursive = TRUE)
    if (length(csv) > 0L) {
      file.copy(csv[1], path, overwrite = TRUE)
      return(path)
    }
  }

  # Fallback: use download.file with libcurl method
  # Resolve file download URL from Dryad API
  tryCatch({
    handle <- curl::new_handle()
    curl::handle_setheaders(handle,
      "User-Agent" = "R/4.5 taxify-backbones",
      "Accept" = "*/*"
    )
    # Try the Dryad v2 API to get download link
    api_url <- "https://datadryad.org/api/v2/datasets/doi%3A10.5061%2Fdryad.63q27/download"
    curl::curl_download(api_url, zip_path, handle = handle)
    utils::unzip(zip_path, exdir = dest)
    csv <- list.files(dest, pattern = "(?i)(woodiness|GlobalWood).*\\.csv$",
                      full.names = TRUE, recursive = TRUE)
    if (length(csv) > 0L) {
      file.copy(csv[1], path, overwrite = TRUE)
      return(path)
    }
  }, error = function(e) {
    message("Dryad API download failed: ", conditionMessage(e))
  })

  # Last resort: direct file stream URL with download.file
  tryCatch({
    utils::download.file(
      "https://datadryad.org/stash/downloads/file_stream/26188",
      path, mode = "wb", quiet = FALSE
    )
    if (file.exists(path) && file.size(path) > 1000) return(path)
  }, error = function(e) {
    message("Direct download failed: ", conditionMessage(e))
  })

  if (!file.exists(path) || file.size(path) < 1000) {
    stop(
      "Could not download Zanne woodiness data programmatically.\n",
      "Dryad blocks automated downloads. Please download manually:\n",
      "  1. Visit https://datadryad.org/stash/dataset/doi:10.5061/dryad.63q27\n",
      "  2. Download the CSV file\n",
      "  3. Place it at: ", path
    )
  }
  path
}

build_woodiness <- function(output_dir) {
  csv_path <- download_woodiness()

  df <- read.csv(csv_path, stringsAsFactors = FALSE)

  # Expected columns: gs (genus+species binomial), woodiness
  # Standardize column names
  if ("gs" %in% names(df)) {
    names(df)[names(df) == "gs"] <- "canonical_name"
  } else if ("Species" %in% names(df)) {
    names(df)[names(df) == "Species"] <- "canonical_name"
  } else {
    # Try first column as name
    names(df)[1] <- "canonical_name"
  }

  # Find woodiness column (may be "woodiness", "Woodiness", "wood1", etc.)
  wood_col <- grep("wood", names(df), ignore.case = TRUE, value = TRUE)
  if (length(wood_col) == 0L) {
    stop("Cannot find woodiness column in source data. Columns: ",
         paste(names(df), collapse = ", "))
  }
  wood_col <- wood_col[1]

  # Normalize values
  raw <- tolower(trimws(df[[wood_col]]))
  woodiness <- ifelse(grepl("^h", raw), "herbaceous",
               ifelse(grepl("^w", raw), "woody",
               ifelse(grepl("^v", raw), "variable", NA_character_)))

  out <- data.frame(
    canonical_name = trimws(df$canonical_name),
    woodiness      = woodiness,
    stringsAsFactors = FALSE
  )

  # Drop NA names and duplicates (keep first)
  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0, ]
  out <- out[!duplicated(out$canonical_name), ]

  # Resolve source names against all 7 backends
  out <- resolve_enrichment_names(out)

  vtr_path <- file.path(output_dir, "woodiness.vtr")
  build_enrichment_vtr(
    out, vtr_path,
    name       = "woodiness",
    version    = "2014.1",
    source_url = .woodiness_url,
    source_doi = "10.5061/dryad.63q27",
    license    = "CC0",
    attribution = "Zanne AE et al. (2014) Three keys to the radiation of angiosperms into freezing environments. Nature 506:89-92."
  )
}
