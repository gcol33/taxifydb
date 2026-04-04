# ---- WCVP enrichment: World Checklist of Vascular Plants (Kew, CC BY) ----
#
# Source: WCVP native range data
# DOI: 10.1038/s41597-021-00997-6
# ~340k plant species with TDWG region-level distribution
#
# WCVP is distributed via Kew. The data files are available from:
#   https://powo.science.kew.org/about-wcvp
# Direct download link (may change):
#   https://sftp.kew.org/pub/data-repositories/WCVP/
#
# Alternative: GBIF hosted version of WCVP
# Single ZIP containing both names and distribution CSVs
.wcvp_url <- "https://sftp.kew.org/pub/data-repositories/WCVP/wcvp.zip"

download_wcvp <- function(dest = tempdir()) {
  zip_path <- file.path(dest, "wcvp.zip")
  extract_dir <- file.path(dest, "wcvp_extract")

  if (!file.exists(zip_path)) {
    message("Downloading WCVP (~85 MB)...")
    curl::curl_download(.wcvp_url, zip_path, quiet = FALSE)
  }

  if (!dir.exists(extract_dir)) {
    dir.create(extract_dir, recursive = TRUE)
    utils::unzip(zip_path, exdir = extract_dir)
  }

  csvs <- list.files(extract_dir, pattern = "\\.csv$|\\.txt$",
                     full.names = TRUE, recursive = TRUE)
  names_file <- grep("(?i)name", csvs, value = TRUE)
  dist_file <- grep("(?i)distribut", csvs, value = TRUE)

  list(
    names = if (length(names_file) > 0L) names_file[1] else csvs[1],
    dist = if (length(dist_file) > 0L) dist_file[1] else csvs[2]
  )
}

build_wcvp <- function(output_dir) {
  files <- download_wcvp()

  if (is.na(files$names) || is.na(files$dist)) {
    stop("Could not find WCVP CSV files after extraction.")
  }

  message("Reading WCVP names...")
  if (requireNamespace("data.table", quietly = TRUE)) {
    names_df <- as.data.frame(data.table::fread(files$names, showProgress = TRUE))
    dist_df <- as.data.frame(data.table::fread(files$dist, showProgress = TRUE))
  } else {
    names_df <- read.csv(files$names, stringsAsFactors = FALSE)
    dist_df <- read.csv(files$dist, stringsAsFactors = FALSE)
  }

  # Names file: plant_name_id, taxon_name, taxon_status, accepted_plant_name_id
  # Distribution file: plant_name_id, area_code_l3, introduced, extinct, location_doubtful

  # Find columns
  find_col <- function(df, patterns) {
    for (p in patterns) {
      m <- grep(paste0("^", p, "$"), names(df), ignore.case = TRUE, value = TRUE)
      if (length(m) > 0L) return(m[1])
    }
    for (p in patterns) {
      m <- grep(p, names(df), ignore.case = TRUE, value = TRUE)
      if (length(m) > 0L) return(m[1])
    }
    NULL
  }

  id_col <- find_col(names_df, c("plant_name_id", "kew_id", "id"))
  name_col <- find_col(names_df, c("taxon_name", "scientific_name",
                                    "full_name", "name"))
  status_col <- find_col(names_df, c("taxon_status", "status",
                                      "taxonomic_status"))

  dist_id_col <- find_col(dist_df, c("plant_name_id", "kew_id", "id"))
  area_col <- find_col(dist_df, c("area_code_l3", "area", "tdwg_code",
                                   "region_code"))
  intro_col <- find_col(dist_df, c("introduced", "is_introduced"))
  extinct_col <- find_col(dist_df, c("extinct", "is_extinct"))

  # Keep only accepted names
  if (!is.null(status_col)) {
    accepted <- names_df[tolower(names_df[[status_col]]) == "accepted", ]
  } else {
    accepted <- names_df
  }

  # Build ID -> canonical_name map
  id_to_name <- stats::setNames(
    trimws(accepted[[name_col]]),
    as.character(accepted[[id_col]])
  )

  # Join distribution with names
  dist_df$canonical_name <- id_to_name[as.character(dist_df[[dist_id_col]])]
  dist_df <- dist_df[!is.na(dist_df$canonical_name), ]

  # Determine native status
  introduced <- if (!is.null(intro_col)) {
    as.integer(dist_df[[intro_col]])
  } else {
    rep(0L, nrow(dist_df))
  }

  extinct <- if (!is.null(extinct_col)) {
    as.integer(dist_df[[extinct_col]])
  } else {
    rep(0L, nrow(dist_df))
  }

  native_status <- ifelse(
    extinct == 1L, "extinct",
    ifelse(introduced == 1L, "introduced", "native")
  )

  # TDWG Level 3 -> Level 2 mapping (first 2 chars of L3 code approximate L2)
  # Actually TDWG L3 codes are numeric; L2 are 2-letter region abbreviations

  # For simplicity, keep L3 codes as tdwg_code
  tdwg_codes <- trimws(dist_df[[area_col]])

  out <- data.frame(
    canonical_name = dist_df$canonical_name,
    tdwg_code      = tdwg_codes,
    native_status  = native_status,
    stringsAsFactors = FALSE
  )

  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0, ]
  out <- out[!is.na(out$tdwg_code) & nchar(out$tdwg_code) > 0, ]
  out <- out[!duplicated(paste(out$canonical_name, out$tdwg_code)), ]

  # Resolve source names against all 7 backends
  out <- resolve_enrichment_names(out, group_cols = "tdwg_code")

  vtr_path <- file.path(output_dir, "wcvp.vtr")
  build_enrichment_vtr(
    out, vtr_path,
    name       = "wcvp",
    version    = "2026.04",
    source_url = .wcvp_url,
    source_doi = "10.1038/s41597-021-00997-6",
    license    = "CC BY",
    attribution = "WCVP (2024) World Checklist of Vascular Plants. Royal Botanic Gardens, Kew.",
    group_col  = "tdwg_code"
  )
}
