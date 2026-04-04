# ---- AmphiBIO enrichment: Oliveira et al. 2017 (Figshare, CC BY 4.0) ----
#
# Source: AmphiBIO — a global database for amphibian ecological traits
# DOI: 10.6084/m9.figshare.4644424.v5
# ~6,776 amphibian species

.amphibio_url <- "https://ndownloader.figshare.com/files/8828578"

download_amphibio <- function(dest = tempdir()) {
  csv_path <- file.path(dest, "AmphiBIO_v1.csv")
  if (file.exists(csv_path)) return(csv_path)

  zip_path <- file.path(dest, "AmphiBIO_v1.zip")
  if (!file.exists(zip_path) || file.size(zip_path) < 1000) {
    message("Downloading AmphiBIO...")
    h <- curl::new_handle()
    curl::handle_setopt(h, followlocation = TRUE, maxredirs = 10)
    curl::handle_setheaders(h, "User-Agent" = "Mozilla/5.0 R/4.5 taxify-backbones")
    curl::curl_download(.amphibio_url, zip_path, handle = h)
  }

  # Extract CSV from zip
  extract_dir <- file.path(dest, "amphibio_extract")
  dir.create(extract_dir, showWarnings = FALSE)
  utils::unzip(zip_path, exdir = extract_dir)
  csvs <- list.files(extract_dir, pattern = "\\.csv$", full.names = TRUE,
                     recursive = TRUE)
  if (length(csvs) == 0L) {
    stop("No CSV found in AmphiBIO zip. Contents: ",
         paste(list.files(extract_dir, recursive = TRUE), collapse = ", "))
  }
  file.copy(csvs[1], csv_path, overwrite = TRUE)
  csv_path
}

build_amphibio <- function(output_dir) {
  csv_path <- download_amphibio()

  df <- read.csv(csv_path, stringsAsFactors = FALSE)

  # AmphiBIO uses "Species" for the binomial name
  name_col <- intersect(names(df), c("Species", "species", "Scientific"))
  if (length(name_col) == 0L) name_col <- names(df)[1]
  else name_col <- name_col[1]

  find_col <- function(patterns) {
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

  safe_num <- function(col_name) {
    if (is.null(col_name)) return(rep(NA_real_, nrow(df)))
    suppressWarnings(as.numeric(df[[col_name]]))
  }

  safe_int <- function(col_name) {
    if (is.null(col_name)) return(rep(NA_integer_, nrow(df)))
    suppressWarnings(as.integer(df[[col_name]]))
  }

  out <- data.frame(
    canonical_name      = trimws(df[[name_col]]),
    body_size_mm        = safe_num(find_col(c("Body_size_mm", "Body.size.mm",
                                              "SVL_mm", "Body_length_mm"))),
    age_maturity_d      = safe_num(find_col(c("Age_at_maturity_min_d",
                                              "Age.at.maturity",
                                              "Age_maturity_d"))),
    longevity_d         = safe_num(find_col(c("Longevity_max_d", "Longevity",
                                              "Longevity_d"))),
    litter_size         = safe_num(find_col(c("Litter_size_max_n",
                                              "Litter.size", "Clutch_size"))),
    reproductive_output = safe_num(find_col(c("Reproductive_output_y",
                                              "Reproductive.output"))),
    offspring_size_mm   = safe_num(find_col(c("Offspring_size_mm",
                                              "Offspring.size"))),
    direct_development  = safe_int(find_col(c("Dir", "Direct_development",
                                              "Devel_direct"))),
    larval              = safe_int(find_col(c("Lar", "Larval", "Has_larva"))),
    aquatic             = safe_int(find_col(c("Aqu", "Aquatic"))),
    fossorial           = safe_int(find_col(c("Fos", "Fossorial"))),
    arboreal            = safe_int(find_col(c("Arb", "Arboreal"))),
    diurnal             = safe_int(find_col(c("Diu", "Diurnal"))),
    nocturnal_amphibio  = safe_int(find_col(c("Noc", "Nocturnal"))),
    stringsAsFactors = FALSE
  )

  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0, ]
  out <- out[!duplicated(out$canonical_name), ]

  # Resolve source names against all 7 backends
  out <- resolve_enrichment_names(out)

  vtr_path <- file.path(output_dir, "amphibio.vtr")
  build_enrichment_vtr(
    out, vtr_path,
    name       = "amphibio",
    version    = "1.0",
    source_url = .amphibio_url,
    source_doi = "10.6084/m9.figshare.4644424.v5",
    license    = "CC BY 4.0",
    attribution = "Oliveira BF et al. (2017) AmphiBIO, a global database for amphibian ecological traits. Scientific Data 4:170123."
  )
}
