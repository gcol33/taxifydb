# ---- Common names enrichment: GBIF vernacular names (CC0) ----
#
# Source: GBIF backbone DwC-A (backbone.zip)
# Contains VernacularName.tsv (1.5M rows) + Taxon.tsv for ID mapping
# Multi-language via ISO 639-1 codes
# Coverage: ~650k name x language combinations, ~210k species, 155 languages

.common_names_backbone_url <- "https://hosted-datasets.gbif.org/datasets/backbone/current/backbone.zip"

download_common_names <- function(dest = tempdir()) {
  cn_dir <- file.path(dest, "gbif_common_names")
  dir.create(cn_dir, showWarnings = FALSE, recursive = TRUE)

  zip_path <- file.path(cn_dir, "backbone.zip")
  vn_path  <- file.path(cn_dir, "VernacularName.tsv")

  if (!file.exists(vn_path)) {
    if (!file.exists(zip_path)) {
      message("Downloading GBIF backbone.zip (~0.9 GB)...")
      curl::curl_download(.common_names_backbone_url, zip_path, quiet = FALSE)
    }

    message("Extracting VernacularName.tsv + Taxon.tsv...")
    zip_contents <- utils::unzip(zip_path, list = TRUE)

    vn_file <- zip_contents$Name[grepl("VernacularName", zip_contents$Name)]
    taxon_file <- zip_contents$Name[grepl("^Taxon\\.tsv$", zip_contents$Name)]

    utils::unzip(zip_path, files = c(vn_file, taxon_file),
                 exdir = cn_dir, junkpaths = TRUE)
  }

  list(
    vernacular = file.path(cn_dir, "VernacularName.tsv"),
    taxon      = file.path(cn_dir, "Taxon.tsv")
  )
}

build_common_names <- function(output_dir) {
  paths <- download_common_names()

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("data.table is required for common_names build (large files). ",
         "Install with: install.packages('data.table')")
  }

  # ---- Read VernacularName.tsv ----
  message("Reading VernacularName.tsv...")
  vn <- data.table::fread(paths$vernacular, header = TRUE, sep = "\t",
                           quote = "", showProgress = TRUE)
  message(sprintf("  %s vernacular name rows", format(nrow(vn), big.mark = ",")))

  # ---- Read Taxon.tsv (taxonID + canonicalName only) ----
  message("Reading Taxon.tsv (ID -> name mapping)...")
  taxon_map <- data.table::fread(
    paths$taxon, header = TRUE, sep = "\t", quote = "",
    select = c("taxonID", "canonicalName"),
    showProgress = TRUE
  )
  data.table::setnames(taxon_map, c("taxon_id", "canonical_name"))
  taxon_map <- taxon_map[!is.na(canonical_name) & nchar(canonical_name) > 0L]
  message(sprintf("  %s backbone entries", format(nrow(taxon_map), big.mark = ",")))

  # ---- Merge taxonID -> canonical_name ----
  message("Merging...")
  vn[, taxon_id := as.integer(taxonID)]
  taxon_map[, taxon_id := as.integer(taxon_id)]
  merged <- merge(vn, taxon_map, by = "taxon_id", all.x = FALSE)
  message(sprintf("  %s merged rows", format(nrow(merged), big.mark = ",")))

  # ---- Build output ----
  out <- data.frame(
    canonical_name = trimws(merged$canonical_name),
    lang           = tolower(trimws(merged$language)),
    common_name    = trimws(merged$vernacularName),
    stringsAsFactors = FALSE
  )

  # Filter: non-empty, valid language codes (2-3 chars)
  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]
  out <- out[!is.na(out$common_name) & nchar(out$common_name) > 0L, ]
  out <- out[!is.na(out$lang) & nchar(out$lang) >= 2L & nchar(out$lang) <= 3L, ]

  # Deduplicate: keep first per name x lang
  out <- out[!duplicated(paste(out$canonical_name, out$lang)), ]

  # Resolve source names against all 7 backends
  out <- resolve_enrichment_names(out, group_cols = "lang")

  message(sprintf("Output: %s rows (%s species, %d languages)",
                  format(nrow(out), big.mark = ","),
                  format(length(unique(out$canonical_name)), big.mark = ","),
                  length(unique(out$lang))))

  vtr_path <- file.path(output_dir, "common_names.vtr")
  build_enrichment_vtr(
    out, vtr_path,
    name       = "common_names",
    version    = "2026.04",
    source_url = .common_names_backbone_url,
    license    = "CC0",
    attribution = "GBIF Secretariat. GBIF Backbone Taxonomy vernacular names.",
    group_col  = "lang"
  )
}
