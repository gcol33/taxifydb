# Common-name parsers (GBIF + NCBI + OTT) for the `common_names` enrichment.


#' Parse common names from GBIF, NCBI, and OTT
#'
#' Merges vernacular names from three sources:
#' - GBIF: VernacularName.tsv (has ISO 639-1 language codes)
#' - NCBI: names.dmp where name_class == "common name" (no language)
#' - OTT: synonyms.tsv where type == "common name" (no language)
#'
#' NCBI and OTT common names have no language tag, so `lang` is set to NA.
#'
#' @param dir_path Character. Directory containing `gbif/`, `ncbi/`, `ott/`
#'   subdirs.
#' @return data.frame with canonical_name + lang + common_name.
#' @export
parse_common_names <- function(dir_path) {
  gbif_dir <- file.path(dir_path, "gbif")
  ncbi_dir <- file.path(dir_path, "ncbi")
  ott_dir  <- file.path(dir_path, "ott")

  parts <- list()

  if (dir.exists(gbif_dir)) {
    gbif <- parse_gbif_common_names(gbif_dir)
    gbif$source <- "gbif"
    parts <- c(parts, list(gbif))
  }

  if (dir.exists(ncbi_dir)) {
    ncbi <- parse_ncbi_common_names(ncbi_dir)
    ncbi$source <- "ncbi"
    parts <- c(parts, list(ncbi))
  }

  if (dir.exists(ott_dir)) {
    ott <- parse_ott_common_names(ott_dir)
    ott$source <- "ott"
    parts <- c(parts, list(ott))
  }

  if (length(parts) == 0L) {
    stop("No common name sources found in: ", dir_path, call. = FALSE)
  }

  out <- do.call(rbind, parts)

  # Prefer rows with a language tag (GBIF) over NA (NCBI/OTT)
  out <- out[order(!is.na(out$lang), decreasing = TRUE), ]
  out <- out[!duplicated(paste(out$canonical_name, out$common_name)), ]

  # Keep the provenance column (which database supplied the vernacular name).
  out
}


#' Parse GBIF vernacular names (VernacularName.tsv + Taxon.tsv)
#' @param dir_path Character. Directory holding VernacularName.tsv and Taxon.tsv.
#' @return data.frame with canonical_name + lang + common_name.
#' @export
parse_gbif_common_names <- function(dir_path) {
  vn_path <- file.path(dir_path, "VernacularName.tsv")
  taxon_path <- file.path(dir_path, "Taxon.tsv")

  if (!file.exists(vn_path) || !file.exists(taxon_path)) {
    vn_found <- list.files(dir_path, pattern = "VernacularName",
                           recursive = TRUE, full.names = TRUE)
    taxon_found <- list.files(dir_path, pattern = "^Taxon\\.tsv$",
                              recursive = TRUE, full.names = TRUE)
    if (length(vn_found) == 0L || length(taxon_found) == 0L) {
      stop(sprintf(
        "Could not find VernacularName.tsv and Taxon.tsv in: %s", dir_path
      ), call. = FALSE)
    }
    vn_path <- vn_found[1L]
    taxon_path <- taxon_found[1L]
  }

  if (requireNamespace("data.table", quietly = TRUE)) {
    vn <- as.data.frame(data.table::fread(
      vn_path, header = TRUE, sep = "\t", quote = "", showProgress = FALSE
    ))
    taxon_map <- as.data.frame(data.table::fread(
      taxon_path, header = TRUE, sep = "\t", quote = "",
      select = c("taxonID", "canonicalName"), showProgress = FALSE
    ))
  } else {
    vn <- utils::read.delim(vn_path, stringsAsFactors = FALSE, quote = "")
    taxon_full <- utils::read.delim(taxon_path, stringsAsFactors = FALSE,
                                    quote = "")
    taxon_map <- taxon_full[, c("taxonID", "canonicalName"), drop = FALSE]
  }

  names(taxon_map) <- c("taxon_id", "canonical_name")
  taxon_map <- taxon_map[!is.na(taxon_map$canonical_name) &
                           nchar(taxon_map$canonical_name) > 0L, ]

  vn$taxon_id <- as.integer(vn$taxonID)
  taxon_map$taxon_id <- as.integer(taxon_map$taxon_id)
  merged <- merge(vn, taxon_map, by = "taxon_id", all.x = FALSE)

  out <- data.frame(
    canonical_name = trimws(merged$canonical_name),
    lang           = tolower(trimws(merged$language)),
    common_name    = trimws(merged$vernacularName),
    stringsAsFactors = FALSE
  )

  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]
  out <- out[!is.na(out$common_name) & nchar(out$common_name) > 0L, ]
  out[!is.na(out$lang) & nchar(out$lang) >= 2L & nchar(out$lang) <= 3L, ]
}


#' Parse NCBI common names from names.dmp
#' @param dir_path Character. Directory containing names.dmp.
#' @return data.frame with canonical_name + lang (NA) + common_name.
#' @export
parse_ncbi_common_names <- function(dir_path) {
  names_file <- file.path(dir_path, "names.dmp")
  if (!file.exists(names_file)) {
    stop("Could not find names.dmp in: ", dir_path, call. = FALSE)
  }

  raw <- readLines(names_file, warn = FALSE)
  split <- strsplit(raw, "\t\\|\t?", perl = TRUE)
  df <- data.frame(
    tax_id     = vapply(split, `[`, character(1L), 1L),
    name_txt   = trimws(vapply(split, `[`, character(1L), 2L)),
    name_class = trimws(vapply(split, `[`, character(1L), 4L)),
    stringsAsFactors = FALSE
  )

  sci <- df[df$name_class == "scientific name", ]
  sci_lookup <- sci$name_txt
  names(sci_lookup) <- sci$tax_id

  common <- df[df$name_class == "common name", ]
  common <- common[!is.na(common$name_txt) & nchar(common$name_txt) > 0L, ]

  canonical <- sci_lookup[common$tax_id]

  out <- data.frame(
    canonical_name = unname(canonical),
    lang           = NA_character_,
    common_name    = common$name_txt,
    stringsAsFactors = FALSE
  )

  out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]
}


#' Parse OTT common names from synonyms.tsv + taxonomy.tsv
#' @param dir_path Character. Directory containing synonyms.tsv + taxonomy.tsv.
#' @return data.frame with canonical_name + lang (NA) + common_name.
#' @export
parse_ott_common_names <- function(dir_path) {
  syn_file <- file.path(dir_path, "synonyms.tsv")
  tax_file <- file.path(dir_path, "taxonomy.tsv")
  if (!file.exists(syn_file) || !file.exists(tax_file)) {
    stop("Could not find synonyms.tsv and taxonomy.tsv in: ", dir_path,
         call. = FALSE)
  }

  tax_raw <- readLines(tax_file, warn = FALSE)
  tax_raw <- tax_raw[-1L]
  tax_split <- strsplit(tax_raw, "\t\\|\t?", perl = TRUE)
  tax_lookup <- trimws(vapply(tax_split, `[`, character(1L), 3L))
  names(tax_lookup) <- vapply(tax_split, `[`, character(1L), 1L)

  syn_raw <- readLines(syn_file, warn = FALSE)
  syn_raw <- syn_raw[-1L]
  syn_split <- strsplit(syn_raw, "\t\\|\t?", perl = TRUE)
  syns <- data.frame(
    name = trimws(vapply(syn_split, `[`, character(1L), 1L)),
    uid  = trimws(vapply(syn_split, `[`, character(1L), 2L)),
    type = trimws(vapply(syn_split, function(x) {
      if (length(x) >= 3L) x[3L] else ""
    }, character(1L))),
    stringsAsFactors = FALSE
  )

  common <- syns[grepl("common", syns$type, ignore.case = TRUE), ]
  common <- common[!is.na(common$name) & nchar(common$name) > 0L, ]

  canonical <- tax_lookup[common$uid]

  out <- data.frame(
    canonical_name = unname(canonical),
    lang           = NA_character_,
    common_name    = common$name,
    stringsAsFactors = FALSE
  )

  out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]
}
