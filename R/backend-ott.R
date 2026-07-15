# Open Tree Taxonomy (OTT): taxonomy.tsv -> normalized df -> .vtr
#
# OTT is published by Open Tree of Life as a TSV tarball. Key file:
#   taxonomy.tsv: uid | parent_uid | name | rank | sourceinfo | uniqname | flags
#
# Features: flags column encodes status (MERGED, EXTINCT, INCERTAE_SEDIS, ...),
# sourceinfo maps to NCBI/GBIF/WoRMS/IRMNG IDs (cross-references), no explicit
# synonym/accepted status (derived from flags), forwards.tsv maps old OTT IDs
# to current IDs (merged taxa).

.ott_url <- "https://files.opentreeoflife.org/ott/ott3.7.3/ott3.7.3.tgz"
.ott_version_default <- "3.7.3"

.ott_keep_ranks <- c(
  "species", "genus", "family", "order", "class", "phylum", "kingdom",
  "domain", "subspecies", "variety", "forma", "infraspecies",
  "subfamily", "superfamily", "suborder", "subclass", "subphylum",
  "tribe", "subtribe", "subgenus"
)

.ott_synonym_flags <- c(
  "merged", "was_container", "inconsistent", "major_rank_conflict"
)

.ott_exclude_flags <- c(
  "not_otu", "environmental", "environmental_inherited",
  "hidden", "hidden_inherited", "barren"
)


#' Download and extract OTT taxonomy
#'
#' @param dest Character. Destination directory.
#' @param verbose Logical.
#' @return Path to the extraction directory containing taxonomy.tsv.
#' @export
download_ott <- function(dest = tempdir(), verbose = TRUE) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)

  if (verbose) message("Downloading OTT taxonomy (~106 MB)...")
  tgz_path <- download_curl_file(.ott_url, dest, "ott.tgz")

  if (verbose) message("Extracting...")
  utils::untar(tgz_path, exdir = dest)

  tsv_files <- list.files(dest, pattern = "^taxonomy\\.tsv$",
                          recursive = TRUE, full.names = TRUE)
  if (length(tsv_files) == 0L) {
    stop("taxonomy.tsv not found in OTT download.")
  }

  unlink(tgz_path)
  dirname(tsv_files[1L])
}


#' Parse OTT flags column into a character vector
#' @noRd
parse_flags <- function(flags_str) {
  if (is.na(flags_str) || !nzchar(flags_str)) return(character(0L))
  trimws(strsplit(flags_str, ",", fixed = TRUE)[[1L]])
}


#' Read and normalize the OTT taxonomy
#'
#' @param ott_dir Character. Path to the extracted OTT directory.
#' @param verbose Logical.
#' @return A normalized data.frame.
#' @export
read_ott <- function(ott_dir, verbose = TRUE) {
  if (verbose) message("Reading taxonomy.tsv...")
  tsv_path <- file.path(ott_dir, "taxonomy.tsv")
  df <- utils::read.delim(tsv_path, stringsAsFactors = FALSE,
                          quote = "", comment.char = "")

  if (verbose) message(sprintf("  %s rows", format(nrow(df), big.mark = ",")))

  if ("uid" %in% names(df)) names(df)[names(df) == "uid"] <- "id"
  if ("parent_uid" %in% names(df)) {
    names(df)[names(df) == "parent_uid"] <- "parent_id"
  }

  if (verbose) message("Filtering by flags and rank...")
  flag_list <- strsplit(
    ifelse(is.na(df$flags) | !nzchar(df$flags), "", df$flags),
    ",", fixed = TRUE
  )

  has_exclude <- vapply(flag_list, function(f) {
    any(trimws(f) %in% .ott_exclude_flags)
  }, logical(1L))
  df <- df[!has_exclude, ]
  flag_list <- flag_list[!has_exclude]

  if (verbose) message(sprintf("  After flag filter: %s rows",
                               format(nrow(df), big.mark = ",")))

  df <- df[!is.na(df$rank) & df$rank %in% .ott_keep_ranks, ]
  if (verbose) message(sprintf("  After rank filter: %s rows",
                               format(nrow(df), big.mark = ",")))

  flag_list_filtered <- strsplit(
    ifelse(is.na(df$flags) | !nzchar(df$flags), "", df$flags),
    ",", fixed = TRUE
  )
  has_syn_flag <- vapply(flag_list_filtered, function(f) {
    any(trimws(f) %in% .ott_synonym_flags)
  }, logical(1L))
  df$taxonomic_status <- ifelse(has_syn_flag, "SYNONYM", "ACCEPTED")

  forwards_path <- file.path(ott_dir, "forwards.tsv")
  if (file.exists(forwards_path)) {
    if (verbose) message("Reading forwards.tsv (merged IDs)...")
    fwd <- utils::read.delim(forwards_path, stringsAsFactors = FALSE,
                             header = TRUE, quote = "", comment.char = "")
    if (nrow(fwd) > 0L &&
        "id" %in% names(fwd) && "replacement" %in% names(fwd)) {
      merged_idx <- df$id %in% fwd$id
      if (any(merged_idx)) {
        df$taxonomic_status[merged_idx] <- "SYNONYM"
        replacement_id <- fwd$replacement[match(df$id[merged_idx], fwd$id)]
        df$accepted_name_usage_id <- NA_character_
        df$accepted_name_usage_id[merged_idx] <- as.character(replacement_id)
      }
    }
  }

  if (!"accepted_name_usage_id" %in% names(df)) {
    df$accepted_name_usage_id <- NA_character_
  }

  syn_no_acc <- df$taxonomic_status == "SYNONYM" &
    (is.na(df$accepted_name_usage_id) | !nzchar(df$accepted_name_usage_id))
  df$accepted_name_usage_id[syn_no_acc] <-
    as.character(df$parent_id[syn_no_acc])

  if ("sourceinfo" %in% names(df)) {
    extract_source_id <- function(sourceinfo, prefix) {
      if (is.na(sourceinfo) || !nzchar(sourceinfo)) return(NA_character_)
      parts <- strsplit(sourceinfo, ",", fixed = TRUE)[[1L]]
      match_part <- parts[startsWith(trimws(parts), paste0(prefix, ":"))]
      if (length(match_part) == 0L) return(NA_character_)
      sub(paste0("^", prefix, ":"), "", trimws(match_part[1L]))
    }
    df$ncbi_id <- vapply(df$sourceinfo, extract_source_id, character(1L),
                         prefix = "ncbi")
    df$gbif_id <- vapply(df$sourceinfo, extract_source_id, character(1L),
                         prefix = "gbif")
    df$worms_id <- vapply(df$sourceinfo, extract_source_id, character(1L),
                          prefix = "worms")
  }

  if (verbose) message("Walking hierarchy for family/genus/kingdom...")
  df$id <- as.character(df$id)
  df$parent_id <- as.character(df$parent_id)
  df <- resolve_hierarchy(df, target_ranks = c("family", "genus", "kingdom"))

  words <- strsplit(df$name, " ", fixed = TRUE)
  df$specific_epithet <- vapply(words, function(w) {
    if (length(w) >= 2L) w[2L] else NA_character_
  }, character(1L))

  if (verbose) message("Normalizing to unified schema...")
  col_map <- list(
    taxon_id                = "id",
    canonical_name          = "name",
    taxon_rank              = "rank",
    taxonomic_status        = "taxonomic_status",
    accepted_name_usage_id  = "accepted_name_usage_id",
    family                  = "resolved_family",
    genus                   = "resolved_genus",
    specific_epithet        = "specific_epithet"
  )

  extra_cols <- list(
    kingdom  = "resolved_kingdom",
    ncbi_id  = "ncbi_id",
    gbif_id  = "gbif_id",
    worms_id = "worms_id"
  )

  normalize_backbone(df, col_map, extra_cols)
}


#' Build the OTT backbone .vtr
#'
#' @param output_dir Character.
#' @param version Character or NULL.
#' @param verbose Logical.
#' @return Path to the .vtr file (invisibly).
#' @export
build_ott <- function(output_dir = "output/ott", version = NULL,
                      verbose = TRUE) {
  if (is.null(version)) version <- .ott_version_default

  tmp <- tempfile("ott_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  ott_dir <- download_ott(dest = tmp, verbose = verbose)
  df <- read_ott(ott_dir, verbose = verbose)

  if (verbose) message("Precomputing keys and embedding synonyms...")
  df <- precompute_backbone(df)

  vtr_path <- file.path(output_dir, "ott.vtr")
  build_vtr(df, vtr_path, "ott", version, .ott_url)

  invisible(vtr_path)
}
