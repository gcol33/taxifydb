# ---- Open Tree of Life (OTL): taxonomy.tsv -> normalized df -> .vtr ----
#
# OTL publishes a synthetic taxonomy (OTT) as a TSV tarball. Key file:
#   taxonomy.tsv: uid | parent_uid | name | rank | sourceinfo | uniqname | flags
#
# Notable features:
#   - flags column encodes status: MERGED, EXTINCT, INCERTAE_SEDIS, etc.
#   - sourceinfo maps to NCBI/GBIF/WoRMS/IRMNG IDs (cross-reference value)
#   - No explicit synonym/accepted status — derived from flags
#   - forwards.tsv maps old OTT IDs to current IDs (merged taxa)

source("shared/normalize.R")
source("shared/precompute.R")
source("shared/build.R")

# Updated to latest available version
.otl_url <- "https://files.opentreeoflife.org/ott/ott3.7.3/ott3.7.3.tgz"
.otl_version_default <- "3.7.3"

# Ranks to keep
.otl_keep_ranks <- c(
  "species", "genus", "family", "order", "class", "phylum", "kingdom",
  "domain", "subspecies", "variety", "forma", "infraspecies",
  "subfamily", "superfamily", "suborder", "subclass", "subphylum",
  "tribe", "subtribe", "subgenus"
)

# Flags indicating the taxon should be treated as non-accepted
.otl_synonym_flags <- c(
  "merged", "was_container", "inconsistent", "major_rank_conflict"
)

# Flags indicating the taxon should be excluded entirely
.otl_exclude_flags <- c(
  "not_otu", "environmental", "environmental_inherited",
  "hidden", "hidden_inherited", "barren"
)


#' Download and extract OTL taxonomy
#'
#' @param dest Character. Destination directory.
#' @param verbose Logical.
#' @return Path to the extraction directory containing taxonomy.tsv.
download_otl <- function(dest = tempdir(), verbose = TRUE) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)

  tgz_path <- file.path(dest, "ott.tgz")

  if (verbose) message("Downloading OTL taxonomy (~106 MB)...")
  utils::download.file(.otl_url, tgz_path, mode = "wb", quiet = !verbose)

  if (verbose) message("Extracting...")
  utils::untar(tgz_path, exdir = dest)

  # Find taxonomy.tsv (inside ott3.7.3/ subdirectory)
  tsv_files <- list.files(dest, pattern = "^taxonomy\\.tsv$",
                          recursive = TRUE, full.names = TRUE)
  if (length(tsv_files) == 0L) {
    stop("taxonomy.tsv not found in OTL download.")
  }

  unlink(tgz_path)
  dirname(tsv_files[1L])
}


#' Parse OTL flags column into a character vector
#'
#' @param flags_str Character. Comma-separated flag string.
#' @return Character vector of flags.
parse_flags <- function(flags_str) {
  if (is.na(flags_str) || !nzchar(flags_str)) return(character(0L))
  trimws(strsplit(flags_str, ",", fixed = TRUE)[[1L]])
}


#' Read and normalize the OTL taxonomy
#'
#' @param otl_dir Character. Path to the extracted OTL directory.
#' @param verbose Logical.
#' @return A normalized data.frame.
read_otl <- function(otl_dir, verbose = TRUE) {
  # ---- 1. Read taxonomy.tsv ----
  if (verbose) message("Reading taxonomy.tsv...")
  tsv_path <- file.path(otl_dir, "taxonomy.tsv")
  # OTL taxonomy.tsv is tab-separated (despite some docs saying pipe-delimited,
  # the actual file uses plain tabs with pipe in column values like sourceinfo)
  df <- utils::read.delim(tsv_path, stringsAsFactors = FALSE,
                          quote = "", comment.char = "")

  if (verbose) message(sprintf("  %s rows", format(nrow(df), big.mark = ",")))

  # Standardize column names (OTL uses uid, not id)
  if ("uid" %in% names(df)) names(df)[names(df) == "uid"] <- "id"
  if ("parent_uid" %in% names(df)) {
    names(df)[names(df) == "parent_uid"] <- "parent_id"
  }

  # ---- 2. Parse flags and filter ----
  if (verbose) message("Filtering by flags and rank...")
  # Parse flags for each row
  flag_list <- strsplit(
    ifelse(is.na(df$flags) | !nzchar(df$flags), "", df$flags),
    ",", fixed = TRUE
  )

  # Exclude rows with exclusion flags
  exclude_regex <- paste(.otl_exclude_flags, collapse = "|")
  has_exclude <- vapply(flag_list, function(f) {
    any(trimws(f) %in% .otl_exclude_flags)
  }, logical(1L))
  df <- df[!has_exclude, ]
  flag_list <- flag_list[!has_exclude]

  if (verbose) message(sprintf("  After flag filter: %s rows",
                               format(nrow(df), big.mark = ",")))

  # Filter by rank
  df <- df[!is.na(df$rank) & df$rank %in% .otl_keep_ranks, ]
  if (verbose) message(sprintf("  After rank filter: %s rows",
                               format(nrow(df), big.mark = ",")))

  # ---- 3. Derive taxonomic status from flags ----
  # Rows with synonym-like flags -> SYNONYM; others -> ACCEPTED
  flag_list_filtered <- strsplit(
    ifelse(is.na(df$flags) | !nzchar(df$flags), "", df$flags),
    ",", fixed = TRUE
  )
  has_syn_flag <- vapply(flag_list_filtered, function(f) {
    any(trimws(f) %in% .otl_synonym_flags)
  }, logical(1L))
  df$taxonomic_status <- ifelse(has_syn_flag, "SYNONYM", "ACCEPTED")

  # ---- 4. Read forwards.tsv for merged taxa ----
  forwards_path <- file.path(otl_dir, "forwards.tsv")
  if (file.exists(forwards_path)) {
    if (verbose) message("Reading forwards.tsv (merged IDs)...")
    fwd <- utils::read.delim(forwards_path, stringsAsFactors = FALSE,
                             header = TRUE, quote = "", comment.char = "")
    # Map merged OTT IDs -> current IDs
    # These are synonyms: old ID -> new ID
    if (nrow(fwd) > 0L && "id" %in% names(fwd) && "replacement" %in% names(fwd)) {
      # Use the parent_id field for merged entries as the accepted ID
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

  # For non-merged synonyms, use parent_id as accepted
  syn_no_acc <- df$taxonomic_status == "SYNONYM" &
    (is.na(df$accepted_name_usage_id) | !nzchar(df$accepted_name_usage_id))
  df$accepted_name_usage_id[syn_no_acc] <- as.character(df$parent_id[syn_no_acc])

  # ---- 5. Parse source info for cross-references ----
  if ("sourceinfo" %in% names(df)) {
    # sourceinfo contains comma-separated source:id pairs
    # e.g., "ncbi:12345,gbif:67890,worms:11111"
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

  # ---- 6. Resolve hierarchy ----
  if (verbose) message("Walking hierarchy for family/genus/kingdom...")
  df$id <- as.character(df$id)
  df$parent_id <- as.character(df$parent_id)
  df <- resolve_hierarchy(df, target_ranks = c("family", "genus", "kingdom"))

  # ---- 7. Parse epithet ----
  words <- strsplit(df$name, " ", fixed = TRUE)
  df$specific_epithet <- vapply(words, function(w) {
    if (length(w) >= 2L) w[2L] else NA_character_
  }, character(1L))

  # ---- 8. Normalize ----
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


#' Build the OTL backbone .vtr
#'
#' @param output_dir Character.
#' @param version Character or NULL.
#' @param verbose Logical.
#' @return Path to the .vtr file (invisibly).
build_otl <- function(output_dir = "output/otl", version = NULL,
                      verbose = TRUE) {
  if (is.null(version)) version <- .otl_version_default

  tmp <- tempfile("otl_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  otl_dir <- download_otl(dest = tmp, verbose = verbose)
  df <- read_otl(otl_dir, verbose = verbose)

  if (verbose) message("Precomputing keys and embedding synonyms...")
  df <- precompute_backbone(df)

  vtr_path <- file.path(output_dir, "otl.vtr")
  build_vtr(df, vtr_path, "otl", version, .otl_url)

  invisible(vtr_path)
}


if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  output_dir <- if (length(args) >= 1L) args[1L] else "output/otl"
  version <- if (length(args) >= 2L) args[2L] else NULL
  build_otl(output_dir, version)
}
