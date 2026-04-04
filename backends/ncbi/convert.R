# ---- NCBI Taxonomy: taxdump -> normalized data.frame -> .vtr ----
#
# NCBI taxonomy uses pipe-delimited .dmp files:
#   - names.dmp: tax_id | name | unique_name | name_class
#   - nodes.dmp: tax_id | parent_id | rank | ...
#   - merged.dmp: old_id | new_id (merged taxa)
#
# The taxonomy is molecular-centric and noisy. We filter aggressively:
#   - Keep only "scientific name" entries from names.dmp
#   - Skip environmental samples, unclassified, unnamed entries
#   - Keep ranks: species, genus, family, order, class, phylum, kingdom,
#     subspecies, varietas, forma

source("shared/normalize.R")
source("shared/precompute.R")
source("shared/build.R")

.ncbi_url <- "https://ftp.ncbi.nlm.nih.gov/pub/taxonomy/new_taxdump/new_taxdump.tar.gz"

# Ranks to keep (skip no rank, clade, etc.)
.ncbi_keep_ranks <- c(
  "species", "genus", "family", "order", "class", "phylum", "kingdom",
  "superkingdom", "subspecies", "varietas", "forma", "subgenus",
  "subfamily", "superfamily", "suborder", "infraorder", "subclass",
  "subphylum", "tribe", "subtribe"
)

# Flags that indicate noise (environmental, unclassified, etc.)
.ncbi_skip_patterns <- c(
  "^environmental samples",
  "^unclassified ",
  "^uncultured ",
  " sp\\.$",
  " cf\\. ",
  " aff\\. ",
  "metagenome",
  "enrichment culture",
  "^Candidatus "
)


#' Download and extract NCBI taxdump
#'
#' @param dest Character. Destination directory.
#' @param verbose Logical.
#' @return Path to the extraction directory.
download_ncbi <- function(dest = tempdir(), verbose = TRUE) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)

  gz_path <- file.path(dest, "new_taxdump.tar.gz")

  if (verbose) message("Downloading NCBI taxonomy dump (~141 MB)...")
  utils::download.file(.ncbi_url, gz_path, mode = "wb", quiet = !verbose)

  if (verbose) message("Extracting...")
  utils::untar(gz_path, exdir = dest)

  unlink(gz_path)
  dest
}


#' Read pipe-delimited NCBI .dmp file
#'
#' @param path Character. Path to the .dmp file.
#' @param col_names Character vector of column names.
#' @return A data.frame.
read_dmp <- function(path, col_names) {
  # NCBI .dmp format: tab-pipe-tab delimited, trailing \t|\n on each line
  lines <- readLines(path, warn = FALSE)
  # Split by \t|\t
  fields <- strsplit(lines, "\t\\|\t?", perl = TRUE)
  # Clean trailing \t| from last field
  fields <- lapply(fields, function(f) {
    f[length(f)] <- sub("\\t?\\|$", "", f[length(f)])
    trimws(f)
  })
  df <- as.data.frame(do.call(rbind, fields), stringsAsFactors = FALSE)
  names(df) <- col_names[seq_len(ncol(df))]
  df
}


#' Read and normalize the NCBI taxonomy dump
#'
#' @param dump_dir Character. Path to the extracted taxdump directory.
#' @param verbose Logical.
#' @return A normalized data.frame ready for precompute_backbone().
read_ncbi <- function(dump_dir, verbose = TRUE) {
  # ---- 1. Read names.dmp ----
  if (verbose) message("Reading names.dmp...")
  names_df <- read_dmp(
    file.path(dump_dir, "names.dmp"),
    c("tax_id", "name", "unique_name", "name_class")
  )
  if (verbose) message(sprintf("  %s name records",
                               format(nrow(names_df), big.mark = ",")))

  # Split into scientific names and synonyms
  sci_names <- names_df[names_df$name_class == "scientific name", ]
  syn_names <- names_df[names_df$name_class == "synonym", ]

  # ---- 2. Read nodes.dmp ----
  if (verbose) message("Reading nodes.dmp...")
  nodes_cols <- c("tax_id", "parent_id", "rank", "embl_code", "division_id",
                  "inherited_div", "genetic_code_id", "inherited_gc",
                  "mito_gc_id", "inherited_mgc", "genbank_hidden",
                  "hidden_subtree", "comments", "plastid_gc_id",
                  "inherited_pgc", "specified_species", "hydrogenosome_gc_id",
                  "inherited_hgc")
  nodes_df <- read_dmp(file.path(dump_dir, "nodes.dmp"), nodes_cols)
  nodes_df <- nodes_df[, c("tax_id", "parent_id", "rank")]

  # ---- 3. Build main table: scientific names + node info ----
  if (verbose) message("Joining names with nodes...")
  df <- merge(sci_names[, c("tax_id", "name")], nodes_df, by = "tax_id")

  # ---- 4. Filter by rank ----
  if (verbose) message("Filtering by rank...")
  n_before <- nrow(df)
  df <- df[df$rank %in% .ncbi_keep_ranks, ]
  if (verbose) message(sprintf("  Kept %s of %s (rank filter)",
                               format(nrow(df), big.mark = ","),
                               format(n_before, big.mark = ",")))

  # ---- 5. Filter noise ----
  if (verbose) message("Filtering noise (environmental, unclassified, etc.)...")
  skip_regex <- paste(.ncbi_skip_patterns, collapse = "|")
  noise <- grepl(skip_regex, df$name, perl = TRUE)
  df <- df[!noise, ]
  if (verbose) message(sprintf("  Kept %s rows after noise filter",
                               format(nrow(df), big.mark = ",")))

  # ---- 6. Add synonyms ----
  if (verbose) message("Adding synonyms...")
  # Only keep synonyms for taxa we retained
  syn_filtered <- syn_names[syn_names$tax_id %in% df$tax_id, ]
  if (nrow(syn_filtered) > 0L) {
    # For synonyms, the accepted ID is the tax_id itself (same node)
    syn_rows <- data.frame(
      tax_id = paste0(syn_filtered$tax_id, "_syn_",
                       seq_len(nrow(syn_filtered))),
      name = syn_filtered$name,
      parent_id = nodes_df$parent_id[match(syn_filtered$tax_id,
                                            nodes_df$tax_id)],
      rank = df$rank[match(syn_filtered$tax_id, df$tax_id)],
      accepted_id = syn_filtered$tax_id,
      is_synonym_flag = TRUE,
      stringsAsFactors = FALSE
    )
    df$accepted_id <- NA_character_
    df$is_synonym_flag <- FALSE
    df <- rbind(df, syn_rows[, names(df)])
    # Fix: synonym rows need the accepted_id column
    # Re-add after rbind since columns may not align
    if (!"accepted_id" %in% names(df)) {
      df$accepted_id <- NA_character_
    }
    syn_idx <- (nrow(df) - nrow(syn_rows) + 1L):nrow(df)
    df$accepted_id[syn_idx] <- syn_rows$accepted_id
  }

  # ---- 7. Resolve family and genus via hierarchy walk ----
  if (verbose) message("Walking hierarchy for family/genus...")
  df$id <- df$tax_id
  df <- resolve_hierarchy(df, target_ranks = c("family", "genus", "kingdom"))

  # ---- 8. Parse epithet from binomial names ----
  words <- strsplit(df$name, " ", fixed = TRUE)
  df$specific_epithet <- vapply(words, function(w) {
    if (length(w) >= 2L) w[2L] else NA_character_
  }, character(1L))
  df$infraspecific_epithet <- vapply(words, function(w) {
    if (length(w) >= 3L) paste(w[3:length(w)], collapse = " ")
    else NA_character_
  }, character(1L))

  # ---- 9. Normalize ----
  if (verbose) message("Normalizing to unified schema...")
  # Map NCBI status
  df$taxonomic_status <- ifelse(
    !is.na(df$accepted_id) & nzchar(df$accepted_id),
    "SYNONYM",
    "ACCEPTED"
  )
  df$accepted_name_usage_id <- df$accepted_id

  col_map <- list(
    taxon_id                = "tax_id",
    canonical_name          = "name",
    taxon_rank              = "rank",
    taxonomic_status        = "taxonomic_status",
    accepted_name_usage_id  = "accepted_name_usage_id",
    family                  = "resolved_family",
    genus                   = "resolved_genus",
    specific_epithet        = "specific_epithet",
    infraspecific_epithet   = "infraspecific_epithet"
  )

  extra_cols <- list(
    kingdom = "resolved_kingdom"
  )

  normalize_backbone(df, col_map, extra_cols)
}


#' Build the NCBI backbone .vtr
#'
#' @param output_dir Character.
#' @param version Character or NULL.
#' @param verbose Logical.
#' @return Path to the .vtr file (invisibly).
build_ncbi <- function(output_dir = "output/ncbi", version = NULL,
                       verbose = TRUE) {
  if (is.null(version)) version <- format(Sys.Date(), "%Y.%m")

  tmp <- tempfile("ncbi_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  dump_dir <- download_ncbi(dest = tmp, verbose = verbose)
  df <- read_ncbi(dump_dir, verbose = verbose)

  if (verbose) message("Precomputing keys and embedding synonyms...")
  df <- precompute_backbone(df)

  vtr_path <- file.path(output_dir, "ncbi.vtr")
  build_vtr(df, vtr_path, "ncbi", version, .ncbi_url)

  invisible(vtr_path)
}


if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  output_dir <- if (length(args) >= 1L) args[1L] else "output/ncbi"
  version <- if (length(args) >= 2L) args[2L] else NULL
  build_ncbi(output_dir, version)
}
