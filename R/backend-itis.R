# ITIS: SQLite -> normalized data.frame -> .vtr
#
# ITIS stores taxonomy in a relational SQLite database. Key tables:
#   - taxonomic_units: main taxon table (tsn, complete_name, rank_id, ...)
#   - synonym_links: tsn -> tsn_accepted pairs
#   - taxon_unit_types: rank_id -> rank_name
#   - taxon_authors_lkp: taxon_author_id -> taxon_author
#
# The hierarchy is stored as parent_tsn links. Family and genus are resolved
# by walking up the tree. Epithet components are in unit_name1..4.

.itis_url <- "https://www.itis.gov/downloads/itisSqlite.zip"


#' Download and extract the ITIS SQLite database
#'
#' @param dest Character. Destination directory.
#' @param verbose Logical.
#' @return Path to the extracted SQLite database file.
#' @export
download_itis <- function(dest = tempdir(), verbose = TRUE) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)

  if (verbose) message("Downloading ITIS SQLite dump (~212 MB)...")
  zip_path <- download_curl_file(.itis_url, dest, "itisSqlite.zip")

  if (verbose) message("Extracting...")
  utils::unzip(zip_path, exdir = dest)

  sqlite_files <- list.files(dest, pattern = "\\.sqlite$",
                             recursive = TRUE, full.names = TRUE)
  if (length(sqlite_files) == 0L) {
    stop("No .sqlite file found in ITIS download.")
  }

  sqlite_path <- sqlite_files[1L]
  if (verbose) message(sprintf("ITIS database: %s", basename(sqlite_path)))

  unlink(zip_path)
  sqlite_path
}


#' Read and normalize the ITIS SQLite database
#'
#' @param sqlite_path Character. Path to the ITIS .sqlite file.
#' @param verbose Logical.
#' @return A normalized data.frame ready for [precompute_backbone()].
#' @export
read_itis <- function(sqlite_path, verbose = TRUE) {
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("RSQLite is required for ITIS conversion. Install with: ",
         "install.packages('RSQLite')")
  }
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("DBI is required for ITIS conversion. Install with: ",
         "install.packages('DBI')")
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  if (verbose) message("Reading taxonomic_units...")
  taxa <- DBI::dbGetQuery(con, "
    SELECT
      tu.tsn,
      tu.complete_name,
      tu.rank_id,
      tu.name_usage,
      tu.parent_tsn,
      tu.unit_name1,
      tu.unit_name2,
      tu.unit_name3,
      tu.unit_name4,
      tu.taxon_author_id,
      tu.kingdom_id
    FROM taxonomic_units tu
  ")
  if (verbose) message(sprintf("  %s rows",
                               format(nrow(taxa), big.mark = ",")))

  if (verbose) message("Joining rank names...")
  ranks <- DBI::dbGetQuery(con, "
    SELECT rank_id, rank_name, kingdom_id
    FROM taxon_unit_types
  ")
  ranks$lookup_key <- paste(ranks$rank_id, ranks$kingdom_id, sep = "_")
  taxa$lookup_key <- paste(taxa$rank_id, taxa$kingdom_id, sep = "_")
  taxa$rank_name <- ranks$rank_name[match(taxa$lookup_key, ranks$lookup_key)]
  taxa$lookup_key <- NULL

  if (verbose) message("Joining authors...")
  authors <- DBI::dbGetQuery(con, "
    SELECT taxon_author_id, taxon_author
    FROM taxon_authors_lkp
  ")
  taxa$authorship <- authors$taxon_author[match(taxa$taxon_author_id,
                                                authors$taxon_author_id)]

  if (verbose) message("Resolving synonyms...")
  syn_links <- DBI::dbGetQuery(con, "
    SELECT tsn, tsn_accepted
    FROM synonym_links
  ")
  taxa$accepted_tsn <- syn_links$tsn_accepted[match(taxa$tsn, syn_links$tsn)]

  taxa$taxonomic_status <- ifelse(
    tolower(taxa$name_usage) %in% c("valid", "accepted"),
    "ACCEPTED",
    "SYNONYM"
  )

  has_syn_link <- !is.na(taxa$accepted_tsn)
  taxa$accepted_name_usage_id <- ifelse(
    has_syn_link,
    as.character(taxa$accepted_tsn),
    ifelse(taxa$taxonomic_status == "SYNONYM",
           as.character(taxa$parent_tsn),
           NA_character_)
  )

  if (verbose) message("Walking hierarchy for family/genus...")
  taxa$id <- as.character(taxa$tsn)
  taxa$parent_id <- as.character(taxa$parent_tsn)
  taxa$rank <- tolower(taxa$rank_name)
  taxa$name <- taxa$complete_name

  taxa <- resolve_hierarchy(taxa,
                            target_ranks = c("family", "genus", "kingdom"))

  taxa$specific_epithet <- ifelse(
    !is.na(taxa$unit_name2) & nzchar(trimws(taxa$unit_name2)),
    trimws(taxa$unit_name2),
    NA_character_
  )
  taxa$infraspecific_epithet <- ifelse(
    !is.na(taxa$unit_name3) & nzchar(trimws(taxa$unit_name3)),
    trimws(taxa$unit_name3),
    ifelse(
      !is.na(taxa$unit_name4) & nzchar(trimws(taxa$unit_name4)),
      trimws(taxa$unit_name4),
      NA_character_
    )
  )

  if (verbose) message("Normalizing to unified schema...")
  col_map <- list(
    taxon_id                = "id",
    canonical_name          = "complete_name",
    taxon_rank              = "rank_name",
    taxonomic_status        = "taxonomic_status",
    accepted_name_usage_id  = "accepted_name_usage_id",
    family                  = "resolved_family",
    genus                   = "resolved_genus",
    specific_epithet        = "specific_epithet",
    authorship              = "authorship",
    infraspecific_epithet   = "infraspecific_epithet"
  )

  extra_cols <- list(
    kingdom = "resolved_kingdom",
    kingdom_id = "kingdom_id"
  )

  df <- normalize_backbone(taxa, col_map, extra_cols)
  if (verbose) message(sprintf("  Normalized: %s rows",
                               format(nrow(df), big.mark = ",")))
  df
}


#' Build the ITIS backbone .vtr from scratch
#'
#' Downloads the ITIS SQLite dump, normalizes it, precomputes matching keys,
#' embeds synonym info, and writes the final .vtr with indexes.
#'
#' @param output_dir Character. Directory for the output .vtr.
#' @param version Character or NULL. If NULL, uses YYYY.MM of build date.
#' @param verbose Logical.
#' @return Path to the .vtr file (invisibly).
#' @export
build_itis <- function(output_dir = "output/itis", version = NULL,
                       verbose = TRUE) {
  if (is.null(version)) {
    version <- format(Sys.Date(), "%Y.%m")
  }

  tmp <- tempfile("itis_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  sqlite_path <- download_itis(dest = tmp, verbose = verbose)

  df <- read_itis(sqlite_path, verbose = verbose)

  if (verbose) message("Precomputing keys and embedding synonyms...")
  df <- precompute_backbone(df)

  vtr_path <- file.path(output_dir, "itis.vtr")
  build_vtr(df, vtr_path, "itis", version, .itis_url)

  invisible(vtr_path)
}
