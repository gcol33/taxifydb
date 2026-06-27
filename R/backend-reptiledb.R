# Reptile Database: taxa.csv + synonyms.xlsx + checklist.xlsx -> .vtr
#
# The Reptile Database (Uetz et al.) is the global taxonomic reference for
# reptiles: snakes, lizards, amphisbaenians, turtles, crocodiles and the
# tuatara (~12.6k accepted species, ~47k synonyms). License: CC-BY 4.0.
#
# Three bulk sources are combined, each used for what it carries cleanly:
#   * taxa.csv  (reptarium structured export) -> accepted species + subspecies
#     with genus/family/subfamily/authority. This is the current release.
#   * reptile_synonyms_*.xlsx -> the synonym -> current-name map. The Reptile
#     Database publishes this as a periodic snapshot (latest 2023-04); it is the
#     only bulk synonym list and lags the accepted release slightly.
#   * reptile_checklist_*.xlsx -> family -> order (suborder) lookup, the one
#     field taxa.csv omits.
#
# Reptiles carry no kingdom/phylum/class columns in the source, so the backbone
# stamps the fixed higher classification (Animalia / Chordata / Reptilia).

.reptiledb_taxa_url      <- "http://reptile-database.reptarium.cz/interfaces/export/taxa.csv"
.reptiledb_synonyms_url  <- "http://www.reptile-database.org/data/reptile_synonyms_2023_04.xlsx"
.reptiledb_checklist_url <- "http://www.reptile-database.org/data/reptile_checklist_2026_06.xlsx"
.reptiledb_version_default <- "2026.06"


#' Download the Reptile Database bulk files
#'
#' @param dest Character. Destination directory.
#' @param verbose Logical.
#' @return Named list with paths: `taxa`, `synonyms`, `checklist`.
#' @export
download_reptiledb <- function(dest = tempdir(), verbose = TRUE) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)

  taxa_path      <- file.path(dest, "taxa.csv")
  synonyms_path  <- file.path(dest, "reptile_synonyms.xlsx")
  checklist_path <- file.path(dest, "reptile_checklist.xlsx")

  # The reptile-database.org host negotiates only legacy TLS; curl's handle
  # default ciphers handle it, but be explicit about a generous timeout.
  h <- curl::new_handle(connecttimeout = 60, timeout = 600)

  if (verbose) message("Downloading Reptile Database taxa export...")
  curl::curl_download(.reptiledb_taxa_url, taxa_path, handle = h,
                      quiet = !verbose)

  if (verbose) message("Downloading Reptile Database synonyms (2023-04)...")
  curl::curl_download(.reptiledb_synonyms_url, synonyms_path, handle = h,
                      quiet = !verbose)

  if (verbose) message("Downloading Reptile Database checklist (family -> order)...")
  curl::curl_download(.reptiledb_checklist_url, checklist_path, handle = h,
                      quiet = !verbose)

  list(taxa = taxa_path, synonyms = synonyms_path, checklist = checklist_path)
}


#' Read and normalize the Reptile Database
#'
#' @param paths Named list from [download_reptiledb()].
#' @param verbose Logical.
#' @return A normalized backbone data.frame.
#' @export
read_reptiledb <- function(paths, verbose = TRUE) {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop("Package 'openxlsx2' is required to read the Reptile Database synonyms.",
         call. = FALSE)
  }

  # ---- Accepted taxa (species + subspecies) from taxa.csv ----
  if (verbose) message("Reading Reptile Database taxa...")
  taxa <- utils::read.csv(paths$taxa, sep = ";", quote = "",
                          stringsAsFactors = FALSE, check.names = FALSE,
                          fileEncoding = "UTF-8")
  if (verbose) message(sprintf("  %s accepted rows", format(nrow(taxa), big.mark = ",")))

  is_subsp <- nzchar(trimws(taxa$infraspecific_epithet))
  canonical <- ifelse(
    is_subsp,
    paste(taxa$genus, taxa$specific_epithet, taxa$infraspecific_epithet),
    paste(taxa$genus, taxa$specific_epithet)
  )
  authorship <- ifelse(is_subsp & nzchar(trimws(taxa$infraspecific_authority)),
                       taxa$infraspecific_authority, taxa$authority)

  accepted <- data.frame(
    taxon_id               = taxa$taxon_id,
    canonical_name         = canonical,
    taxon_rank             = ifelse(is_subsp, "SUBSPECIES", "SPECIES"),
    taxonomic_status       = "ACCEPTED",
    accepted_name_usage_id = NA_character_,
    family                 = taxa$family,
    genus                  = taxa$genus,
    specific_epithet       = taxa$specific_epithet,
    authorship             = authorship,
    infraspecific_epithet  = ifelse(is_subsp, taxa$infraspecific_epithet,
                                    NA_character_),
    stringsAsFactors       = FALSE
  )

  # ---- family -> order (suborder) from the checklist ----
  order_by_family <- NULL
  if (!is.null(paths$checklist) && file.exists(paths$checklist)) {
    if (verbose) message("Building family -> order lookup from checklist...")
    chk <- openxlsx2::wb_to_df(openxlsx2::wb_load(paths$checklist),
                               sheet = "Checklist", col_names = TRUE)
    keep <- !is.na(chk$Family) & !is.na(chk$order)
    fam <- trimws(chk$Family[keep])
    ord <- trimws(chk$order[keep])
    first <- !duplicated(fam)
    order_by_family <- stats::setNames(ord[first], fam[first])
  }
  accepted$order <- if (is.null(order_by_family)) {
    NA_character_
  } else {
    unname(order_by_family[accepted$family])
  }

  # ---- Synonyms: synonym -> current accepted name ----
  if (verbose) message("Reading Reptile Database synonyms...")
  syn <- openxlsx2::wb_to_df(openxlsx2::wb_load(paths$synonyms),
                             sheet = 1, col_names = TRUE)
  names(syn) <- tolower(trimws(names(syn)))
  syn_name <- trimws(syn[["synonym"]])
  cur_name <- trimws(syn[["current name"]])

  # Map current name -> accepted taxon_id. Accepted canonical names are unique
  # per taxon, so a direct match resolves the synonym's accepted id.
  acc_id_by_name <- stats::setNames(accepted$taxon_id, accepted$canonical_name)
  acc_id <- unname(acc_id_by_name[cur_name])

  # The synonym list includes each accepted name among its own synonyms; those
  # self-references just duplicate the accepted row, so drop them.
  resolvable <- !is.na(syn_name) & nzchar(syn_name) & !is.na(acc_id)
  keep_syn   <- resolvable & syn_name != cur_name
  n_dropped  <- sum(!is.na(syn_name) & nzchar(syn_name) & is.na(acc_id))
  n_selfref  <- sum(resolvable & syn_name == cur_name)
  syn_name <- syn_name[keep_syn]
  acc_id   <- acc_id[keep_syn]

  # Deduplicate identical synonym -> accepted pairs.
  dup <- duplicated(paste(syn_name, acc_id, sep = "\r"))
  syn_name <- syn_name[!dup]
  acc_id   <- acc_id[!dup]

  if (verbose) {
    message(sprintf(
      "  %s synonyms (%s dropped: no accepted match, %s self-references)",
      format(length(syn_name), big.mark = ","),
      format(n_dropped, big.mark = ","),
      format(n_selfref, big.mark = ",")))
  }

  # Inherit family/genus/order from the accepted taxon each synonym points to.
  acc_row <- match(acc_id, accepted$taxon_id)
  syn_words <- strsplit(syn_name, "\\s+")
  syn_genus <- vapply(syn_words, function(w) if (length(w) >= 1L) w[1L] else NA_character_,
                      character(1L))
  syn_epithet <- vapply(syn_words, function(w) if (length(w) >= 2L) w[2L] else NA_character_,
                        character(1L))

  synonyms <- data.frame(
    taxon_id               = paste0("rdbsyn_", seq_along(syn_name)),
    canonical_name         = syn_name,
    taxon_rank             = "SPECIES",
    taxonomic_status       = "SYNONYM",
    accepted_name_usage_id = acc_id,
    family                 = accepted$family[acc_row],
    genus                  = syn_genus,
    specific_epithet       = syn_epithet,
    authorship             = NA_character_,
    infraspecific_epithet  = NA_character_,
    order                  = accepted$order[acc_row],
    stringsAsFactors       = FALSE
  )

  out <- rbind(accepted, synonyms)

  # ---- Fixed higher classification (reptiles only) ----
  out$kingdom <- "Animalia"
  out$phylum  <- "Chordata"
  out$class   <- "Reptilia"

  if (verbose) {
    message(sprintf("Normalizing to unified schema (%s rows: %s accepted, %s synonyms)...",
                    format(nrow(out), big.mark = ","),
                    format(nrow(accepted), big.mark = ","),
                    format(nrow(synonyms), big.mark = ",")))
  }

  col_map <- list(
    taxon_id               = "taxon_id",
    canonical_name         = "canonical_name",
    taxon_rank             = "taxon_rank",
    taxonomic_status       = "taxonomic_status",
    accepted_name_usage_id = "accepted_name_usage_id",
    family                 = "family",
    genus                  = "genus",
    specific_epithet       = "specific_epithet",
    authorship             = "authorship",
    infraspecific_epithet  = "infraspecific_epithet"
  )
  extra_cols <- list(kingdom = "kingdom", phylum = "phylum",
                     class = "class", order = "order")

  normalize_backbone(out, col_map, extra_cols)
}


#' Build the Reptile Database backbone .vtr
#'
#' @param output_dir Character.
#' @param version Character or NULL.
#' @param verbose Logical.
#' @return Path to the .vtr file (invisibly).
#' @export
build_reptiledb <- function(output_dir = "output/reptiledb", version = NULL,
                            verbose = TRUE) {
  if (is.null(version)) version <- .reptiledb_version_default

  tmp <- tempfile("reptiledb_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  paths <- download_reptiledb(dest = tmp, verbose = verbose)
  df <- read_reptiledb(paths, verbose = verbose)

  if (verbose) message("Precomputing keys and embedding synonyms...")
  df <- precompute_backbone(df)

  vtr_path <- file.path(output_dir, "reptiledb.vtr")
  build_vtr(df, vtr_path, "reptiledb", version, .reptiledb_taxa_url)

  invisible(vtr_path)
}
