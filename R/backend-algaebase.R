# AlgaeBase: ChecklistBank /nameusage/search -> normalized data.frame -> .vtr
#
# Curated algal taxonomy (~172k names; micro/macroalgae, cyanobacteria, some
# protists). Source: GBIF ChecklistBank dataset 304756. The /archive endpoint
# is disabled for this dataset (CC BY-NC license), so we paginate the
# /nameusage/search endpoint instead.
#
# Each search record embeds a full classification[] trail, so family/genus
# extraction is direct (no parent-id walk).
#
# Pagination strategy:
#   * ChecklistBank caps `offset` at 100,000 per slice.
#   * `synonym`, `bare name`, `provisionally accepted` statuses each fit
#     under the cap and are paginated as a single status slice.
#   * `accepted` (~122k) exceeds the cap, so it is sub-sliced by rank.
#   * Each (status[, rank]) slice paginates with limit=1000 until exhausted.
#
# License note: AlgaeBase is CC BY-NC. The derived .vtr may only be used for
# non-commercial purposes (academic/research is fine).

.algaebase_search_url <-
  "https://api.checklistbank.org/dataset/304756/nameusage/search"
.algaebase_url <- .algaebase_search_url       # provenance URL for build_vtr()
.algaebase_version_default <- "2025.04"
.algaebase_page_size <- 1000L
.algaebase_offset_cap <- 100000L


#' Fetch all AlgaeBase records via /nameusage/search
#'
#' @param verbose Logical.
#' @return A list of record objects (raw JSON list-of-lists).
#' @export
download_algaebase <- function(verbose = TRUE) {
  if (verbose) {
    message("NOTE: AlgaeBase is CC BY-NC (non-commercial use only).")
    message("Fetching AlgaeBase from ChecklistBank /nameusage/search ...")
    message(sprintf("  Endpoint: %s", .algaebase_search_url))
  }
  algaebase_fetch_all(verbose = verbose)
}


#' Read and normalize a fetched list of AlgaeBase records
#'
#' @param records A list of raw search records (from `download_algaebase()`).
#' @param verbose Logical.
#' @return A normalized data.frame.
#' @export
read_algaebase <- function(records, verbose = TRUE) {
  if (verbose) {
    message(sprintf("  %s records fetched",
                    format(length(records), big.mark = ",")))
    message("Flattening JSON records to data.frame...")
  }
  df <- algaebase_records_to_df(records)

  if (verbose) message("Normalizing taxonomy...")
  df <- algaebase_normalize(df, verbose = verbose)

  if (verbose) message("Normalizing to unified schema...")
  col_map <- list(
    taxon_id                = "taxon_id",
    canonical_name          = "canonical_name",
    taxon_rank              = "taxon_rank",
    taxonomic_status        = "taxonomic_status",
    accepted_name_usage_id  = "accepted_name_usage_id",
    family                  = "family",
    genus                   = "genus",
    specific_epithet        = "specific_epithet",
    authorship              = "authorship",
    infraspecific_epithet   = "infraspecific_epithet"
  )

  normalize_backbone(df, col_map)
}


#' Build the AlgaeBase backbone .vtr
#'
#' @param output_dir Character.
#' @param version Character or NULL.
#' @param verbose Logical.
#' @return Path to the .vtr file (invisibly).
#' @export
build_algaebase <- function(output_dir = "output/algaebase", version = NULL,
                            verbose = TRUE) {
  if (is.null(version)) version <- .algaebase_version_default

  records <- download_algaebase(verbose = verbose)
  df <- read_algaebase(records, verbose = verbose)

  if (verbose) message("Precomputing keys and embedding synonyms...")
  df <- precompute_backbone(df)

  vtr_path <- file.path(output_dir, "algaebase.vtr")
  build_vtr(df, vtr_path, "algaebase", version, .algaebase_search_url)

  invisible(vtr_path)
}


#' Fetch all AlgaeBase records, sliced to fit the 100k offset cap
#'
#' @noRd
algaebase_fetch_all <- function(verbose = TRUE) {
  records <- list()

  # Statuses that fit under the offset cap as a single slice
  for (st in c("synonym", "bare name", "provisionally accepted")) {
    rs <- algaebase_paginate(
      filters = list(status = st),
      label   = sprintf("status=%s", st),
      verbose = verbose
    )
    records <- c(records, rs)
  }

  # accepted (~122k) is over the cap — slice by rank
  ranks <- algaebase_facet_ranks(status = "accepted", verbose = verbose)
  for (rk in ranks) {
    rs <- algaebase_paginate(
      filters = list(status = "accepted", rank = rk),
      label   = sprintf("status=accepted&rank=%s", rk),
      verbose = verbose
    )
    records <- c(records, rs)
  }

  records
}


#' Discover the rank values in a status slice via the search facet API
#'
#' `facetLimit=50` overrides the default of 10; without it, tiny ranks
#' (unranked, subgenus, strain) get silently dropped.
#'
#' @noRd
algaebase_facet_ranks <- function(status, verbose = TRUE) {
  url <- sprintf("%s?%s&limit=0&facet=rank&facetLimit=50&facetMinCount=1",
                 .algaebase_search_url,
                 algaebase_qs(list(status = status)))
  res <- jsonlite::fromJSON(url, simplifyVector = FALSE)
  facet <- res$facets$rank %||% list()
  ranks <- vapply(facet, function(f) f$value %||% NA_character_,
                  character(1L))
  ranks <- ranks[!is.na(ranks) & nzchar(ranks)]
  if (verbose) {
    message(sprintf("  status=%s spans %d ranks", status, length(ranks)))
  }
  ranks
}


#' Paginate one status (and optional rank) slice until exhausted
#'
#' Aborts with an informative error if `total` would force an offset
#' above the 100,000 ChecklistBank cap.
#'
#' @noRd
algaebase_paginate <- function(filters, label, verbose = TRUE) {
  page_size <- .algaebase_page_size
  base_qs <- algaebase_qs(filters)

  fetch_offset <- function(offset) {
    url <- sprintf("%s?%s&limit=%d&offset=%d",
                   .algaebase_search_url, base_qs, page_size, offset)
    jsonlite::fromJSON(url, simplifyVector = FALSE)
  }

  first <- fetch_offset(0L)
  total <- first$total %||% 0L
  if (total == 0L) return(list())

  n_pages <- as.integer(ceiling(total / page_size))
  max_offset <- (n_pages - 1L) * page_size
  if (max_offset > .algaebase_offset_cap) {
    stop(sprintf(
      "Slice [%s] needs offset=%d which exceeds ChecklistBank's %d cap; refine filters",
      label, max_offset, .algaebase_offset_cap), call. = FALSE)
  }

  if (verbose) {
    message(sprintf("  [%s] %s records, %d page(s)",
                    label, format(total, big.mark = ","), n_pages))
  }

  pages <- vector("list", n_pages)
  pages[[1]] <- first$result
  for (i in seq_len(n_pages - 1L)) {
    pages[[i + 1L]] <- fetch_offset(i * page_size)$result
  }
  unlist(pages, recursive = FALSE)
}


#' Build a URL-encoded query string from a named filter list
#' @noRd
algaebase_qs <- function(filters) {
  paste(
    vapply(names(filters), function(k) {
      sprintf("%s=%s", k,
              utils::URLencode(as.character(filters[[k]]), reserved = TRUE))
    }, character(1L)),
    collapse = "&"
  )
}


#' Pull a (possibly nested) field from each record
#' @noRd
.algaebase_pluck <- function(records, ...) {
  path <- c(...)
  vapply(records, function(r) {
    val <- r
    for (key in path) {
      if (is.null(val)) return(NA_character_)
      val <- val[[key]]
    }
    if (is.null(val) || length(val) == 0L) NA_character_ else as.character(val)
  }, character(1L))
}


#' Pull family/genus from each record's classification[] trail
#' @noRd
.algaebase_pluck_classification_rank <- function(records, target_rank) {
  vapply(records, function(r) {
    cls <- r$classification
    if (is.null(cls) || length(cls) == 0L) return(NA_character_)
    for (entry in cls) {
      if (identical(entry$rank, target_rank)) {
        return(entry$name %||% NA_character_)
      }
    }
    NA_character_
  }, character(1L))
}


#' Flatten /nameusage/search records into a wide data.frame
#'
#' Each record has top-level `id`, a nested `usage` with `name.{...}`,
#' `status`, optional `accepted.id`, plus `classification[]`.
#'
#' @noRd
algaebase_records_to_df <- function(records) {
  data.frame(
    taxon_id              = .algaebase_pluck(records, "usage", "id"),
    canonical_name        = .algaebase_pluck(records, "usage", "name",
                                             "scientificName"),
    taxon_rank_raw        = .algaebase_pluck(records, "usage", "name", "rank"),
    raw_status            = .algaebase_pluck(records, "usage", "status"),
    accepted_id           = .algaebase_pluck(records, "usage", "accepted",
                                             "id"),
    name_genus            = .algaebase_pluck(records, "usage", "name",
                                             "genus"),
    cls_genus             = .algaebase_pluck_classification_rank(records,
                                                                  "genus"),
    cls_family            = .algaebase_pluck_classification_rank(records,
                                                                  "family"),
    specific_epithet      = .algaebase_pluck(records, "usage", "name",
                                             "specificEpithet"),
    authorship            = .algaebase_pluck(records, "usage", "name",
                                             "authorship"),
    infraspecific_epithet = .algaebase_pluck(records, "usage", "name",
                                             "infraspecificEpithet"),
    stringsAsFactors      = FALSE
  )
}


#' Normalize the flattened search frame
#'
#' Family/genus come straight from the embedded classification trail; rows
#' that ARE family or genus rank fill their own classification field.
#'
#' @noRd
algaebase_normalize <- function(df, verbose = TRUE) {
  rank_lower <- tolower(df$taxon_rank_raw)
  status_lower <- tolower(df$raw_status)

  status <- ifelse(
    status_lower %in% c("accepted", "provisionally accepted"),
    "ACCEPTED", "SYNONYM"
  )
  is_synonym <- status == "SYNONYM"

  acc_id <- ifelse(is_synonym, df$accepted_id, NA_character_)

  family <- df$cls_family
  family[rank_lower == "family"] <- df$canonical_name[rank_lower == "family"]

  # Prefer the parsed name's `genus` field; fall back to classification trail
  genus <- ifelse(is.na(df$name_genus) | !nzchar(df$name_genus),
                  df$cls_genus, df$name_genus)
  genus[rank_lower == "genus"] <- df$canonical_name[rank_lower == "genus"]

  species_ranks <- c("species", "subspecies", "variety", "varietas",
                     "form", "forma", "infraspecies",
                     "infraspecific name", "infrasubspecific name")
  no_genus <- is.na(genus) & rank_lower %in% species_ranks
  if (any(no_genus)) {
    genus[no_genus] <- split_scientific_name(df$canonical_name[no_genus])$genus
  }

  data.frame(
    taxon_id                = df$taxon_id,
    canonical_name          = trimws(df$canonical_name),
    taxon_rank              = toupper(df$taxon_rank_raw),
    taxonomic_status        = status,
    accepted_name_usage_id  = acc_id,
    family                  = trimws(family),
    genus                   = trimws(genus),
    specific_epithet        = trimws(df$specific_epithet),
    authorship              = trimws(df$authorship),
    infraspecific_epithet   = trimws(df$infraspecific_epithet),
    stringsAsFactors        = FALSE
  )
}
