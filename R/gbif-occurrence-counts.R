# GBIF occurrence counts per taxon key, for the GBIF backbone's `n_occurrences`
# column.
#
# taxify picks among the records of a name partly by how many GBIF occurrence
# records sit under each record's key: a name GBIF files both as an accepted
# record with no data and as a doubtful or synonym record carrying the data
# resolves to the key a download can be made with. The count read here is the
# one a GBIF occurrence search by `taxonKey` returns, which for an accepted key
# includes its synonyms and descendants and for a synonym key only the records
# matched to it.
#
# Counts are taken only for the keys that can decide a pick: every record whose
# matching key (`key_ci` or `key_normalized`) is shared by records pointing to
# two or more accepted taxa, plus the accepted targets of those records (which
# taxify_ids() reports). Everything else carries `NA`, which taxify sorts after
# any known count. The keys come from a previously built GBIF `.vtr`; the status
# fix that accompanies these counts does not touch the matching keys.
#
# The counts are frozen as a release asset so a backbone build reads a fixed
# input rather than the live API. Taking them runs a faceted occurrence search
# over batches of keys (`facet=taxonKey` with one `taxonKey` filter per key),
# roughly 0.2 s per 200 keys.

.gbif_counts_version <- "2026.09"
.gbif_counts_release <- paste0("gbif-occurrence-counts-", .gbif_counts_version)
.gbif_counts_asset   <- "gbif_occurrence_counts.tsv.gz"
.gbif_occurrence_api <- "https://api.gbif.org/v1/occurrence/search"


#' Taxon keys whose occurrence count can decide a GBIF pick
#'
#' @param vtr_path Character. Path to a built GBIF backbone `.vtr`.
#' @return Character vector of taxon keys: every record sharing a matching key
#'   with a record of another accepted taxon, plus the accepted targets of those
#'   records.
#' @export
gbif_count_keys <- function(vtr_path) {
  cols <- c("taxon_id", "key_ci", "key_normalized", "accepted_taxon_id")
  d <- vectra::tbl(vtr_path) |>
    vectra::select(!!!lapply(cols, as.name)) |>
    vectra::collect()
  colliding <- function(key) {
    ok <- !is.na(key) & !is.na(d$accepted_taxon_id)
    u <- unique(data.frame(k = key[ok], a = d$accepted_taxon_id[ok],
                           stringsAsFactors = FALSE))
    n <- table(u$k)
    ok & key %in% names(n)[n > 1L]
  }
  hit <- colliding(d$key_ci) | colliding(d$key_normalized)
  keys <- unique(c(d$taxon_id[hit], d$accepted_taxon_id[hit]))
  as.character(keys[!is.na(keys)])
}


#' One faceted count request for a batch of keys
#'
#' @param keys Character vector of taxon keys.
#' @param facet_limit Integer. Facet entries to request.
#' @return A named numeric vector of counts along `keys` (0 for a key absent
#'   from the facet), with attribute `truncated` set when the facet came back
#'   full, in which case an absent key may have been cut rather than empty.
#' @noRd
.gbif_facet_counts <- function(keys, facet_limit) {
  url <- paste0(.gbif_occurrence_api, "?limit=0&facet=taxonKey&facetLimit=",
                facet_limit, "&", paste0("taxonKey=", keys, collapse = "&"))
  body <- .gbif_get_json(url)
  f <- body$facets$counts[[1L]]
  counts <- if (is.null(f) || NROW(f) == 0L) {
    rep(0, length(keys))
  } else {
    x <- as.numeric(f$count[match(keys, as.character(f$name))])
    x[is.na(x)] <- 0
    x
  }
  structure(stats::setNames(counts, keys),
            truncated = !is.null(f) && NROW(f) >= facet_limit)
}


#' GET a GBIF API URL as parsed JSON, retrying transient failures
#'
#' @param url Character.
#' @param tries Integer.
#' @return The parsed body.
#' @noRd
.gbif_get_json <- function(url, tries = 5L) {
  for (i in seq_len(tries)) {
    resp <- tryCatch(curl::curl_fetch_memory(url), error = function(e) e)
    if (!inherits(resp, "error") && resp$status_code == 200L) {
      return(jsonlite::fromJSON(rawToChar(resp$content)))
    }
    if (i < tries) Sys.sleep(2^i)
  }
  what <- if (inherits(resp, "error")) conditionMessage(resp) else
    sprintf("HTTP %d", resp$status_code)
  stop(sprintf("GBIF API request failed after %d tries (%s): %s",
               tries, what, substr(url, 1L, 200L)), call. = FALSE)
}


#' Count GBIF occurrence records per taxon key
#'
#' @param keys Character vector of GBIF taxon keys.
#' @param batch_size Integer. Keys per request.
#' @param facet_limit Integer. Facet entries per request; a batch whose facet
#'   comes back full is re-counted key by key.
#' @param verbose Logical.
#' @return A data.frame with `taxon_id` and `n_occurrences`.
#' @export
count_gbif_occurrences <- function(keys, batch_size = 200L,
                                   facet_limit = 50000L, verbose = TRUE) {
  keys <- unique(as.character(keys[!is.na(keys)]))
  batches <- split(keys, ceiling(seq_along(keys) / batch_size))
  out <- vector("list", length(batches))
  for (b in seq_along(batches)) {
    x <- .gbif_facet_counts(batches[[b]], facet_limit)
    if (isTRUE(attr(x, "truncated"))) {
      # A full facet may have cut a requested key: count each one on its own.
      x[] <- vapply(names(x), function(k) {
        as.numeric(.gbif_get_json(sprintf("%s?limit=0&taxonKey=%s",
                                          .gbif_occurrence_api, k))$count)
      }, numeric(1L))
    }
    out[[b]] <- x
    if (verbose && (b %% 100L == 0L || b == length(batches))) {
      message(sprintf("  counted %s of %s keys",
                      format(sum(lengths(batches[seq_len(b)])), big.mark = ","),
                      format(length(keys), big.mark = ",")))
    }
  }
  counts <- unlist(out, use.names = TRUE)
  data.frame(taxon_id = names(counts), n_occurrences = unname(counts),
             stringsAsFactors = FALSE)
}


#' Build the GBIF occurrence-count snapshot
#'
#' Collects the keys [gbif_count_keys()] selects from a built GBIF backbone,
#' counts their occurrence records, and writes them as a gzipped TSV for
#' release under `gbif-occurrence-counts-<version>`.
#'
#' @param vtr_path Character. Path to a built GBIF backbone `.vtr`.
#' @param out_dir Character. Output directory.
#' @param verbose Logical.
#' @return Path to the written `.tsv.gz` (invisibly).
#' @export
build_gbif_occurrence_counts <- function(vtr_path, out_dir = "output/gbif",
                                         verbose = TRUE) {
  if (verbose) message("Selecting keys whose count can decide a pick...")
  keys <- gbif_count_keys(vtr_path)
  if (verbose) {
    message(sprintf("  %s keys", format(length(keys), big.mark = ",")))
  }
  counts <- count_gbif_occurrences(keys, verbose = verbose)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(out_dir, .gbif_counts_asset)
  con <- gzfile(path, "w")
  on.exit(close(con), add = TRUE)
  utils::write.table(counts, con, sep = "\t", quote = FALSE, row.names = FALSE)
  if (verbose) {
    message(sprintf("  wrote %s (%s keys, %s with records)", path,
                    format(nrow(counts), big.mark = ","),
                    format(sum(counts$n_occurrences > 0), big.mark = ",")))
  }
  invisible(path)
}


#' Download the published GBIF occurrence-count snapshot
#'
#' @param dest Character. Destination directory.
#' @param verbose Logical.
#' @return Path to the downloaded `.tsv.gz`.
#' @export
download_gbif_occurrence_counts <- function(dest = tempdir(), verbose = TRUE) {
  url <- sprintf("https://github.com/gcol33/taxifydb/releases/download/%s/%s",
                 .gbif_counts_release, .gbif_counts_asset)
  if (verbose) message(sprintf("Downloading GBIF occurrence counts: %s", url))
  download_curl_file(url, dest, .gbif_counts_asset)
}


#' Read a GBIF occurrence-count snapshot
#'
#' @param path Character. Path to the `.tsv.gz`.
#' @return Named numeric vector of counts by taxon key.
#' @export
read_gbif_occurrence_counts <- function(path) {
  df <- utils::read.delim(path, colClasses = c("character", "numeric"),
                          stringsAsFactors = FALSE)
  stats::setNames(df$n_occurrences, df$taxon_id)
}
