# Generic download helpers for enrichment build pipeline.
#
# These are shared by many parse functions in the enrichment registry. Each
# helper handles caching (skip re-download if the file is already present
# and non-empty), follows redirects, and sets a User-Agent header.


#' Download a file via curl
#'
#' Downloads `url` into `dest_dir/filename` if the destination does not
#' already exist (or is empty). Follows redirects, sets a User-Agent header,
#' and optionally a Referer.
#'
#' @param url Character. URL to download.
#' @param dest_dir Character. Directory to save into. Created if missing.
#' @param filename Character. Output filename.
#' @param referer Character or `NULL`. Optional Referer header.
#' @param user_agent Character or `NULL`. Override the default User-Agent.
#' @return Path to the downloaded file.
#' @export
download_curl_file <- function(url, dest_dir, filename, referer = NULL,
                               user_agent = NULL) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(dest_dir, filename)
  if (file.exists(dest) && file.size(dest) > 100L) return(dest)

  h <- curl::new_handle()
  curl::handle_setopt(h, followlocation = TRUE, maxredirs = 10L)
  ua <- user_agent %||% "Mozilla/5.0 (compatible; taxifydb/0.1)"
  headers <- list("User-Agent" = ua)
  if (!is.null(referer)) headers[["Referer"]] <- referer
  do.call(curl::handle_setheaders, c(list(h), headers))
  curl::curl_download(url, dest, handle = h)

  if (!file.exists(dest) || file.size(dest) < 100L) {
    stop(sprintf("Download failed or produced empty file: %s", url),
         call. = FALSE)
  }
  dest
}


#' Download a ZIP and extract, returning the path to a matching file
#'
#' Downloads `url` to `dest_dir/source.zip` (if not cached), extracts into
#' `dest_dir/extracted/`, and returns the first file matching `pattern`. If
#' `pattern` is `NULL`, returns the extraction directory itself.
#'
#' @param url Character. URL to a ZIP archive.
#' @param dest_dir Character. Directory for download and extraction.
#' @param pattern Character or `NULL`. Regex to match the target file inside
#'   the ZIP. If `NULL`, returns the extraction directory itself.
#' @return Path to the matched file, or the extraction directory if `pattern`
#'   is `NULL`.
#' @export
download_and_unzip <- function(url, dest_dir, pattern = NULL) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  zip_path <- file.path(dest_dir, "source.zip")

  if (!file.exists(zip_path) || file.size(zip_path) < 100L) {
    h <- curl::new_handle()
    curl::handle_setopt(h, followlocation = TRUE, maxredirs = 10L)
    curl::handle_setheaders(h, "User-Agent" = "R/4.5 taxifydb")
    curl::curl_download(url, zip_path, handle = h)
  }

  extract_dir <- file.path(dest_dir, "extracted")
  if (!dir.exists(extract_dir)) {
    dir.create(extract_dir, recursive = TRUE)
    utils::unzip(zip_path, exdir = extract_dir)
  }

  if (is.null(pattern)) return(extract_dir)

  files <- list.files(extract_dir, pattern = pattern, full.names = TRUE,
                      recursive = TRUE, ignore.case = TRUE)
  if (length(files) == 0L) {
    stop(sprintf(
      "No file matching '%s' found in ZIP.\nContents: %s",
      pattern,
      paste(list.files(extract_dir, recursive = TRUE), collapse = ", ")
    ), call. = FALSE)
  }
  files[1L]
}


#' Fetch paginated GBIF API results
#'
#' Pages through a GBIF API endpoint that uses `offset` + `limit` and an
#' `endOfRecords` flag. Returns a combined data.frame of `$results` payloads.
#'
#' @param base_url Character. GBIF API endpoint.
#' @param params Named list. Query parameters (excluding `offset`/`limit`).
#' @param limit Integer. Page size.
#' @param max_pages Integer. Maximum pages to fetch.
#' @return A data.frame of combined results. Empty data.frame if the endpoint
#'   returned no rows.
#' @export
download_gbif_api_pages <- function(base_url, params, limit = 1000L,
                                    max_pages = 100L) {
  all_rows <- vector("list", max_pages)
  offset <- 0L
  page <- 1L

  repeat {
    if (page > max_pages) break

    query <- c(params, list(limit = limit, offset = offset))
    query_str <- paste(
      vapply(names(query), function(k) {
        paste0(k, "=", utils::URLencode(as.character(query[[k]]),
                                        reserved = TRUE))
      }, character(1L)),
      collapse = "&"
    )
    url <- paste0(base_url, "?", query_str)

    resp <- curl::curl_fetch_memory(url)
    if (resp$status_code != 200L) break

    data <- jsonlite::fromJSON(rawToChar(resp$content))
    results <- data$results
    if (is.null(results) || nrow(results) == 0L) break

    all_rows[[page]] <- results
    page <- page + 1L

    if (isTRUE(data$endOfRecords)) break
    offset <- offset + limit
  }

  all_rows <- all_rows[!vapply(all_rows, is.null, logical(1L))]
  if (length(all_rows) == 0L) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  flat <- lapply(all_rows, function(df) {
    for (col in names(df)) {
      if (is.data.frame(df[[col]]) || is.list(df[[col]])) df[[col]] <- NULL
    }
    row.names(df) <- NULL
    df
  })

  if (requireNamespace("data.table", quietly = TRUE)) {
    as.data.frame(data.table::rbindlist(flat, fill = TRUE),
                  stringsAsFactors = FALSE)
  } else {
    # Base R fallback: align columns, fill missing with NA, rbind
    all_cols <- unique(unlist(lapply(flat, names)))
    flat2 <- lapply(flat, function(df) {
      miss <- setdiff(all_cols, names(df))
      for (col in miss) df[[col]] <- NA
      df[, all_cols, drop = FALSE]
    })
    do.call(rbind, flat2)
  }
}
