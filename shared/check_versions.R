# ---- Enrichment source version checker ----
#
# Checks upstream sources for each non-static enrichment to detect
# whether new versions are available. Used by CI to open/update
# GitHub issues when enrichments become outdated.
#
# Usage: Rscript shared/check_versions.R [manifest_path]
#   Returns JSON array of outdated enrichments to stdout.

suppressPackageStartupMessages(library(jsonlite))


# ---- Source-specific version checkers ----

#' Check Zenodo record for latest version
#'
#' Zenodo records have a DOI concept that points to the latest version.
#' We extract the record ID from the source_url and query the API.
#'
#' @param source_url Character. Zenodo download URL.
#' @return Named list with `version` (publication date) and `url`.
check_zenodo_version <- function(source_url) {
  # Extract record ID: https://zenodo.org/records/7534792/files/...
  m <- regmatches(source_url, regexpr("records/([0-9]+)", source_url))
  if (length(m) == 0L) return(NULL)
  record_id <- sub("records/", "", m)

  api_url <- sprintf("https://zenodo.org/api/records/%s", record_id)
  resp <- tryCatch(
    jsonlite::read_json(api_url),
    error = function(e) NULL
  )
  if (is.null(resp)) return(NULL)

  list(
    version = resp$metadata$publication_date %||% resp$metadata$version %||% NA_character_,
    url = api_url
  )
}


#' Check Figshare article for latest version
#'
#' @param source_url Character. Figshare download URL.
#' @return Named list with `version` and `url`.
check_figshare_version <- function(source_url) {
  # Extract article ID: https://ndownloader.figshare.com/files/NNNNN
  # Need to resolve to the article first
  m <- regmatches(source_url, regexpr("files/([0-9]+)", source_url))
  if (length(m) == 0L) return(NULL)
  file_id <- sub("files/", "", m)

  # Figshare file endpoint gives us the article
  api_url <- sprintf("https://api.figshare.com/v2/files/%s", file_id)
  resp <- tryCatch(jsonlite::read_json(api_url), error = function(e) NULL)
  if (is.null(resp)) return(NULL)

  # Get article versions if we have an article_id
  article_id <- resp$article_id
  if (is.null(article_id)) return(NULL)

  versions_url <- sprintf("https://api.figshare.com/v2/articles/%s/versions",
                           article_id)
  versions <- tryCatch(jsonlite::read_json(versions_url), error = function(e) NULL)
  if (is.null(versions) || length(versions) == 0L) return(NULL)

  latest <- versions[[1L]]  # First is most recent
  list(
    version = as.character(latest$version %||% NA_character_),
    url = versions_url
  )
}


#' Check Dryad dataset for latest version
#'
#' @param source_url Character. Dryad download URL containing a DOI.
#' @return Named list with `version` and `url`.
check_dryad_version <- function(source_url) {
  # Extract DOI: doi%3A10.5061%2Fdryad.XXXXX
  m <- regmatches(source_url, regexpr("doi%3A[^/]+", source_url))
  if (length(m) == 0L) return(NULL)
  doi <- utils::URLdecode(m)

  api_url <- sprintf("https://datadryad.org/api/v2/datasets/%s",
                      utils::URLencode(doi, reserved = TRUE))
  resp <- tryCatch(jsonlite::read_json(api_url), error = function(e) NULL)
  if (is.null(resp)) return(NULL)

  list(
    version = as.character(resp$versionNumber %||% NA_character_),
    url = api_url
  )
}


#' Check GBIF hosted dataset for last-modified date
#'
#' Uses HEAD request to check Last-Modified header.
#'
#' @param source_url Character. GBIF hosted dataset URL.
#' @return Named list with `version` (date string) and `url`.
check_gbif_version <- function(source_url) {
  resp <- tryCatch({
    con <- url(source_url, method = "libcurl")
    on.exit(close(con))
    headers <- curlGetHeaders(source_url)
    lm <- grep("^Last-Modified:", headers, value = TRUE, ignore.case = TRUE)
    if (length(lm) > 0L) {
      date_str <- trimws(sub("^Last-Modified:\\s*", "", lm[1L],
                              ignore.case = TRUE))
      # Parse HTTP date → YYYY.MM
      parsed <- as.Date(date_str, format = "%a, %d %b %Y %H:%M:%S")
      if (!is.na(parsed)) {
        format(parsed, "%Y.%m")
      } else {
        date_str
      }
    } else {
      NA_character_
    }
  }, error = function(e) NA_character_)

  list(version = resp, url = source_url)
}


#' Check GBIF API for latest species data version
#'
#' @param source_url Character. GBIF API URL.
#' @return Named list with `version` and `url`.
check_gbif_api_version <- function(source_url) {
  # GBIF species API doesn't have a version endpoint per se.
  # Check the backbone dataset metadata for last updated date.
  api_url <- "https://api.gbif.org/v1/dataset/d7dddbf4-2cf0-4f39-9b2a-bb099caae36c"
  resp <- tryCatch(jsonlite::read_json(api_url), error = function(e) NULL)
  if (is.null(resp)) return(NULL)

  modified <- resp$modified %||% resp$pubDate
  if (!is.null(modified)) {
    date <- as.Date(substr(modified, 1, 10))
    version <- format(date, "%Y.%m")
  } else {
    version <- NA_character_
  }

  list(version = version, url = api_url)
}


#' Check Kew WCVP for latest version
#'
#' @param source_url Character. WCVP download URL.
#' @return Named list with `version` and `url`.
check_wcvp_version <- function(source_url) {
  # WCVP doesn't have a great API — use HEAD for last-modified
  check_gbif_version(source_url)
}


# ---- Dispatcher ----

#' Check version for an enrichment based on its source format
#'
#' @param entry List. Manifest enrichment entry with source_url, source_format,
#'   source_version, static.
#' @return Named list with `source_version` (current known), `upstream_version`,
#'   `outdated` (logical), `check_url`.
check_enrichment_source_version <- function(entry) {
  if (isTRUE(entry$static)) {
    return(list(
      source_version = entry$source_version,
      upstream_version = entry$source_version,
      outdated = FALSE,
      check_url = NA_character_,
      note = "static dataset, skipped"
    ))
  }

  result <- switch(entry$source_format,
    "xlsx" = check_zenodo_version(entry$source_url),
    "csv"  = check_zenodo_version(entry$source_url),
    "zip"  = {
      if (grepl("gbif\\.org", entry$source_url)) {
        check_gbif_version(entry$source_url)
      } else if (grepl("kew\\.org", entry$source_url)) {
        check_wcvp_version(entry$source_url)
      } else if (grepl("zenodo\\.org", entry$source_url)) {
        check_zenodo_version(entry$source_url)
      } else {
        NULL
      }
    },
    "gbif_api" = check_gbif_api_version(entry$source_url),
    "tsv" = {
      if (grepl("figshare\\.com", entry$source_url)) {
        check_figshare_version(entry$source_url)
      } else {
        NULL
      }
    },
    "dryad_zip" = check_dryad_version(entry$source_url),
    "multi_tsv" = NULL,  # LEDA has no API
    NULL
  )

  if (is.null(result) || is.na(result$version)) {
    return(list(
      source_version = entry$source_version,
      upstream_version = NA_character_,
      outdated = NA,
      check_url = if (!is.null(result)) result$url else NA_character_,
      note = "could not determine upstream version"
    ))
  }

  list(
    source_version = entry$source_version,
    upstream_version = result$version,
    outdated = !identical(entry$source_version, result$version),
    check_url = result$url
  )
}


# ---- Main ----

#' Check all non-static enrichments for version freshness
#'
#' @param manifest_path Character. Path to manifest.json.
#' @return Data.frame with columns: name, source_version, upstream_version,
#'   outdated, check_url, note.
check_all_enrichment_versions <- function(manifest_path = "manifest/manifest.json") {
  manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  enrichments <- manifest$enrichments
  if (is.null(enrichments) || length(enrichments) == 0L) {
    message("No enrichments in manifest.")
    return(data.frame())
  }

  results <- lapply(names(enrichments), function(name) {
    entry <- enrichments[[name]]
    message(sprintf("Checking '%s' (%s)...", name, entry$source_format %||% "unknown"))
    res <- tryCatch(
      check_enrichment_source_version(entry),
      error = function(e) {
        list(
          source_version = entry$source_version,
          upstream_version = NA_character_,
          outdated = NA,
          check_url = NA_character_,
          note = conditionMessage(e)
        )
      }
    )
    res$name <- name
    res
  })

  do.call(rbind, lapply(results, function(r) {
    data.frame(
      name = r$name,
      source_version = r$source_version %||% NA_character_,
      upstream_version = r$upstream_version %||% NA_character_,
      outdated = r$outdated %||% NA,
      check_url = r$check_url %||% NA_character_,
      note = r$note %||% "",
      stringsAsFactors = FALSE
    )
  }))
}


# ---- CLI entry point ----
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  manifest_path <- if (length(args) >= 1L) args[1L] else "manifest/manifest.json"

  results <- check_all_enrichment_versions(manifest_path)

  outdated <- results[!is.na(results$outdated) & results$outdated, , drop = FALSE]
  unknown <- results[is.na(results$outdated), , drop = FALSE]

  if (nrow(outdated) > 0L) {
    message("\nOutdated enrichments:")
    for (i in seq_len(nrow(outdated))) {
      message(sprintf("  %s: %s -> %s",
                      outdated$name[i],
                      outdated$source_version[i],
                      outdated$upstream_version[i]))
    }
  }

  if (nrow(unknown) > 0L) {
    message("\nCould not check:")
    for (i in seq_len(nrow(unknown))) {
      message(sprintf("  %s: %s", unknown$name[i], unknown$note[i]))
    }
  }

  if (nrow(outdated) == 0L) {
    message("\nAll enrichments are up to date.")
  }

  # Output JSON for CI consumption
  cat(jsonlite::toJSON(results, pretty = TRUE, auto_unbox = TRUE))
}
