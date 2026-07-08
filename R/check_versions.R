# Upstream source version checker for enrichments.
#
# Checks for each non-static enrichment in the manifest whether a newer
# upstream version is available. Used by CI to open/update GitHub issues
# when enrichments become outdated.

#' Check a Zenodo record for the latest version
#'
#' @param source_url Character. Zenodo download URL.
#' @return Named list with `version` (publication date) and `url`.
#' @export
check_zenodo_version <- function(source_url) {
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
    version = resp$metadata$publication_date %||%
      resp$metadata$version %||% NA_character_,
    url = api_url
  )
}


#' Check a Figshare article for the latest version
#'
#' @param source_url Character. Figshare download URL.
#' @return Named list with `version` and `url`.
#' @export
check_figshare_version <- function(source_url) {
  m <- regmatches(source_url, regexpr("files/([0-9]+)", source_url))
  if (length(m) == 0L) return(NULL)
  file_id <- sub("files/", "", m)

  api_url <- sprintf("https://api.figshare.com/v2/files/%s", file_id)
  resp <- tryCatch(jsonlite::read_json(api_url), error = function(e) NULL)
  if (is.null(resp)) return(NULL)

  article_id <- resp$article_id
  if (is.null(article_id)) return(NULL)

  versions_url <- sprintf("https://api.figshare.com/v2/articles/%s/versions",
                          article_id)
  versions <- tryCatch(jsonlite::read_json(versions_url),
                       error = function(e) NULL)
  if (is.null(versions) || length(versions) == 0L) return(NULL)

  latest <- versions[[1L]]
  list(
    version = as.character(latest$version %||% NA_character_),
    url = versions_url
  )
}


#' Check a Dryad dataset for the latest version
#'
#' @param source_url Character. Dryad download URL containing a DOI.
#' @return Named list with `version` and `url`.
#' @export
check_dryad_version <- function(source_url) {
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


#' Check a GBIF hosted dataset for the Last-Modified date
#'
#' @param source_url Character. GBIF hosted dataset URL.
#' @return Named list with `version` (date string) and `url`.
#' @export
check_gbif_version <- function(source_url) {
  resp <- tryCatch({
    headers <- curlGetHeaders(source_url)
    lm <- grep("^Last-Modified:", headers, value = TRUE, ignore.case = TRUE)
    if (length(lm) > 0L) {
      date_str <- trimws(sub("^Last-Modified:\\s*", "", lm[1L],
                             ignore.case = TRUE))
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


#' Check the GBIF backbone API for the latest update date
#'
#' @param source_url Character. (Unused; the GBIF backbone dataset ID is fixed.)
#' @return Named list with `version` and `url`.
#' @export
check_gbif_api_version <- function(source_url) {
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


#' Check Kew WCVP for the latest version
#'
#' WCVP has no dedicated API, so we fall back to a HEAD request via
#' [check_gbif_version()], which reads the bulk file's `Last-Modified` header
#' and formats it as `YYYY.MM`.
#'
#' @param source_url Character. WCVP download URL.
#' @return Named list with `version` and `url`.
#' @export
check_wcvp_version <- function(source_url) {
  check_gbif_version(source_url)
}


#' Check the LCVP data package for the latest version
#'
#' LCVP is distributed as an R data package; its data version is the `Version`
#' field of the package `DESCRIPTION` on GitHub.
#'
#' @param source_url Character. LCVP `tab_lcvp.rda` download URL (unused; the
#'   `DESCRIPTION` location is derived from the fixed repository).
#' @return Named list with `version` and `url`.
#' @export
check_lcvp_version <- function(source_url) {
  desc_url <- paste0("https://raw.githubusercontent.com/",
                     "idiv-biodiversity/LCVP/master/DESCRIPTION")
  lines <- tryCatch(readLines(desc_url, warn = FALSE),
                    error = function(e) NULL)
  if (is.null(lines)) return(NULL)

  ver_line <- grep("^Version:", lines, value = TRUE)
  version <- if (length(ver_line) > 0L) {
    trimws(sub("^Version:", "", ver_line[1L]))
  } else {
    NA_character_
  }
  list(version = version, url = desc_url)
}


#' Dispatch version check based on a manifest entry's source format
#'
#' @param entry List. Manifest enrichment entry with `source_url`,
#'   `source_format`, `source_version`, `static`.
#' @return Named list with `source_version`, `upstream_version`, `outdated`,
#'   `check_url`, and optionally `note`.
#' @export
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
    "multi_tsv" = NULL,
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


#' Check all non-static enrichments in a manifest for version freshness
#'
#' @param manifest_path Character. Path to manifest.json.
#' @return Data.frame with columns: name, source_version, upstream_version,
#'   outdated, check_url, note.
#' @export
check_all_enrichment_versions <- function(
    manifest_path = "manifest/manifest.json") {
  manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  enrichments <- manifest$enrichments
  if (is.null(enrichments) || length(enrichments) == 0L) {
    message("No enrichments in manifest.")
    return(data.frame())
  }

  results <- lapply(names(enrichments), function(name) {
    entry <- enrichments[[name]]
    message(sprintf("Checking '%s' (%s)...", name,
                    entry$source_format %||% "unknown"))
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
