# Upstream source version checker for enrichments.
#
# Checks for each non-static enrichment in the manifest whether a newer
# upstream version is available. Used by CI to open/update GitHub issues
# when enrichments become outdated.

# Every probe below answers one question about a source host -- what identity
# does it give its newest version right now -- and answers it the same way:
#
#   id      identity of the newest upstream version, in whatever the host counts
#           in (a Zenodo record number, a Figshare or Dryad version number, a
#           Last-Modified stamp). This is what a build records and what a later
#           check compares against, so the comparison is like-for-like.
#   version the same identity in a form a reader recognises.
#   pinned  the identity the source URL itself names, where it names one. Only
#           Zenodo does; elsewhere the URL serves whatever is newest.
#   url     where the check looked.
#
# A probe returns NULL when it cannot reach or parse the host. Freshness is not
# decided here: probes report, check_enrichment_source_version() compares.

#' Check a Zenodo record for the latest version
#'
#' A Zenodo record is immutable, so querying the pinned record reports the day
#' that record was published however many newer versions its concept has since
#' gained. The `versions/latest` endpoint resolves the pinned record to the
#' newest one in the same concept.
#'
#' @param source_url Character. Zenodo download URL.
#' @return Named list with `id` (the latest record's number), `version` (its
#'   publication date), `pinned` (the record number the URL names), and `url`;
#'   `NULL` if the record cannot be read.
#' @export
check_zenodo_version <- function(source_url) {
  # Zenodo serves both the current /records/ and the legacy /record/ path.
  m <- regmatches(source_url, regexpr("records?/([0-9]+)", source_url))
  if (length(m) == 0L) return(NULL)
  record_id <- sub("records?/", "", m)

  api_url <- sprintf("https://zenodo.org/api/records/%s/versions/latest",
                     record_id)
  resp <- tryCatch(
    jsonlite::read_json(api_url),
    error = function(e) NULL
  )
  if (is.null(resp)) return(NULL)

  latest_id <- as.character(resp$id %||% NA_character_)
  if (is.na(latest_id)) return(NULL)
  list(
    id = latest_id,
    version = as.character(resp$metadata$publication_date %||%
                             resp$metadata$version %||% NA_character_),
    pinned = record_id,
    url = sprintf("https://zenodo.org/records/%s", latest_id)
  )
}


#' Check a Figshare article for the latest version
#'
#' Sources point either at an article or at one file within it; an article URL
#' names the article directly, a file URL is resolved to its article first.
#'
#' @param source_url Character. Figshare article or download URL.
#' @return Named list with `id` and `version` (both the newest version number)
#'   and `url`; `NULL` if the article cannot be read.
#' @export
check_figshare_version <- function(source_url) {
  article_id <- NULL

  a <- regmatches(source_url, regexpr("articles/([0-9]+)", source_url))
  if (length(a) > 0L) {
    article_id <- sub("articles/", "", a)
  } else {
    m <- regmatches(source_url, regexpr("files/([0-9]+)", source_url))
    if (length(m) == 0L) return(NULL)
    file_id <- sub("files/", "", m)

    api_url <- sprintf("https://api.figshare.com/v2/files/%s", file_id)
    resp <- tryCatch(jsonlite::read_json(api_url), error = function(e) NULL)
    if (is.null(resp)) return(NULL)
    article_id <- resp$article_id
  }
  if (is.null(article_id)) return(NULL)

  versions_url <- sprintf("https://api.figshare.com/v2/articles/%s/versions",
                          article_id)
  versions <- tryCatch(jsonlite::read_json(versions_url),
                       error = function(e) NULL)
  if (is.null(versions) || length(versions) == 0L) return(NULL)

  # Figshare lists versions ascending, so the newest is the last element.
  numbers <- vapply(versions, function(v) as.numeric(v$version %||% NA), 0)
  latest <- as.character(versions[[which.max(numbers)]]$version %||% NA_character_)
  if (is.na(latest)) return(NULL)
  list(
    id = latest,
    version = latest,
    url = versions_url
  )
}


#' Check a Dryad dataset for the latest version
#'
#' @param source_url Character. Dryad download URL containing a DOI.
#' @return Named list with `id` and `version` (both the newest version number)
#'   and `url`; `NULL` if the dataset cannot be read.
#' @export
check_dryad_version <- function(source_url) {
  m <- regmatches(source_url, regexpr("doi%3A[^/]+", source_url))
  if (length(m) == 0L) return(NULL)
  doi <- utils::URLdecode(m)

  api_url <- sprintf("https://datadryad.org/api/v2/datasets/%s",
                     utils::URLencode(doi, reserved = TRUE))
  resp <- tryCatch(jsonlite::read_json(api_url), error = function(e) NULL)
  if (is.null(resp)) return(NULL)

  version <- as.character(resp$versionNumber %||% NA_character_)
  if (is.na(version)) return(NULL)
  list(
    id = version,
    version = version,
    url = api_url
  )
}


#' Check a GBIF hosted dataset for the Last-Modified date
#'
#' The header stamp is kept whole as the identity and shortened to `YYYY.MM`
#' only for display: two files a fortnight apart share a month, and an identity
#' that cannot tell them apart reports a moved file as unchanged.
#'
#' @param source_url Character. GBIF hosted dataset URL.
#' @return Named list with `id` (the `Last-Modified` stamp), `version` (that
#'   stamp as `YYYY.MM`), and `url`; `NULL` if the header is absent.
#' @export
check_gbif_version <- function(source_url) {
  headers <- tryCatch(curlGetHeaders(source_url), error = function(e) NULL)
  if (is.null(headers)) return(NULL)

  lm <- grep("^Last-Modified:", headers, value = TRUE, ignore.case = TRUE)
  if (length(lm) == 0L) return(NULL)

  stamp <- trimws(sub("^Last-Modified:\\s*", "", lm[1L], ignore.case = TRUE))
  parsed <- as.Date(stamp, format = "%a, %d %b %Y %H:%M:%S")
  list(
    id = stamp,
    version = if (!is.na(parsed)) format(parsed, "%Y.%m") else stamp,
    url = source_url
  )
}


#' Check the GBIF backbone API for the latest update date
#'
#' @param source_url Character. (Unused; the GBIF backbone dataset ID is fixed.)
#' @return Named list with `id` (the dataset's `modified` timestamp), `version`
#'   (that timestamp as `YYYY.MM`), and `url`; `NULL` if the dataset cannot be
#'   read.
#' @export
check_gbif_api_version <- function(source_url) {
  api_url <- "https://api.gbif.org/v1/dataset/d7dddbf4-2cf0-4f39-9b2a-bb099caae36c"
  resp <- tryCatch(jsonlite::read_json(api_url), error = function(e) NULL)
  if (is.null(resp)) return(NULL)

  modified <- resp$modified %||% resp$pubDate
  if (is.null(modified)) return(NULL)

  date <- as.Date(substr(modified, 1, 10))
  list(
    id = as.character(modified),
    version = if (!is.na(date)) format(date, "%Y.%m") else as.character(modified),
    url = api_url
  )
}


#' Check Kew WCVP for the latest version
#'
#' WCVP has no dedicated API, so we fall back to a HEAD request via
#' [check_gbif_version()], which reads the bulk file's `Last-Modified` header
#' and formats it as `YYYY.MM`.
#'
#' @param source_url Character. WCVP download URL.
#' @return Named list with `id`, `version`, and `url`; `NULL` if the header is
#'   absent.
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


# Which probe answers for a source, keyed on the host the URL points at -- the
# one field every manifest entry carries. `source_format` describes the payload
# rather than where it lives, and the manifest writer emits it for a handful of
# entries only. Probes are named rather than held as functions so the table
# reads as a table; adding a host is one row.
#
# Order is significant: api.gbif.org is matched before the gbif.org it contains.
.upstream_probes <- list(
  list(host = "zenodo\\.org",      probe = "check_zenodo_version"),
  list(host = "figshare\\.com",    probe = "check_figshare_version"),
  list(host = "datadryad\\.org",   probe = "check_dryad_version"),
  list(host = "api\\.gbif\\.org",  probe = "check_gbif_api_version"),
  list(host = "gbif\\.org",        probe = "check_gbif_version"),
  list(host = "kew\\.org",         probe = "check_wcvp_version")
)


#' Name the probe that answers for a source URL
#'
#' @param source_url Character or NULL.
#' @return The probe's function name, or `NULL` when no host matches.
#' @noRd
.upstream_probe_for <- function(source_url) {
  url <- source_url %||% ""
  for (p in .upstream_probes) {
    if (grepl(p$host, url)) return(p$probe)
  }
  NULL
}


#' Ask a source host what identity its newest version carries
#'
#' The single entry point to the probes: a build calls it to record what it
#' read, and the weekly check calls it to see what upstream now offers. Both
#' therefore speak in the same identity, which is what makes the two comparable.
#'
#' @param source_url Character. The URL a build downloads its source from.
#' @return The probe's result (`id`, `version`, optionally `pinned`, `url`), or
#'   `NULL` when no probe covers the host or the host cannot be reached.
#' @export
probe_upstream_identity <- function(source_url) {
  probe <- .upstream_probe_for(source_url)
  if (is.null(probe)) return(NULL)
  tryCatch(match.fun(probe)(source_url), error = function(e) NULL)
}


#' Check one manifest enrichment entry against its upstream source
#'
#' Freshness is decided by comparing the upstream identity the build read
#' against the one upstream carries now. The build's identity comes from
#' `upstream_id`, written into the entry when the `.vtr` was built; for a Zenodo
#' source the pinned record number in the URL says the same thing, so those are
#' answerable without it.
#'
#' Where neither is available the answer is `NA`, not a guess: the recorded
#' `source_version` is this package's own release string and the upstream
#' version is whatever the host counts in, so comparing the two answers a
#' different question than the one asked. An entry built before `upstream_id`
#' was recorded reports `NA` until its next rebuild.
#'
#' @param entry List. Manifest enrichment entry with `source_url`,
#'   `source_version`, `upstream_id`, `static`.
#' @param probe Function taking a URL and returning an upstream identity.
#'   Defaults to [probe_upstream_identity()].
#' @return Named list with `source_version`, `built_id`, `upstream_version`,
#'   `outdated`, `check_url`, and optionally `note`.
#' @export
check_enrichment_source_version <- function(entry,
                                            probe = probe_upstream_identity) {
  unknown <- function(note, check_url = NA_character_) {
    list(
      source_version = entry$source_version,
      built_id = entry$upstream_id %||% NA_character_,
      upstream_version = NA_character_,
      outdated = NA,
      check_url = check_url,
      note = note
    )
  }

  if (isTRUE(entry$static)) {
    return(list(
      source_version = entry$source_version,
      built_id = entry$upstream_id %||% NA_character_,
      upstream_version = entry$source_version,
      outdated = FALSE,
      check_url = NA_character_,
      note = "static dataset, skipped"
    ))
  }

  url <- entry$source_url %||% ""
  result <- probe(url)

  # A host with no probe needs one written; a host with a probe that came back
  # empty needs the recorded URL looked at. Reporting both the same way is how
  # a source URL that no longer resolves passes for a source nobody checks.
  if (is.null(result)) {
    return(unknown(if (is.null(.upstream_probe_for(url))) {
      sprintf("no version check for source host: %s", url)
    } else {
      sprintf("upstream host could not be read: %s", url)
    }))
  }
  if (is.null(result$id) || is.na(result$id)) {
    return(unknown("could not determine upstream version",
                   result$url %||% NA_character_))
  }

  built_id <- entry$upstream_id %||% result$pinned
  if (is.null(built_id) || !nzchar(built_id)) {
    res <- unknown("no upstream identity recorded at build time",
                   result$url)
    res$upstream_version <- result$version
    return(res)
  }

  list(
    source_version = entry$source_version,
    built_id = as.character(built_id),
    upstream_version = result$version,
    outdated = !identical(as.character(built_id), as.character(result$id)),
    check_url = result$url
  )
}


#' Check all non-static enrichments in a manifest for version freshness
#'
#' @param manifest_path Character. Path to manifest.json.
#' @return Data.frame with columns: name, source_version, built_id,
#'   upstream_version, outdated, check_url, note.
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
          built_id = entry$upstream_id %||% NA_character_,
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
      built_id = r$built_id %||% NA_character_,
      upstream_version = r$upstream_version %||% NA_character_,
      outdated = r$outdated %||% NA,
      check_url = r$check_url %||% NA_character_,
      note = r$note %||% "",
      stringsAsFactors = FALSE
    )
  }))
}
