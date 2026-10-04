# Upstream source version checker for enrichments.
#
# Checks for each non-static enrichment in the manifest whether a newer
# upstream version is available. Used by CI to open/update GitHub issues
# when enrichments become outdated.

# Every probe below answers one question about a source host -- what identity
# does it give its newest version right now -- and answers it the same way:
#
#   id      identity of the newest upstream version. Where the source URL names
#           one file, this is that file's content hash on the host (a Zenodo or
#           Figshare md5, a git commit touching the path), so a new record that
#           only adds a metadata file does not read as new data. Otherwise it is
#           whatever the host counts versions in (a Zenodo record number, a
#           Figshare or Dryad version number, a Last-Modified stamp).
#   version the same identity in a form a reader recognises.
#   date    the day the newest version was published, where the host says.
#   pinned  the identity of what the source URL itself names, where the URL
#           names a fixed version (a Zenodo record that is not a concept
#           record, a Figshare file id). Elsewhere the URL serves whatever is
#           newest and `pinned` is absent.
#   url     where the check looked.
#
# A probe returns NULL when it cannot reach or parse the host. Freshness is not
# decided here: probes report, check_enrichment_source_version() compares.
#
# Every probe takes the source URL and the source's host record (`record`):
# the manifest `source_record`, else `source_doi`. Only Figshare needs it, since
# a Figshare download URL carries a file id and nothing names its article.


#' Read a JSON API response, or NULL
#'
#' Sends the GitHub token when one is set, so the weekly CI run is not held to
#' the unauthenticated rate limit.
#'
#' @param url Character. API URL.
#' @return Parsed JSON as nested lists, or `NULL` on any failure.
#' @noRd
.api_json <- function(url) {
  token <- Sys.getenv("GITHUB_PAT", Sys.getenv("GITHUB_TOKEN", ""))
  headers <- c(Accept = "application/json")
  if (nzchar(token) && grepl("^https://api\\.github\\.com/", url)) {
    headers <- c(headers, Authorization = paste("token", token))
  }
  tryCatch({
    con <- url(url, headers = headers)
    on.exit(close(con))
    jsonlite::fromJSON(paste(suppressWarnings(readLines(con, warn = FALSE)),
                             collapse = "\n"),
                       simplifyVector = FALSE)
  }, error = function(e) NULL)
}


#' Check a Zenodo record for the latest version
#'
#' A Zenodo record is immutable, so querying the pinned record reports the day
#' that record was published however many newer versions its concept has since
#' gained. The `versions/latest` endpoint resolves the pinned record to the
#' newest one in the same concept. A URL naming the concept record itself
#' floats: Zenodo serves its newest version, so it pins nothing.
#'
#' Where the URL names one file of the record, the identity is that file's
#' checksum, read from the pinned and the newest record alike: a new version
#' that only adds a metadata file leaves the data the build reads unchanged. A
#' file the newest record no longer carries reads as `missing:<key>`.
#'
#' @param source_url Character. Zenodo record or file download URL.
#' @param record Unused; the URL names the record.
#' @return Named list with `id`, `version`, `date`, `pinned` (absent for a
#'   concept record), and `url`; `NULL` if the record cannot be read.
#' @export
check_zenodo_version <- function(source_url, record = NULL) {
  # Zenodo serves both the current /records/ and the legacy /record/ path.
  m <- regmatches(source_url, regexpr("records?/([0-9]+)", source_url))
  if (length(m) == 0L) return(NULL)
  record_id <- sub("records?/", "", m)

  file_key <- NULL
  if (grepl("/files/", source_url)) {
    file_key <- sub("^.*?/files/", "", source_url)
    file_key <- sub("[?#].*$", "", file_key)
    file_key <- utils::URLdecode(sub("/content$", "", file_key))
  }

  named <- .api_json(sprintf("https://zenodo.org/api/records/%s", record_id))
  latest <- .api_json(sprintf("https://zenodo.org/api/records/%s/versions/latest",
                              record_id))
  if (is.null(named) || is.null(latest)) return(NULL)
  latest_id <- as.character(latest$id %||% NA_character_)
  if (is.na(latest_id)) return(NULL)

  identity <- function(rec) {
    if (is.null(file_key)) return(as.character(rec$id))
    for (f in rec$files %||% list()) {
      if (identical(f$key, file_key)) return(as.character(f$checksum))
    }
    paste0("missing:", file_key)
  }

  # A concept record id resolves to its newest version, so the record read back
  # carries another id than the one the URL names.
  floating <- !identical(as.character(named$id), record_id)
  date <- as.character(latest$metadata$publication_date %||% NA_character_)
  list(
    id = identity(latest),
    version = as.character(latest$metadata$version %||% date),
    date = date,
    pinned = if (!floating) identity(named),
    url = sprintf("https://zenodo.org/records/%s", latest_id)
  )
}


#' Check a Figshare article for the latest version
#'
#' An article URL is compared by the article's version number. A file download
#' URL (`ndownloader.figshare.com/files/<id>`) names one immutable file, so it
#' is compared by content: the file's md5 is the pinned identity, and the
#' newest version's file of the same name is the upstream one (`missing:<name>`
#' when the newest version dropped it).
#'
#' Nothing in a file URL names its article, so the article comes from the
#' source's host record: a Figshare DOI (`10.6084/m9.figshare.<id>`), a
#' collection DOI (`10.6084/m9.figshare.c.<id>`, whose articles are searched for
#' the file), or a Figshare article URL.
#'
#' @param source_url Character. Figshare article or file download URL.
#' @param record Character or NULL. The source's host record, as above.
#' @return Named list with `id`, `version`, `date`, `pinned` (file URLs only),
#'   and `url`; `NULL` if the article cannot be read.
#' @export
check_figshare_version <- function(source_url, record = NULL) {
  api <- "https://api.figshare.com/v2"
  id_after <- function(x, pattern) {
    m <- regmatches(x, regexpr(pattern, x))
    if (length(m) == 0L) NULL else sub("^.*[^0-9]", "", m)
  }

  file_id <- id_after(source_url, "files?/(download/)?[0-9]+")
  article_ids <- id_after(source_url, "articles/[0-9]+")
  rec <- record %||% ""
  article_ids <- article_ids %||% id_after(rec, "articles/[0-9]+") %||%
    id_after(rec, "figshare\\.[0-9]+")
  if (is.null(article_ids)) {
    collection <- id_after(rec, "figshare\\.c\\.[0-9]+")
    if (is.null(collection)) return(NULL)
    arts <- .api_json(sprintf("%s/collections/%s/articles?page_size=1000",
                              api, collection))
    article_ids <- vapply(arts %||% list(), function(a) as.character(a$id), "")
  }

  for (aid in article_ids) {
    latest <- .api_json(sprintf("%s/articles/%s", api, aid))
    if (is.null(latest)) next
    version <- as.character(latest$version %||% NA_character_)
    date <- substr(as.character(latest$published_date %||% NA_character_), 1, 10)
    url <- sprintf("%s/articles/%s", api, aid)

    if (is.null(file_id)) {
      if (is.na(version)) return(NULL)
      return(list(id = version, version = version, date = date, url = url))
    }

    find_file <- function(files, key, value) {
      for (f in files %||% list()) {
        if (identical(as.character(f[[key]]), as.character(value))) return(f)
      }
      NULL
    }
    pinned <- find_file(latest$files, "id", file_id)
    if (is.null(pinned)) {
      # A file the newest version dropped is still served by its id; its name
      # and checksum come from the version that carried it.
      vers <- .api_json(sprintf("%s/articles/%s/versions", api, aid))
      for (v in rev(vers %||% list())) {
        pinned <- find_file(.api_json(v$url)$files, "id", file_id)
        if (!is.null(pinned)) break
      }
    }
    if (is.null(pinned)) next

    now <- find_file(latest$files, "name", pinned$name)
    return(list(
      id = if (is.null(now)) paste0("missing:", pinned$name) else now$computed_md5,
      version = version,
      date = date,
      pinned = pinned$computed_md5,
      url = url
    ))
  }
  NULL
}


#' Check a Dryad dataset for the latest version
#'
#' @param source_url Character. Dryad download URL containing a DOI, or the
#'   bare Dryad DOI (`10.5061/dryad.<id>`, optionally behind `doi.org`).
#' @param record Unused; the URL names the dataset.
#' @return Named list with `id` and `version` (both the newest version number),
#'   `date`, and `url`; `NULL` if the dataset cannot be read.
#' @export
check_dryad_version <- function(source_url, record = NULL) {
  m <- regmatches(source_url,
                  regexpr("10\\.5061/dryad\\.[A-Za-z0-9]+", source_url))
  if (length(m) == 0L) {
    m <- regmatches(source_url, regexpr("doi%3A[^/&?]+", source_url))
    if (length(m) == 0L) return(NULL)
    m <- sub("^doi:", "", utils::URLdecode(m))
  }

  api_url <- sprintf("https://datadryad.org/api/v2/datasets/%s",
                     utils::URLencode(paste0("doi:", m), reserved = TRUE))
  resp <- .api_json(api_url)
  if (is.null(resp)) return(NULL)

  version <- as.character(resp$versionNumber %||% NA_character_)
  if (is.na(version)) return(NULL)
  list(
    id = version,
    version = version,
    date = as.character(resp$publicationDate %||%
                          resp$lastModificationDate %||% NA_character_),
    url = api_url
  )
}


#' Check a file on GitHub for its latest change
#'
#' A `raw.githubusercontent.com` URL serves the file at the head of a branch,
#' so it pins nothing. The identity is the newest commit touching that path.
#'
#' @param source_url Character. `raw.githubusercontent.com/<owner>/<repo>/
#'   <branch>/<path>` URL.
#' @param record Unused; the URL names the file.
#' @return Named list with `id` (the commit SHA), `version` (its short form),
#'   `date`, and `url`; `NULL` if the commit cannot be read.
#' @export
check_github_version <- function(source_url, record = NULL) {
  parts <- strsplit(sub("^https?://raw\\.githubusercontent\\.com/", "",
                        sub("[?#].*$", "", source_url)), "/")[[1L]]
  if (length(parts) < 4L) return(NULL)
  api_url <- sprintf(
    "https://api.github.com/repos/%s/%s/commits?sha=%s&path=%s&per_page=1",
    parts[1L], parts[2L], parts[3L],
    utils::URLencode(utils::URLdecode(paste(parts[-(1:3)], collapse = "/"))))
  resp <- .api_json(api_url)
  if (length(resp) == 0L || is.null(resp[[1L]]$sha)) return(NULL)

  sha <- resp[[1L]]$sha
  list(
    id = sha,
    version = substr(sha, 1L, 7L),
    date = substr(as.character(resp[[1L]]$commit$committer$date %||%
                                 NA_character_), 1, 10),
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
#' @param record Unused.
#' @return Named list with `id` (the `Last-Modified` stamp), `version` (that
#'   stamp as `YYYY.MM`), `date`, and `url`; `NULL` if the header is absent.
#' @export
check_gbif_version <- function(source_url, record = NULL) {
  headers <- tryCatch(curlGetHeaders(source_url), error = function(e) NULL)
  if (is.null(headers)) return(NULL)

  lm <- grep("^Last-Modified:", headers, value = TRUE, ignore.case = TRUE)
  if (length(lm) == 0L) return(NULL)

  stamp <- trimws(sub("^Last-Modified:\\s*", "", lm[1L], ignore.case = TRUE))
  parsed <- as.Date(stamp, format = "%a, %d %b %Y %H:%M:%S")
  list(
    id = stamp,
    version = if (!is.na(parsed)) format(parsed, "%Y.%m") else stamp,
    date = if (!is.na(parsed)) format(parsed) else NA_character_,
    url = source_url
  )
}


#' Check the GBIF backbone API for the latest update date
#'
#' @param source_url Character. (Unused; the GBIF backbone dataset ID is fixed.)
#' @param record Unused.
#' @return Named list with `id` (the dataset's `modified` timestamp), `version`
#'   (that timestamp as `YYYY.MM`), `date`, and `url`; `NULL` if the dataset
#'   cannot be read.
#' @export
check_gbif_api_version <- function(source_url, record = NULL) {
  api_url <- "https://api.gbif.org/v1/dataset/d7dddbf4-2cf0-4f39-9b2a-bb099caae36c"
  resp <- .api_json(api_url)
  if (is.null(resp)) return(NULL)

  modified <- resp$modified %||% resp$pubDate
  if (is.null(modified)) return(NULL)

  date <- as.Date(substr(modified, 1, 10))
  list(
    id = as.character(modified),
    version = if (!is.na(date)) format(date, "%Y.%m") else as.character(modified),
    date = if (!is.na(date)) format(date) else NA_character_,
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
#' @param record Unused.
#' @return Named list with `id`, `version`, `date`, and `url`; `NULL` if the
#'   header is absent.
#' @export
check_wcvp_version <- function(source_url, record = NULL) {
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
  list(host = "zenodo\\.org",                probe = "check_zenodo_version"),
  list(host = "figshare\\.com",              probe = "check_figshare_version"),
  list(host = "datadryad\\.org|10\\.5061/dryad\\.", probe = "check_dryad_version"),
  list(host = "raw\\.githubusercontent\\.com", probe = "check_github_version"),
  list(host = "api\\.gbif\\.org",            probe = "check_gbif_api_version"),
  list(host = "gbif\\.org",                  probe = "check_gbif_version"),
  list(host = "kew\\.org",                   probe = "check_wcvp_version")
)

# A source frozen into a release of this repository: a crawl or scrape snapshot
# whose access date is its version. Nothing upstream can move under it; taking a
# newer harvest is a decision to re-crawl, not a version to detect.
.snapshot_source <- "^https://github\\.com/gcol33/taxifydb/releases/download/"


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
#' @param record Character or NULL. The source's host record (registry
#'   `source_record`, else `source_doi`); see [check_figshare_version()].
#' @return The probe's result (`id`, `version`, `date`, optionally `pinned`,
#'   `url`), or `NULL` when no probe covers the host or the host cannot be
#'   reached.
#' @export
probe_upstream_identity <- function(source_url, record = NULL) {
  probe <- .upstream_probe_for(source_url)
  if (is.null(probe)) return(NULL)
  tryCatch(match.fun(probe)(source_url, record), error = function(e) NULL)
}


#' Check one manifest enrichment entry against its upstream source
#'
#' Freshness is decided by comparing the upstream identity the build read
#' against the one upstream carries now. Where the source URL pins a version,
#' that pinned identity is what the build read, whatever was recorded. Where
#' it floats, the build's identity comes from `upstream_id`, written into the
#' entry when the `.vtr` was built.
#'
#' A floating source with no recorded identity is still answerable when the
#' newest upstream version is older than the build: the entry's release tag
#' (`latest`, `YYYY.MM`) bounds the build date from below, and a build reads the
#' newest version there is, so the build read that version.
#'
#' Where none of these holds the answer is `NA`, not a guess: the recorded
#' `source_version` is this package's own release string and the upstream
#' version is whatever the host counts in, so comparing the two answers a
#' different question than the one asked.
#'
#' @param entry List. Manifest enrichment entry with `source_url`,
#'   `source_version`, `upstream_id`, `latest`, `static`, and optionally
#'   `source_record` / `source_doi`.
#' @param probe Function taking a URL and a host record and returning an
#'   upstream identity. Defaults to [probe_upstream_identity()].
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
  skipped <- function(note) {
    list(
      source_version = entry$source_version,
      built_id = entry$upstream_id %||% NA_character_,
      upstream_version = entry$source_version,
      outdated = FALSE,
      check_url = NA_character_,
      note = note
    )
  }

  if (isTRUE(entry$static)) return(skipped("static dataset, skipped"))

  url <- entry$source_url %||% ""
  if (grepl(.snapshot_source, url)) {
    return(skipped("frozen snapshot, its access date is the version"))
  }

  result <- probe(url, entry$source_record %||% entry$source_doi)

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

  built_id <- result$pinned %||% entry$upstream_id
  note <- NULL
  if (is.null(built_id) || !nzchar(built_id)) {
    built_on <- tryCatch(as.Date(paste0(sub("\\.", "-", entry$latest %||% ""),
                                        "-01")),
                         error = function(e) NA)
    published <- tryCatch(as.Date(substr(result$date %||% "", 1, 10)),
                          error = function(e) NA)
    if (length(built_on) == 1L && length(published) == 1L &&
        !is.na(built_on) && !is.na(published) && published < built_on) {
      built_id <- result$id
      note <- sprintf("newest upstream version (%s) predates the %s build",
                      published, entry$latest)
    } else {
      res <- unknown("no upstream identity recorded at build time",
                     result$url)
      res$upstream_version <- result$version
      return(res)
    }
  }

  res <- list(
    source_version = entry$source_version,
    built_id = as.character(built_id),
    upstream_version = result$version,
    outdated = !identical(as.character(built_id), as.character(result$id)),
    check_url = result$url
  )
  if (!is.null(note)) res$note <- note
  res
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
