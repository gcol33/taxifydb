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
  # Archive and repository APIs answer an occasional gateway timeout to a
  # request that succeeds a moment later.
  for (try in seq_len(3L)) {
    out <- tryCatch({
      con <- url(url, headers = headers)
      text <- tryCatch(suppressWarnings(readLines(con, warn = FALSE)),
                       finally = close(con))
      jsonlite::fromJSON(paste(text, collapse = "\n"), simplifyVector = FALSE)
    }, error = function(e) NULL)
    if (!is.null(out)) return(out)
    if (try < 3L) Sys.sleep(2 * try)
  }
  NULL
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


#' Check a file for its Last-Modified date
#'
#' For a host that serves a static file and says when it last changed. The
#' header stamp is kept whole as the identity and shortened to `YYYY.MM` only
#' for display: two files a fortnight apart share a month, and an identity that
#' cannot tell them apart reports a moved file as unchanged.
#'
#' @param source_url Character. File URL.
#' @param record Unused.
#' @return Named list with `id` (the `Last-Modified` stamp), `version` (that
#'   stamp as `YYYY.MM`), `date`, and `url`; `NULL` if the header is absent.
#' @export
check_last_modified <- function(source_url, record = NULL) {
  headers <- tryCatch(curlGetHeaders(source_url), error = function(e) NULL)
  if (is.null(headers)) return(NULL)

  lm <- grep("^Last-Modified:", headers, value = TRUE, ignore.case = TRUE)
  if (length(lm) == 0L) return(NULL)

  # After a redirect the last header block is the file's own.
  stamp <- trimws(sub("^Last-Modified:\\s*", "", lm[length(lm)],
                      ignore.case = TRUE))
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


#' Check a file by the md5 of its bytes
#'
#' For a host that serves a generated export with no version and no
#' Last-Modified header (a PHP endpoint, a database export). The bytes are the
#' only identity there is, so the probe downloads them; it is listed only for
#' sources small enough to fetch weekly, and only where two downloads hash
#' alike.
#'
#' @param source_url Character. File URL.
#' @param record Unused.
#' @return Named list with `id` (the md5), `version` (its first 8 characters),
#'   and `url`; `NULL` if the file cannot be read.
#' @export
check_content_md5 <- function(source_url, record = NULL) {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  ok <- tryCatch({
    utils::download.file(source_url, tmp, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok || !file.exists(tmp) || file.size(tmp) == 0) return(NULL)

  md5 <- unname(tools::md5sum(tmp))
  list(id = md5, version = substr(md5, 1L, 8L), url = source_url)
}


#' Check an EDI data package for a newer revision
#'
#' An EDI (`pasta.lternet.edu`) data URL names a package revision
#' (`<scope>/<identifier>/<revision>`) and usually one data entity in it. An
#' entity keeps its id across revisions, so an entity URL is compared by the
#' entity's checksum in the revision it names and in the newest one: a
#' revision that changes only metadata or other tables leaves it current. A
#' URL naming the revision alone is compared by revision number.
#'
#' PASTA has required an API key for every call since 2026-07-30, read here
#' from `EDI_API_KEY` as [download_edi_file()] reads it.
#'
#' @param source_url Character. EDI package data URL.
#' @param record Unused.
#' @return Named list with `id` and `pinned` (entity checksums, or revision
#'   numbers), `version` (the newest revision), `date` (its publication date),
#'   and `url`; `NULL` without a key or when PASTA cannot be read.
#' @export
check_edi_version <- function(source_url, record = NULL) {
  m <- regmatches(source_url, regexec(
    "pasta\\.lternet\\.edu/package/data/eml/([^/]+)/([0-9]+)/([0-9]+)/?([0-9a-f]*)",
    source_url))[[1L]]
  key <- Sys.getenv("EDI_API_KEY", "")
  if (length(m) != 5L || !nzchar(key)) return(NULL)
  entity <- m[5L]

  pasta <- "https://pasta.lternet.edu/package"
  read <- function(url) {
    tryCatch(suppressWarnings(readLines(
      sprintf("%s%skey=%s", url, if (grepl("?", url, fixed = TRUE)) "&" else "?",
              utils::URLencode(key, reserved = TRUE)), warn = FALSE)),
      error = function(e) NULL)
  }
  newest_url <- sprintf("%s/eml/%s/%s?filter=newest", pasta, m[2L], m[3L])
  newest <- trimws(read(newest_url)[1L])
  if (length(newest) == 0L || !grepl("^[0-9]+$", newest)) return(NULL)

  eml <- paste(read(sprintf("%s/metadata/eml/%s/%s/%s", pasta, m[2L], m[3L],
                            newest)), collapse = "\n")
  date <- regmatches(eml, regexec("<pubDate>([0-9-]+)", eml))[[1L]]

  id <- newest
  pinned <- m[4L]
  if (nzchar(entity)) {
    checksum <- function(revision) {
      out <- trimws(read(sprintf("%s/data/checksum/eml/%s/%s/%s/%s", pasta,
                                 m[2L], m[3L], revision, entity))[1L])
      if (length(out) == 0L || is.na(out) || !nzchar(out)) {
        paste0("missing:", entity)
      } else {
        out
      }
    }
    id <- checksum(newest)
    pinned <- checksum(m[4L])
  }
  list(
    id = id,
    version = newest,
    date = if (length(date) == 2L) date[2L] else NA_character_,
    pinned = pinned,
    url = newest_url
  )
}


#' Check a Wayback Machine snapshot for a newer capture
#'
#' A snapshot URL (`web.archive.org/web/<timestamp>id_/<original>`) fixes one
#' capture of a page that may since have disappeared. Each capture carries a
#' digest of the bytes it holds, so the capture the URL names and the newest
#' capture of the same original compare by content: a re-capture of an
#' unchanged file is not a new version.
#'
#' @param source_url Character. Wayback snapshot URL.
#' @param record Unused.
#' @return Named list with `id` (the newest capture's digest), `version` (its
#'   timestamp), `date`, `pinned` (the digest of the capture the URL names), and
#'   `url`; `NULL` if the capture index cannot be read.
#' @export
check_wayback_version <- function(source_url, record = NULL) {
  m <- regmatches(source_url, regexec(
    "web\\.archive\\.org/web/([0-9]{14})[a-z_]*/(.+)$", source_url))[[1L]]
  if (length(m) != 3L) return(NULL)

  cdx_url <- sprintf(
    "https://web.archive.org/cdx/search/cdx?url=%s&output=json&fl=timestamp,digest&filter=statuscode:200",
    utils::URLencode(sub("^https?://", "", m[3L]), reserved = TRUE))
  rows <- .api_json(cdx_url)
  if (length(rows) < 2L) return(NULL)
  rows <- rows[-1L]

  stamps <- vapply(rows, function(r) as.character(r[[1L]]), "")
  digests <- vapply(rows, function(r) as.character(r[[2L]]), "")
  pinned <- digests[stamps == m[2L]]
  newest <- which.max(as.numeric(stamps))
  list(
    id = digests[newest],
    version = stamps[newest],
    date = format(as.Date(substr(stamps[newest], 1, 8), "%Y%m%d")),
    pinned = if (length(pinned)) pinned[1L],
    url = cdx_url
  )
}


#' Check a dataset DOI for the version DataCite records
#'
#' For a host whose own pages give no version but whose DataCite record
#' carries one. The record's `updated` stamp moves with any metadata edit, so
#' as a date it errs late, never early.
#'
#' @param source_url Character. Download URL (unused beyond selecting this
#'   probe).
#' @param record Character. The dataset DOI.
#' @return Named list with `id` and `version` (DataCite's `version`), `date`,
#'   and `url`; `NULL` if the record has no version.
#' @export
check_datacite_version <- function(source_url, record = NULL) {
  doi <- regmatches(record %||% "", regexpr("10\\.[0-9]{4,}/[^ ?#]+",
                                            record %||% ""))
  if (length(doi) == 0L) return(NULL)

  api_url <- sprintf("https://api.datacite.org/dois/%s", doi)
  attrs <- .api_json(api_url)$data$attributes
  if (is.null(attrs$version)) return(NULL)
  list(
    id = as.character(attrs$version),
    version = as.character(attrs$version),
    date = substr(as.character(attrs$updated %||% NA_character_), 1, 10),
    url = api_url
  )
}


#' Check a DataONE object for a newer version
#'
#' DataONE system metadata links each object to the one that replaced it
#' (`obsoletedBy`), so following the chain from the object the URL names
#' reaches the newest version.
#'
#' @param source_url Character. DataONE member-node object URL
#'   (`.../d1/mn/v2/object/<identifier>`).
#' @param record Unused.
#' @return Named list with `id` (the newest identifier), `version`, `date`,
#'   `pinned` (the identifier the URL names), and `url`; `NULL` if the system
#'   metadata cannot be read.
#' @export
check_dataone_version <- function(source_url, record = NULL) {
  if (!grepl("/object/", source_url)) return(NULL)
  pid <- utils::URLdecode(sub("[?#].*$", "", sub("^.*/object/", "", source_url)))

  meta_url <- function(id) {
    sprintf("https://cn.dataone.org/cn/v2/meta/%s",
            utils::URLencode(id, reserved = TRUE))
  }
  sysmeta <- function(id) {
    xml <- tryCatch(suppressWarnings(readLines(meta_url(id), warn = FALSE)),
                    error = function(e) NULL)
    if (is.null(xml)) return(NULL)
    xml <- paste(xml, collapse = "\n")
    field <- function(tag) {
      m <- regmatches(xml, regexec(sprintf("<%s>([^<]*)</%s>", tag, tag),
                                   xml))[[1L]]
      if (length(m) == 2L) m[2L] else NULL
    }
    list(obsoleted_by = field("obsoletedBy"), uploaded = field("dateUploaded"))
  }

  current <- pid
  meta <- sysmeta(current)
  if (is.null(meta)) return(NULL)
  while (!is.null(meta$obsoleted_by)) {
    nxt <- sysmeta(meta$obsoleted_by)
    if (is.null(nxt)) break
    current <- meta$obsoleted_by
    meta <- nxt
  }
  list(
    id = current,
    version = sub("^.*/", "", current),
    date = substr(meta$uploaded %||% NA_character_, 1, 10),
    pinned = pid,
    url = meta_url(current)
  )
}


#' Check a Dataverse dataset for its newest released version
#'
#' The URL either is a Dataverse API call naming the dataset by
#' `persistentId=doi:...` or is the dataset DOI itself, in which case the
#' installation serving it is the host DataCite resolves the DOI to.
#'
#' @param source_url Character. Dataverse API URL or `doi.org` DOI.
#' @param record Unused.
#' @return Named list with `id` and `version` (`major.minor`), `date`, and
#'   `url`; `NULL` if the versions cannot be read.
#' @export
check_dataverse_version <- function(source_url, record = NULL) {
  m <- regmatches(source_url, regexpr("10\\.[0-9]+/[^?&# ]+", source_url))
  if (length(m) == 0L) return(NULL)
  doi <- utils::URLdecode(m)

  base <- if (grepl("persistentId=", source_url)) {
    sub("^(https?://[^/]+).*$", "\\1", source_url)
  } else {
    landing <- .api_json(sprintf("https://api.datacite.org/dois/%s",
                                 doi))$data$attributes$url
    if (is.null(landing)) return(NULL)
    sub("^(https?://[^/]+).*$", "\\1", landing)
  }

  api_url <- sprintf(
    "%s/api/datasets/:persistentId/versions?persistentId=doi:%s", base, doi)
  released <- Filter(function(v) identical(v$versionState, "RELEASED"),
                     .api_json(api_url)$data %||% list())
  if (length(released) == 0L) return(NULL)

  number <- vapply(released, function(v) {
    v$versionNumber + (v$versionMinorNumber %||% 0) / 1000
  }, 0)
  newest <- released[[which.max(number)]]
  label <- sprintf("%s.%s", newest$versionNumber,
                   newest$versionMinorNumber %||% 0)
  list(
    id = label,
    version = label,
    date = substr(as.character(newest$releaseTime %||% NA_character_), 1, 10),
    url = api_url
  )
}


#' Check a CKAN resource for its last modification
#'
#' The resource is named by `/resource/<id>` in the URL, else in the source's
#' host record (a portal whose downloads go through its datastore API).
#'
#' @param source_url Character. CKAN resource download URL.
#' @param record Character or NULL. CKAN resource URL, when `source_url` does
#'   not carry one.
#' @return Named list with `id` (the resource's `last_modified` stamp),
#'   `version`, `date`, and `url`; `NULL` if the resource cannot be read.
#' @export
check_ckan_version <- function(source_url, record = NULL) {
  pattern <- "/resource/([0-9a-f-]{36})"
  where <- if (grepl(pattern, source_url)) source_url else record %||% ""
  m <- regmatches(where, regexec(pattern, where))[[1L]]
  if (length(m) != 2L) return(NULL)

  api_url <- sprintf("%s/api/3/action/resource_show?id=%s",
                     sub("^(https?://[^/]+).*$", "\\1", where), m[2L])
  res <- .api_json(api_url)$result
  stamp <- res$last_modified %||% res$metadata_modified
  if (is.null(stamp)) return(NULL)
  list(
    id = as.character(stamp),
    version = substr(stamp, 1, 10),
    date = substr(stamp, 1, 10),
    url = api_url
  )
}


#' Check an IPT resource for its newest published version
#'
#' An IPT archive URL names its version (`v=`); the same URL without it serves
#' the newest, under a file name carrying that version.
#'
#' @param source_url Character. IPT `archive.do?r=<resource>&v=<version>` URL.
#' @param record Unused.
#' @return Named list with `id` and `version` (the newest version), `date`,
#'   `pinned` (the version the URL names), and `url`; `NULL` if the archive
#'   cannot be read.
#' @export
check_ipt_version <- function(source_url, record = NULL) {
  resource <- regmatches(source_url,
                         regexec("[?&]r=([^&#]+)", source_url))[[1L]]
  if (length(resource) != 2L) return(NULL)
  pinned <- regmatches(source_url, regexec("[?&]v=([^&#]+)", source_url))[[1L]]

  latest_url <- sprintf("%s?r=%s", sub("[?].*$", "", source_url), resource[2L])
  headers <- tryCatch(curlGetHeaders(latest_url), error = function(e) NULL)
  disp <- grep("^Content-Disposition:", headers %||% character(0),
               value = TRUE, ignore.case = TRUE)
  v <- unlist(lapply(regmatches(disp, regexec("-v([0-9][0-9.]*)\\.zip", disp)),
                     `[`, 2L))
  if (length(v) == 0L || is.na(v[1L])) return(NULL)

  lm <- grep("^Last-Modified:", headers, value = TRUE, ignore.case = TRUE)
  date <- if (length(lm)) {
    as.Date(trimws(sub("^Last-Modified:\\s*", "", lm[1L], ignore.case = TRUE)),
            format = "%a, %d %b %Y %H:%M:%S")
  } else {
    NA
  }
  list(
    id = v[1L],
    version = v[1L],
    date = if (!is.na(date)) format(date) else NA_character_,
    pinned = if (length(pinned) == 2L) pinned[2L],
    url = latest_url
  )
}


#' Check the GIFT database for its newest version
#'
#' The GIFT API lists every database version; each description opens with the
#' date the version was released.
#'
#' @param source_url Character. GIFT host URL.
#' @param record Unused.
#' @return Named list with `id` and `version` (the newest version), `date`,
#'   and `url`; `NULL` if the versions cannot be read.
#' @export
check_gift_version <- function(source_url, record = NULL) {
  api_url <- "https://gift.uni-goettingen.de/api/index.php?query=versions"
  versions <- .api_json(api_url)
  if (length(versions) == 0L) return(NULL)

  ids <- vapply(versions, function(v) as.numeric(v$ID %||% NA), 0)
  newest <- versions[[which.max(ids)]]
  desc <- newest$description %||% ""
  date <- regmatches(desc, regexpr("^[0-9]{4}-[0-9]{2}-[0-9]{2}", desc))
  list(
    id = as.character(newest$version),
    version = as.character(newest$version),
    date = if (length(date)) date else NA_character_,
    url = api_url
  )
}


#' Check a journal article for corrections
#'
#' A published article does not get new versions, it gets corrections and
#' retractions, which Crossref records against it as `updated-by`. A source
#' whose values come from an article is therefore checked by that list.
#'
#' @param source_url Character. `doi.org` URL of the article, or a URL that is
#'   not one, in which case the DOI comes from `record`.
#' @param record Character or NULL. The article DOI.
#' @return Named list with `id` (the updating DOIs, or `"none"`), `version`,
#'   `date` (the latest of publication and update), and `url`; `NULL` if
#'   Crossref does not know the DOI.
#' @export
check_crossref_version <- function(source_url, record = NULL) {
  pattern <- "10\\.[0-9]{4,}/[^ ?#]+"
  doi <- regmatches(source_url, regexpr(pattern, source_url))
  if (length(doi) == 0L) {
    doi <- regmatches(record %||% "", regexpr(pattern, record %||% ""))
  }
  if (length(doi) == 0L) return(NULL)

  api_url <- sprintf("https://api.crossref.org/works/%s", doi)
  work <- .api_json(api_url)$message
  if (is.null(work)) return(NULL)

  as_date <- function(x) {
    p <- unlist(x$`date-parts`[[1L]])
    if (length(p) == 0L) return(NA_character_)
    p <- c(p, 1L, 1L)[1:3]
    sprintf("%04d-%02d-%02d", p[1L], p[2L], p[3L])
  }
  updates <- work$`updated-by` %||% list()
  ids <- sort(vapply(updates, function(u) as.character(u$DOI), ""))
  dates <- c(as_date(work$published %||% work$issued),
             vapply(updates, function(u) as_date(u$updated), ""))
  list(
    id = if (length(ids)) paste(ids, collapse = " ; ") else "none",
    version = sprintf("%d update(s)", length(ids)),
    date = max(dates, na.rm = TRUE),
    url = api_url
  )
}


#' Check an rfishbase server for its newest release
#'
#' rfishbase reads FishBase and SeaLifeBase snapshots published as release
#' folders of one Hugging Face dataset, and takes the newest by default.
#'
#' @param source_url Character. `fishbase.ropensci.org` or
#'   `sealifebase.ropensci.org`.
#' @param record Unused.
#' @return Named list with `id` and `version` (the release label), `date` (the
#'   dataset's last change, an upper bound on the release date), and `url`;
#'   `NULL` if the releases cannot be read.
#' @export
check_rfishbase_version <- function(source_url, record = NULL) {
  server <- if (grepl("sealifebase", source_url)) "slb" else "fb"
  repo <- "https://huggingface.co/api/datasets/cboettig/fishbase"
  tree_url <- sprintf("%s/tree/main/data/%s", repo, server)
  labels <- vapply(.api_json(tree_url) %||% list(),
                   function(x) sub("^.*/v", "", x$path), "")
  labels <- labels[grepl("^[0-9]+(\\.[0-9]+)*$", labels)]
  if (length(labels) == 0L) return(NULL)

  newest <- labels[order(numeric_version(labels), decreasing = TRUE)][1L]
  modified <- .api_json(repo)$lastModified
  list(
    id = newest,
    version = newest,
    date = substr(as.character(modified %||% NA_character_), 1, 10),
    url = tree_url
  )
}


#' Check the Kew Plant DNA C-values database for its newest release
#'
#' @param source_url Character. A `cvalues.science.kew.org` URL.
#' @param record Unused.
#' @return Named list with `id` and `version` (the release number), `date`
#'   (the release month), and `url`; `NULL` if the release history cannot be
#'   read.
#' @export
check_kew_cvalues_version <- function(source_url, record = NULL) {
  page <- "https://cvalues.science.kew.org/releases"
  html <- tryCatch(paste(suppressWarnings(readLines(page, warn = FALSE)),
                         collapse = "\n"),
                   error = function(e) NULL)
  if (is.null(html)) return(NULL)
  m <- regmatches(html, regexec(
    "Release ([0-9.]+) [(]([A-Z][a-z]{2}) ([0-9]{4})[)]", html))[[1L]]
  if (length(m) != 4L) return(NULL)

  month <- match(m[3L], month.abb)
  list(
    id = m[2L],
    version = m[2L],
    date = if (!is.na(month)) sprintf("%s-%02d-01", m[4L], month) else NA_character_,
    url = page
  )
}


#' Check a Phaidra object for a newer version
#'
#' Phaidra (University of Vienna) objects are immutable; a new version is a new
#' object listed under the old one's `versions`. The portal sits behind an
#' Anubis proof-of-work page, which `.anubis_fetch()` clears.
#'
#' @param source_url Character. Phaidra object URL (`.../object/o:<n>/...`).
#' @param record Unused.
#' @return Named list with `id` (the newest object), `version`, `date`,
#'   `pinned` (the object the URL names), and `url`; `NULL` if the object
#'   cannot be read.
#' @export
check_phaidra_version <- function(source_url, record = NULL) {
  pid <- regmatches(source_url, regexpr("o:[0-9]+", source_url))
  if (length(pid) == 0L) return(NULL)

  info_of <- function(id) {
    url <- sprintf("https://phaidra.univie.ac.at/api/object/%s/info", id)
    raw <- tryCatch(.anubis_fetch(url, paste0(
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 ",
      "(KHTML, like Gecko) Chrome/130.0 Safari/537.36"), max_tries = 3L),
      error = function(e) NULL)
    if (is.null(raw)) return(NULL)
    tryCatch(jsonlite::fromJSON(rawToChar(raw), simplifyVector = FALSE)$info,
             error = function(e) NULL)
  }

  info <- info_of(pid)
  if (is.null(info)) return(NULL)
  number <- function(x) as.numeric(sub("^o:", "", x))
  versions <- unlist(lapply(info$versions %||% list(),
                            function(v) v$pid %||% v))
  newest <- pid
  if (length(versions) && max(number(versions)) > number(pid)) {
    newest <- versions[which.max(number(versions))]
    info <- info_of(newest) %||% info
  }
  list(
    id = newest,
    version = newest,
    date = substr(as.character(info$modified %||% NA_character_), 1, 10),
    pinned = pid,
    url = sprintf("https://phaidra.univie.ac.at/%s", newest)
  )
}


#' Check a SEANOE dataset for its file set
#'
#' SEANOE updates a dataset in place under one DOI, adding the new files beside
#' the old ones. The set of file names is therefore the dataset's identity: a
#' new version shows up as a file the set did not have.
#'
#' @param source_url Character. SEANOE dataset URL (`.../data/<n>/<id>/`).
#' @param record Unused.
#' @return Named list with `id` (the sorted file names), `version` (the file
#'   count), and `url`; `NULL` if the dataset cannot be read.
#' @export
check_seanoe_version <- function(source_url, record = NULL) {
  id <- regmatches(source_url,
                   regexec("/data/[0-9]+/([0-9]+)", source_url))[[1L]]
  if (length(id) != 2L) return(NULL)

  api_url <- sprintf("https://www.seanoe.org/api/find-by-id/%s", id[2L])
  files <- .api_json(api_url)$files
  if (length(files) == 0L) return(NULL)

  file_names <- sort(vapply(files, function(f) as.character(f$fileName), ""))
  list(
    id = paste(file_names, collapse = ";"),
    version = sprintf("%d files", length(file_names)),
    url = api_url
  )
}


#' Check the Open Tree Taxonomy for its newest release
#'
#' @param source_url Character. OTT archive URL naming its release
#'   (`.../ott/ott<version>/...`).
#' @param record Unused.
#' @return Named list with `id` and `version` (the newest release), `pinned`
#'   (the release the URL names), and `url`; `NULL` if the listing cannot be
#'   read.
#' @export
check_ott_version <- function(source_url, record = NULL) {
  listing <- "https://files.opentreeoflife.org/ott/"
  html <- tryCatch(paste(suppressWarnings(readLines(listing, warn = FALSE)),
                         collapse = "\n"),
                   error = function(e) NULL)
  if (is.null(html)) return(NULL)

  found <- unique(sub("^ott", "", regmatches(
    html, gregexpr("ott[0-9]+(\\.[0-9]+)+(?=/)", html, perl = TRUE))[[1L]]))
  if (length(found) == 0L) return(NULL)
  newest <- found[order(numeric_version(found), decreasing = TRUE)][1L]
  pinned <- regmatches(source_url,
                       regexec("/ott([0-9]+(\\.[0-9]+)+)/", source_url))[[1L]]
  list(
    id = newest,
    version = newest,
    pinned = if (length(pinned) >= 2L) pinned[2L],
    url = listing
  )
}


#' Check a PANGAEA dataset for changes and successors
#'
#' A PANGAEA DOI names one dataset, which is replaced by a new DOI rather than
#' edited; the metadata record states the data's last modification, its
#' status, and any DataCite relation to a successor.
#'
#' @param source_url Character. `doi.pangaea.de` URL of the dataset.
#' @param record Unused.
#' @return Named list with `id` (last modification, status and any successor),
#'   `version`, `date`, and `url`; `NULL` if the record cannot be read.
#' @export
check_pangaea_version <- function(source_url, record = NULL) {
  doi <- regmatches(source_url, regexpr("10\\.1594/PANGAEA\\.[0-9]+", source_url))
  if (length(doi) == 0L) return(NULL)

  meta_url <- sprintf("https://doi.pangaea.de/%s?format=metainfo_xml", doi)
  xml <- tryCatch(paste(suppressWarnings(readLines(meta_url, warn = FALSE)),
                        collapse = "\n"),
                  error = function(e) NULL)
  if (is.null(xml)) return(NULL)

  entry <- function(key) {
    m <- regmatches(xml, regexec(
      sprintf('<md:entry key="%s" value="([^"]*)"', key), xml))[[1L]]
    if (length(m) == 2L) m[2L] else NA_character_
  }
  modified <- entry("lastModified")
  if (is.na(modified)) return(NULL)
  successors <- regmatches(xml, gregexpr(
    'dataciteRelType="(IsPreviousVersionOf|IsObsoletedBy)"[^>]*>', xml))[[1L]]

  list(
    id = paste(c(modified, entry("status"), successors), collapse = " ; "),
    version = substr(modified, 1, 10),
    date = substr(modified, 1, 10),
    url = meta_url
  )
}


#' Check the World Spider Trait database for new datasets
#'
#' The database grows by datasets, each uploaded with a date and a record
#' count, so their number, their records and the newest upload together say
#' whether the export would read differently.
#'
#' @param source_url Character. A `spidertraits.sci.muni.cz` URL.
#' @param record Unused.
#' @return Named list with `id`, `version`, `date` (the newest upload), and
#'   `url`; `NULL` if the dataset list cannot be read.
#' @export
check_wst_version <- function(source_url, record = NULL) {
  # The API serves at most 100 datasets per page.
  api_url <- "https://spidertraits.sci.muni.cz/backend/datasets"
  items <- list()
  repeat {
    page <- .api_json(sprintf("%s?limit=100&offset=%d", api_url, length(items)))
    if (is.null(page)) return(NULL)
    items <- c(items, page$items)
    if (length(page$items) == 0L || length(items) >= (page$count %||% 0)) break
  }
  if (length(items) == 0L) return(NULL)

  records <- sum(vapply(items, function(x) as.numeric(x$records %||% 0), 0))
  newest <- max(vapply(items, function(x) substr(x$date %||% "", 1, 10), ""))
  label <- sprintf("%d datasets, %.0f records", length(items), records)
  list(
    id = sprintf("%s, newest %s", label, newest),
    version = label,
    date = newest,
    url = api_url
  )
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
  list(host = "zenodo\\.org",                   probe = "check_zenodo_version"),
  list(host = "figshare\\.com",                 probe = "check_figshare_version"),
  list(host = "datadryad\\.org|10\\.5061/dryad\\.", probe = "check_dryad_version"),
  list(host = "raw\\.githubusercontent\\.com",  probe = "check_github_version"),
  list(host = "api\\.gbif\\.org",               probe = "check_gbif_api_version"),
  list(host = "pasta\\.lternet\\.edu/package/data/", probe = "check_edi_version"),
  list(host = "/d1/mn/v2/object/",              probe = "check_dataone_version"),
  list(host = "web\\.archive\\.org/web/[0-9]{14}", probe = "check_wayback_version"),
  list(host = "data-api\\.cefas\\.co\\.uk",     probe = "check_datacite_version"),
  list(host = "persistentId=doi:|doi\\.org/10\\.15454/",
       probe = "check_dataverse_version"),
  list(host = "/resource/[0-9a-f-]{36}|data\\.nhm\\.ac\\.uk",
       probe = "check_ckan_version"),
  list(host = "/archive\\.do\\?r=",             probe = "check_ipt_version"),
  list(host = "gift\\.uni-goettingen\\.de",     probe = "check_gift_version"),
  list(host = "(fish|sealife)base\\.ropensci\\.org",
       probe = "check_rfishbase_version"),
  list(host = "cvalues\\.science\\.kew\\.org",  probe = "check_kew_cvalues_version"),
  list(host = "phaidra\\.univie\\.ac\\.at",     probe = "check_phaidra_version"),
  list(host = "seanoe\\.org/data/",             probe = "check_seanoe_version"),
  list(host = "opentreeoflife\\.org/ott/",      probe = "check_ott_version"),
  list(host = "doi\\.pangaea\\.de",             probe = "check_pangaea_version"),
  list(host = "spidertraits\\.sci\\.muni\\.cz", probe = "check_wst_version"),
  list(host = "stbates\\.org|ofmpub\\.epa\\.gov|mda\\.vliz\\.be",
       probe = "check_content_md5"),
  # An Ecological Archives directory and a doi.org article have no file to
  # date; the article DOI they belong to is checked for corrections.
  list(host = "esapubs\\.org/.*/$|doi\\.org/",  probe = "check_crossref_version"),
  list(host = paste0("gbif\\.org|kew\\.org|esapubs\\.org|genomics\\.senescence\\.info|",
                     "static-content\\.springer\\.com|store\\.pangaea\\.de|",
                     "uol\\.de/|ncbi\\.nlm\\.nih\\.gov"),
       probe = "check_last_modified")
)

# Sources read live from a database that gives no release identity and no
# change date, so no probe can say whether it moved; a rebuild records the
# access month as the version. Reported apart from an unprobed host, which only
# lacks a probe.
.live_sources <- "ser-sid\\.org|bien\\.nceas\\.ucsb\\.edu"

# Hosts that answer an unattended request with a JavaScript challenge only a
# real browser clears (taxifydb's cf_fetch.py browser rung), so the weekly CI
# check cannot reach them; a build records what it read.
.challenge_sources <- "sciencebase\\.gov"

# Several sources download from more than one URL, written `url ; url ; ...`.
.source_sep <- " ; "

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
  if (grepl(.source_sep, url, fixed = TRUE)) {
    parts <- lapply(strsplit(url, .source_sep, fixed = TRUE)[[1L]],
                    .upstream_probe_for)
    if (any(vapply(parts, is.null, TRUE))) return(NULL)
    return(unlist(parts))
  }
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
#' A source downloading from several URLs (`url ; url`) is probed part by
#' part, and its identity is theirs joined: it moves when any part moves, it is
#' pinned only when every part is, and it dates from its newest part.
#'
#' @return The probe's result (`id`, `version`, `date`, optionally `pinned`,
#'   `url`), or `NULL` when no probe covers the host or the host cannot be
#'   reached (for a multi-URL source: any of its hosts).
#' @export
probe_upstream_identity <- function(source_url, record = NULL) {
  url <- source_url %||% ""
  if (grepl(.source_sep, url, fixed = TRUE)) {
    parts <- lapply(strsplit(url, .source_sep, fixed = TRUE)[[1L]],
                    probe_upstream_identity, record = record)
    if (any(vapply(parts, is.null, TRUE))) return(NULL)
    field <- function(name) {
      vapply(parts, function(p) as.character(p[[name]] %||% NA_character_), "")
    }
    pinned <- field("pinned")
    dates <- field("date")
    return(list(
      id = paste(field("id"), collapse = " | "),
      version = paste(field("version"), collapse = " | "),
      date = if (anyNA(dates)) NA_character_ else max(dates),
      pinned = if (!anyNA(pinned)) paste(pinned, collapse = " | "),
      url = paste(field("url"), collapse = .source_sep)
    ))
  }

  probe <- .upstream_probe_for(url)
  if (is.null(probe)) return(NULL)
  tryCatch(match.fun(probe)(url, record), error = function(e) NULL)
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

  if (grepl(.live_sources, url)) {
    return(unknown(paste("live database with no release identity;",
                         "a rebuild records the access month as the version")))
  }
  if (grepl(.challenge_sources, url)) {
    return(unknown(paste("host serves a browser-only JavaScript challenge;",
                         "only a rebuild can read it")))
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
