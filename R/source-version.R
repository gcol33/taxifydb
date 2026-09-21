# Source release identity for backbone builds.
#
# A backbone's version is the release of the source it was built from, never the
# date the build ran. It is written to the `.meta` sidecar, becomes the release
# tag and the manifest's `latest`, and is what taxify_lock() stamps downstream,
# so a build month in its place records a taxonomy date that is not the
# taxonomy's. Each backend resolves its version from the same place it downloads
# from, through the helpers below, and write_backbone_meta() refuses a value
# that cannot name a release.

.checklistbank_api <- "https://api.checklistbank.org"


#' Normalize a source release date to the `YYYY.MM` version form
#'
#' @param x A `Date`, a `POSIXct`, or a character string beginning `YYYY-MM`
#'   (`"2026-06"`, `"2026-08-26 XR"`) or consisting of a year alone (`"2021"`).
#' @return Character scalar: `"YYYY.MM"`, or `"YYYY"` for a year-only release.
#' @noRd
release_version_from_date <- function(x) {
  if (inherits(x, c("Date", "POSIXt"))) {
    if (is.na(x)) stop("Source release date is missing.", call. = FALSE)
    return(format(x, "%Y.%m"))
  }
  x <- trimws(as.character(x))
  if (length(x) == 1L && !is.na(x)) {
    if (grepl("^[0-9]{4}-[0-9]{2}", x)) {
      return(paste0(substr(x, 1L, 4L), ".", substr(x, 6L, 7L)))
    }
    if (grepl("^[0-9]{4}$", x)) return(x)
  }
  stop(sprintf("Cannot read a release date from '%s'.", paste(x, collapse = ", ")),
       call. = FALSE)
}


#' Normalize a source release date to the `source_date` form
#'
#' A backbone's version names its release; `source_date` records the day the
#' source dates it, at the precision the source gives. The version alone cannot
#' carry it: a `YYYY.MM` version drops the day, and a named release (`3.7.3`,
#' `2025b`) carries no date at all.
#'
#' @param x A `Date`, a `POSIXct`, or a character string beginning
#'   `YYYY-MM-DD` (`"2026-08-26 XR"`), a compact `YYYYMMDD`, a month
#'   `YYYY-MM`, or a year `YYYY`.
#' @return Character scalar: `"YYYY-MM-DD"`, `"YYYY-MM"` or `"YYYY"`.
#' @noRd
source_date_from <- function(x) {
  if (inherits(x, c("Date", "POSIXt"))) {
    if (is.na(x)) stop("Source release date is missing.", call. = FALSE)
    return(format(x, "%Y-%m-%d", tz = "UTC"))
  }
  x <- trimws(as.character(x))
  if (length(x) == 1L && !is.na(x)) {
    if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x)) return(substr(x, 1L, 10L))
    if (grepl("^[0-9]{8}$", x)) {
      return(paste(substr(x, 1L, 4L), substr(x, 5L, 6L), substr(x, 7L, 8L),
                   sep = "-"))
    }
    if (grepl("^[0-9]{4}-[0-9]{2}$", x) || grepl("^[0-9]{4}$", x)) return(x)
  }
  stop(sprintf("Cannot read a source date from '%s'.", paste(x, collapse = ", ")),
       call. = FALSE)
}


#' A release read from one source date: its version and its `source_date`
#'
#' @param x Anything [source_date_from()] and [release_version_from_date()]
#'   both accept.
#' @return A list with `version` (`YYYY.MM` or `YYYY`) and `date`.
#' @noRd
release_from_date <- function(x) {
  list(version = release_version_from_date(x), date = source_date_from(x))
}


#' Stop unless a version can name a backbone release
#'
#' The version becomes the `<backend>-<version>` release tag, so it has to be a
#' release identifier: it starts with a digit and holds only letters, digits and
#' dots (`2026.06`, `3.7.3`, `2025b`). A label such as `current` names no
#' release, and a date with spaces or dashes would mint a second spelling of a
#' version the `YYYY.MM` form already covers.
#'
#' @param version Character scalar.
#' @param backend_name Character. Used in the error message.
#' @return `version`, invisibly.
#' @noRd
check_release_version <- function(version, backend_name) {
  ok <- is.character(version) && length(version) == 1L && !is.na(version) &&
    grepl("^[0-9][0-9A-Za-z.]*$", version)
  if (!ok) {
    stop(sprintf(paste0(
      "%s: version '%s' cannot name a release. A backbone's version is the ",
      "source release it was built from, in a form such as 2026.06 or 3.7.3."),
      backend_name, paste(format(version), collapse = ", ")), call. = FALSE)
  }
  invisible(version)
}


#' Resolve the release a ChecklistBank dataset currently serves
#'
#' A ChecklistBank `/dataset/<key>/archive` download is the dataset's latest
#' import, so its identity is read from the dataset record at build time rather
#' than pinned beside the URL. `issued` is the date the source gives its own
#' release.
#'
#' @param key Character or integer. ChecklistBank dataset key.
#' @return A list with `key`, `issued` (as recorded), `version` (`YYYY.MM`)
#'   and `date` (the `source_date`).
#' @noRd
checklistbank_release <- function(key) {
  url <- sprintf("%s/dataset/%s", .checklistbank_api, key)
  rec <- tryCatch(
    jsonlite::fromJSON(url, simplifyVector = FALSE),
    error = function(e) {
      stop(sprintf("Could not read ChecklistBank dataset %s: %s", key,
                   conditionMessage(e)), call. = FALSE)
    }
  )
  issued <- rec$issued %||% ""
  if (!nzchar(issued)) {
    stop(sprintf("ChecklistBank dataset %s records no issued date.", key),
         call. = FALSE)
  }
  c(list(key = as.character(key), issued = issued), release_from_date(issued))
}


#' Release date of a file a source republishes in place
#'
#' ITIS, NCBI and Kew overwrite one download URL with each release and state no
#' version beside it; the file's `Last-Modified` header is the date of the
#' release it currently holds.
#'
#' @param url Character. Download URL.
#' @return `POSIXct` modification time.
#' @noRd
source_last_modified <- function(url) {
  h <- curl::new_handle(nobody = TRUE, followlocation = TRUE,
                        connecttimeout = 60L, timeout = 120L)
  res <- tryCatch(
    curl::curl_fetch_memory(url, handle = h),
    error = function(e) {
      stop(sprintf("Could not reach %s: %s", url, conditionMessage(e)),
           call. = FALSE)
    }
  )
  if (res$status_code >= 400L) {
    stop(sprintf("%s answered HTTP %d.", url, res$status_code), call. = FALSE)
  }
  if (is.na(res$modified)) {
    stop(sprintf("%s sends no Last-Modified header to date its release.", url),
         call. = FALSE)
  }
  res$modified
}


#' The release a republished-in-place file currently holds
#'
#' @param url Character. Download URL.
#' @return A list with `version` and `date`, read from `Last-Modified`.
#' @noRd
last_modified_release <- function(url) {
  release_from_date(source_last_modified(url))
}
