# Internal helpers shared across modules.

#' Null-coalescing operator
#'
#' Returns `x` unless it is `NULL`, in which case it returns `y`.
#' Defined locally so the package does not need to import rlang just for this.
#'
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x


#' Test whether a value would serialize to `null` or empty object
#'
#' A field is empty when `jsonlite::toJSON(auto_unbox = TRUE)` would render
#' it as `null` or `{}`: `NULL`, a zero-length atomic vector or list, a
#' length-one `NA`, or an empty / whitespace-only string.
#'
#' @param v Any value.
#' @return `TRUE` if the value is empty for serialization purposes.
#' @noRd
is_empty_field <- function(v) {
  if (is.null(v)) return(TRUE)
  if (is.list(v)) return(length(v) == 0L)
  if (length(v) == 0L) return(TRUE)
  if (length(v) == 1L) {
    if (is.na(v)) return(TRUE)
    if (is.character(v) && !nzchar(trimws(v))) return(TRUE)
  }
  FALSE
}


#' Drop absent or empty fields from a named list before serialization
#'
#' Removes elements that would serialize to `null` or an empty object `{}`
#' under `jsonlite::write_json(auto_unbox = TRUE)`. Named lists are compacted
#' recursively and dropped when they become empty, so a nested block such as a
#' citation with an absent `doi` loses that field entirely rather than emitting
#' `"doi": {}`. Unnamed lists (JSON arrays) and non-empty atomic vectors pass
#' through unchanged.
#'
#' @param x A named list.
#' @return The list with empty fields removed.
#' @noRd
drop_empty_fields <- function(x) {
  if (!is.list(x) || is.null(names(x))) return(x)
  out <- list()
  for (nm in names(x)) {
    v <- x[[nm]]
    if (is.list(v) && !is.null(names(v))) v <- drop_empty_fields(v)
    if (is_empty_field(v)) next
    out[[nm]] <- v
  }
  out
}


#' Write JSON with newline line endings on every platform
#'
#' `jsonlite::write_json()` writes through a text connection, so a file written
#' on Windows ends every line CRLF where the same file written by CI ends LF.
#' `manifest.json` is committed, and the backbones too large for the hosted
#' runner are published by hand from Windows, so writing it there rewrote all
#' 5,800 of its lines and buried the one entry that had changed. Serializing to
#' a string and writing it through a binary connection leaves the endings alone.
#'
#' @param x The value to serialize.
#' @param path Character. Destination file.
#' @param ... Passed to [jsonlite::toJSON()].
#' @return `path`, invisibly.
#' @noRd
write_json_lf <- function(x, path, ...) {
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(enc2utf8(as.character(jsonlite::toJSON(x, ...))), con,
             useBytes = TRUE)
  invisible(path)
}


#' Strip a trailing authorship string from a scientific name
#'
#' Produces the canonical name by removing `authorship` from the end of
#' `sci_name`. The authorship is removed only when `sci_name` actually ends
#' with it; sources occasionally carry a `scientificNameAuthorship` that is
#' not a literal suffix of `scientificName`, in which case a length-based
#' truncation would cut the name at an arbitrary position. A degenerate
#' result (empty, or shorter than three characters with no internal space)
#' falls back to the original `sci_name`.
#'
#' @param sci_name Character vector of scientific names (with authorship).
#' @param authorship Character vector of authorship strings, same length.
#' @return Character vector of canonical names.
#' @noRd
strip_authorship <- function(sci_name, authorship) {
  canonical <- sci_name
  has_both <- !is.na(sci_name) & !is.na(authorship) & nzchar(authorship)
  if (any(has_both)) {
    sn <- sci_name[has_both]
    au <- authorship[has_both]
    suffix_match <- endsWith(sn, au)
    if (any(suffix_match)) {
      sn_match <- sn[suffix_match]
      au_match <- au[suffix_match]
      stripped <- trimws(substr(sn_match, 1L,
                                nchar(sn_match) - nchar(au_match)))
      degenerate <- !nzchar(stripped) |
        (nchar(stripped) < 3L & !grepl(" ", stripped))
      stripped[degenerate] <- sn_match[degenerate]
      canonical[has_both][suffix_match] <- stripped
    }
  }
  canonical
}
