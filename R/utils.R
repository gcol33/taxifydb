# Internal helpers shared across modules.

#' Null-coalescing operator
#'
#' Returns `x` unless it is `NULL`, in which case it returns `y`.
#' Defined locally so the package does not need to import rlang just for this.
#'
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x


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
