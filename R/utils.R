# Internal helpers shared across modules.

#' Null-coalescing operator
#'
#' Returns `x` unless it is `NULL`, in which case it returns `y`.
#' Defined locally so the package does not need to import rlang just for this.
#'
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
