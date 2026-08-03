# Backend build dispatcher.
#
# Single entry point for `build_backend(name, ...)`. Routes to the
# backend-specific `build_<name>()` function. New backends register
# themselves by adding a case in `.backend_builders`.

.backend_builders <- list(
  itis      = function(...) build_itis(...),
  ncbi      = function(...) build_ncbi(...),
  ott       = function(...) build_ott(...),
  worms     = function(...) build_worms(...),
  wfo       = function(...) build_wfo(...),
  col       = function(...) build_col(...),
  gbif      = function(...) build_gbif(...),
  euromed   = function(...) build_euromed(...),
  fungorum  = function(...) build_fungorum(...),
  algaebase = function(...) build_algaebase(...),
  fishbase    = function(...) build_fishbase(...),
  sealifebase = function(...) build_sealifebase(...),
  reptiledb   = function(...) build_reptiledb(...),
  wcvp        = function(...) build_wcvp(...),
  lcvp        = function(...) build_lcvp(...),
  mdd         = function(...) build_mdd(...),
  avilist     = function(...) build_avilist(...),
  lpsn        = function(...) build_lpsn(...)
)


#' Build a taxify backbone `.vtr` file from source
#'
#' Single entry point for backbone builds. Downloads the raw source,
#' normalizes to the unified schema, precomputes matching keys, and writes
#' the final `.vtr` with indexes and metadata sidecar.
#'
#' @param name Character. Backend identifier (e.g., "itis"). See
#'   [list_backends()] for available names.
#' @param output_dir Character. Output directory. Default: `output/<name>`.
#' @param version Character or NULL. Version string for the build. If `NULL`,
#'   defaults to the current YYYY.MM (or the backend's bundled default).
#' @param verbose Logical.
#' @return Path to the built `.vtr` file (invisibly).
#' @export
build_backend <- function(name, output_dir = NULL, version = NULL,
                          verbose = TRUE) {
  if (!name %in% names(.backend_builders)) {
    stop(sprintf("Unknown backend: '%s'. Available: %s",
                 name, paste(names(.backend_builders), collapse = ", ")),
         call. = FALSE)
  }

  if (is.null(output_dir)) {
    output_dir <- file.path("output", name)
  }

  .backend_builders[[name]](output_dir = output_dir, version = version,
                            verbose = verbose)
}


#' List available backend builders
#'
#' @return Character vector of backend identifiers.
#' @export
list_backends <- function() {
  names(.backend_builders)
}
