# Backend build dispatcher.
#
# Single entry point for `build_backend(name, ...)`. Routes to the
# backend-specific `build_<name>()` function. New backends register
# themselves by adding a case in `.backend_builders`.
#
# Two registries, because "buildable" and "is a taxonomic backbone" are not the
# same set. `.backend_builders` is the second: `list_backends()` returns exactly
# it, and enrichment name resolution (`build_all_name_lookups()`,
# `resolve_name_map()`) iterates it to build a per-backbone accepted-name lookup.
# Only sources carrying taxonomic names may sit there.

.backend_builders <- list(
  itis      = function(...) build_itis(...),
  ncbi      = function(...) build_ncbi(...),
  ott       = function(...) build_ott(...),
  worms     = function(...) build_worms(...),
  wfo       = function(...) build_wfo(...),
  col       = function(...) build_col(...),
  colxr     = function(...) build_colxr(...),
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


# Reference geometry: published, versioned, byte-gated and downloaded exactly
# like a backbone, but carrying polygon vertices (code/geom/ring/seq/lon/lat)
# instead of names. Dispatchable through build_backend() so the build workflow
# can cut them by name like everything else -- until now they had an exported
# builder but no dispatch entry and no workflow, so they could only be produced
# by hand, which is why neither release carries the .meta sidecar its builder
# writes.
#
# Deliberately outside .backend_builders, and so outside list_backends(): a
# name lookup built from a vertex table has no names in it, and every enrichment
# resolved through that backbone would quietly lose its matches.
.geometry_builders <- list(
  wgsrpd = function(...) build_wgsrpd(...),
  meow   = function(...) build_meow(...)
)


#' Every buildable artifact, backbones and reference geometry alike
#' @noRd
.all_builders <- function() c(.backend_builders, .geometry_builders)


#' Build a taxify backbone `.vtr` file from source
#'
#' Single entry point for backbone builds. Downloads the raw source,
#' normalizes to the unified schema, precomputes matching keys, and writes
#' the final `.vtr` with indexes and metadata sidecar.
#'
#' @param name Character. Backend identifier (e.g., "itis"). See
#'   [list_backends()] for the taxonomic backbones and [list_geometry()] for the
#'   reference-geometry artifacts; both are buildable here.
#' @param output_dir Character. Output directory. Default: `output/<name>`.
#' @param version Character or NULL. Version string for the build. If `NULL`,
#'   defaults to the current YYYY.MM (or the backend's bundled default).
#' @param verbose Logical.
#' @return Path to the built `.vtr` file (invisibly).
#' @export
build_backend <- function(name, output_dir = NULL, version = NULL,
                          verbose = TRUE) {
  builders <- .all_builders()
  if (!name %in% names(builders)) {
    stop(sprintf("Unknown backend: '%s'. Available: %s",
                 name, paste(names(builders), collapse = ", ")),
         call. = FALSE)
  }

  if (is.null(output_dir)) {
    output_dir <- file.path("output", name)
  }

  builders[[name]](output_dir = output_dir, version = version,
                   verbose = verbose)
}


#' List available backend builders
#'
#' The taxonomic backbones only. This is the set enrichment name resolution
#' unions over, so reference geometry ([list_geometry()]) is not part of it even
#' though [build_backend()] builds both.
#'
#' @return Character vector of backend identifiers.
#' @export
list_backends <- function() {
  names(.backend_builders)
}


#' List available reference-geometry builders
#'
#' Boundary polygons taxify reads for the `region=` / `coords=` constraint.
#' Published and versioned like a backbone, but they carry no taxonomic names.
#'
#' @return Character vector of geometry identifiers.
#' @export
list_geometry <- function() {
  names(.geometry_builders)
}
