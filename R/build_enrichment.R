# Enrichment build dispatcher.
#
# build_enrichment() drives one entry from the .enrichment_build_registry:
# download raw source, parse to data.frame, run resolve_enrichment_names()
# to expand across all 7 backbones, then write the indexed .vtr via
# build_enrichment_vtr().


#' List available enrichment builders
#'
#' @return Character vector of enrichment identifiers.
#' @export
list_enrichments <- function() {
  names(.enrichment_build_registry)
}


#' Build an enrichment `.vtr` from source
#'
#' Downloads the raw source for an enrichment, parses it via the registered
#' parser, expands names across all 7 taxify backbones via
#' [resolve_enrichment_names()], and writes a `.vtr` plus `meta.json` sidecar
#' via [build_enrichment_vtr()].
#'
#' @param name Character. Enrichment identifier. See `list_enrichments()`.
#' @param output_dir Character or `NULL`. Output directory. Default
#'   `output/enrichment/<name>`.
#' @param version Character or `NULL`. Version override. If `NULL`, uses the
#'   registry's pinned version.
#' @param url Character or `NULL`. Custom source URL override. If supplied,
#'   the build version defaults to `format(Sys.Date(), "%Y.%m")` and
#'   `source_doi` is set to `NULL`.
#' @param resolve_names Logical. Run [resolve_enrichment_names()] before
#'   writing. Default `TRUE`.
#' @param verbose Logical. Default `TRUE`.
#' @return Path to the built `.vtr` file (invisibly).
#' @export
build_enrichment <- function(name, output_dir = NULL, version = NULL,
                             url = NULL, resolve_names = TRUE,
                             verbose = TRUE) {
  reg <- .enrichment_build_registry[[name]]
  if (is.null(reg)) {
    available <- paste(names(.enrichment_build_registry), collapse = ", ")
    stop(sprintf("Unknown enrichment '%s'. Available: %s", name, available),
         call. = FALSE)
  }

  for (pkg in reg$requires) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf(
        paste0("Package '%s' is required to build enrichment '%s' from ",
               "source. Install with: install.packages('%s')"),
        pkg, name, pkg
      ), call. = FALSE)
    }
  }

  source_url <- url %||% reg$source_url
  if (verbose) {
    message(sprintf("Building enrichment '%s' from source...", name))
  }

  dl_dir <- file.path(tempdir(), "taxifydb_enrichment_build", name)
  if (verbose) message("  Downloading source data...")
  source_path <- reg$download_fn(source_url, dl_dir)

  if (verbose) message("  Parsing...")
  df <- reg$parse_fn(source_path)

  if (!is.data.frame(df) || nrow(df) == 0L) {
    stop(sprintf("Parse function for '%s' returned no data.", name),
         call. = FALSE)
  }

  if (verbose) {
    message(sprintf("  Parsed %s rows.", format(nrow(df), big.mark = ",")))
  }

  # A registry entry may set resolve_names = FALSE when its parser already
  # resolves to the accepted-name grain (e.g. host-breadth rollups), so the
  # pipeline must not resolve a second time.
  reg_resolves <- if (is.null(reg$resolve_names)) TRUE else isTRUE(reg$resolve_names)
  if (isTRUE(resolve_names) && reg_resolves && "canonical_name" %in% names(df)) {
    if (verbose) message("  Resolving names against backbones...")
    group_cols <- if (!is.null(reg$group_col)) reg$group_col else NULL
    # A source whose names carry authorship (e.g. ITALIC's "Genus sp. Author")
    # cannot match the clean-binomial fast-path lookup key; such an entry sets
    # use_lookup = FALSE to take the authorship-aware taxify() resolution path.
    use_lookup <- if (is.null(reg$use_lookup)) TRUE else isTRUE(reg$use_lookup)
    df <- resolve_enrichment_names(df, group_cols = group_cols,
                                   verbose = verbose, use_lookup = use_lookup)
  }

  if (is.null(output_dir)) {
    output_dir <- file.path("output", "enrichment", name)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  vtr_path <- file.path(output_dir, paste0(name, ".vtr"))

  build_enrichment_vtr(
    df, vtr_path,
    name          = name,
    version       = version %||%
                    (if (!is.null(url)) format(Sys.Date(), "%Y.%m") else reg$version),
    source_url    = source_url,
    source_doi    = if (!is.null(url)) NULL else reg$source_doi,
    license       = reg$license,
    attribution   = reg$attribution,
    group_col     = reg$group_col,
    # Optional registry fields for the runtime manifest; sensible defaults when a
    # registry entry omits them (species-grain, frozen snapshot).
    species_col   = reg$species_col,
    static        = reg$static %||% TRUE,
    source_format = reg$source_format
  )

  if (verbose) {
    size_mb <- file.size(vtr_path) / 1048576
    message(sprintf(
      "  Built '%s' enrichment: %s rows, %.1f MB.",
      name, format(nrow(df), big.mark = ","), size_mb
    ))
  }

  invisible(vtr_path)
}


#' Build enrichment from source and return the raw data.frame
#'
#' Same as `build_enrichment()` but returns the parsed data.frame instead of
#' writing a `.vtr` file. Useful when the runtime needs the raw data in
#' memory (e.g. emergency fallback wired into `taxify`).
#'
#' @param name Character. Enrichment identifier.
#' @param verbose Logical. Default `TRUE`.
#' @return data.frame with `canonical_name` + trait columns.
#' @export
enrichment_emergency_fallback <- function(name, verbose = TRUE) {
  reg <- .enrichment_build_registry[[name]]
  if (is.null(reg)) {
    available <- paste(names(.enrichment_build_registry), collapse = ", ")
    stop(sprintf("Unknown enrichment '%s'. Available: %s", name, available),
         call. = FALSE)
  }

  for (pkg in reg$requires) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf(
        paste0("Package '%s' is required for enrichment '%s'. ",
               "Install with: install.packages('%s')"),
        pkg, name, pkg
      ), call. = FALSE)
    }
  }

  if (verbose) {
    warning(sprintf(
      paste0("Building enrichment '%s' in emergency fallback mode. ",
             "This returns a temporary in-memory data.frame, not a ",
             "persistent .vtr file. Run build_enrichment('%s') for a ",
             "permanent build."),
      name, name
    ), call. = FALSE, immediate. = TRUE)
  }

  dl_dir <- file.path(tempdir(), "taxifydb_enrichment_fallback", name)
  if (verbose) message(sprintf("Downloading '%s' source data...", name))
  source_path <- reg$download_fn(reg$source_url, dl_dir)

  if (verbose) message("Parsing...")
  df <- reg$parse_fn(source_path)

  if (verbose) {
    message(sprintf(
      "Emergency fallback: %s rows for '%s'.",
      format(nrow(df), big.mark = ","), name
    ))
  }

  df
}
