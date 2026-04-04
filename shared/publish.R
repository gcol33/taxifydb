# ---- Publish backbone releases to GitHub ----
#
# Uses `gh` CLI to create releases and upload .vtr + .xdelta artifacts.
# Assumes `gh` is authenticated and available on PATH.

#' Create a GitHub release and upload backbone artifacts
#'
#' @param backend_name Character. Backend identifier.
#' @param version Character. Version string.
#' @param vtr_path Character. Path to the .vtr file.
#' @param delta_path Character or NULL. Path to the .xdelta file.
#' @param meta_path Character. Path to the .meta sidecar.
#' @param repo Character. GitHub repo (e.g., "gcol33/taxify-backbones").
#' @param notes Character. Release notes.
#' @return The release tag (invisibly).
publish_release <- function(backend_name, version, vtr_path,
                            delta_path = NULL, meta_path = NULL,
                            repo = "gcol33/taxify-backbones",
                            notes = NULL) {
  tag <- sprintf("%s-%s", backend_name, version)

  if (is.null(notes)) {
    size_mb <- file.size(vtr_path) / 1048576
    notes <- sprintf(
      "%s backbone v%s\n\nBuilt: %s\nRows: %s\nSize: %.1f MB",
      toupper(backend_name), version, Sys.Date(),
      format(count_vtr_rows(vtr_path), big.mark = ","), size_mb
    )
  }

  # Create release
  system2("gh", c(
    "release", "create", tag,
    "--repo", repo,
    "--title", shQuote(sprintf("%s v%s", toupper(backend_name), version)),
    "--notes", shQuote(notes)
  ))

  # Upload artifacts
  artifacts <- vtr_path
  if (!is.null(delta_path) && file.exists(delta_path)) {
    artifacts <- c(artifacts, delta_path)
  }
  if (!is.null(meta_path) && file.exists(meta_path)) {
    artifacts <- c(artifacts, meta_path)
  }

  system2("gh", c(
    "release", "upload", tag,
    artifacts,
    "--repo", repo,
    "--clobber"
  ))

  message(sprintf("Published release: %s (%d artifacts)", tag, length(artifacts)))
  invisible(tag)
}


#' Count rows in a .vtr file (for metadata)
#'
#' @param vtr_path Character.
#' @return Integer.
count_vtr_rows <- function(vtr_path) {
  nrow(vectra::tbl(vtr_path) |>
         vectra::summarize(n = vectra::n()) |>
         vectra::collect())
}


#' Update manifest.json after a successful build
#'
#' Reads the existing manifest, updates the entry for the built backend,
#' and writes back. Also computes SHA-256 checksums.
#'
#' @param manifest_path Character. Path to manifest.json.
#' @param backend_name Character.
#' @param version Character.
#' @param vtr_path Character.
#' @param delta_path Character or NULL.
#' @param delta_from Character or NULL. Previous version the delta is from.
#' @param repo Character. GitHub repo for URL construction.
#' @param source_url Character. Original data source URL.
#' @return The updated manifest (invisibly).
update_manifest <- function(manifest_path, backend_name, version,
                            vtr_path, delta_path = NULL,
                            delta_from = NULL,
                            repo = "gcol33/taxify-backbones",
                            source_url = NULL) {
  # Read existing manifest
  if (file.exists(manifest_path)) {
    manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  } else {
    manifest <- list(schema_version = 2L, backends = list(), enrichment = list())
  }

  # Ensure schema_version 2 structure
  if (is.null(manifest$schema_version)) {
    manifest <- list(
      schema_version = 2L,
      backends = manifest,
      enrichment = list()
    )
  }

  tag <- sprintf("%s-%s", backend_name, version)
  base_url <- sprintf(
    "https://github.com/%s/releases/download/%s", repo, tag
  )

  entry <- list(
    latest     = version,
    full_url   = sprintf("%s/%s.vtr", base_url, backend_name),
    full_size  = file.size(vtr_path),
    full_sha256 = sha256(vtr_path),
    nrow       = count_vtr_rows(vtr_path)
  )

  if (!is.null(source_url)) {
    entry$source_url <- source_url
  }

  if (!is.null(delta_path) && file.exists(delta_path)) {
    entry$delta_from <- delta_from
    entry$delta_url  <- sprintf("%s/%s.xdelta", base_url, backend_name)
    entry$delta_size <- file.size(delta_path)
  }

  manifest$backends[[backend_name]] <- entry

  jsonlite::write_json(manifest, manifest_path, pretty = TRUE,
                       auto_unbox = TRUE)

  message(sprintf("Manifest updated: %s v%s", backend_name, version))
  invisible(manifest)
}


#' Update manifest.json enrichment entry from built meta.json
#'
#' Reads the meta.json sidecar produced by `build_enrichment_vtr()` and
#' updates the corresponding entry under `manifest$enrichments`.
#'
#' @param manifest_path Character. Path to manifest.json.
#' @param name Character. Enrichment identifier.
#' @param vtr_path Character. Path to the built .vtr file.
#' @param repo Character. GitHub repo for URL construction.
#' @return The updated manifest (invisibly).
update_enrichment_manifest <- function(manifest_path, name, vtr_path,
                                       repo = "gcol33/taxify-backbones") {
  # Read meta.json sidecar
  meta_path <- file.path(dirname(vtr_path), "meta.json")
  if (!file.exists(meta_path)) {
    stop(sprintf("No meta.json found at: %s", meta_path))
  }
  meta <- jsonlite::read_json(meta_path, simplifyVector = TRUE)

  # Read existing manifest
  if (file.exists(manifest_path)) {
    manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  } else {
    manifest <- list(schema_version = 2L, backends = list(), enrichments = list())
  }
  if (is.null(manifest$enrichments)) manifest$enrichments <- list()

  # Build release URL
  tag <- sprintf("enrichment-%s", meta$version)
  base_url <- sprintf(
    "https://github.com/%s/releases/download/%s", repo, tag
  )

  entry <- manifest$enrichments[[name]]
  if (is.null(entry)) entry <- list()

  entry$latest     <- meta$version
  entry$full_url   <- sprintf("%s/%s.vtr", base_url, name)
  entry$nrow       <- meta$nrow

  if (!is.null(meta$source_url))  entry$source_url  <- meta$source_url
  if (!is.null(meta$source_doi))  entry$source_doi  <- meta$source_doi
  if (!is.null(meta$license))     entry$license      <- meta$license

  # Group metadata
  if (!is.null(meta$available_groups)) {
    entry$available_groups <- meta$available_groups
  }

  manifest$enrichments[[name]] <- entry

  jsonlite::write_json(manifest, manifest_path, pretty = TRUE,
                       auto_unbox = TRUE)

  message(sprintf("Manifest enrichment updated: %s v%s", name, meta$version))
  invisible(manifest)
}
