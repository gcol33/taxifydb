# Publish backbone releases to GitHub and update the manifest.

#' Create a GitHub release and upload backbone artifacts
#'
#' Uses the `gh` CLI. Assumes `gh` is authenticated and on PATH.
#'
#' @param backend_name Character. Backend identifier.
#' @param version Character. Version string.
#' @param vtr_path Character. Path to the main `.vtr` file.
#' @param delta_path Character or NULL. Path to the `.xdelta` file.
#' @param meta_path Character or NULL. Path to the `.meta` sidecar.
#' @param extras Character vector. Paths to additional sidecar artifacts
#'   (e.g., `col_species_profile.vtr`) that should be uploaded with the
#'   release and recorded in the manifest. Must exist on disk; basenames
#'   are used as the manifest entry names.
#' @param repo Character. GitHub repo (e.g., "gcol33/taxifydb").
#' @param notes Character. Release notes.
#' @return The release tag (invisibly).
#' @export
publish_release <- function(backend_name, version, vtr_path,
                            delta_path = NULL, meta_path = NULL,
                            extras = character(0L),
                            repo = "gcol33/taxifydb",
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

  # Create the release only if it doesn't already exist (idempotent: a
  # previous failed run can leave an empty release behind, and we still
  # want to be able to re-upload assets without manual cleanup).
  view_status <- suppressWarnings(system2(
    "gh", c("release", "view", tag, "--repo", repo),
    stdout = FALSE, stderr = FALSE
  ))
  if (view_status != 0L) {
    create_status <- system2("gh", c(
      "release", "create", tag,
      "--repo", repo,
      "--title", shQuote(sprintf("%s v%s", toupper(backend_name), version)),
      "--notes", shQuote(notes)
    ))
    if (create_status != 0L) {
      stop(sprintf("gh release create failed for %s (exit %d)",
                   tag, create_status), call. = FALSE)
    }
  } else {
    message(sprintf("Release %s already exists — uploading assets only.",
                    tag))
  }

  artifacts <- vtr_path
  if (!is.null(delta_path) && file.exists(delta_path)) {
    artifacts <- c(artifacts, delta_path)
  }
  if (!is.null(meta_path) && file.exists(meta_path)) {
    artifacts <- c(artifacts, meta_path)
  }
  if (length(extras) > 0L) {
    missing <- extras[!file.exists(extras)]
    if (length(missing) > 0L) {
      stop("Missing extras files: ", paste(missing, collapse = ", "),
           call. = FALSE)
    }
    artifacts <- c(artifacts, extras)
  }

  upload_out <- system2("gh", c(
    "release", "upload", tag,
    artifacts,
    "--repo", repo,
    "--clobber"
  ), stdout = TRUE, stderr = TRUE)
  upload_status <- attr(upload_out, "status")
  if (length(upload_out) > 0L) {
    message(paste(upload_out, collapse = "\n"))
  }
  if (!is.null(upload_status) && upload_status != 0L) {
    stop(sprintf("gh release upload failed for %s (exit %d)",
                 tag, upload_status), call. = FALSE)
  }

  message(sprintf("Published release: %s (%d artifacts)",
                  tag, length(artifacts)))
  invisible(tag)
}


#' Count rows in a .vtr file
#'
#' @param vtr_path Character.
#' @return Integer.
#' @export
count_vtr_rows <- function(vtr_path) {
  agg <- vectra::tbl(vtr_path) |>
    vectra::summarize(n = vectra::n()) |>
    vectra::collect()
  as.integer(agg$n)
}


#' Update manifest.json after a successful backbone build
#'
#' @param manifest_path Character. Path to manifest.json.
#' @param backend_name Character.
#' @param version Character.
#' @param vtr_path Character.
#' @param delta_path Character or NULL.
#' @param delta_from Character or NULL. Previous version the delta is from.
#' @param extras Character vector. Paths to sidecar artifacts uploaded with
#'   the release. Recorded as `extras: [{name, url, size, sha256}]` in the
#'   manifest entry; the runtime downloader fetches each into the same
#'   versioned directory as the main `.vtr`.
#' @param repo Character. GitHub repo for URL construction.
#' @param source_url Character. Original data source URL.
#' @return The updated manifest (invisibly).
#' @export
update_manifest <- function(manifest_path, backend_name, version,
                            vtr_path, delta_path = NULL,
                            delta_from = NULL,
                            extras = character(0L),
                            repo = "gcol33/taxifydb",
                            source_url = NULL) {
  if (file.exists(manifest_path)) {
    manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  } else {
    manifest <- list(schema_version = 2L, backends = list(),
                     enrichment = list())
  }

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

  # Merge into the existing entry: preserve any fields we don't set
  # (e.g., citation blocks in taxify/inst/manifest.json).
  entry <- manifest$backends[[backend_name]]
  if (is.null(entry)) entry <- list()

  entry$latest      <- version
  entry$full_url    <- sprintf("%s/%s.vtr", base_url, backend_name)
  entry$full_size   <- file.size(vtr_path)
  entry$full_sha256 <- sha256(vtr_path)
  entry$nrow        <- count_vtr_rows(vtr_path)

  if (!is.null(source_url)) {
    entry$source_url <- source_url
  }

  if (!is.null(delta_path) && file.exists(delta_path)) {
    entry$delta_from <- delta_from
    entry$delta_url  <- sprintf("%s/%s.xdelta", base_url, backend_name)
    entry$delta_size <- file.size(delta_path)
  } else {
    # Drop stale delta fields if a previous version had them
    entry$delta_from <- NULL
    entry$delta_url  <- NULL
    entry$delta_size <- NULL
  }

  if (length(extras) > 0L) {
    missing <- extras[!file.exists(extras)]
    if (length(missing) > 0L) {
      stop("Missing extras files: ", paste(missing, collapse = ", "),
           call. = FALSE)
    }
    entry$extras <- lapply(extras, function(p) {
      name <- basename(p)
      list(
        name   = name,
        url    = sprintf("%s/%s", base_url, name),
        size   = file.size(p),
        sha256 = sha256(p)
      )
    })
  } else {
    entry$extras <- NULL
  }

  manifest$backends[[backend_name]] <- entry

  jsonlite::write_json(manifest, manifest_path, pretty = TRUE,
                       auto_unbox = TRUE)

  message(sprintf("Manifest updated: %s v%s", backend_name, version))
  invisible(manifest)
}


#' Update manifest.json enrichment entry from built meta.json
#'
#' Enrichment `.vtr` files are published under a single rolling release tag
#' (e.g. `enrichment-2026.06`) regardless of the dataset's own version. The
#' manifest entry therefore records two distinct versions: `latest` holds the
#' dataset version (from the meta sidecar) while the download URL points at the
#' rolling release. Pass `release_version` to set the release tag explicitly;
#' when omitted it falls back to the dataset version (correct only when the two
#' happen to coincide).
#'
#' @param manifest_path Character. Path to manifest.json.
#' @param name Character. Enrichment identifier.
#' @param vtr_path Character. Path to the built .vtr file.
#' @param release_version Character or NULL. Rolling release version used in the
#'   download URL tag (`enrichment-<release_version>`). Defaults to the dataset
#'   version from the meta sidecar.
#' @param repo Character. GitHub repo for URL construction.
#' @return The updated manifest (invisibly).
#' @export
update_enrichment_manifest <- function(manifest_path, name, vtr_path,
                                       release_version = NULL,
                                       repo = "gcol33/taxifydb") {
  meta_path <- file.path(dirname(vtr_path), "meta.json")
  if (!file.exists(meta_path)) {
    stop(sprintf("No meta.json found at: %s", meta_path))
  }
  meta <- jsonlite::read_json(meta_path, simplifyVector = TRUE)

  if (file.exists(manifest_path)) {
    manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  } else {
    manifest <- list(schema_version = 2L, backends = list(),
                     enrichments = list())
  }
  if (is.null(manifest$enrichments)) manifest$enrichments <- list()

  release_tag_version <- if (is.null(release_version)) {
    meta$version
  } else {
    release_version
  }
  tag <- sprintf("enrichment-%s", release_tag_version)
  base_url <- sprintf(
    "https://github.com/%s/releases/download/%s", repo, tag
  )

  entry <- manifest$enrichments[[name]]
  if (is.null(entry)) entry <- list()

  entry$latest   <- meta$version
  entry$full_url <- sprintf("%s/%s.vtr", base_url, name)
  entry$nrow     <- meta$nrow

  if (!is.null(meta$source_url)) entry$source_url <- meta$source_url
  if (!is.null(meta$source_doi)) entry$source_doi <- meta$source_doi
  if (!is.null(meta$license))    entry$license    <- meta$license

  if (!is.null(meta$available_groups)) {
    entry$available_groups <- meta$available_groups
  }

  manifest$enrichments[[name]] <- entry

  jsonlite::write_json(manifest, manifest_path, pretty = TRUE,
                       auto_unbox = TRUE)

  message(sprintf("Manifest enrichment updated: %s v%s", name, meta$version))
  invisible(manifest)
}
