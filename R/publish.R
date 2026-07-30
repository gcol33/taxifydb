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


#' Publish enrichment `.vtr` files to the rolling enrichment release
#'
#' Every enrichment is published under one shared, rolling release tag
#' (`enrichment-<version>`), unlike backbones which get a tag each. The release
#' is therefore created only when missing and never deleted: assets are uploaded
#' with `--clobber`, which replaces only the same-named file and leaves every
#' other enrichment's `.vtr` in place. Deleting and recreating the tag (as the
#' per-backbone workflow does) would wipe all the other enrichments' assets.
#'
#' @param version Character. Rolling release version (the `enrichment-<version>`
#'   tag).
#' @param vtr_paths Character vector. Paths to the enrichment `.vtr` files to
#'   upload. Basenames become the release asset names.
#' @param repo Character. GitHub repo (e.g. "gcol33/taxifydb").
#' @param notes Character or NULL. Release notes, used only when the release is
#'   created for the first time.
#' @return The release tag (invisibly).
#' @export
publish_enrichment_release <- function(version, vtr_paths,
                                       repo = "gcol33/taxifydb",
                                       notes = NULL) {
  tag <- sprintf("enrichment-%s", version)

  missing <- vtr_paths[!file.exists(vtr_paths)]
  if (length(missing) > 0L) {
    stop("Missing enrichment .vtr files: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  if (is.null(notes)) {
    notes <- sprintf("Enrichment .vtr assets (rolling release %s)", version)
  }

  # Create the shared release only if absent. It is never deleted -- doing so
  # would drop every other enrichment's asset from the tag.
  view_status <- suppressWarnings(system2(
    "gh", c("release", "view", tag, "--repo", repo),
    stdout = FALSE, stderr = FALSE
  ))
  if (view_status != 0L) {
    create_status <- system2("gh", c(
      "release", "create", tag,
      "--repo", repo,
      "--title", shQuote(sprintf("Enrichments %s", version)),
      "--notes", shQuote(notes)
    ))
    if (create_status != 0L) {
      stop(sprintf("gh release create failed for %s (exit %d)",
                   tag, create_status), call. = FALSE)
    }
  }

  # Quote each path: an asset path may contain spaces (e.g. a Windows user
  # directory), and system2() does not quote its args. Use cmd-style quoting on
  # Windows (double quotes, understood by CreateProcess) and sh-style elsewhere.
  quoted_paths <- shQuote(
    vtr_paths,
    type = if (.Platform$OS.type == "windows") "cmd" else "sh"
  )
  upload_out <- system2("gh", c(
    "release", "upload", tag,
    quoted_paths,
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

  message(sprintf("Published enrichment release: %s (%d asset%s)",
                  tag, length(vtr_paths),
                  if (length(vtr_paths) == 1L) "" else "s"))
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
#' @param source_url Character or NULL. Original data source URL. When `NULL`
#'   (the default), it is read from the `url` field of the `.meta` sidecar that
#'   `build_vtr()` writes next to `vtr_path`, which is the backend's single
#'   source of truth for provenance.
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
  # md5 of the .vtr; taxify's runtime uses this (base-R tools::md5sum) as the
  # content id for its same-tag-republish refresh gate on backbones.
  entry$content_id  <- unname(tools::md5sum(vtr_path))
  entry$nrow        <- count_vtr_rows(vtr_path)

  # Provenance defaults to the download URL that build_vtr() records in the
  # .meta sidecar next to every .vtr (the backend's single source of truth).
  # An explicit source_url argument overrides it.
  if (is.null(source_url)) {
    meta <- read_meta(paste0(tools::file_path_sans_ext(vtr_path), ".meta"))
    if (!is.null(meta) && "url" %in% names(meta) && nzchar(meta[["url"]])) {
      source_url <- meta[["url"]]
    }
  }
  if (!is.null(source_url) && nzchar(source_url)) {
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

  manifest$backends[[backend_name]] <- drop_empty_fields(entry)

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
#' rolling release version (matching the `enrichment-<latest>` download tag)
#' while `source_version` holds the dataset's own version (from the meta
#' sidecar). Pass `release_version` to set the release tag explicitly; when
#' omitted it falls back to the dataset version (correct only when the two
#' happen to coincide).
#'
#' @param manifest_path Character. Path to manifest.json.
#' @param name Character. Enrichment identifier.
#' @param vtr_path Character. Path to the built .vtr file.
#' @param release_version Character or NULL. Rolling release version used in the
#'   download URL tag (`enrichment-<release_version>`). Defaults to the dataset
#'   version from the meta sidecar.
#' @param repo Character. GitHub repo for URL construction.
#' @param runtime Logical. When `TRUE`, also populate the runtime-only fields the
#'   taxify door needs (`trait_cols`, `species_col`, `static`, `source_format`,
#'   `citation`) from the meta sidecar, filling only fields the entry does not
#'   already carry so hand-curated values are preserved. Used when writing
#'   taxify's `inst/manifest.json`; left `FALSE` for this repo's lean build-side
#'   manifest. A new enrichment's runtime entry is then complete from the build
#'   alone, with no manual curation step.
#' @return The updated manifest (invisibly).
#' @export
update_enrichment_manifest <- function(manifest_path, name, vtr_path,
                                       release_version = NULL,
                                       repo = "gcol33/taxifydb",
                                       runtime = FALSE) {
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

  # `latest` is the rolling release version (the enrichment-<latest> tag the URL
  # points at); the dataset's own version is recorded separately in
  # `source_version`. taxify's runtime writes the downloaded meta$version from
  # `latest`, so the once-per-session freshness check (meta$version != latest)
  # only fires when a new release is cut -- keeping the two versions distinct.
  entry$latest         <- release_tag_version
  entry$source_version <- meta$version
  entry$full_url       <- sprintf("%s/%s.vtr", base_url, name)
  entry$nrow           <- meta$nrow
  # md5 of the .vtr (from the build sidecar); taxify's runtime uses it to detect
  # a same-tag republish and refresh an otherwise version-locked enrichment cache
  # -- the enrichment analogue of what update_manifest() records for backbones.
  if (!is.null(meta$content_id)) entry$content_id <- meta$content_id

  if (!is.null(meta$source_url)) entry$source_url <- meta$source_url
  if (!is.null(meta$source_doi)) entry$source_doi <- meta$source_doi
  if (!is.null(meta$license))    entry$license    <- meta$license
  if (!is.null(meta$group_col))  entry$group_col  <- meta$group_col

  if (!is.null(meta$available_groups)) {
    entry$available_groups <- meta$available_groups
  }

  # Runtime-only fields (what the taxify door reads). Filled only when absent so
  # a hand-curated value in an existing entry is never overwritten; a brand-new
  # entry gets the complete set straight from the build. trait_cols come from the
  # built .vtr columns, citation from the registry attribution, and static
  # defaults to a frozen snapshot.
  if (isTRUE(runtime)) {
    if (is.null(entry$trait_cols) && !is.null(meta$trait_cols)) {
      entry$trait_cols <- as.list(meta$trait_cols)
    }
    if (is.null(entry$species_col) && !is.null(meta$species_col) &&
        !is.na(meta$species_col)) {
      entry$species_col <- meta$species_col
    }
    if (is.null(entry$static) && !is.null(meta$static)) {
      entry$static <- isTRUE(meta$static)
    }
    if (is.null(entry$source_format) && !is.null(meta$source_format) &&
        !is.na(meta$source_format)) {
      entry$source_format <- meta$source_format
    }
    if (is.null(entry$citation)) {
      cit <- if (!is.null(meta$citation)) meta$citation else meta$attribution
      if (!is.null(cit) && !is.na(cit)) entry$citation <- cit
    }
  }

  manifest$enrichments[[name]] <- drop_empty_fields(entry)

  jsonlite::write_json(manifest, manifest_path, pretty = TRUE,
                       auto_unbox = TRUE)

  message(sprintf("Manifest enrichment updated: %s v%s", name, meta$version))
  invisible(manifest)
}
