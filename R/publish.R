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

  # Quote each path: an artifact path may contain spaces (a Windows user
  # directory does, and the backbone .vtr is read from the data dir), and
  # system2() does not quote its args. Use cmd-style quoting on Windows
  # (double quotes, understood by CreateProcess) and sh-style elsewhere.
  quoted_artifacts <- shQuote(
    artifacts,
    type = if (.Platform$OS.type == "windows") "cmd" else "sh"
  )
  upload_out <- system2("gh", c(
    "release", "upload", tag,
    quoted_artifacts,
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


#' Validate a provenance URL before it reaches the manifest
#'
#' `source_url` is the link a reader follows from a manifest entry back to the
#' original data, so a value with no resolvable host ships a dead link wearing
#' the look of real provenance. Every reachable host carries a dot inside it,
#' which is what separates a source from a stand-in. A field holding several
#' sources separated by `;` is checked piece by piece, and a value that is not
#' a URL at all (the register's `derived from: ...`) is left alone.
#'
#' @param url Character or NULL. The candidate `source_url`.
#' @param what Character. Entry name, used in the error message.
#' @return `url`, unchanged.
#' @noRd
check_source_url <- function(url, what) {
  if (is.null(url) || !nzchar(url)) return(url)

  parts <- trimws(strsplit(url, ";", fixed = TRUE)[[1L]])
  urls  <- parts[grepl("^https?://", parts)]
  hosts <- sub("[/?#].*$", "", sub("^https?://", "", urls))
  bad   <- urls[!grepl(".\\..", hosts)]

  if (length(bad) > 0L) {
    stop(sprintf("%s: source_url has no resolvable host: %s",
                 what, paste(bad, collapse = ", ")), call. = FALSE)
  }
  url
}


#' Update manifest.json after a successful backbone build
#'
#' The entry records two versions where they differ: `latest` is the release
#' tag the download URL points at (stamped `YYYY.MM` by the build cycle) and
#' `source_version` is the version the upstream dataset gives itself, read from
#' the `.meta` sidecar. Rolling sources carry only `latest`, having no version
#' of their own.
#'
#' @param manifest_path Character. Path to manifest.json.
#' @param backend_name Character.
#' @param version Character.
#' @param vtr_path Character.
#' @param delta_path Character or NULL.
#' @param delta_from Character or NULL. Previous version the delta is from.
#' @param extras Character vector or NULL. Paths to sidecar artifacts uploaded
#'   with the release. Recorded as `extras: [{name, url, size, sha256}]` in the
#'   manifest entry; the runtime downloader fetches each into the same
#'   versioned directory as the main `.vtr`. `NULL` (the default) leaves an
#'   existing `extras` block untouched, since a sidecar is published on its own
#'   release cadence and its recorded URL names the tag it came from, not this
#'   one. Pass `character(0)` to remove the block.
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
                            extras = NULL,
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

  # The .meta sidecar build_vtr() writes next to every .vtr is the backend's
  # single source of truth for provenance: the URL the data was downloaded from
  # and the version the source calls itself. An explicit source_url argument
  # overrides the recorded one.
  meta <- read_meta(paste0(tools::file_path_sans_ext(vtr_path), ".meta"))

  if (is.null(source_url) && !is.null(meta) &&
      "url" %in% names(meta) && nzchar(meta[["url"]])) {
    source_url <- meta[["url"]]
  }
  if (!is.null(source_url) && nzchar(source_url)) {
    entry$source_url <- check_source_url(source_url, backend_name)
  }

  # `latest` is the release tag, stamped YYYY.MM by the build cycle, so it
  # records when a build ran rather than which upstream release it read. Where
  # the source names its own version (OTT 3.7.3, LCVP 3.0.1, MDD 2.5) that
  # identity is otherwise lost the moment the tag is cut, so it is recorded
  # alongside -- the same split the enrichment manifest makes between `latest`
  # and `source_version`. A rolling source (ITIS, NCBI, WoRMS) has no version
  # of its own and stamps the build date, which the tag already carries, so
  # nothing is recorded for it.
  entry$source_version <- NULL
  if (!is.null(meta) && "version" %in% names(meta) &&
      nzchar(meta[["version"]]) && !identical(meta[["version"]], version)) {
    entry$source_version <- meta[["version"]]
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

  # A stale delta is dropped above because its URL names this release's tag and
  # would 404 against it. A sidecar is the other way round: it records the tag
  # it was published under, so it survives a release that does not carry one.
  # The manifest is the only record of that URL, and rewriting the entry
  # without it is how a sidecar stops being downloaded at all.
  if (!is.null(extras)) {
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
  }

  manifest$backends[[backend_name]] <- drop_empty_fields(entry)

  write_json_lf(manifest, manifest_path, pretty = TRUE, auto_unbox = TRUE)

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
#'   already carry so hand-curated values are preserved. Two fields are
#'   exceptions, both because the stored value describes the source rather than
#'   merely accompanying it. `citation` is rewritten when the build has moved to
#'   a different `source_url` or `source_doi`, since the stored text names the
#'   source it was written for; a stored citation that already names the entry's
#'   `source_doi` is kept, as it names the same work the new URL serves.
#'   `trait_cols` is rewritten when it names a column the build no longer
#'   produces, since it then describes a file that does not exist; one naming
#'   only columns still built is kept, curation and all. Used when
#'   writing
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

  # A citation names the source it was written for, so it cannot outlive a move
  # to a different one: ThermoFresh's runtime entry went on citing the record
  # deposited before peer review after the build had moved to the published one.
  # Curated text survives everything here except a change of source, which is
  # the one case where keeping it means publishing a citation to data nobody
  # read.
  # A citation that already names the DOI the entry cites still names the right
  # work, whatever URL this build read it from. A versioned deposit moves its
  # URL on every release -- GloNAF's Zenodo concept cut record 17105725 over
  # 13235357 while the data paper being cited stayed put -- and overwriting
  # there replaces curated text, often a structured block carrying authors,
  # journal and DOI, with the registry's one-line attribution.
  cited_doi <- if (is.list(entry$citation)) entry$citation$doi else NULL
  cites_same_work <- !is.null(cited_doi) &&
    identical(cited_doi, meta$source_doi %||% entry$source_doi %||% NULL)

  source_moved <- (!identical(entry$source_url %||% NULL, meta$source_url) ||
    !identical(entry$source_doi %||% NULL, meta$source_doi)) &&
    !cites_same_work

  if (!is.null(meta$source_url)) {
    entry$source_url <- check_source_url(meta$source_url, name)
  }
  if (!is.null(meta$source_doi)) entry$source_doi <- meta$source_doi
  # The identity the source host gave the version this build read. The weekly
  # freshness check compares it against what the host offers now, which is the
  # only comparison that answers whether the enrichment has gone stale.
  if (!is.null(meta$upstream_id)) entry$upstream_id <- meta$upstream_id
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
    # A stored trait_cols is a promise about which columns the .vtr carries.
    # Curation may narrow or reorder that list, so it is kept whole while every
    # column it names is still built; once the build stops producing one the
    # list describes a file that no longer exists and is taken from the build
    # instead. Whether the group column belongs in the list is a per-entry
    # choice (some carry it, some do not) and is preserved across the rewrite.
    # Without this a source that renames its columns leaves the runtime
    # advertising the old ones: v4.0 of the first-record database restructured
    # every field, and the entry went on naming thirteen columns the built
    # .vtr no longer had while hiding the nine it did.
    if (!is.null(meta$trait_cols)) {
      built  <- as.character(unlist(meta$trait_cols))
      stored <- as.character(unlist(entry$trait_cols))
      valid  <- c(built, as.character(meta$group_col))
      if (is.null(entry$trait_cols) || !all(stored %in% valid)) {
        keep_group <- length(stored) > 0L && !is.null(meta$group_col) &&
          as.character(meta$group_col) %in% stored
        entry$trait_cols <- as.list(
          c(if (keep_group) as.character(meta$group_col), built))
      }
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
    if (is.null(entry$citation) || source_moved) {
      cit <- if (!is.null(meta$citation)) meta$citation else meta$attribution
      if (!is.null(cit) && !is.na(cit)) entry$citation <- cit
    }
  }

  manifest$enrichments[[name]] <- drop_empty_fields(entry)

  write_json_lf(manifest, manifest_path, pretty = TRUE, auto_unbox = TRUE)

  message(sprintf("Manifest enrichment updated: %s v%s", name, meta$version))
  invisible(manifest)
}


#' Did a build produce bytes the manifest is not already pointing at?
#'
#' The version a build stamps is `date +%Y.%m`, which records when it ran rather
#' than what it read. Several backbones read a pinned source and rebuild to the
#' same bytes every time -- Euro+Med from a frozen snapshot release, WFO from a
#' fixed Zenodo record, COL from the pinned annual archive. Releasing those again
#' mints a version whose only difference is its name, and taxify's runtime treats
#' a fresh version as reason to refetch, so every user downloads a file they
#' already hold.
#'
#' Fails open: anything that cannot be determined -- no manifest, no entry for
#' this backbone, no recorded hash, a first-ever build -- counts as changed, so
#' an uncertain case still publishes.
#'
#' @param manifest_path Character. Path to `manifest.json`.
#' @param backend_name Character. Backend identifier.
#' @param vtr_path Character. Path to the freshly built `.vtr`.
#' @return Logical scalar. `TRUE` when the build differs from the published
#'   asset and a release is warranted.
#' @export
vtr_changed <- function(manifest_path, backend_name, vtr_path) {
  if (!file.exists(manifest_path) || !file.exists(vtr_path)) return(TRUE)

  entry <- tryCatch(
    jsonlite::read_json(manifest_path, simplifyVector = FALSE)$backends[[backend_name]],
    error = function(e) NULL
  )
  published <- entry$full_sha256
  if (is.null(published) || !nzchar(published)) return(TRUE)

  !identical(as.character(published), sha256(vtr_path))
}
