# xdelta3 binary diff for backbone updates.
#
# Produces compact binary diffs between .vtr versions. Users with xdelta3
# installed download the small patch instead of the full .vtr.

#' Check if xdelta3 is available on PATH
#'
#' @return Logical.
#' @export
has_xdelta3 <- function() {
  tryCatch(
    {
      out <- system2("xdelta3", "-V", stdout = TRUE, stderr = TRUE)
      length(out) > 0L
    },
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
}


#' Create a binary diff between two .vtr files
#'
#' Besides the patch, writes `<delta_path>.base`: one line holding the content
#' id (md5) of `old_path`. A patch applies only to the exact bytes it was cut
#' against, and a release tag does not identify those bytes (a re-cut reuses
#' it), so the manifest records this id as `delta_from_content_id` and the
#' runtime patches only a local build carrying it.
#'
#' @param old_path Character. Path to the previous-version .vtr.
#' @param new_path Character. Path to the new-version .vtr.
#' @param delta_path Character. Output path for the .xdelta file.
#' @return The delta path (invisibly), or `NULL` if xdelta3 is unavailable.
#' @export
create_delta <- function(old_path, new_path, delta_path) {
  base_path <- delta_base_path(delta_path)
  if (file.exists(base_path)) unlink(base_path)

  if (!has_xdelta3()) {
    message("xdelta3 not found on PATH. Skipping delta creation.")
    return(invisible(NULL))
  }

  if (!file.exists(old_path)) {
    message("No previous version found. Skipping delta creation.")
    return(invisible(NULL))
  }

  dir.create(dirname(delta_path), recursive = TRUE, showWarnings = FALSE)

  status <- system2("xdelta3", c("-e", "-s", old_path, new_path, delta_path))

  if (status != 0L) {
    warning("xdelta3 failed with exit code ", status)
    return(invisible(NULL))
  }

  writeLines(unname(tools::md5sum(old_path)), base_path)

  old_size <- file.size(old_path)
  new_size <- file.size(new_path)
  delta_size <- file.size(delta_path)
  ratio <- delta_size / new_size * 100

  message(sprintf(
    "Delta: %.1f MB -> %.1f MB (patch: %.1f MB, %.0f%% of full)",
    old_size / 1048576, new_size / 1048576, delta_size / 1048576, ratio
  ))

  invisible(delta_path)
}


#' Path of the sidecar recording which build a delta was cut against
#' @noRd
delta_base_path <- function(delta_path) paste0(delta_path, ".base")


#' Content id of the build a delta was cut against, or `NULL`
#' @noRd
read_delta_base <- function(delta_path) {
  if (is.null(delta_path)) return(NULL)
  base_path <- delta_base_path(delta_path)
  if (!file.exists(base_path)) return(NULL)
  id <- trimws(readLines(base_path, n = 1L, warn = FALSE))
  if (length(id) != 1L || !grepl("^[0-9a-f]{32}$", id)) return(NULL)
  id
}


#' Apply a binary delta to produce a new .vtr
#'
#' @param old_path Character. Path to the current local .vtr.
#' @param delta_path Character. Path to the downloaded .xdelta file.
#' @param new_path Character. Output path for the patched .vtr.
#' @return The new path (invisibly), or `NULL` on failure.
#' @export
apply_delta <- function(old_path, delta_path, new_path) {
  if (!has_xdelta3()) {
    message("xdelta3 not found. Cannot apply delta.")
    return(invisible(NULL))
  }

  status <- system2("xdelta3", c("-d", "-s", old_path, delta_path, new_path))

  if (status != 0L) {
    warning("xdelta3 patch failed with exit code ", status)
    return(invisible(NULL))
  }

  invisible(new_path)
}
