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
#' @param old_path Character. Path to the previous-version .vtr.
#' @param new_path Character. Path to the new-version .vtr.
#' @param delta_path Character. Output path for the .xdelta file.
#' @return The delta path (invisibly), or `NULL` if xdelta3 is unavailable.
#' @export
create_delta <- function(old_path, new_path, delta_path) {
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
