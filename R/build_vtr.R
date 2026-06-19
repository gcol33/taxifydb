# Shared build pipeline: normalized df -> .vtr backbone.

#' Write a backbone .vtr file from a precomputed data.frame
#'
#' Sorts by genus (for zone-map pruning), writes the .vtr, creates hash indexes,
#' and writes a metadata sidecar.
#'
#' @param df A precomputed backbone data.frame (from `precompute_backbone()`).
#' @param vtr_path Character. Output path for the .vtr file.
#' @param backend_name Character. Backend identifier (e.g., "itis").
#' @param version Character. Version string.
#' @param source_url Character. URL the source data was downloaded from.
#' @param batch_size Integer. Row group size for vectra (default 50000).
#' @return The path to the .vtr file (invisibly).
#' @export
build_vtr <- function(df, vtr_path, backend_name, version, source_url,
                      batch_size = 50000L) {
  genus_col <- if ("genus" %in% names(df)) "genus" else "resolved_genus"
  df <- df[order(df[[genus_col]], na.last = TRUE), ]
  rownames(df) <- NULL

  dir.create(dirname(vtr_path), recursive = TRUE, showWarnings = FALSE)
  vectra::write_vtr(df, vtr_path, batch_size = batch_size)

  vectra::create_index(vtr_path, genus_col)
  vectra::create_index(vtr_path, "canonical_name")
  vectra::create_index(vtr_path, "key_ci")

  # Field names follow the .meta contract taxify reads: download_date /
  # download_timestamp / url (the publish date of the built artifact).
  meta_path <- paste0(tools::file_path_sans_ext(vtr_path), ".meta")
  lines <- c(
    paste0("backend=", backend_name),
    paste0("version=", version),
    paste0("download_date=", format(Sys.time(), "%Y-%m-%d")),
    paste0("download_timestamp=", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
    paste0("url=", source_url),
    paste0("nrow=", nrow(df))
  )
  writeLines(lines, meta_path)

  message(sprintf(
    "[%s] Built %s: %s rows, %.1f MB",
    backend_name, basename(vtr_path), nrow(df),
    file.size(vtr_path) / 1048576
  ))

  invisible(vtr_path)
}


#' Compute SHA-256 checksum of a file
#'
#' @param path Character. Path to the file.
#' @return Character. Hex-encoded SHA-256 hash.
#' @export
sha256 <- function(path) {
  digest::digest(path, algo = "sha256", file = TRUE)
}
