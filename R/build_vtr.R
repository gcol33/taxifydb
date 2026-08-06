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

  index_backbone_vtr(vtr_path, genus_col)
  write_backbone_meta(vtr_path, backend_name, version, source_url, nrow(df))
  report_built_backbone(vtr_path, backend_name, nrow(df))

  invisible(vtr_path)
}


#' Create the hash indexes every backbone .vtr carries
#'
#' @param vtr_path Character. Path to the written .vtr.
#' @param genus_col Character. Name of the genus column.
#' @return Invisible `NULL`.
#' @noRd
index_backbone_vtr <- function(vtr_path, genus_col = "genus") {
  vectra::create_index(vtr_path, genus_col)
  vectra::create_index(vtr_path, "canonical_name")
  vectra::create_index(vtr_path, "key_ci")
  invisible(NULL)
}


#' Write the .meta sidecar taxify reads next to a backbone .vtr
#'
#' Field names follow the .meta contract taxify reads: download_date /
#' download_timestamp / url (the publish date of the built artifact).
#'
#' @param vtr_path Character. Path to the written .vtr.
#' @param backend_name Character. Backend identifier.
#' @param version Character. Version string.
#' @param source_url Character. URL the source data was downloaded from.
#' @param n_rows Integer. Row count of the built store.
#' @return Invisible path to the .meta file.
#' @noRd
write_backbone_meta <- function(vtr_path, backend_name, version, source_url,
                                n_rows) {
  meta_path <- paste0(tools::file_path_sans_ext(vtr_path), ".meta")
  writeLines(c(
    paste0("backend=", backend_name),
    paste0("version=", version),
    paste0("download_date=", format(Sys.time(), "%Y-%m-%d")),
    paste0("download_timestamp=", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
    paste0("url=", source_url),
    paste0("nrow=", n_rows)
  ), meta_path)
  invisible(meta_path)
}


#' Report a finished backbone build
#'
#' @param vtr_path Character. Path to the written .vtr.
#' @param backend_name Character. Backend identifier.
#' @param n_rows Integer. Row count of the built store.
#' @return Invisible `NULL`.
#' @noRd
report_built_backbone <- function(vtr_path, backend_name, n_rows) {
  message(sprintf(
    "[%s] Built %s: %s rows, %.1f MB",
    backend_name, basename(vtr_path), n_rows,
    file.size(vtr_path) / 1048576
  ))
  invisible(NULL)
}


#' Read a `.meta` sidecar written by `build_vtr()`
#'
#' Parses the `key=value` lines into a named character vector. The sidecar is
#' the authoritative provenance record for a built `.vtr` (source `url`,
#' `version`, `download_date`, `nrow`). Only the first `=` on a line splits the
#' key from the value, so URLs carrying `?download=1` query strings survive.
#'
#' @param meta_path Character. Path to the `.meta` file.
#' @return Named character vector of fields, or `NULL` if the file is absent.
#' @noRd
read_meta <- function(meta_path) {
  if (!file.exists(meta_path)) return(NULL)
  lines <- readLines(meta_path, warn = FALSE)
  lines <- lines[nzchar(lines)]
  eq <- regexpr("=", lines, fixed = TRUE)
  keep <- eq > 0L
  vals <- substring(lines[keep], eq[keep] + 1L)
  names(vals) <- substring(lines[keep], 1L, eq[keep] - 1L)
  vals
}


#' Compute SHA-256 checksum of a file
#'
#' @param path Character. Path to the file.
#' @return Character. Hex-encoded SHA-256 hash.
#' @export
sha256 <- function(path) {
  digest::digest(path, algo = "sha256", file = TRUE)
}
