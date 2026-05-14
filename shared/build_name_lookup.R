# ---- Build per-backbone name-equivalence lookup tables ----
#
# Each backbone .vtr already contains the resolution:
#   row -> (key_ci, accepted_name)
#
# A "name lookup" is just the projection (key_ci, accepted_name) from the
# backbone, deduplicated. This becomes a hash-joinable table that maps any
# input name (case-insensitive, including all synonyms) to the resolved
# accepted name in that backbone.
#
# Joining 1M enrichment names against 7 lookup tables is a sub-second hash
# join, replacing what the per-name-per-backend taxify() loop does in hours.

suppressPackageStartupMessages({
  library(vectra)
})

#' Build a name-lookup .vtr from a backbone .vtr
#'
#' @param bb_path Character. Backbone .vtr path.
#' @param out_path Character. Lookup .vtr destination.
#' @param verbose Logical.
#' @return out_path (invisibly).
build_name_lookup <- function(bb_path, out_path, verbose = TRUE) {
  if (!file.exists(bb_path)) {
    stop(sprintf("backbone .vtr not found: %s", bb_path), call. = FALSE)
  }

  if (verbose) message(sprintf("[lookup] reading %s", basename(bb_path)))

  # Materialize only the two columns we need
  bb <- vectra::tbl(bb_path) |>
    vectra::select("key_ci", "accepted_name") |>
    vectra::collect()

  before <- nrow(bb)

  bb <- bb[!is.na(bb$key_ci) & nzchar(bb$key_ci) &
           !is.na(bb$accepted_name) & nzchar(bb$accepted_name), ]
  bb <- bb[!duplicated(bb[, c("key_ci", "accepted_name")]), ]
  bb <- bb[order(bb$key_ci), ]
  rownames(bb) <- NULL

  if (verbose) {
    message(sprintf(
      "[lookup] %s -> %s rows (was %s)",
      basename(bb_path), format(nrow(bb), big.mark = ","),
      format(before, big.mark = ",")
    ))
  }

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  vectra::write_vtr(bb, out_path, batch_size = 50000L)
  vectra::create_index(out_path, "key_ci")

  size_mb <- file.size(out_path) / 1048576
  if (verbose) {
    message(sprintf("[lookup] wrote %s (%.1f MB)", out_path, size_mb))
  }

  invisible(out_path)
}


#' Build name-lookup tables for all installed taxify backbones
#'
#' Locates each backbone in the user's taxify data dir and writes a
#' {backend}_name_lookup.vtr alongside it (in latest/).
#'
#' @param backends Character vector. Default: 7 standard backends.
#' @param overwrite Logical. Rebuild even if the lookup .vtr already exists.
#' @return Character vector of paths to the built lookups.
build_all_name_lookups <- function(backends = c("wfo", "col", "gbif",
                                                 "itis", "ncbi", "ott",
                                                 "worms"),
                                    overwrite = FALSE) {
  data_root <- file.path(Sys.getenv("APPDATA"), "R", "data", "R", "taxify")

  paths <- character()
  for (bb in backends) {
    bb_dir <- file.path(data_root, bb, "latest")
    bb_vtr <- file.path(bb_dir, sprintf("%s.vtr", bb))
    out_vtr <- file.path(bb_dir, sprintf("%s_name_lookup.vtr", bb))

    if (!file.exists(bb_vtr)) {
      message(sprintf("[lookup] SKIP %s (backbone not installed)", bb))
      next
    }

    if (file.exists(out_vtr) && !overwrite) {
      message(sprintf("[lookup] SKIP %s (lookup exists; use overwrite=TRUE)",
                      bb))
      paths <- c(paths, out_vtr)
      next
    }

    t0 <- proc.time()
    build_name_lookup(bb_vtr, out_vtr)
    elapsed <- (proc.time() - t0)["elapsed"]
    message(sprintf("[lookup] %s done in %.1fs\n", bb, elapsed))
    paths <- c(paths, out_vtr)
  }

  paths
}


# ---- CLI entry point ----
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  overwrite <- "--overwrite" %in% args
  build_all_name_lookups(overwrite = overwrite)
}
