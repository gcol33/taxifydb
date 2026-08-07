#!/usr/bin/env Rscript
#
# Record one freshly built backbone in a manifest.
#
# Every artifact of a backbone build sits in one output directory under a name
# derived from the backbone, so the workflows pass the directory and this
# derives the rest. It exists because three call sites -- the light build, the
# heavy build, and the taxify runtime sync -- were each rebuilding those paths
# in their own embedded R one-liner, which is how they came to disagree about
# which artifacts a release carries.
#
# Usage:
#   Rscript scripts/update_manifest_entry.R <manifest> <backend> <version> \
#           <output_dir> [delta_from]

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4L) {
  stop("Usage: update_manifest_entry.R <manifest> <backend> <version> ",
       "<output_dir> [delta_from]", call. = FALSE)
}

manifest   <- args[[1L]]
backend    <- args[[2L]]
version    <- args[[3L]]
output_dir <- args[[4L]]
delta_from <- if (length(args) >= 5L && nzchar(args[[5L]])) args[[5L]] else NULL

vtr   <- file.path(output_dir, paste0(backend, ".vtr"))
delta <- file.path(output_dir, paste0(backend, ".xdelta"))

# Sidecars are named <backend>_<what>.vtr next to the backbone -- the habitat
# flags COL and WoRMS both publish as a species profile. Passing NULL when the
# build produced none leaves whatever the manifest already records, since a
# sidecar keeps the tag it was published under rather than this release's.
extras <- list.files(output_dir, pattern = "_.*\\.vtr$", full.names = TRUE)
extras <- extras[basename(extras) != basename(vtr)]

taxifydb::update_manifest(
  manifest, backend, version, vtr,
  delta_path = if (file.exists(delta)) delta else NULL,
  delta_from = delta_from,
  extras     = if (length(extras) > 0L) extras else NULL
)

if (length(extras) > 0L) {
  message("Sidecars recorded: ", paste(basename(extras), collapse = ", "))
}
