#!/usr/bin/env Rscript
#
# Record one freshly built backbone in a manifest.
#
# Every artifact of a backbone build sits in one output directory under a name
# derived from the backbone, so the workflows pass the directory and the
# artifact set and version are derived from it by the same helpers
# scripts/publish_backbone_release.R uploads from. Three call sites -- the light
# build, the heavy build, and the taxify runtime sync -- record an entry through
# this script, so none of them can disagree with the release about which
# artifacts it carries or which version it is.
#
# Usage:
#   Rscript scripts/update_manifest_entry.R <manifest> <backend> <output_dir> \
#           [delta_from]

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3L) {
  stop("Usage: update_manifest_entry.R <manifest> <backend> <output_dir> ",
       "[delta_from]", call. = FALSE)
}

manifest   <- args[[1L]]
backend    <- args[[2L]]
output_dir <- args[[3L]]
delta_from <- if (length(args) >= 4L && nzchar(args[[4L]])) args[[4L]] else NULL

a <- taxifydb:::.backbone_artifacts(output_dir, backend)

# Passing NULL when the build produced no sidecar leaves whatever the manifest
# already records, since a sidecar keeps the tag it was published under rather
# than this release's.
taxifydb::update_manifest(
  manifest, backend, taxifydb:::.backbone_release_version(output_dir, backend),
  a$vtr,
  delta_path = a$delta,
  delta_from = delta_from,
  extras     = if (length(a$extras) > 0L) a$extras else NULL
)

if (length(a$extras) > 0L) {
  message("Sidecars recorded: ", paste(basename(a$extras), collapse = ", "))
}
