#!/usr/bin/env Rscript
#
# Publish one freshly built backbone to its <backend>-<version> release.
#
# The release goes through taxifydb::publish_release(), which uploads the
# rolling assets and the content-addressed <backend>-<content_id>.vtr copy that
# update_manifest() records as content_url. The artifact set is read from the
# output directory by the same helper scripts/update_manifest_entry.R uses, so
# the release carries exactly the files its manifest entry describes.
#
# Usage:
#   Rscript scripts/publish_backbone_release.R <backend> <version> \
#           <output_dir> [repo]

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3L) {
  stop("Usage: publish_backbone_release.R <backend> <version> <output_dir> ",
       "[repo]", call. = FALSE)
}

backend    <- args[[1L]]
version    <- args[[2L]]
output_dir <- args[[3L]]
repo       <- if (length(args) >= 4L && nzchar(args[[4L]])) {
  args[[4L]]
} else {
  "gcol33/taxifydb"
}

a <- taxifydb:::.backbone_artifacts(output_dir, backend)

taxifydb::publish_release(
  backend, version, a$vtr,
  delta_path = a$delta,
  meta_path  = a$meta,
  extras     = a$extras,
  repo       = repo
)
