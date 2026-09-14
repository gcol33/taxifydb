#!/usr/bin/env Rscript
#
# Publish one freshly built backbone to its <backend>-<version> release.
#
# The release goes through taxifydb::publish_release(), which uploads the
# rolling assets and the content-addressed <backend>-<content_id>.vtr copy that
# update_manifest() records as content_url. The artifact set and the version are
# read from the output directory by the same helper
# scripts/update_manifest_entry.R uses, so the release carries exactly the files
# its manifest entry describes, under the source release the build recorded.
#
# Usage:
#   Rscript scripts/publish_backbone_release.R <backend> <output_dir> [repo]

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) {
  stop("Usage: publish_backbone_release.R <backend> <output_dir> [repo]",
       call. = FALSE)
}

backend    <- args[[1L]]
output_dir <- args[[2L]]
repo       <- if (length(args) >= 3L && nzchar(args[[3L]])) {
  args[[3L]]
} else {
  "gcol33/taxifydb"
}

a <- taxifydb:::.backbone_artifacts(output_dir, backend)

taxifydb::publish_release(
  backend, taxifydb:::.backbone_release_version(output_dir, backend), a$vtr,
  delta_path = a$delta,
  meta_path  = a$meta,
  extras     = a$extras,
  repo       = repo
)
