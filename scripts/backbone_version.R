#!/usr/bin/env Rscript
#
# Print the release version of one freshly built backbone: the source release
# its build recorded in <backend>.meta. Both build workflows read the release
# tag, the manifest version and the runtime sync version from this one line, so
# none of them can name a build by the month it ran.
#
# Usage:
#   Rscript scripts/backbone_version.R <backend> <output_dir>

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) {
  stop("Usage: backbone_version.R <backend> <output_dir>", call. = FALSE)
}

cat(taxifydb:::.backbone_release_version(args[[2L]], args[[1L]]))
