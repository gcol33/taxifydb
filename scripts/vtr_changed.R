# CLI wrapper over taxifydb::vtr_changed(), called by both build workflows to
# decide whether a release is warranted. Prints "true" or "false" for a step
# output; the decision itself lives in the package, where it is tested.
#
# Usage:
#   Rscript scripts/vtr_changed.R <manifest> <backend> <vtr_path>

args <- commandArgs(trailingOnly = TRUE)

backend  <- args[[2L]]
vtr_path <- args[[3L]]
version  <- taxifydb:::.backbone_release_version(dirname(vtr_path), backend)

changed <- taxifydb::vtr_changed(
  manifest_path = args[[1L]],
  backend_name  = backend,
  vtr_path      = vtr_path,
  version       = version
)

message(sprintf("%s %s: %s", backend, version,
                if (changed) "release warranted (new bytes or new version)"
                else "identical to the published asset under the same version"))

cat(if (changed) "true" else "false")
