# CLI wrapper over taxifydb::vtr_changed(), called by both build workflows to
# decide whether a release is warranted. Prints "true" or "false" for a step
# output; the decision itself lives in the package, where it is tested.

args <- commandArgs(trailingOnly = TRUE)

changed <- taxifydb::vtr_changed(
  manifest_path = args[[1L]],
  backend_name  = args[[2L]],
  vtr_path      = args[[3L]]
)

message(sprintf("%s: built .vtr %s the published asset",
                args[[2L]],
                if (changed) "differs from" else "is identical to"))

cat(if (changed) "true" else "false")
