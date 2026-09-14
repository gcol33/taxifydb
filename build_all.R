# Backbone build CLI entry point. Thin wrapper around taxifydb::build_backend().
#
# Usage:
#   Rscript build_all.R [backend|all|publish] [output_dir|backend]
#
# Examples:
#   Rscript build_all.R itis output/itis
#   Rscript build_all.R all  output
#   Rscript build_all.R publish itis
#
# A publish releases the build under the source version it recorded in
# output/<backend>/<backend>.meta.

args <- commandArgs(trailingOnly = TRUE)
action <- if (length(args) >= 1L) args[1L] else "all"

# Install/load taxifydb from the local source tree
if (!requireNamespace("taxifydb", quietly = TRUE)) {
  if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".")
  } else {
    stop("Install taxifydb first: devtools::install_local('.')", call. = FALSE)
  }
}

if (action == "all") {
  output_dir <- if (length(args) >= 2L) args[2L] else "output"
  for (be in taxifydb::list_backends()) {
    tryCatch(
      taxifydb::build_backend(be, output_dir = file.path(output_dir, be)),
      error = function(e) {
        message(sprintf("FAILED: %s -- %s", be, conditionMessage(e)))
      }
    )
  }
} else if (action == "publish") {
  be_name <- args[2L]
  a <- taxifydb:::.backbone_artifacts(file.path("output", be_name), be_name)
  version <- taxifydb:::.backbone_release_version(file.path("output", be_name),
                                                  be_name)

  taxifydb::publish_release(
    be_name, version, a$vtr,
    delta_path = a$delta,
    meta_path  = a$meta,
    extras     = a$extras
  )

  taxifydb::update_manifest(
    "manifest/manifest.json", be_name, version, a$vtr,
    delta_path = a$delta,
    delta_from_content_id = a$delta_from_content_id,
    extras     = if (length(a$extras) > 0L) a$extras else NULL
  )
} else {
  output_dir <- if (length(args) >= 2L) args[2L] else file.path("output", action)
  taxifydb::build_backend(action, output_dir = output_dir)
}
