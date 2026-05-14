# Backbone build CLI entry point. Thin wrapper around taxifydb::build_backend().
#
# Usage:
#   Rscript build_all.R [backend|all|publish] [output_dir|backend] [version]
#
# Examples:
#   Rscript build_all.R itis output/itis
#   Rscript build_all.R all  output
#   Rscript build_all.R publish itis 2026.05

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
  version <- args[3L]
  be_out <- file.path("output", be_name)
  vtr_path <- file.path(be_out, paste0(be_name, ".vtr"))
  delta_path <- file.path(be_out, paste0(be_name, ".xdelta"))
  meta_path <- paste0(tools::file_path_sans_ext(vtr_path), ".meta")

  if (!file.exists(vtr_path)) {
    stop(sprintf("No .vtr found at %s. Build first.", vtr_path))
  }

  taxifydb::publish_release(
    be_name, version, vtr_path,
    delta_path = if (file.exists(delta_path)) delta_path else NULL,
    meta_path = if (file.exists(meta_path)) meta_path else NULL
  )

  taxifydb::update_manifest(
    "manifest/manifest.json", be_name, version, vtr_path,
    delta_path = if (file.exists(delta_path)) delta_path else NULL
  )
} else {
  output_dir <- if (length(args) >= 2L) args[2L] else file.path("output", action)
  taxifydb::build_backend(action, output_dir = output_dir)
}
