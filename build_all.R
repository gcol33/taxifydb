# ---- Master build script for taxify backbones ----
#
# Usage:
#   Rscript build_all.R [backend] [output_dir]
#
# Examples:
#   Rscript build_all.R itis output/itis
#   Rscript build_all.R ncbi output/ncbi
#   Rscript build_all.R otl  output/otl
#   Rscript build_all.R all  output        # builds all backends
#
# After building, optionally:
#   Rscript build_all.R publish itis 2025.04

args <- commandArgs(trailingOnly = TRUE)
action <- if (length(args) >= 1L) args[1L] else "all"
output_dir <- if (length(args) >= 2L) args[2L] else "output"

# Source shared code
source("shared/normalize.R")
source("shared/precompute.R")
source("shared/build.R")
source("shared/diff.R")
source("shared/publish.R")

# Available backends and their build functions
backend_scripts <- list(
  itis  = "backends/itis/convert.R",
  ncbi  = "backends/ncbi/convert.R",
  otl   = "backends/otl/convert.R",
  worms = "backends/worms/convert.R"
)

backend_builders <- list(
  itis  = function(out) { source("backends/itis/convert.R"); build_itis(out) },
  ncbi  = function(out) { source("backends/ncbi/convert.R"); build_ncbi(out) },
  otl   = function(out) { source("backends/otl/convert.R"); build_otl(out) },
  worms = function(out) { source("backends/worms/convert.R"); build_worms(out) }
)


build_one <- function(backend_name, out_dir) {
  if (!backend_name %in% names(backend_builders)) {
    stop(sprintf("Unknown backend: %s. Available: %s",
                 backend_name, paste(names(backend_builders), collapse = ", ")))
  }

  be_out <- file.path(out_dir, backend_name)
  message(sprintf("\n=== Building %s ===\n", toupper(backend_name)))
  t0 <- proc.time()
  vtr_path <- backend_builders[[backend_name]](be_out)
  elapsed <- (proc.time() - t0)["elapsed"]
  message(sprintf("\n=== %s done in %.0f seconds ===\n",
                  toupper(backend_name), elapsed))

  # Try to compute delta against previous version
  prev_dir <- file.path(out_dir, paste0(backend_name, "_prev"))
  prev_vtr <- file.path(prev_dir, paste0(backend_name, ".vtr"))
  if (file.exists(prev_vtr)) {
    delta_path <- file.path(be_out, paste0(backend_name, ".xdelta"))
    create_delta(prev_vtr, vtr_path, delta_path)
  }

  vtr_path
}


if (action == "all") {
  for (be in names(backend_builders)) {
    tryCatch(
      build_one(be, output_dir),
      error = function(e) {
        message(sprintf("FAILED: %s — %s", be, conditionMessage(e)))
      }
    )
  }
} else if (action == "publish") {
  # Rscript build_all.R publish <backend> <version>
  be_name <- args[2L]
  version <- args[3L]
  be_out <- file.path("output", be_name)
  vtr_path <- file.path(be_out, paste0(be_name, ".vtr"))
  delta_path <- file.path(be_out, paste0(be_name, ".xdelta"))
  meta_path <- paste0(tools::file_path_sans_ext(vtr_path), ".meta")

  if (!file.exists(vtr_path)) {
    stop(sprintf("No .vtr found at %s. Build first.", vtr_path))
  }

  publish_release(
    be_name, version, vtr_path,
    delta_path = if (file.exists(delta_path)) delta_path else NULL,
    meta_path = if (file.exists(meta_path)) meta_path else NULL
  )

  update_manifest(
    "manifest/manifest.json", be_name, version,
    vtr_path,
    delta_path = if (file.exists(delta_path)) delta_path else NULL
  )
} else {
  build_one(action, output_dir)
}
