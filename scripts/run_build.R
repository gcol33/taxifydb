# Wrapper: ensure the dev taxify is importable, then delegate to the canonical
# enrichment build CLI (build_enrichments.R). taxifydb::build_enrichment()
# resolves source names against every backbone via taxify::taxify(), so taxify
# must be loadable; this wrapper load_all()s the sibling dev repo when taxify is
# not installed system-wide. Used by scripts/launch_enrichment.ps1.
#
# Usage:  Rscript scripts/run_build.R <enrichment_name|all|list> [output_dir]

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop("Usage: Rscript scripts/run_build.R <enrichment_name|all|list> [output_dir]",
       call. = FALSE)
}

if (!requireNamespace("taxify", quietly = TRUE)) {
  taxify_src <- normalizePath(file.path(getwd(), "..", "taxify"), mustWork = FALSE)
  if (!dir.exists(taxify_src)) {
    stop(sprintf(
      "Cannot find dev taxify at %s. Install taxify or place the dev repo there.",
      taxify_src), call. = FALSE)
  }
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("devtools is required to load_all() the dev taxify package.",
         call. = FALSE)
  }
  cat(sprintf("[run_build] devtools::load_all('%s')\n", taxify_src))
  suppressMessages(devtools::load_all(taxify_src, quiet = TRUE))
  cat(sprintf("[run_build] taxify dev version loaded: %s\n",
              as.character(packageVersion("taxify"))))
}

# Delegate to the canonical CLI. It reads the same commandArgs(trailingOnly =
# TRUE) (action + output_dir), loads taxifydb, builds, and updates the manifest.
build_cli <- file.path(getwd(), "build_enrichments.R")
if (!file.exists(build_cli)) {
  stop(sprintf(
    "build_enrichments.R not found at %s -- run from the taxifydb repo root.",
    build_cli), call. = FALSE)
}
source(build_cli)
