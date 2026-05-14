# Wrapper: load the dev taxify (if not installed), then dispatch to
# build_enrichments.R. Used by launch_enrichment.ps1 so the canonical build
# pipeline can call taxify::taxify() for cross-backbone name resolution
# without requiring a system-wide install.
#
# Usage:  Rscript scripts/run_build.R <enrichment_name> [output_dir]

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop("Usage: Rscript scripts/run_build.R <enrichment_name> [output_dir]",
       call. = FALSE)
}

if (!requireNamespace("taxify", quietly = TRUE)) {
  cat("[run_build] taxify not installed; load_all() from sibling dev dir\n")
  taxify_src <- normalizePath(
    file.path(getwd(), "..", "taxify"),
    mustWork = FALSE
  )
  if (!dir.exists(taxify_src)) {
    stop(sprintf(
      "Cannot find dev taxify at %s. Either install taxify or place the dev repo there.",
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

# Re-dispatch to build_enrichments.R via commandArgs hack
# (simpler: just source it after restoring args)
old_args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath("build_enrichments.R")

# Rscript leaves nothing to override; build_enrichments.R reads its own
# commandArgs(trailingOnly = TRUE), so we patch that namespace function.
local({
  ns <- as.environment("package:base")
  # NB: we cannot patch base::commandArgs reliably across versions.
  # Easiest path: just replicate the dispatch logic here.
})

action <- args[1L]
output_dir <- if (length(args) >= 2L) args[2L] else "output/enrichment"

# Source the build registry and helpers (same as build_enrichments.R)
source("shared/build_enrichment.R")
source("shared/resolve_names.R")
source("shared/publish.R")

# Re-source build_enrichments.R partially: we want the registry but not its
# top-level dispatch. Easiest: read the file and eval up to the registry.
# Cleanest: just inline a thin re-dispatch.
build_script <- "build_enrichments.R"
src <- parse(file = build_script)
# Skip the final 'if (action == "list") ... else if ... else ...' block by
# evaluating only the assignment and helper definitions.
for (expr in src) {
  ch <- as.character(expr[[1L]])
  if (length(ch) >= 1L && ch[1L] == "if") next
  eval(expr, envir = globalenv())
}

# Now dispatch
if (action == "list") {
  message("Available enrichments:\n")
  for (name in names(enrichment_registry)) {
    reg <- enrichment_registry[[name]]
    message(sprintf("  %-25s %s", name, reg$desc))
  }
} else if (action == "all") {
  for (name in names(enrichment_registry)) {
    tryCatch(
      build_one_enrichment(name, output_dir),
      error = function(e) {
        message(sprintf("FAILED: %s -- %s", name, conditionMessage(e)))
      }
    )
  }
} else {
  build_one_enrichment(action, output_dir)
}
