# Enrichment build CLI entry point. Thin wrapper around taxifydb::build_enrichment().
#
# Usage:
#   Rscript build_enrichments.R [name|all|list] [output_dir] [release_version]
#
# Examples:
#   Rscript build_enrichments.R woodiness output/enrichment/woodiness
#   Rscript build_enrichments.R all       output/enrichment
#   Rscript build_enrichments.R all       output/enrichment 2026.06
#   Rscript build_enrichments.R list
#
# release_version sets the rolling enrichment release tag (enrichment-<v>) the
# manifest URLs point at; omit it only when every dataset's own version equals
# the release version.

args <- commandArgs(trailingOnly = TRUE)
action <- if (length(args) >= 1L) args[1L] else "all"
output_dir <- if (length(args) >= 2L) args[2L] else "output/enrichment"
release_version <- if (length(args) >= 3L) args[3L] else NULL

if (!requireNamespace("taxifydb", quietly = TRUE)) {
  if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".")
  } else {
    stop("Install taxifydb first: devtools::install_local('.')", call. = FALSE)
  }
}

if (action == "list") {
  cat("Available enrichments:\n")
  for (name in taxifydb::list_enrichments()) {
    cat(sprintf("  %s\n", name))
  }
} else if (action == "all") {
  results <- list()
  for (name in taxifydb::list_enrichments()) {
    enr_out <- file.path(output_dir, name)
    dir.create(enr_out, recursive = TRUE, showWarnings = FALSE)
    tryCatch(
      {
        results[[name]] <- taxifydb::build_enrichment(name, output_dir = enr_out)
        manifest_path <- "manifest/manifest.json"
        if (file.exists(manifest_path) && !is.null(results[[name]])) {
          tryCatch(
            taxifydb::update_enrichment_manifest(manifest_path, name,
                                                 results[[name]],
                                                 release_version = release_version),
            error = function(e) {
              message(sprintf("Warning: manifest update failed for %s: %s",
                              name, conditionMessage(e)))
            }
          )
        }
      },
      error = function(e) {
        message(sprintf("FAILED: %s -- %s", name, conditionMessage(e)))
        results[[name]] <<- NULL
      }
    )
  }

  message("\n=== Enrichment build summary ===\n")
  for (name in names(results)) {
    status <- if (!is.null(results[[name]])) "OK" else "FAILED"
    message(sprintf("  %-25s %s", name, status))
  }
} else {
  enr_out <- file.path(output_dir, action)
  dir.create(enr_out, recursive = TRUE, showWarnings = FALSE)
  vtr_path <- taxifydb::build_enrichment(action, output_dir = enr_out)
  manifest_path <- "manifest/manifest.json"
  if (file.exists(manifest_path) && !is.null(vtr_path)) {
    taxifydb::update_enrichment_manifest(manifest_path, action, vtr_path,
                                         release_version = release_version)
  }
}
