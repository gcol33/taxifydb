# Enrichment build CLI entry point. Thin wrapper around taxifydb::build_enrichment().
#
# Usage:
#   Rscript build_enrichments.R [name|all|list] [output_dir] [release_version]
#   Rscript build_enrichments.R publish <name|all> <version>
#
# Examples:
#   Rscript build_enrichments.R woodiness output/enrichment/woodiness
#   Rscript build_enrichments.R all       output/enrichment
#   Rscript build_enrichments.R all       output/enrichment 2026.06
#   Rscript build_enrichments.R list
#   Rscript build_enrichments.R publish invacost 2026.07
#   Rscript build_enrichments.R publish all       2026.07
#
# release_version sets the rolling enrichment release tag (enrichment-<v>) the
# manifest URLs point at; omit it only when every dataset's own version equals
# the release version.
#
# The `publish` action builds the enrichment(s), uploads the .vtr to the rolling
# enrichment-<version> release, and updates BOTH manifests in one step: this
# repo's build-side manifest/manifest.json and (when ../taxify is checked out
# alongside) taxify's runtime inst/manifest.json. Updating both together is the
# point -- a publish that touches only one leaves the two drifting (issue #20).

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

if (action == "publish") {
  # Rscript build_enrichments.R publish <name|all> <version>
  target  <- if (length(args) >= 2L) args[2L] else stop(
    "publish needs a name or 'all': Rscript build_enrichments.R publish <name|all> <version>")
  version <- if (length(args) >= 3L) args[3L] else stop(
    "publish needs a version: Rscript build_enrichments.R publish <name|all> <version>")

  names <- if (target == "all") taxifydb::list_enrichments() else target
  db_manifest <- "manifest/manifest.json"
  # taxify's runtime manifest, when the two repos are checked out side by side.
  # Written with the same updater as the build-side copy so the mechanical
  # fields (latest, source_version, full_url, nrow, content_id, ...) stay in
  # lockstep; curated runtime fields (trait_cols, citation, static) are merged
  # and preserved. A brand-new enrichment still needs those added once by hand.
  tx_manifest <- "../taxify/inst/manifest.json"

  built <- list()
  for (name in names) {
    enr_out <- file.path(output_dir, name)
    dir.create(enr_out, recursive = TRUE, showWarnings = FALSE)
    vtr <- tryCatch(
      taxifydb::build_enrichment(name, output_dir = enr_out),
      error = function(e) {
        message(sprintf("FAILED to build %s: %s", name, conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(vtr)) built[[name]] <- vtr
  }

  if (length(built) == 0L) stop("Nothing built; nothing to publish.")

  taxifydb::publish_enrichment_release(version, unlist(built, use.names = FALSE))

  for (name in names(built)) {
    taxifydb::update_enrichment_manifest(db_manifest, name, built[[name]],
                                         release_version = version)
    if (file.exists(tx_manifest)) {
      taxifydb::update_enrichment_manifest(tx_manifest, name, built[[name]],
                                           release_version = version)
    }
  }

  if (file.exists(tx_manifest)) {
    message("\nUpdated both manifests. Commit + push each repo:")
    message("  taxifydb: git add manifest/manifest.json")
    message("  taxify:   git add inst/manifest.json")
  } else {
    message("\nUpdated manifest/manifest.json. ../taxify not found -- sync its ",
            "runtime manifest separately (CI does this via a PR).")
  }
} else if (action == "list") {
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
