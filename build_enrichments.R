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

# Build-only enrichments carry a citation-only or unstated licence: taxifydb can
# build them on a user's machine, but taxify redistributes no .vtr asset and no
# manifest entry for them (tests assert their absence). They must never be
# published or written to a manifest. The authoritative set lives in taxify;
# the literal is a fallback for when that accessor is unreachable.
.build_only_set <- tryCatch(
  taxify:::.build_only_enrichments(),
  error = function(e) c("ccdb", "gmpd", "plantatt", "bryoatt", "clopla")
)

if (action == "publish") {
  # Rscript build_enrichments.R publish <name|all> <version>
  target  <- if (length(args) >= 2L) args[2L] else stop(
    "publish needs a name or 'all': Rscript build_enrichments.R publish <name|all> <version>")
  version <- if (length(args) >= 3L) args[3L] else stop(
    "publish needs a version: Rscript build_enrichments.R publish <name|all> <version>")

  if (target == "all") {
    names <- setdiff(taxifydb::list_enrichments(), .build_only_set)
  } else {
    # A comma-separated target publishes a named subset in one release upload and
    # one dual-manifest pass. Every name is validated up front: an unknown name,
    # or a build-only (no-redistribution) name, is rejected before any build runs
    # rather than part-way through a multi-name publish.
    names <- trimws(strsplit(target, ",", fixed = TRUE)[[1L]])
    names <- names[nzchar(names)]
    unknown <- setdiff(names, taxifydb::list_enrichments())
    if (length(unknown) > 0L) {
      stop(sprintf("Unknown enrichment(s): %s",
                   paste(unknown, collapse = ", ")), call. = FALSE)
    }
    blocked <- intersect(names, .build_only_set)
    if (length(blocked) > 0L) {
      stop(sprintf(paste0(
        "Build-only enrichment(s) have no redistribution licence and must not ",
        "be published: %s"), paste(blocked, collapse = ", ")), call. = FALSE)
    }
  }
  # The publish action's positional args are <target> <version>; there is no
  # output_dir slot, so the global `output_dir <- args[2L]` above is the target
  # string here, not a directory. Builds and TAXIFYDB_PUBLISH_RESUME reuse must
  # go through the standard enrichment output base -- the same tree that
  # `build_enrichments.R all` writes to -- so resume actually finds prior builds.
  output_dir <- "output/enrichment"
  db_manifest <- "manifest/manifest.json"
  # taxify's runtime manifest, when the two repos are checked out side by side.
  # Written with the same updater (runtime = TRUE) so the mechanical fields
  # (latest, source_version, full_url, nrow, content_id, ...) stay in lockstep
  # with the build-side copy, while the runtime fields (trait_cols, species_col,
  # citation, static, source_format) are populated from the build for a new
  # enrichment and preserved for an existing one -- no manual curation step.
  tx_manifest <- "../taxify/inst/manifest.json"

  # Set TAXIFYDB_PUBLISH_RESUME=1 to reuse an enrichment's already-built .vtr
  # instead of rebuilding it, so a publish run interrupted partway resumes
  # cheaply. Start from an empty output_dir for a clean full rebuild.
  resume <- nzchar(Sys.getenv("TAXIFYDB_PUBLISH_RESUME", ""))
  built <- list()
  for (name in names) {
    enr_out <- file.path(output_dir, name)
    dir.create(enr_out, recursive = TRUE, showWarnings = FALSE)
    existing <- list.files(enr_out, pattern = "\\.vtr$", full.names = TRUE)
    if (resume && length(existing) == 1L) {
      message(sprintf("RESUME: reusing built %s", basename(existing)))
      built[[name]] <- existing
      next
    }
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
                                           release_version = version,
                                           runtime = TRUE)
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
  for (name in setdiff(taxifydb::list_enrichments(), .build_only_set)) {
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
