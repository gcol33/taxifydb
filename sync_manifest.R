# ---- Sync enrichment metadata from built output to taxify inst/manifest.json ----
#
# After building enrichments with build_enrichments.R, run this script to
# update the taxify package's manifest with nrow and available_groups from
# the built meta.json sidecars.
#
# Usage:
#   Rscript sync_manifest.R [taxify_manifest_path] [output_dir]
#
# Defaults:
#   taxify_manifest_path = ../taxify/inst/manifest.json
#   output_dir           = output/enrichment

args <- commandArgs(trailingOnly = TRUE)
taxify_manifest <- if (length(args) >= 1L) args[1L] else "../taxify/inst/manifest.json"
output_dir      <- if (length(args) >= 2L) args[2L] else "output/enrichment"

if (!file.exists(taxify_manifest)) {
  stop(sprintf("taxify manifest not found: %s", taxify_manifest))
}
if (!dir.exists(output_dir)) {
  stop(sprintf("Output directory not found: %s", output_dir))
}

manifest <- jsonlite::read_json(taxify_manifest, simplifyVector = FALSE)
if (is.null(manifest$enrichments)) {
  stop("No 'enrichments' section in manifest.")
}

# Scan built enrichments for meta.json sidecars
enrichment_dirs <- list.dirs(output_dir, recursive = FALSE, full.names = TRUE)
updated <- character(0L)

for (enr_dir in enrichment_dirs) {
  name <- basename(enr_dir)
  meta_path <- file.path(enr_dir, "meta.json")
  if (!file.exists(meta_path)) next

  meta <- jsonlite::read_json(meta_path, simplifyVector = TRUE)
  entry <- manifest$enrichments[[name]]
  if (is.null(entry)) {
    message(sprintf("  [skip] '%s' not in taxify manifest — add it manually first", name))
    next
  }

  changed <- FALSE

  # Sync nrow
  if (!is.null(meta$nrow) && !identical(entry$nrow, meta$nrow)) {
    entry$nrow <- meta$nrow
    changed <- TRUE
  }

  # Sync available_groups
  if (!is.null(meta$available_groups)) {
    if (!identical(entry$available_groups, as.list(meta$available_groups))) {
      entry$available_groups <- as.list(meta$available_groups)
      changed <- TRUE
    }
  }

  if (changed) {
    manifest$enrichments[[name]] <- entry
    updated <- c(updated, name)
  }
}

if (length(updated) == 0L) {
  message("Nothing to sync — all entries are current.")
} else {
  jsonlite::write_json(manifest, taxify_manifest, pretty = TRUE,
                       auto_unbox = TRUE)
  message(sprintf("Updated %d enrichment(s) in %s:", length(updated),
                  taxify_manifest))
  for (name in updated) {
    message(sprintf("  %s", name))
  }
}
