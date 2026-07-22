# Sync enrichment metadata from built output to taxify's bundled manifest.json.
#
# After running build_enrichments.R, this script propagates nrow,
# available_groups, and group_col from each enrichment's meta.json sidecar
# into the taxify package's inst/manifest.json.
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

enrichment_dirs <- list.dirs(output_dir, recursive = FALSE, full.names = TRUE)
updated <- character(0L)

for (enr_dir in enrichment_dirs) {
  name <- basename(enr_dir)
  meta_path <- file.path(enr_dir, "meta.json")
  if (!file.exists(meta_path)) next

  meta <- jsonlite::read_json(meta_path, simplifyVector = TRUE)
  entry <- manifest$enrichments[[name]]
  if (is.null(entry)) {
    message(sprintf("  [skip] '%s' not in taxify manifest -- add it manually first",
                    name))
    next
  }

  changed <- FALSE

  if (!is.null(meta$nrow) && !identical(entry$nrow, meta$nrow)) {
    entry$nrow <- meta$nrow
    changed <- TRUE
  }

  # Content id (md5 of the built .vtr): drives taxify's offline static-cache
  # refresh gate, so a rebuilt asset re-released under the same tag no longer
  # leaves existing caches stale.
  if (!is.null(meta$content_id) && !identical(entry$content_id, meta$content_id)) {
    entry$content_id <- meta$content_id
    changed <- TRUE
  }

  if (!is.null(meta$available_groups)) {
    if (!identical(entry$available_groups, as.list(meta$available_groups))) {
      entry$available_groups <- as.list(meta$available_groups)
      changed <- TRUE
    }
  }

  if (!is.null(meta$group_col) && !identical(entry$group_col, meta$group_col)) {
    entry$group_col <- meta$group_col
    changed <- TRUE
  }

  if (changed) {
    manifest$enrichments[[name]] <- entry
    updated <- c(updated, name)
  }
}

if (length(updated) == 0L) {
  message("Nothing to sync -- all entries are current.")
} else {
  jsonlite::write_json(manifest, taxify_manifest, pretty = TRUE,
                       auto_unbox = TRUE)
  message(sprintf("Updated %d enrichment(s) in %s:",
                  length(updated), taxify_manifest))
  for (name in updated) {
    message(sprintf("  %s", name))
  }
}
