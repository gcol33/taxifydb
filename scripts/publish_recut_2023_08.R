# Publish the enrichments whose re-cut against gbif-2023.08 changed their
# bytes, and record them in both manifests. Reads the builds left by
# scripts/recut_gbif_2023_08.R; an enrichment whose md5 equals the manifest's
# content_id is identical to the published asset and is skipped.
#
# Usage: Rscript scripts/publish_recut_2023_08.R [--dry-run]

repo <- "C:/Users/GillesC/Documents/dev/taxifydb"
suppressMessages(devtools::load_all(repo, quiet = TRUE))

dry <- "--dry-run" %in% commandArgs(trailingOnly = TRUE)
release_version <- "2026.08"
built_dir <- file.path(repo, "output", "recut_2023_08", "built")
db_manifest <- file.path(repo, "manifest", "manifest.json")
rt_manifest <- "C:/Users/GillesC/Documents/dev/taxify/inst/manifest.json"

man <- jsonlite::read_json(db_manifest, simplifyVector = FALSE)
names_built <- list.dirs(built_dir, recursive = FALSE, full.names = FALSE)
changed <- character(0)
same <- character(0)
missing <- character(0)

build_only <- character(0)

for (nm in names_built) {
  p <- file.path(built_dir, nm, paste0(nm, ".vtr"))
  if (!file.exists(p)) { missing <- c(missing, nm); next }
  # A source with no manifest entry publishes no asset (build-only: its licence
  # does not allow redistribution), so it is built locally and never uploaded.
  if (is.null(man$enrichments[[nm]])) { build_only <- c(build_only, nm); next }
  md5 <- unname(tools::md5sum(p))
  old <- man$enrichments[[nm]]$content_id
  if (!is.null(old) && identical(old, md5)) same <- c(same, nm) else
    changed <- c(changed, nm)
}
cat(sprintf("built %d | unchanged %d | changed %d | build-only %d | missing .vtr %d\n",
            length(names_built), length(same), length(changed),
            length(build_only), length(missing)))
cat("changed:", paste(changed, collapse = ", "), "\n")
cat("build-only (not published):", paste(build_only, collapse = ", "), "\n")
if (length(missing)) cat("missing:", paste(missing, collapse = ", "), "\n")
if (dry || length(changed) == 0L) quit(save = "no", status = 0)

for (nm in changed) {
  p <- file.path(built_dir, nm, paste0(nm, ".vtr"))
  message("=== ", nm, " ===")
  publish_enrichment_release(release_version, p)
  update_enrichment_manifest(db_manifest, nm, p,
                             release_version = release_version)
  update_enrichment_manifest(rt_manifest, nm, p,
                             release_version = release_version, runtime = TRUE)
}
cat("published and recorded:", length(changed), "enrichments\n")
