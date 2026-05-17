# Publish the three rebuilt backbones to GitHub Releases and update
# manifest.json. Run AFTER build_three.R succeeds.
#
#   Rscript --vanilla dev_notes/publish_three.R

suppressPackageStartupMessages(devtools::load_all("."))

versions <- list(
  wfo  = "2024-12-r1",   # source WFO 2024-12; r1 = unified-schema rebuild
  col  = "2025-r1",      # source COL 2025 annual; r1 = unified-schema rebuild
  gbif = "2026.05"       # GBIF has no source-side version tag
)

source_urls <- list(
  wfo  = "https://zenodo.org/records/14538251/files/_DwC_backbone_R.zip",
  col  = "https://download.checklistbank.org/col/annual/2025_dwca.zip",
  gbif = "https://hosted-datasets.gbif.org/datasets/backbone/current/simple.txt.gz"
)

# Sidecar artifacts published alongside the main .vtr. The runtime
# downloader pulls these into the same versioned dir.
extras <- list(
  wfo  = character(0L),
  col  = "output/col/col_species_profile.vtr",
  gbif = character(0L)
)

# Update both the taxify-backbones build-side manifest and the taxify
# runtime-side bundled manifest (which is what the downloader fetches via
# raw.githubusercontent.com). update_manifest() merges into existing
# entries, so citation blocks in taxify/inst/manifest.json are preserved.
manifests <- c(
  "manifest/manifest.json",
  "../taxify/inst/manifest.json"
)

for (bb in c("wfo", "col", "gbif")) {
  vtr <- file.path("output", bb, paste0(bb, ".vtr"))
  if (!file.exists(vtr)) stop("missing: ", vtr)
  ver <- versions[[bb]]
  ex  <- extras[[bb]]

  message(sprintf("\n=== Publishing %s v%s ===", bb, ver))
  publish_release(bb, ver, vtr, extras = ex)
  for (mf in manifests) {
    if (!file.exists(mf)) {
      message(sprintf("  (skipping missing manifest: %s)", mf))
      next
    }
    update_manifest(mf, bb, ver, vtr,
                    extras = ex,
                    source_url = source_urls[[bb]])
  }
}

message("\nAll three published. Review manifest/manifest.json and commit.")
