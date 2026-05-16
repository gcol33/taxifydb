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

for (bb in c("wfo", "col", "gbif")) {
  vtr <- file.path("output", bb, paste0(bb, ".vtr"))
  if (!file.exists(vtr)) stop("missing: ", vtr)
  ver <- versions[[bb]]

  message(sprintf("\n=== Publishing %s v%s ===", bb, ver))
  publish_release(bb, ver, vtr)
  update_manifest("manifest/manifest.json", bb, ver, vtr,
                  source_url = source_urls[[bb]])
}

message("\nAll three published. Review manifest/manifest.json and commit.")
