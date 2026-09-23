# Publish the rebuilt genus register and backend coverage, and record both in
# the build-side and runtime manifests.

repo <- "C:/Users/GillesC/Documents/dev/taxifydb"
suppressMessages(devtools::load_all(repo, quiet = TRUE))
setwd(repo)
version <- "2026.09"
db_manifest <- file.path(repo, "manifest", "manifest.json")
rt_manifest <- "C:/Users/GillesC/Documents/dev/taxify/inst/manifest.json"

notes <- paste0(
  "Genus register and backend coverage over the nineteen backbones, rebuilt ",
  format(Sys.Date()), " against the current builds, including the GBIF ",
  "backbone gbif-2023.08 (DOUBTFUL status kept, per-key occurrence counts).")

for (nm in c("genus_register", "backend_coverage")) {
  out <- file.path(repo, "output", nm)
  vtr <- file.path(out, paste0(nm, ".vtr"))
  meta <- file.path(out, paste0(nm, ".meta"))
  message("=== ", nm, " ===")
  publish_release(nm, version, vtr,
                  meta_path = if (file.exists(meta)) meta else NULL,
                  notes = notes)
  for (m in c(db_manifest, rt_manifest)) update_manifest(m, nm, version, vtr)
}
cat("published and recorded: genus_register, backend_coverage", version, "\n")
