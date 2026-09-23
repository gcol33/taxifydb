# Rebuild the genus register and backend coverage against the installed
# backbones, including the gbif-2023.08 build. Build only: publishing the two
# releases and recording them in both manifests runs separately.

repo <- "C:/Users/GillesC/Documents/dev/taxifydb"
suppressMessages(devtools::load_all(repo, quiet = TRUE))
setwd(repo)
version <- "2026.09"
run_dir <- file.path(repo, "output", "register_2026_09")
dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
unlink(file.path(run_dir, "COMPLETE.marker"))

dd <- taxify:::taxify_data_dir()
needed <- register_backbones()
paths <- stats::setNames(character(0), character(0))
for (nm in needed) {
  p <- if (nm == "col") file.path(repo, "output/col/col.vtr")
       else if (nm == "lcvp") file.path(repo, "output/lcvp/lcvp.vtr")
       else file.path(dd, nm, "latest", paste0(nm, ".vtr"))
  paths[nm] <- normalizePath(p, winslash = "/", mustWork = FALSE)
}
cat("=== register inputs ===\n")
missing <- character(0)
for (nm in names(paths)) {
  ok <- file.exists(paths[[nm]])
  if (!ok) missing <- c(missing, nm)
  cat(sprintf("  %-18s %-7s %8.1f MB  %s\n", nm, if (ok) "ok" else "MISSING",
              if (ok) file.size(paths[[nm]]) / 1048576 else 0,
              if (ok) format(file.info(paths[[nm]])$mtime, "%m-%d %H:%M") else "-"))
}
if (length(missing)) stop("missing backbones: ", paste(missing, collapse = ", "))

res <- build_register(backbone_paths = paths, version = version,
                      manifest_path = file.path(repo, "manifest/manifest.json"),
                      verbose = TRUE)
d <- vectra::collect(vectra::tbl(res$register))
cat("\n=== register ===\n  rows:", format(nrow(d), big.mark = ","), "\n")
for (cl in c("kingdom", "phylum", "class", "order", "family", "kingdom_group",
             "taxon_group", "life_form")) {
  if (cl %in% names(d)) {
    cat(sprintf("  %-14s %6.2f%% populated\n", cl,
                100 * mean(!is.na(d[[cl]]) & nzchar(as.character(d[[cl]])))))
  }
}
cv <- vectra::collect(vectra::tbl(res$coverage))
cat("\n=== coverage ===\n  rows:", format(nrow(cv), big.mark = ","),
    " backbones:", length(unique(cv$backend)), "\n")
saveRDS(res, file.path(run_dir, "paths.rds"))
writeLines(format(Sys.time()), file.path(run_dir, "COMPLETE.marker"))
