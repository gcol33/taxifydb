# Re-encode every published backbone and enrichment in vectra 0.14.0's TDCF
# format, upload the stores beside the legacy assets under the same tags, and
# write taxify's inst/manifest_v3.json and rewrite taxifydb's manifest. Resumes from work/done/.
#
#   Rscript scripts/republish_tdcf_2026_10.R [entry ...]
#
# Entry names restrict the run (and skip the manifest write).

work <- "E:/research/projects/taxifydb/republish_tdcf_2026_10"
.libPaths(c(file.path(work, "vlib"), .libPaths()))
repo <- "C:/GillesC/Documents/dev/taxifydb"
rt <- "C:/GillesC/Documents/dev/taxify/inst"
marker <- file.path(work, "done.txt")
unlink(marker)

tryCatch({
  suppressMessages(devtools::load_all(repo, quiet = TRUE))
  stopifnot(packageVersion("vectra") >= "0.14.0")
  message("vectra ", packageVersion("vectra"), ", started ", Sys.time())
  only <- commandArgs(trailingOnly = TRUE)
  # taxify < 0.7.0 fetches inst/manifest.json, so taxify's legacy manifest stays;
  # no released client reads taxifydb's, which is rewritten in place.
  manifests <- c(file.path(rt, "manifest_v3.json"),
                 file.path(repo, "manifest", "manifest.json"))
  names(manifests) <- c(file.path(rt, "manifest.json"),
                        file.path(repo, "manifest", "manifest.json"))
  republish_store_format(
    manifests, file.path(work, "work"),
    cache_dir = "C:/Users/GillesC/AppData/Roaming/R/data/R/taxify",
    only = if (length(only)) only)
  message("finished ", Sys.time())
  writeLines(paste("ok", format(Sys.time())), marker)
}, error = function(e) {
  message("FAILED: ", conditionMessage(e))
  writeLines(paste("failed", format(Sys.time()), conditionMessage(e)), marker)
})
