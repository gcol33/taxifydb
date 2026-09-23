# Re-cut every enrichment against the gbif-2023.08 name lookup (taxify 0.6.0
# pick: occurrence records, then priority of publication). Build only -- the
# publishing step compares each build with the published asset and runs
# separately, so an unattended run never uploads.
#
# Resumable (per-name done/failed markers), single-instance locked, logged.

`%||%` <- function(a, b) if (is.null(a)) b else a
repo <- "C:/Users/GillesC/Documents/dev/taxifydb"
suppressMessages(devtools::load_all(repo, quiet = TRUE))

run_dir <- file.path(repo, "output", "recut_2023_08")
out_dir <- file.path(run_dir, "built")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
done_f <- file.path(run_dir, "done.txt")
fail_f <- file.path(run_dir, "failed.txt")
log_f <- file.path(run_dir, "recut.log")

lock_d <- file.path(run_dir, "LOCK.d")
if (dir.exists(lock_d) &&
    difftime(Sys.time(), file.info(lock_d)$mtime, units = "hours") > 6) {
  unlink(lock_d, recursive = TRUE)
}
if (!dir.create(lock_d, showWarnings = FALSE)) {
  cat("Another recut instance holds the lock; exiting.\n")
  quit(save = "no", status = 0)
}
writeLines(as.character(Sys.getpid()), file.path(lock_d, "pid"))
on.exit(unlink(lock_d, recursive = TRUE), add = TRUE)

logln <- function(...) {
  msg <- sprintf("[%s] %s", format(Sys.time(), "%H:%M:%S"),
                 paste0(..., collapse = ""))
  cat(msg, "\n")
  cat(msg, "\n", file = log_f, append = TRUE)
}

done <- if (file.exists(done_f)) trimws(readLines(done_f)) else character(0)
done <- done[nzchar(done)]
all_names <- list_enrichments()
# Live-API and very large sources last, so the bulk lands first.
heavy <- c("bien", "fishbase", "sealifebase", "gift", "common_names",
           "austraits", "wcvp", "griis", "glonaf")
todo <- setdiff(all_names, done)
todo <- c(setdiff(todo, heavy), intersect(heavy, todo))
logln(sprintf("Re-cut start: %d enrichments, %d done, %d to do.",
              length(all_names), length(done), length(todo)))

for (nm in todo) {
  logln(sprintf("=== %s ===", nm))
  tryCatch({
    d <- file.path(out_dir, nm)
    unlink(d, recursive = TRUE)
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    p <- build_enrichment(nm, output_dir = d, verbose = FALSE)
    logln(sprintf("OK %s: %d rows, md5 %s", nm, count_vtr_rows(p),
                  unname(tools::md5sum(p))))
    cat(nm, "\n", sep = "", file = done_f, append = TRUE)
  }, error = function(e) {
    logln(sprintf("FAIL %s: %s", nm, conditionMessage(e)))
    cat(sprintf("%s\t%s\n", nm, conditionMessage(e)), file = fail_f,
        append = TRUE)
  })
}

done2 <- unique(if (file.exists(done_f)) readLines(done_f) else character(0))
fail2 <- if (file.exists(fail_f)) readLines(fail_f) else character(0)
logln(sprintf("Re-cut DONE. %d/%d built, %d failed.", length(done2),
              length(all_names), length(fail2)))
writeLines(format(Sys.time()), file.path(run_dir, "COMPLETE.marker"))
