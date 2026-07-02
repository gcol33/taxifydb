# Rebuild every enrichment .vtr from source with the widened parsers, installing
# each into the live taxify data dir so the extra columns materialize for
# add_*() / add_trait(). Resumable (per-name done/failed markers), safe (builds
# to a temp dir, installs only on success, leaves the old .vtr in place on any
# failure), and logged (column count per enrichment so widening is verifiable).
#
# Driven detached by scripts/relaunch_rebuild.ps1 (Scheduled Task) so it
# outlives the launching session.

`%||%` <- function(a, b) if (is.null(a)) b else a
repo    <- "C:/Users/GillesC/Documents/dev/taxifydb"
suppressMessages(devtools::load_all(repo, quiet = TRUE))

data_dir <- taxify::taxify_data_dir()
run_dir  <- file.path(repo, "output", "rebuild_run")
tmp_root <- file.path(run_dir, "tmp")
dir.create(tmp_root, recursive = TRUE, showWarnings = FALSE)
done_f   <- file.path(run_dir, "done.txt")
failed_f <- file.path(run_dir, "failed.txt")
log_f    <- file.path(run_dir, "rebuild.log")

# Single-instance lock: concurrent runs race on the data dir and clobber each
# other's .vtr. dir.create() is atomic -- exactly one caller wins even if the
# task spawns several processes at once; the rest exit. A stale lock (>3h,
# e.g. from a crash) is reclaimed.
lock_d <- file.path(run_dir, "LOCK.d")
if (dir.exists(lock_d) &&
    difftime(Sys.time(), file.info(lock_d)$mtime, units = "hours") > 3) {
  unlink(lock_d, recursive = TRUE)
}
if (!dir.create(lock_d, showWarnings = FALSE)) {
  cat("Another rebuild instance holds the lock; exiting.\n")
  quit(save = "no", status = 0)
}
writeLines(as.character(Sys.getpid()), file.path(lock_d, "pid"))
on.exit(unlink(lock_d, recursive = TRUE), add = TRUE)

logln <- function(...) {
  msg <- sprintf("[%s] %s", format(Sys.time(), "%H:%M:%S"), paste0(..., collapse = ""))
  cat(msg, "\n"); cat(msg, "\n", file = log_f, append = TRUE)
}

done <- if (file.exists(done_f)) trimws(readLines(done_f)) else character(0)
done <- done[nzchar(done)]
names_all <- list_enrichments()
# Heavy / API-bound sources last so the bulk materializes first even if the
# tail is slow (BIEN now fetches every trait; rfishbase/GIFT/common_names are
# live-API; austraits/wcvp/griis/glonaf are large).
heavy <- c("bien", "fishbase", "sealifebase", "gift", "common_names",
           "austraits", "wcvp", "griis", "glonaf")
todo <- setdiff(names_all, done)
todo <- c(setdiff(todo, heavy), intersect(heavy, todo))
logln(sprintf("Rebuild start: %d enrichments, %d already done, %d to do.",
              length(names_all), length(done), length(todo)))

install_latest <- function(built_vtr, name) {
  dest <- file.path(data_dir, "enrichment", name, "latest")
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  # remove stale files for this enrichment, then copy the freshly built ones
  old <- list.files(dest, pattern = paste0("^", name, "\\.vtr"), full.names = TRUE)
  file.remove(old)
  src_dir <- dirname(built_vtr)
  new <- list.files(src_dir, pattern = paste0("^", name, "\\.vtr"), full.names = TRUE)
  file.copy(new, dest, overwrite = TRUE)
  meta <- file.path(src_dir, "meta.json")
  if (file.exists(meta)) {
    # Mark the locally rebuilt .vtr static so the runtime treats it as
    # authoritative: without this, the first add_*() call sees the build's
    # version differ from the manifest's release version and re-downloads the
    # OLD narrow release, clobbering the widened build.
    m <- jsonlite::read_json(meta, simplifyVector = TRUE)
    m$static <- TRUE
    m$widened <- TRUE
    jsonlite::write_json(m, file.path(dest, "meta.json"), pretty = TRUE,
                         auto_unbox = TRUE, null = "null")
  }
  dest
}

for (nm in todo) {
  logln(sprintf("=== %s ===", nm))
  ok <- tryCatch({
    out_dir <- file.path(tmp_root, nm)
    unlink(out_dir, recursive = TRUE)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    p <- build_enrichment(nm, output_dir = out_dir, verbose = FALSE)
    sch <- vectra::tbl(p) |> head(1) |> vectra::collect()
    nrw <- taxifydb::count_vtr_rows(p)
    install_latest(p, nm)
    unlink(out_dir, recursive = TRUE)
    logln(sprintf("OK %s: %d rows, %d cols", nm, nrw, ncol(sch)))
    cat(nm, "\n", sep = "", file = done_f, append = TRUE)
    TRUE
  }, error = function(e) {
    logln(sprintf("FAIL %s: %s", nm, conditionMessage(e)))
    cat(sprintf("%s\t%s\n", nm, conditionMessage(e)), file = failed_f, append = TRUE)
    FALSE
  })
}

done2 <- if (file.exists(done_f)) readLines(done_f) else character(0)
fail2 <- if (file.exists(failed_f)) readLines(failed_f) else character(0)
logln(sprintf("Rebuild DONE. %d/%d succeeded, %d failed.",
              length(unique(done2)), length(names_all), length(fail2)))
writeLines("done", file.path(run_dir, "COMPLETE.marker"))
