# Build WFO + COL + GBIF locally with the unified schema. Run via
#   Rscript --vanilla dev_notes/build_three.R
# Output goes to output/{wfo,col,gbif}/<backend>.vtr.

main <- function() {
  suppressPackageStartupMessages(devtools::load_all("."))

  start <- Sys.time()
  step <- function(label, expr) {
    t0 <- Sys.time()
    message(sprintf("[%s] >>> %s",
                    format(Sys.time(), "%H:%M:%S"), label))
    res <- force(expr)
    message(sprintf("[%s] <<< %s  (%.1f min)",
                    format(Sys.time(), "%H:%M:%S"), label,
                    as.numeric(difftime(Sys.time(), t0, units = "mins"))))
    invisible(res)
  }

  step("WFO build",  build_wfo (output_dir = "output/wfo"))
  step("COL build",  build_col (output_dir = "output/col"))
  step("GBIF build", build_gbif(output_dir = "output/gbif"))

  # Schema sanity check
  required <- c("taxon_id", "canonical_name", "taxon_rank",
                "taxonomic_status", "accepted_name_usage_id",
                "family", "genus", "specific_epithet",
                "authorship", "infraspecific_epithet")
  for (bb in c("wfo", "col", "gbif")) {
    path <- file.path("output", bb, paste0(bb, ".vtr"))
    cols <- names(vectra::tbl(path) |> utils::head(1L) |> vectra::collect())
    missing <- setdiff(required, cols)
    if (length(missing) > 0L) {
      stop(sprintf("[%s] missing unified-schema cols: %s",
                   bb, paste(missing, collapse = ", ")))
    }
    message(sprintf("[%s] schema OK (%d cols)", bb, length(cols)))
  }

  message(sprintf("\nAll done in %.1f min.",
                  as.numeric(difftime(Sys.time(), start, units = "mins"))))
}

tryCatch(main(),
         error = function(e) {
           message("\n*** FAILED: ", conditionMessage(e))
           quit(status = 1L)
         })
