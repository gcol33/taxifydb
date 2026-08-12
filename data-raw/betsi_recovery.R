# Freeze the recovered BETSI-derived matrices into inst/extdata/betsi/.
#
# The source matrices live under datasets/betsi/compiled/raw/ (local-only,
# gitignored: extracted from copyrighted papers whose PDFs never enter git).
# This script validates each one against its descriptor in R/betsi-recovery.R --
# the same trait-set, modality, fuzzy-coding (sum-to-100) and type checks the
# build runs -- and writes the committed, reviewable CSV the enrichment build
# reads. Re-run whenever a recovery matrix is added or re-extracted.

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("devtools is required to run data-raw/betsi_recovery.R", call. = FALSE)
}
devtools::load_all(".", quiet = TRUE)

# recovery-source name -> local source file (fuzzy: species,trait,class,pct;
# hard: species + one column per trait).
sources <- list(
  pelosi2014_earthworm = "datasets/betsi/compiled/raw/pelosi2014_appendix1.csv",
  lu2025_collembola    = "datasets/betsi/compiled/raw/lu2025_collembola.csv",
  # decoded + merged + normalized by datasets/betsi/scripts/extract/build_inrae_long.R
  inrae_collembola     = "datasets/betsi/compiled/raw/inrae_collembola_long.csv"
)

out_dir <- file.path("inst", "extdata", "betsi")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (key in names(sources)) {
  src <- sources[[key]]
  if (!file.exists(src)) {
    stop(sprintf(paste0("Recovery source '%s' not found at %s (local-only; ",
                        "see datasets/betsi/betsi_handoff.md)."), key, src),
         call. = FALSE)
  }
  spec <- taxifydb:::.betsi_recovery_sources[[key]]
  tab  <- utils::read.csv(src, stringsAsFactors = FALSE, check.names = FALSE)

  # Validate through the real parser path: errors on any trait/modality/type
  # mismatch or sum-to-100 violation before anything is frozen.
  if (identical(spec$shape, "fuzzy")) {
    wide <- taxifydb:::.betsi_pivot_matrix(tab, spec, key)
    tab  <- tab[order(tab$species, tab$trait, tab$class),
                c("species", "trait", "class", "pct")]
  } else {
    wide <- taxifydb:::.betsi_read_hard(tab, spec, key)
    tab  <- tab[order(tab$species), c("species", names(spec$traits))]
  }
  message(sprintf("[%s] validated %s: %d species, %d trait columns",
                  key, spec$shape, nrow(wide),
                  length(setdiff(names(wide), "canonical_name"))))

  # Write LF, not the platform line ending: a plain path opens a text-mode
  # connection that translates "\n" to CRLF on Windows, so a re-freeze there would
  # rewrite every committed (LF) row. A binary connection keeps the bytes stable.
  dest <- file.path(out_dir, spec$file)
  con <- file(dest, open = "wb")
  utils::write.csv(tab, con, row.names = FALSE)
  close(con)
  message(sprintf("[%s] wrote %s (%d rows)", key, dest, nrow(tab)))
}
