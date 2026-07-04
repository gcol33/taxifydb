# Build the BIEN enrichment from EVERY BIEN trait (not a curated subset).
# Resumable: each trait's reduced (name, trait, value) records are cached to
# disk, so a restart skips completed traits. The per-trait download is a large
# global scrape; the output is one row per species x trait. Detached via a
# Scheduled Task so it outlives the launching session.

`%||%` <- function(a, b) if (is.null(a)) b else a
repo <- "C:/Users/GillesC/Documents/dev/taxifydb"
suppressMessages(devtools::load_all(repo, quiet = TRUE))

run_dir <- file.path(repo, "output", "bien_run")
cache   <- file.path(run_dir, "cache")
dir.create(cache, recursive = TRUE, showWarnings = FALSE)
log_f   <- file.path(run_dir, "bien.log")
logln <- function(...) {
  m <- sprintf("[%s] %s", format(Sys.time(), "%H:%M:%S"), paste0(..., collapse = ""))
  cat(m, "\n"); cat(m, "\n", file = log_f, append = TRUE)
}

# atomic single-instance lock
lock_d <- file.path(run_dir, "LOCK.d")
if (dir.exists(lock_d) &&
    difftime(Sys.time(), file.info(lock_d)$mtime, units = "hours") > 12) {
  unlink(lock_d, recursive = TRUE)
}
if (!dir.create(lock_d, showWarnings = FALSE)) {
  cat("bien build already running; exiting.\n"); quit(save = "no", status = 0)
}
on.exit(unlink(lock_d, recursive = TRUE), add = TRUE)

# curated spec (nice names / types); keep_all pivots every other fetched trait
spec <- list(
  plant_height_m       = list(trait = "whole plant height", type = "num"),
  max_plant_height_m   = list(trait = "maximum whole plant height", type = "num"),
  dbh_cm               = list(trait = "diameter at breast height (1.3 m)", type = "num"),
  sla_mm2_mg           = list(trait = "leaf area per leaf dry mass", type = "num"),
  leaf_area_mm2        = list(trait = "leaf area", type = "num"),
  leaf_dry_mass_mg     = list(trait = "leaf dry mass", type = "num"),
  leaf_n_per_dry_mass  = list(trait = "leaf nitrogen content per leaf dry mass", type = "num"),
  leaf_p_per_dry_mass  = list(trait = "leaf phosphorus content per leaf dry mass", type = "num"),
  leaf_thickness_mm    = list(trait = "leaf thickness", type = "num"),
  seed_mass_mg         = list(trait = "seed mass", type = "num"),
  wood_density_g_cm3   = list(trait = "stem wood density", type = "num"),
  leaf_lifespan        = list(trait = "leaf life span", type = "num"),
  growth_form          = list(trait = "whole plant growth form", type = "cat"),
  woodiness            = list(trait = "whole plant woodiness", type = "cat"),
  dispersal_syndrome   = list(trait = "whole plant dispersal syndrome", type = "cat"),
  flower_color         = list(trait = "flower color", type = "cat")
)

tl <- BIEN::BIEN_trait_list()
traits <- if (is.data.frame(tl)) {
  tc <- intersect(c("trait_name", "trait"), names(tl)); unique(as.character(tl[[tc[1L]]]))
} else unique(as.character(tl))
traits <- traits[!is.na(traits) & nzchar(trimws(traits))]
# Curated traits first (known-fetchable, gets the important ones cached fast),
# then the rest; the biggest record-level traits (DBH etc.) fall to the end
# where a per-trait timeout can skip them without blocking the fast majority.
traits <- union(unname(vapply(spec, function(s) s$trait, character(1L))), traits)
logln(sprintf("BIEN trait catalogue: %d traits", length(traits)))

safe <- function(s) gsub("[^A-Za-z0-9]+", "_", s)
# Per-trait fetch reducer, run in a child process so a mega-trait (e.g. DBH,
# millions of records) that hangs the record-level API can be killed and
# skipped after a wall-clock budget instead of blocking the whole build.
fetch_one <- function(tr) {
  raw <- BIEN::BIEN_trait_trait(trait = tr)
  if (is.data.frame(raw) && nrow(raw) > 0L) {
    if ("access" %in% names(raw))
      raw <- raw[!is.na(raw$access) & raw$access == "public", , drop = FALSE]
    if (nrow(raw) > 0L)
      return(data.frame(name = as.character(raw$scrubbed_species_binomial),
                        trait = as.character(raw$trait_name),
                        value = as.character(raw$trait_value),
                        stringsAsFactors = FALSE))
  }
  NULL
}
PER_TRAIT_TIMEOUT <- 1800L   # 30 min: fair for big traits, caps the intractable
for (i in seq_along(traits)) {
  tr <- traits[i]
  f  <- file.path(cache, paste0(sprintf("%03d_", i), safe(tr), ".rds"))
  s  <- paste0(tools::file_path_sans_ext(f), ".skip")
  if (file.exists(f) || file.exists(s)) {
    logln(sprintf("skip %d/%d %s (done)", i, length(traits), tr)); next
  }
  t0 <- Sys.time()
  red <- tryCatch(
    callr::r(fetch_one, args = list(tr = tr), timeout = PER_TRAIT_TIMEOUT),
    error = function(e) structure(list(), .err = conditionMessage(e)))
  el <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
  if (is.list(red) && !is.data.frame(red) && !is.null(attr(red, ".err"))) {
    writeLines(attr(red, ".err"), s)      # timeout/error: mark skipped, don't retry
    logln(sprintf("SKIP %d/%d %s (%s min): %s", i, length(traits), tr, el,
                  attr(red, ".err")))
    next
  }
  saveRDS(red, f)
  logln(sprintf("OK %d/%d %s (%s min): %s rows", i, length(traits), tr, el,
                if (is.null(red)) 0L else nrow(red)))
}

# assemble once every trait is either cached or skipped
cached  <- list.files(cache, pattern = "\\.rds$",  full.names = TRUE)
skipped <- list.files(cache, pattern = "\\.skip$")
if (length(cached) + length(skipped) < length(traits)) {
  logln(sprintf("INCOMPLETE: %d cached + %d skipped of %d; rerun to finish.",
                length(cached), length(skipped), length(traits)))
  quit(save = "no", status = 0)
}
if (length(skipped)) logln(sprintf("skipped (too large for record API): %s",
                                   paste(sub("\\.skip$","",skipped), collapse=", ")))
logln("all traits cached; assembling...")
parts <- lapply(cached, readRDS)
parts <- parts[!vapply(parts, is.null, logical(1L))]
long <- do.call(rbind, parts)
logln(sprintf("long records: %s", format(nrow(long), big.mark = ",")))

res <- taxifydb:::.trait_finalize(taxifydb:::.pivot_species_traits(long, spec))
logln(sprintf("pivoted: %d species x %d cols", nrow(res), ncol(res)))
df <- resolve_enrichment_names(res, group_cols = NULL, verbose = FALSE)

data_dir <- taxify::taxify_data_dir()
od <- file.path(run_dir, "out"); dir.create(od, showWarnings = FALSE, recursive = TRUE)
vp <- file.path(od, "bien.vtr")
reg <- taxifydb:::.enrichment_build_registry$bien
build_enrichment_vtr(df, vp, name = "bien", version = reg$version,
                     source_url = reg$source_url, source_doi = reg$source_doi,
                     license = reg$license, attribution = reg$attribution,
                     group_col = NULL)

dest <- file.path(data_dir, "enrichment", "bien", "latest")
dir.create(dest, recursive = TRUE, showWarnings = FALSE)
file.remove(list.files(dest, pattern = "^bien\\.vtr", full.names = TRUE))
file.copy(list.files(od, pattern = "^bien\\.vtr", full.names = TRUE), dest, overwrite = TRUE)
m <- jsonlite::read_json(file.path(od, "meta.json"), simplifyVector = TRUE)
m$static <- TRUE; m$widened <- TRUE
jsonlite::write_json(m, file.path(dest, "meta.json"), pretty = TRUE,
                     auto_unbox = TRUE, null = "null")
logln(sprintf("BIEN DONE: %d cols installed static", ncol(res)))
writeLines("done", file.path(run_dir, "COMPLETE.marker"))
