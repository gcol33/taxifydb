# ---- Master build script for taxify enrichments ----
#
# Usage:
#   Rscript build_enrichments.R [enrichment] [output_dir]
#
# Examples:
#   Rscript build_enrichments.R woodiness output/enrichment/woodiness
#   Rscript build_enrichments.R all output/enrichment
#   Rscript build_enrichments.R list              # list available enrichments
#
# Enrichments are simpler than backbones: no hierarchy walk, no precompute.
# Each convert.R downloads source data, cleans to canonical_name + trait
# columns, and writes a .vtr with meta.json sidecar.

args <- commandArgs(trailingOnly = TRUE)
action <- if (length(args) >= 1L) args[1L] else "all"
output_dir <- if (length(args) >= 2L) args[2L] else "output/enrichment"

# Source shared code
source("shared/build_enrichment.R")
source("shared/resolve_names.R")
source("shared/publish.R")

# Registry of enrichments and their build functions
enrichment_registry <- list(
  woodiness = list(
    script  = "enrichment/woodiness/convert.R",
    builder = "build_woodiness",
    desc    = "Zanne et al. 2014 woody/herbaceous classification"
  ),
  eive = list(
    script  = "enrichment/eive/convert.R",
    builder = "build_eive",
    desc    = "EIVE 1.0 ecological indicator values (European plants)"
  ),
  elton_traits = list(
    script  = "enrichment/elton_traits/convert.R",
    builder = "build_elton_traits",
    desc    = "EltonTraits 1.0 diet and foraging (birds + mammals)"
  ),
  avonet = list(
    script  = "enrichment/avonet/convert.R",
    builder = "build_avonet",
    desc    = "AVONET bird morphology and migration"
  ),
  pantheria = list(
    script  = "enrichment/pantheria/convert.R",
    builder = "build_pantheria",
    desc    = "PanTHERIA mammal life-history traits"
  ),
  amphibio = list(
    script  = "enrichment/amphibio/convert.R",
    builder = "build_amphibio",
    desc    = "AmphiBIO amphibian life-history traits"
  ),
  leda = list(
    script  = "enrichment/leda/convert.R",
    builder = "build_leda",
    desc    = "LEDA Traitbase NW European plant traits"
  ),
  diaz_traits = list(
    script  = "enrichment/diaz_traits/convert.R",
    builder = "build_diaz_traits",
    desc    = "Diaz et al. 2022 seed mass and plant height"
  ),
  griis = list(
    script  = "enrichment/griis/convert.R",
    builder = "build_griis",
    desc    = "GRIIS invasive species status by country"
  ),
  conservation_status = list(
    script  = "enrichment/conservation_status/convert.R",
    builder = "build_conservation_status",
    desc    = "IUCN conservation status (from GBIF backbone)"
  ),
  wcvp = list(
    script  = "enrichment/wcvp/convert.R",
    builder = "build_wcvp",
    desc    = "WCVP native range by TDWG region"
  ),
  common_names = list(
    script  = "enrichment/common_names/convert.R",
    builder = "build_common_names",
    desc    = "GBIF vernacular names (multi-language)"
  )
)


build_one_enrichment <- function(name, out_dir) {
  reg <- enrichment_registry[[name]]
  if (is.null(reg)) {
    stop(sprintf(
      "Unknown enrichment: '%s'. Available: %s",
      name, paste(names(enrichment_registry), collapse = ", ")
    ))
  }

  enr_out <- file.path(out_dir, name)
  dir.create(enr_out, recursive = TRUE, showWarnings = FALSE)

  message(sprintf("\n=== Building enrichment: %s ===", name))
  message(sprintf("    %s\n", reg$desc))

  t0 <- proc.time()

  source(reg$script, local = TRUE)
  builder_fn <- get(reg$builder)
  vtr_path <- builder_fn(enr_out)

  # Update manifest with built metadata (nrow, available_groups, etc.)
  manifest_path <- "manifest/manifest.json"
  if (file.exists(manifest_path)) {
    tryCatch(
      update_enrichment_manifest(manifest_path, name, vtr_path),
      error = function(e) {
        message(sprintf("Warning: manifest update failed for %s: %s",
                        name, conditionMessage(e)))
      }
    )
  }

  elapsed <- (proc.time() - t0)["elapsed"]
  message(sprintf("\n=== %s done in %.0f seconds ===\n", name, elapsed))

  vtr_path
}


if (action == "list") {
  message("Available enrichments:\n")
  for (name in names(enrichment_registry)) {
    reg <- enrichment_registry[[name]]
    message(sprintf("  %-25s %s", name, reg$desc))
  }
} else if (action == "all") {
  results <- list()
  for (name in names(enrichment_registry)) {
    tryCatch(
      {
        results[[name]] <- build_one_enrichment(name, output_dir)
      },
      error = function(e) {
        message(sprintf("FAILED: %s -- %s", name, conditionMessage(e)))
        results[[name]] <<- NULL
      }
    )
  }

  # Summary
  message("\n=== Enrichment build summary ===\n")
  for (name in names(enrichment_registry)) {
    status <- if (!is.null(results[[name]])) "OK" else "FAILED"
    message(sprintf("  %-25s %s", name, status))
  }
} else {
  build_one_enrichment(action, output_dir)
}
