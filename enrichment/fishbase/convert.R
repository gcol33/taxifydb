# ---- FishBase enrichment: species + ecology traits ----
#
# Source: rfishbase package, which mirrors the FishBase database
# DOI: 10.14284/XXX
# License: CC BY-NC 3.0 (non-commercial)
# Reference: Froese R, Pauly D (eds.) (2024) FishBase. World Wide Web
# electronic publication. https://www.fishbase.org
#
# rfishbase fetches data over HTTPS into a local cache; no manual download
# step is required.

build_fishbase <- function(output_dir) {
  if (!requireNamespace("rfishbase", quietly = TRUE)) {
    stop("rfishbase is required to build the FishBase enrichment. ",
         "Install with: install.packages(\"rfishbase\")", call. = FALSE)
  }

  message("Fetching FishBase taxonomy via rfishbase::load_taxa()...")
  tx <- rfishbase::load_taxa(server = "fishbase")
  # load_taxa()'s `Species` column is the full binomial (e.g. "Aapticheilichthys websteri")
  tx <- tx[, c("SpecCode", "Species", "Genus", "Family")]
  names(tx)[2] <- "canonical_name"

  message("Fetching FishBase species traits via rfishbase::species()...")
  sp <- rfishbase::species(server = "fishbase")
  sp <- sp[, intersect(names(sp), c("SpecCode", "Length", "Weight",
                                     "DepthRangeShallow", "DepthRangeDeep",
                                     "Vulnerability", "DemersPelag",
                                     "Importance"))]

  message("Fetching FishBase ecology table via rfishbase::ecology()...")
  eco <- rfishbase::ecology(server = "fishbase")

  sp <- merge(tx, sp, by = "SpecCode", all.x = TRUE)
  sp$canonical_name <- trimws(sp$canonical_name)

  eco_keep <- intersect(names(eco),
                        c("SpecCode", "FeedingType", "DietTroph"))
  eco_sub <- eco[, eco_keep, drop = FALSE]
  eco_sub <- eco_sub[!duplicated(eco_sub$SpecCode), ]

  merged <- merge(sp, eco_sub, by = "SpecCode", all.x = TRUE)

  safe_num <- function(col_name) {
    if (!col_name %in% names(merged)) return(rep(NA_real_, nrow(merged)))
    suppressWarnings(as.numeric(merged[[col_name]]))
  }
  safe_chr <- function(col_name) {
    if (!col_name %in% names(merged)) return(rep(NA_character_, nrow(merged)))
    x <- as.character(merged[[col_name]])
    x[is.na(x) | nchar(trimws(x)) == 0L] <- NA_character_
    trimws(x)
  }

  out <- data.frame(
    canonical_name  = merged$canonical_name,
    body_length_cm  = safe_num("Length"),
    body_mass_g     = safe_num("Weight"),
    trophic_level   = safe_num("DietTroph"),
    depth_min_m     = safe_num("DepthRangeShallow"),
    depth_max_m     = safe_num("DepthRangeDeep"),
    vulnerability   = safe_num("Vulnerability"),
    habitat         = safe_chr("DemersPelag"),
    importance      = safe_chr("Importance"),
    stringsAsFactors = FALSE
  )

  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]
  out <- out[!duplicated(out$canonical_name), ]

  message(sprintf("  %d FishBase species with traits", nrow(out)))

  # Resolve source names against all 7 backends
  out <- resolve_enrichment_names(out)

  vtr_path <- file.path(output_dir, "fishbase.vtr")
  build_enrichment_vtr(
    out, vtr_path,
    name        = "fishbase",
    version     = format(Sys.Date(), "%Y.%m"),
    source_url  = "https://fishbase.ropensci.org",
    source_doi  = NULL,
    license     = "CC BY-NC 3.0",
    attribution = paste0(
      "Froese R, Pauly D (eds.) (2024) FishBase. World Wide Web ",
      "electronic publication, https://www.fishbase.org."
    )
  )
}
