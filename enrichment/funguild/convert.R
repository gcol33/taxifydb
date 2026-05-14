# ---- FUNGuild enrichment: fungal trophic mode + ecological guild ----
#
# Source: FUNGuild database, served as JSON via stbates.org
# DOI: 10.1016/j.funeco.2015.06.006
# License: CC BY 4.0
# Reference: Nguyen NH et al. (2016) FUNGuild: An open annotation tool for
# parsing fungal community datasets by ecological guild.
# Fungal Ecology 20:241-248.
#
# The endpoint returns an HTML wrapper around a JSON array. We strip the
# wrapper and parse the JSON.

.funguild_url <- "http://www.stbates.org/funguild_db_2.php"

download_funguild <- function(dest = tempdir()) {
  path <- file.path(dest, "funguild_db.html")
  if (!file.exists(path) || file.size(path) < 1000L) {
    message("Downloading FUNGuild database...")
    h <- curl::new_handle()
    curl::handle_setopt(h, followlocation = TRUE, maxredirs = 10L)
    curl::handle_setheaders(h, "User-Agent" = "taxify-backbones/1.0")
    curl::curl_download(.funguild_url, path, handle = h)
  }
  path
}

build_funguild <- function(output_dir) {
  path <- download_funguild()

  message("Parsing FUNGuild JSON wrapper...")
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  start <- regexpr("\\[", txt)
  end   <- max(gregexpr("\\]", txt)[[1L]])
  if (start <= 0L || end <= start) {
    stop("Cannot locate JSON array in FUNGuild response.", call. = FALSE)
  }
  raw <- jsonlite::fromJSON(substr(txt, start, end), simplifyVector = TRUE)

  # Index Fungorum numeric taxonomic levels:
  # 12=family, 13=genus, 20=species, 25=variety, 26=form, 27=subspecies
  level_col <- intersect(names(raw), c("taxonomicLevel", "taxonLevel"))
  if (length(level_col) == 0L) {
    stop("FUNGuild response missing taxonomicLevel/taxonLevel column.",
         call. = FALSE)
  }
  level_raw <- trimws(as.character(raw[[level_col[1L]]]))
  level_norm <- ifelse(grepl("^[0-9]+$", level_raw),
    c("13" = "genus", "20" = "species", "25" = "species",
      "26" = "species", "27" = "species")[level_raw],
    tolower(level_raw)
  )

  keep <- level_norm %in% c("genus", "species")
  df <- raw[keep, ]
  taxon_level <- level_norm[keep]

  if (nrow(df) == 0L) {
    stop("No genus/species-level entries found in FUNGuild JSON.",
         call. = FALSE)
  }

  pick_col <- function(...) {
    cands <- c(...)
    hit <- intersect(cands, names(df))
    if (length(hit) == 0L) return(rep(NA_character_, nrow(df)))
    trimws(as.character(df[[hit[1L]]]))
  }

  trophic <- pick_col("trophicMode", "trophic_mode")
  guild   <- pick_col("guild")
  growth  <- pick_col("growthMorphology", "growthForm", "growth_morphology")
  conf    <- pick_col("confidenceRanking", "confidence_ranking", "confidence")

  # Empty -> NA
  for (v in list(trophic, guild, growth, conf)) {
    v[!nzchar(v)] <- NA_character_
  }

  out <- data.frame(
    canonical_name     = trimws(df$taxon),
    taxon_level        = taxon_level,
    trophic_mode       = trophic,
    guild              = guild,
    growth_morphology  = growth,
    confidence_ranking = conf,
    stringsAsFactors   = FALSE
  )

  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]

  # Species-level rows beat genus-level rows for the same canonical_name
  out <- out[order(out$taxon_level != "species", out$canonical_name), ]
  out <- out[!duplicated(out$canonical_name), ]
  out$taxon_level <- NULL

  message(sprintf("  %d FUNGuild entries", nrow(out)))

  # Resolve source names against all 7 backends
  out <- resolve_enrichment_names(out)

  vtr_path <- file.path(output_dir, "funguild.vtr")
  build_enrichment_vtr(
    out, vtr_path,
    name        = "funguild",
    version     = format(Sys.Date(), "%Y.%m"),
    source_url  = .funguild_url,
    source_doi  = "10.1016/j.funeco.2015.06.006",
    license     = "CC BY 4.0",
    attribution = paste0(
      "Nguyen NH et al. (2016) FUNGuild: An open annotation tool for ",
      "parsing fungal community datasets by ecological guild. ",
      "Fungal Ecology 20:241-248."
    )
  )
}
