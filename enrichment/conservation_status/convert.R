# ---- Conservation status enrichment: GBIF species threat status ----
#
# Source: GBIF Backbone Taxonomy (species with IUCN Red List categories)
# Uses the GBIF species search API to collect all species with threat status.
# Coverage: ~160k species (excluding LC and DD for smaller .vtr)
#
# Alternatively, use all ~485k species including LC and DD.

.conservation_base_url <- "https://api.gbif.org/v1/species/search"

# Map GBIF threat names to standard IUCN abbreviations
.iucn_abbrev <- c(
  "LEAST_CONCERN"          = "LC",
  "NEAR_THREATENED"        = "NT",
  "VULNERABLE"             = "VU",
  "ENDANGERED"             = "EN",
  "CRITICALLY_ENDANGERED"  = "CR",
  "EXTINCT_IN_THE_WILD"    = "EW",
  "EXTINCT"                = "EX",
  "DATA_DEFICIENT"         = "DD"
)

# Categories to include (all of them — users can filter downstream)
.categories <- names(.iucn_abbrev)

fetch_category <- function(category, limit = 1000L) {
  abbrev <- .iucn_abbrev[[category]]
  offset <- 0L
  all_rows <- list()
  batch <- 1L
  max_offset <- 9999L  # GBIF API hard limit

  repeat {
    if (offset > max_offset) {
      message(sprintf("  Reached GBIF API offset limit (%d) for %s; splitting by rank",
                      max_offset, category))
      # Split by rank to bypass the limit
      extra <- fetch_category_by_rank(category, limit)
      if (nrow(extra) > 0L) all_rows[[batch]] <- extra
      break
    }

    url <- sprintf(
      "%s?threat=%s&limit=%d&offset=%d",
      .conservation_base_url, category, limit, offset
    )
    resp <- curl::curl_fetch_memory(url)
    if (resp$status_code != 200L) {
      message(sprintf("  API returned %d at offset %d, stopping", resp$status_code, offset))
      break
    }

    data <- jsonlite::fromJSON(rawToChar(resp$content))
    results <- data$results

    if (is.null(results) || nrow(results) == 0L) break

    names_vec <- results$canonicalName
    if (is.null(names_vec)) {
      names_vec <- sub("\\s+[A-Z].*$", "", results$scientificName)
    }

    rows <- data.frame(
      canonical_name      = names_vec,
      conservation_status = abbrev,
      stringsAsFactors = FALSE
    )
    all_rows[[batch]] <- rows
    batch <- batch + 1L

    if (data$endOfRecords || offset + limit >= min(data$count, max_offset + 1L)) break
    offset <- offset + limit
  }

  if (length(all_rows) == 0L) {
    return(data.frame(
      canonical_name = character(0),
      conservation_status = character(0),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, all_rows)
}

# For categories with >10k species, split by taxonomic rank
fetch_category_by_rank <- function(category, limit = 1000L) {
  abbrev <- .iucn_abbrev[[category]]
  ranks <- c("SPECIES", "SUBSPECIES", "VARIETY")
  all_rows <- list()
  batch <- 1L

  for (rank in ranks) {
    offset <- 0L
    repeat {
      if (offset > 9999L) break

      url <- sprintf(
        "%s?threat=%s&rank=%s&limit=%d&offset=%d",
        .conservation_base_url, category, rank, limit, offset
      )
      resp <- curl::curl_fetch_memory(url)
      if (resp$status_code != 200L) break

      data <- jsonlite::fromJSON(rawToChar(resp$content))
      results <- data$results
      if (is.null(results) || nrow(results) == 0L) break

      names_vec <- results$canonicalName
      if (is.null(names_vec)) {
        names_vec <- sub("\\s+[A-Z].*$", "", results$scientificName)
      }

      rows <- data.frame(
        canonical_name      = names_vec,
        conservation_status = abbrev,
        stringsAsFactors = FALSE
      )
      all_rows[[batch]] <- rows
      batch <- batch + 1L

      if (data$endOfRecords || offset + limit >= min(data$count, 10000L)) break
      offset <- offset + limit
    }
  }

  if (length(all_rows) == 0L) {
    return(data.frame(
      canonical_name = character(0),
      conservation_status = character(0),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, all_rows)
}

build_conservation_status <- function(output_dir) {
  all_data <- list()

  for (category in .categories) {
    abbrev <- .iucn_abbrev[[category]]
    message(sprintf("Fetching %s (%s)...", category, abbrev))
    df <- fetch_category(category)
    message(sprintf("  %s species", format(nrow(df), big.mark = ",")))
    all_data[[category]] <- df
  }

  out <- do.call(rbind, all_data)
  rownames(out) <- NULL

  # Clean
  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0, ]
  # Some species appear under multiple threat categories; keep the most
  # threatened status (CR > EN > VU > NT > LC, DD separate)
  severity <- c("EX" = 1, "EW" = 2, "CR" = 3, "EN" = 4, "VU" = 5,
                "NT" = 6, "LC" = 7, "DD" = 8)
  out$sev <- severity[out$conservation_status]
  out <- out[order(out$canonical_name, out$sev), ]
  out <- out[!duplicated(out$canonical_name), ]
  out$sev <- NULL

  # Resolve source names against all 7 backends
  out <- resolve_enrichment_names(out)

  vtr_path <- file.path(output_dir, "conservation_status.vtr")
  build_enrichment_vtr(
    out, vtr_path,
    name       = "conservation_status",
    version    = "2026.04",
    source_url = .conservation_base_url,
    license    = "Factual data (not copyrightable)",
    attribution = "Conservation status from GBIF Backbone Taxonomy (IUCN Red List categories)."
  )
}
