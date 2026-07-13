# Blank/whitespace-only strings to NA.
.sid_blank <- function(x) {
  x <- trimws(as.character(x))
  x[!nzchar(x)] <- NA_character_
  x
}

# Collapse SER-SID storage-behaviour qualifiers to the base class: a trailing
# "?" or " p" marks provisional evidence, not a distinct behaviour, so
# "Orthodox"/"Orthodox?"/"Orthodox p" all become "Orthodox" (Uncertain kept).
.sid_storage_norm <- function(x) {
  x <- .sid_blank(x)
  x <- sub("[`'\"]+$", "", x)
  x <- sub("\\s*\\?+$", "", x)
  x <- sub("\\s+p$", "", x)
  x <- trimws(x)
  x[!nzchar(x)] <- NA_character_
  x
}


#' Parse SER-SID (Kew Seed Information Database) tables to per-species traits
#'
#' Reduces the per-record SER-SID tables (many measurements per species) to one
#' row per species: seed weight and nutrient contents by median, storage
#' behaviour and fruit type by mode, with a record count for the seed-weight
#' evidence.
#'
#' @param path Character. Directory holding the per-table RDS files written by
#'   the registry download function (`species`, `seed_weights`,
#'   `storage_behaviour`, `oil_content`, `protein_content`, `morphology`).
#' @return data.frame with `canonical_name` and the trait columns
#'   `thousand_seed_weight` (grams per 1000 seeds, median),
#'   `n_seed_weight_records`, `storage_behaviour`, `oil_content_pct`,
#'   `protein_content_pct`, `lifeform` (Raunkiaer code), and `fruit_type`.
#' @export
parse_kew_sid <- function(path) {
  rd <- function(nm) {
    f <- file.path(path, paste0(nm, ".rds"))
    if (file.exists(f)) readRDS(f) else NULL
  }

  sp <- rd("species")
  if (is.null(sp) || !nrow(sp)) {
    return(data.frame(canonical_name = character(0), stringsAsFactors = FALSE))
  }
  genus   <- trimws(as.character(sp$genus))
  epithet <- trimws(as.character(sp$epithet))
  ok <- nzchar(genus) & nzchar(epithet) & !is.na(sp$int_id)
  out <- data.frame(
    species_id     = sp$int_id[ok],
    canonical_name = trimws(paste(genus[ok], epithet[ok])),
    lifeform       = .sid_blank(sp$lifeform[ok]),
    stringsAsFactors = FALSE
  )

  median_by <- function(tbl, col, out_name) {
    if (is.null(tbl) || !nrow(tbl)) return(NULL)
    v  <- suppressWarnings(as.numeric(tbl[[col]]))
    sp <- .num_group_spread(v, tbl$species_id)
    if (!nrow(sp)) return(NULL)
    a  <- data.frame(
      species_id = tbl$species_id[match(sp$group, as.character(tbl$species_id))],
      stringsAsFactors = FALSE
    )
    a[[out_name]] <- sp$med
    # Add the within-species range only where some species shows a genuine range
    # (seed weight, oil, protein are per-accession measurements that vary).
    if (any(sp$max > sp$min, na.rm = TRUE)) {
      a[[paste0(out_name, "_min")]] <- sp$min
      a[[paste0(out_name, "_max")]] <- sp$max
    }
    a
  }
  count_by <- function(tbl, col, out_name) {
    if (is.null(tbl) || !nrow(tbl)) return(NULL)
    v <- suppressWarnings(as.numeric(tbl[[col]]))
    a <- stats::aggregate(
      list(v = v), by = list(species_id = tbl$species_id),
      FUN = function(z) {
        n <- sum(is.finite(z))
        if (n == 0L) NA_integer_ else as.integer(n)
      })
    names(a)[names(a) == "v"] <- out_name
    a
  }
  mode_by <- function(tbl, col, out_name, norm = .sid_blank) {
    if (is.null(tbl) || !nrow(tbl)) return(NULL)
    z <- norm(tbl[[col]])
    a <- stats::aggregate(
      list(v = z), by = list(species_id = tbl$species_id),
      FUN = function(w) {
        w <- w[!is.na(w)]
        if (!length(w)) NA_character_ else names(sort(table(w), decreasing = TRUE))[1]
      })
    names(a)[names(a) == "v"] <- out_name
    a
  }
  join <- function(x, y) if (is.null(y)) x else merge(x, y, by = "species_id", all.x = TRUE)

  sw <- rd("seed_weights")
  if (!is.null(sw) && nrow(sw)) {
    w <- suppressWarnings(as.numeric(sw$thousandseedweight))
    sw$thousandseedweight[!is.finite(w) | w <= 0] <- NA
  }
  out <- join(out, median_by(sw, "thousandseedweight", "thousand_seed_weight"))
  out <- join(out, count_by(sw, "thousandseedweight", "n_seed_weight_records"))
  out <- join(out, mode_by(rd("storage_behaviour"), "storage_behaviour",
                           "storage_behaviour", norm = .sid_storage_norm))
  out <- join(out, median_by(rd("oil_content"), "oil_content", "oil_content_pct"))
  out <- join(out, median_by(rd("protein_content"), "protein_content", "protein_content_pct"))
  out <- join(out, mode_by(rd("morphology"), "fruit_type", "fruit_type"))

  out$species_id <- NULL
  out <- .trait_finalize(out)
  out$n_seed_weight_records[is.na(out$n_seed_weight_records)] <- 0L
  out
}
