# Marine distribution parser: WoRMS distributions -> MEOW ecoregions.
#
# WoRMS records a species' distribution as fine-grained Marine Regions
# localities (MRGID), from ocean basins down to individual bays and shipwrecks
# (issue #21). For a `region=`/`coords=` disambiguation filter those are too
# fine and overlapping, so parse_marine_distribution rolls every MRGID up to the
# MEOW ecoregion(s) it falls in (Spalding et al. 2007; 232 ecoregions) using a
# frozen mrgid -> ecoregion crosswalk, giving the marine analogue of the WCVP
# range table: canonical_name + region_code (MEOW ECO_CODE) + native_status.
#
# The two frozen snapshots are produced by inst/py/crawlers:
#   worms_distributions.jsonl  -- one line per (species, MRGID) distribution
#                                 record, keyed on the accepted canonical name
#                                 (crawl_worms_distributions.py)
#   mrgid_meow.tsv             -- mrgid -> one or more MEOW ecoregions
#                                 (crosswalk_mrgid_meow.py)


# WoRMS occurrence values that are not evidence of wild presence: an explicit
# absence, a retracted record, a population that is gone, or one held only under
# human care. Every other value (including "Uncertain" and "Sometimes present")
# is treated as presence, since the range filter is a soft disambiguation aid
# and a borderline record should widen a species' range rather than narrow it.
.worms_non_presence <- c(
  "Absent",
  "Recorded in error",
  "Extirpated",
  "Eradicated",
  "In captivity/cultivated"
)


#' Parse WoRMS marine distributions rolled up to MEOW ecoregions
#'
#' Reads the frozen WoRMS distribution snapshot and the MRGID -> MEOW ecoregion
#' crosswalk from `dir_path`, drops records WoRMS flags as doubtful or
#' inaccurate along with those reporting non-presence, expands each remaining
#' distribution record to every MEOW ecoregion its MRGID maps to, and collapses
#' to one row per (species, ecoregion) with a native/introduced status.
#'
#' @param dir_path Character. Directory containing `worms_distributions.jsonl`
#'   (or `*.ndjson`) and `mrgid_meow.tsv`.
#' @return data.frame with `canonical_name`, `region_code` (MEOW ECO_CODE as a
#'   string), `ecoregion`, `province`, `realm`, and `native_status`.
#' @export
parse_marine_distribution <- function(dir_path) {
  dist_file <- list.files(
    dir_path, pattern = "worms_distributions.*\\.(jsonl|ndjson)$",
    full.names = TRUE, recursive = TRUE
  )
  xw_file <- list.files(
    dir_path, pattern = "mrgid_meow.*\\.tsv$",
    full.names = TRUE, recursive = TRUE
  )
  if (length(dist_file) == 0L || length(xw_file) == 0L) {
    stop(sprintf(
      paste0("Could not find the WoRMS distribution snapshot and MRGID->MEOW ",
             "crosswalk in: %s\nFiles: %s"),
      dir_path,
      paste(basename(list.files(dir_path, recursive = TRUE)), collapse = ", ")
    ), call. = FALSE)
  }

  dist_df <- .read_worms_distributions(dist_file[1L])
  xw_df   <- utils::read.delim(xw_file[1L], stringsAsFactors = FALSE,
                               colClasses = "character", na.strings = "")

  # A distribution record is evidence of presence in the wild only when it is a
  # valid record that does not itself report the species as absent. Records
  # WoRMS flags "doubtful" or "inaccurate" are dropped, as are the occurrence
  # values that assert non-presence (never there, no longer there) or presence
  # only under human care, so the range filter never places a species where
  # WoRMS says it is not, or only doubtfully, present.
  keep <- (is.na(dist_df$record_status) | dist_df$record_status == "valid") &
    (is.na(dist_df$occurrence) | !dist_df$occurrence %in% .worms_non_presence)
  dist_df <- dist_df[keep, , drop = FALSE]

  dist_df <- dist_df[!is.na(dist_df$canonical_name) &
                       nchar(dist_df$canonical_name) > 0L &
                       !is.na(dist_df$mrgid), , drop = FALSE]

  # WoRMS establishmentMeans is the primary native/alien signal. It qualifies
  # nativeness ("Native - Endemic", "Native - Non-endemic") where it can, so
  # every Native variant counts as native; "Origin unknown", "Origin uncertain"
  # and the unscored default become "unknown". Presence still counts for the
  # range filter regardless of status.
  em <- dist_df$establishment_means
  dist_df$native_status <- ifelse(
    !is.na(em) & startsWith(em, "Native"), "native",
    ifelse(!is.na(em) & em == "Alien", "introduced", "unknown")
  )

  # Roll each MRGID up to its MEOW ecoregion(s). A coarse region (e.g. an ocean
  # basin) maps to several ecoregions, so this many-to-many join expands rows.
  merged <- merge(
    dist_df[, c("canonical_name", "mrgid", "native_status")],
    xw_df[, c("mrgid", "eco_code", "ecoregion", "province", "realm")],
    by = "mrgid"
  )
  if (nrow(merged) == 0L) {
    stop("No WoRMS distribution records mapped to a MEOW ecoregion.",
         call. = FALSE)
  }

  merged$region_code <- as.character(merged$eco_code)
  merged <- merged[!is.na(merged$region_code) &
                     nchar(merged$region_code) > 0L, , drop = FALSE]

  # Collapse to one row per (species, ecoregion). A native record anywhere in an
  # ecoregion makes the species native there; failing that an introduced record
  # makes it introduced; only when neither is recorded is it left unknown.
  key <- paste(merged$canonical_name, merged$region_code, sep = "\r")
  ord <- order(match(merged$native_status,
                     c("native", "introduced", "unknown")))
  merged <- merged[ord, , drop = FALSE]
  status_by_key <- tapply(merged$native_status, key[ord], function(s) {
    if ("native" %in% s) "native"
    else if ("introduced" %in% s) "introduced"
    else "unknown"
  })

  out <- merged[!duplicated(key[ord]), , drop = FALSE]
  out$native_status <- as.character(status_by_key[
    paste(out$canonical_name, out$region_code, sep = "\r")])

  out <- out[, c("canonical_name", "region_code", "ecoregion",
                 "province", "realm", "native_status")]
  rownames(out) <- NULL
  out
}


#' Read a WoRMS distribution snapshot (JSON lines) into a data.frame
#' @noRd
.read_worms_distributions <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) == 0L) {
    stop("Empty WoRMS distribution snapshot: ", path, call. = FALSE)
  }

  get <- function(rec, key) {
    v <- rec[[key]]
    if (is.null(v) || length(v) == 0L) NA_character_ else as.character(v[[1L]])
  }
  recs <- lapply(lines, function(ln) jsonlite::fromJSON(ln, simplifyVector = FALSE))

  data.frame(
    canonical_name     = vapply(recs, get, character(1L), "canonical_name"),
    mrgid              = suppressWarnings(as.integer(
                           vapply(recs, get, character(1L), "mrgid"))),
    establishment_means = vapply(recs, get, character(1L), "establishment_means"),
    invasiveness       = vapply(recs, get, character(1L), "invasiveness"),
    occurrence         = vapply(recs, get, character(1L), "occurrence"),
    record_status      = vapply(recs, get, character(1L), "record_status"),
    stringsAsFactors   = FALSE
  )
}
