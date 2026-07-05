# Host-breadth enrichment parsers.
#
# Two interaction datasets rolled up to per-species host breadth:
#   parse_hosts             NHM HOSTS Lepidoptera -> hostplants per moth/butterfly
#   parse_usda_fungus_host  USDA Fungus-Host      -> host plants per fungus
#
# Both reduce a long consumer -> host table to one row per consumer species with
# counts of distinct hosts. The rollup is done at the ACCEPTED-name grain: each
# consumer name is first resolved to its cross-backbone accepted name(s) via
# resolve_name_map(), and host sets are unioned across synonyms before the
# distinct count. Pre-aggregating per raw source name and letting the build
# pipeline collapse synonyms afterwards would keep only one arbitrary row per
# accepted name and silently discard the others' hosts (e.g. the fungus
# Rhizoctonia solani, with ~1200 hosts, otherwise collapses to a synonym's 2).
# These parsers therefore self-resolve; their registry entries set
# `resolve_names = FALSE` so build_enrichment() does not resolve a second time.


#' Is a string a proper Latin binomial? (genus + species epithet, no `sp.`)
#' @noRd
.is_binomial <- function(x) {
  x <- trimws(x)
  parts <- strsplit(x, "\\s+")
  vapply(parts, function(p) {
    length(p) >= 2L &&
      grepl("^[A-Z][a-z-]+$", p[1L]) &&
      grepl("^[a-z][a-z-]+$", p[2L]) &&
      !p[2L] %in% c("sp", "spp", "cf", "aff", "nr", "var", "indet")
  }, logical(1L))
}


#' Roll up consumer -> host pairs to distinct host counts per accepted consumer
#'
#' @param pairs data.frame of unique (consumer, value...) rows.
#' @param consumer Name of the consumer column (resolved to accepted names).
#' @param count_cols Named character vector `c(out_col = value_col, ...)`: each
#'   output column counts the distinct non-empty values of `value_col` per
#'   accepted consumer name.
#' @return data.frame with `canonical_name` (accepted consumer) + count columns,
#'   keeping only rows where the first count column is positive.
#' @noRd
.rollup_host_breadth <- function(pairs, consumer, count_cols) {
  map <- resolve_name_map(unique(pairs[[consumer]]), verbose = FALSE)
  m <- merge(pairs, map, by.x = consumer, by.y = "input_name")
  if (nrow(m) == 0L) {
    stop("host-breadth rollup: no consumer names resolved.", call. = FALSE)
  }

  acc <- sort(unique(m$accepted_name))
  out <- data.frame(canonical_name = acc, stringsAsFactors = FALSE)
  for (onm in names(count_cols)) {
    vcol <- count_cols[[onm]]
    u <- unique(m[, c("accepted_name", vcol)])
    u <- u[nzchar(trimws(u[[vcol]])), , drop = FALSE]
    cnt <- tapply(u[[vcol]], u$accepted_name, function(v) length(unique(v)))
    out[[onm]] <- as.integer(cnt[out$canonical_name])
    out[[onm]][is.na(out[[onm]])] <- 0L
  }

  primary <- names(count_cols)[1L]
  out[out[[primary]] > 0L, , drop = FALSE]
}


#' Parse NHM HOSTS Lepidoptera hostplant records into per-insect host breadth
#'
#' Reads the harvested datastore JSONL (one record per line) and rolls up per
#' accepted insect name to the number of distinct hostplant binomials and
#' distinct hostplant families.
#'
#' @param path Character. Path to the `hosts.jsonl` file.
#' @return data.frame with `canonical_name`, `host_plant_count`,
#'   `host_family_count`.
#' @export
parse_hosts <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite is required to parse the HOSTS JSONL.", call. = FALSE)
  }
  df <- jsonlite::stream_in(file(path), verbose = FALSE)

  insect <- paste(trimws(as.character(df[["Insect Genus"]])),
                  trimws(as.character(df[["Insect Species"]])))
  host   <- paste(trimws(as.character(df[["Hostplant Genus"]])),
                  trimws(as.character(df[["Hostplant Species"]])))
  family <- trimws(as.character(df[["Hostplant Family"]]))

  keep <- .is_binomial(insect)
  insect <- insect[keep]; host <- host[keep]; family <- family[keep]
  # host binomial where present, else "" (dropped from the plant count but the
  # family may still be recorded and counts toward host_family_count)
  host[!.is_binomial(host)] <- ""

  pairs <- unique(data.frame(insect = insect, host = host, family = family,
                             stringsAsFactors = FALSE))
  .rollup_host_breadth(
    pairs, consumer = "insect",
    count_cols = c(host_plant_count = "host", host_family_count = "family")
  )
}


#' Parse USDA Fungus-Host records into per-fungus host breadth
#'
#' Rolls up the USDA National Fungus Collections fungus->host table to one row
#' per accepted fungus name with the number of distinct host binomials and
#' distinct host genera.
#'
#' @param path Character. Path to the Fungus-Host CSV.
#' @return data.frame with `canonical_name`, `fungus_host_count`,
#'   `fungus_host_genus_count`.
#' @export
parse_usda_fungus_host <- function(path) {
  df <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                        colClasses = "character")
  fungus <- trimws(df[["sciName"]])
  host   <- trimws(df[["host"]])

  keep <- .is_binomial(fungus) & .is_binomial(host)
  fungus <- fungus[keep]; host <- host[keep]
  host_genus <- sub("\\s.*$", "", host)

  pairs <- unique(data.frame(fungus = fungus, host = host,
                             host_genus = host_genus, stringsAsFactors = FALSE))
  .rollup_host_breadth(
    pairs, consumer = "fungus",
    count_cols = c(fungus_host_count = "host",
                   fungus_host_genus_count = "host_genus")
  )
}
