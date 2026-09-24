# Euro+Med PlantBase distribution parser.
#
# The euromed backbone (R/backend-euromed.R) holds names, ids and synonymy. The
# same CyberTaxonomy CDM server states, per accepted taxon, its status in each
# Euro+Med area. inst/py/crawlers/crawl_euromed_distribution.py freezes those
# Distribution elements as euromed_distribution.jsonl, one line per accepted
# taxon carrying the taxon's CDM UUID (the euromed backbone's `taxon_id`), its
# canonical name and one record per (area, status, citing reference set).


# Status terms of the Euro+Med PresenceAbsenceTerm vocabulary, in the order a
# taxon-area with conflicting records resolves: a taxon is native wherever a
# record says so, and a weaker claim never overrides a stronger one.
.euromed_status_order <- c("native", "naturalised", "introduced", "casual",
                           "cultivated", "doubtful", "extinct")


#' Status class of a Euro+Med presence/absence term
#'
#' `term` is the term's label as the portal prints it (`"native"`,
#' `"introduced: uncertain degree of naturalisation"`, `"native: formerly
#' native"`), `absent` its `absenceTerm` flag. A term that only records a
#' retracted report (`"reported in error"`) maps to `NA`, as does one the
#' vocabulary map does not know, which the caller reports.
#' @noRd
.euromed_status_class <- function(term, absent) {
  t <- tolower(trimws(term))
  out <- rep(NA_character_, length(t))
  known <- nzchar(t)
  out[known & grepl("questionable|doubtful", t)]    <- "doubtful"
  out[known & t == "undefined"]                     <- "doubtful"
  out[known & is.na(out) & grepl("^(native|endemic|not endemic|unknown endemism)", t)] <- "native"
  out[known & is.na(out) & t == "naturalised"]      <- "naturalised"
  out[known & is.na(out) & t == "casual"]           <- "casual"
  out[known & is.na(out) & t == "cultivated"]       <- "cultivated"
  out[known & is.na(out) & grepl("^introduced", t)] <- "introduced"
  out[known & is.na(out) & t == "introduced"]       <- "introduced"
  out[absent & grepl("formerly", t)] <- "extinct"
  out[absent & !grepl("formerly", t)] <- NA_character_
  out
}


# Euro+Med area names that denote exactly one country, and its ISO 3166-1
# alpha-2 code. An area is mapped by its own name, so a combined area ("Italy,
# with San Marino and Vatican City", "Ireland, with N Ireland") and a part of a
# country (a Russian region, an island group) carry no code: the sub-areas of a
# combined area (Au(A), Au(L); Ga(F), Ga(M); Hs(S), Hs(A), Hs(G)) do. "Great
# Britain" is Euro+Med's area for the United Kingdom without Northern Ireland
# and is given GB.
.euromed_area_iso <- c(
  "Albania" = "AL", "Algeria" = "DZ", "Andorra" = "AD", "Armenia" = "AM",
  "Austria" = "AT", "Azerbaijan" = "AZ", "Belarus" = "BY", "Belgium" = "BE",
  "Bosnia-Herzegovina" = "BA", "Bulgaria" = "BG", "Croatia" = "HR",
  "Cyprus" = "CY", "Czech Republic" = "CZ", "Denmark" = "DK", "Egypt" = "EG",
  "Estonia" = "EE", "Finland" = "FI", "France" = "FR", "Georgia" = "GE",
  "Germany" = "DE", "Gibraltar" = "GI", "Great Britain" = "GB",
  "Greece" = "GR", "Hungary" = "HU", "Iceland" = "IS", "Ireland" = "IE",
  "Israel" = "IL", "Italy" = "IT", "Jordan" = "JO", "Latvia" = "LV",
  "Lebanon" = "LB", "Libya" = "LY", "Liechtenstein" = "LI",
  "Lithuania" = "LT", "Luxembourg" = "LU", "Malta" = "MT", "Moldova" = "MD",
  "Monaco" = "MC", "Montenegro" = "ME", "Morocco" = "MA",
  "Netherlands" = "NL", "North Macedonia" = "MK", "Norway" = "NO",
  "Poland" = "PL", "Portugal" = "PT", "Romania" = "RO", "Serbia" = "RS",
  "Slovakia" = "SK", "Slovenia" = "SI", "Spain" = "ES", "Sweden" = "SE",
  "Switzerland" = "CH", "Syria" = "SY", "Tunisia" = "TN", "Turkey" = "TR",
  "Türkiye" = "TR",
  "Ukraine" = "UA"
)


#' Parse the Euro+Med distribution snapshot
#'
#' Reads `euromed_distribution.jsonl` and emits one row per (accepted taxon,
#' Euro+Med area): the status class, the term as the portal states it, the area
#' with its ISO 3166-1 alpha-2 country code where the area is one country, the
#' taxon's CDM UUID and the ids of the references behind the reported status.
#' A retracted report (`reported in error`) is dropped; a former presence is
#' `extinct`. Where records of one taxon-area disagree the status follows
#' `.euromed_status_order` and the references are those stating it.
#'
#' Areas whose name is not in `.euromed_area_iso` and is not a combined area or
#' part of a country are reported so the table can be extended.
#'
#' @param path Character. Path to `euromed_distribution.jsonl`.
#' @return data.frame with `canonical_name`, `area_code`, `area_name`,
#'   `area_level`, `iso2`, `euromed_status`, `euromed_status_detail`,
#'   `euromed_status_source` and `taxon_id`, with a reference table
#'   (`ref_id`, `citation`, `doi`) attached by [attach_references()].
#' @export
parse_euromed_distribution <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite is required to parse the Euro+Med distribution snapshot.",
         call. = FALSE)
  }
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) {
    stop("Empty Euro+Med distribution snapshot: ", path, call. = FALSE)
  }

  chr <- function(x) if (is.null(x) || !length(x)) "" else as.character(x[[1L]])
  taxa <- vector("list", length(lines))
  refs <- vector("list", length(lines))
  for (i in seq_along(lines)) {
    r <- jsonlite::fromJSON(lines[[i]], simplifyVector = FALSE)
    els <- r$distribution
    if (!length(els)) next
    taxa[[i]] <- data.frame(
      taxon_id       = r$uuid,
      canonical_name = chr(r$name),
      area_code      = vapply(els, function(e) chr(e$area), ""),
      area_name      = vapply(els, function(e) chr(e$area_name), ""),
      area_level     = vapply(els, function(e) chr(e$area_level), ""),
      term           = vapply(els, function(e) chr(e$status), ""),
      absent         = vapply(els, function(e) isTRUE(e$absent), NA),
      ref_ids        = vapply(els, function(e) {
        ids <- vapply(e$refs, function(s) chr(s$uuid), "")
        .ref_join(ids)
      }, ""),
      stringsAsFactors = FALSE
    )
    refs[[i]] <- do.call(rbind, lapply(els, function(e) {
      if (!length(e$refs)) return(NULL)
      data.frame(
        ref_id   = vapply(e$refs, function(s) chr(s$uuid), ""),
        citation = vapply(e$refs, function(s) chr(s$citation), ""),
        stringsAsFactors = FALSE
      )
    }))
  }
  d <- do.call(rbind, taxa)
  if (is.null(d) || !nrow(d)) {
    stop("No Euro+Med distribution records in the snapshot.", call. = FALSE)
  }

  d$status <- .euromed_status_class(d$term, d$absent)
  unmapped <- unique(d$term[is.na(d$status) & !d$absent & nzchar(d$term)])
  if (length(unmapped)) {
    warning("Euro+Med status terms with no class: ",
            paste(shQuote(unmapped), collapse = ", "), call. = FALSE)
  }
  d <- d[!is.na(d$status) & nzchar(d$canonical_name) & nzchar(d$area_code), ,
         drop = FALSE]

  # One row per (taxon, area): the strongest status, its wording and the
  # references of the records stating that class.
  key <- paste(d$taxon_id, d$area_code, sep = "\r")
  d   <- d[order(key, match(d$status, .euromed_status_order)), , drop = FALSE]
  key <- paste(d$taxon_id, d$area_code, sep = "\r")
  first <- !duplicated(key)
  same  <- d$status == d$status[first][match(key, key[first])]
  ref_by_key <- tapply(d$ref_ids[same], key[same], function(z) {
    .ref_join(unlist(strsplit(z[nzchar(z)], "|", fixed = TRUE)))
  })
  out <- d[first, , drop = FALSE]
  out$euromed_status_source <- as.character(
    ref_by_key[paste(out$taxon_id, out$area_code, sep = "\r")])
  out$euromed_status_detail <- out$term
  out$euromed_status <- out$status
  out$iso2 <- unname(.euromed_area_iso[out$area_name])

  no_iso <- unique(out$area_name[is.na(out$iso2)])
  if (length(no_iso)) {
    message("Euro+Med areas without an ISO country code: ",
            paste(sort(no_iso), collapse = "; "))
  }

  out <- out[, c("canonical_name", "area_code", "area_name", "area_level",
                 "iso2", "euromed_status", "euromed_status_detail",
                 "euromed_status_source", "taxon_id")]
  rownames(out) <- NULL

  ref_tab <- do.call(rbind, refs)
  ref_tab <- ref_tab[nzchar(ref_tab$ref_id), , drop = FALSE]
  ref_tab$citation <- ifelse(nzchar(ref_tab$citation), ref_tab$citation,
                             NA_character_)
  ref_tab <- ref_tab[!duplicated(ref_tab$ref_id), , drop = FALSE]
  ref_tab <- ref_tab[ref_tab$ref_id %in% unlist(strsplit(
    stats::na.omit(out$euromed_status_source), "|", fixed = TRUE)), ,
    drop = FALSE]
  ref_tab$doi <- .extract_doi(ref_tab$citation)

  attach_references(out, ref_tab)
}
