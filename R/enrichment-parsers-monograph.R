# Body length mined from OCR'd taxonomic monographs.
#
# BETSI's Collembola body-length floor is a fixed 2017 extraction of the European
# compendia. The monographs behind it are per-species accounts that a treatment
# miner can read the same way it reads a Plazi archive, once the page images
# carry a text layer. This parser takes the text of one monograph and pulls a
# body length (mm) per species, reusing the classification core in
# `enrichment-parsers-plazi.R`: `.plazi_body_length()` decides whether a number
# is a whole-animal body length rather than an organ, a congener's size, a trunk
# length or a juvenile, and `.plazi_meas` / `.plazi_to_num` / `.plazi_bounds`
# parse and bound the value.
#
# The one thing that differs per book is layout, so each source is a small spec:
# how a species-name line looks, and how the length sits relative to it. Two
# layouts cover the set. In an "anchored" book the length is written out
# ("Length 0.5 mm", "Total length 0.7 mm in females") and the species name is a
# nearby header or key terminal, so the body-length core reads every clause and
# each measurement is attached to the nearest name line. In a "parenthetical"
# book the species summary states the length with no anchor word, in brackets
# right after the name ("Entomobrya lanuginosa (2.0 mm; ...)"), so the first
# measurement in the bracket following each name is taken.


.monograph_specs <- list(
  # Stach (1957), Apterygotan Fauna of Poland, Neelidae and Dicyrtomidae. A key
  # whose couplets end "... Length 0,5 mm." then a "Genus species Author, Year"
  # line, plus fuller accounts headed the same way. Not in BETSI.
  stach_1957 = list(
    prefix   = "stach1957",
    strategy = "anchored",
    name_re  = "^\\s*([A-Z][a-z]{2,})\\s+([a-z][a-z-]{2,})\\b.*\\b1[89][0-9]{2}\\b",
    window   = 10L
  ),
  # Hopkin (2007), Key to the Collembola of Britain and Ireland. Numbered species
  # summaries "186. Entomobrya lanuginosa (2.0 mm; ...)"; also all-caps terminals
  # "178. STENAPHORURA QUADRISPINA". Already in BETSI (validation source).
  hopkin_2007 = list(
    prefix   = "hopkin2007",
    strategy = "parenthetical"
  ),
  # Bretfeld (1999), Synopses vol. 2, Symphypleona. Accounts stating "Total length
  # 0.7 mm in females" under "Genus species Author, Year" headers. Already in
  # BETSI (validation source).
  bretfeld_1999 = list(
    prefix   = "bretfeld1999",
    strategy = "anchored",
    name_re  = "^\\s*([A-Z][a-z]{2,})\\s+([a-z][a-z-]{2,})\\b.*\\b1[89][0-9]{2}\\b",
    window   = 14L
  )
)


# A "Genus species Author, Year" line and an ordinary sentence that opens with a
# capitalised word both fit the binomial shape ("The genus ... 1900" reads as
# "The genus"), so a capitalised sentence-opener or document-structure word is
# rejected as a genus.
.monograph_stop_genus <- c(
  "The", "This", "These", "That", "A", "An", "In", "On", "At", "By", "As",
  "Since", "From", "With", "For", "All", "Both", "Its", "Their", "Only",
  "Also", "Now", "Here", "When", "After", "Before", "Genus", "Family",
  "Order", "Suborder", "Tribe", "Subfamily", "Figure", "Fig", "Table",
  "Plate", "Insects", "Length", "Total", "Body", "Head", "Type")

# The binomial from a matched name line: first capitalised token + next token,
# normalised to "Genus species" and OCR-cleaned. Returns NA if it is not a
# plausible binomial once cleaned.
.monograph_name <- function(line, name_re) {
  m <- regmatches(line, regexec(name_re, line, perl = TRUE))[[1L]]
  if (length(m) < 3L) return(NA_character_)
  genus <- m[2L]
  epithet <- m[3L]
  genus <- paste0(toupper(substr(genus, 1L, 1L)),
                  tolower(substr(genus, 2L, nchar(genus))))
  if (genus %in% .monograph_stop_genus) return(NA_character_)
  epithet <- tolower(epithet)
  nm <- paste(genus, epithet)
  if (.is_binomial(nm)) nm else NA_character_
}


# Anchored layout: every clause is read by the body-length core, and each
# measurement is attached to the nearest species-name line within the window.
.monograph_anchored <- function(lines, spec) {
  name_at <- integer(0L)
  name_of <- character(0L)
  for (i in seq_along(lines)) {
    if (grepl(spec$name_re, lines[i], perl = TRUE)) {
      nm <- .monograph_name(lines[i], spec$name_re)
      if (!is.na(nm)) { name_at <- c(name_at, i); name_of <- c(name_of, nm) }
    }
  }
  if (!length(name_at)) return(NULL)

  out <- list()
  for (i in seq_along(lines)) {
    e <- .plazi_body_length(lines[i])
    if (is.null(e)) next
    d <- abs(name_at - i)
    j <- which(d == min(d))
    if (min(d) > spec$window) next
    # nearest name wins; a key couplet ("... Length X mm.\nGenus species Year")
    # is settled by distance, so the tie-break only fires on an exact tie and
    # prefers the preceding name, the account-header convention
    j <- j[which.max(name_at[j] <= i)]
    for (r in seq_len(nrow(e))) {
      out[[length(out) + 1L]] <- data.frame(
        canonical_name = name_of[j], lo = e$lo[r], hi = e$hi[r],
        stringsAsFactors = FALSE)
    }
  }
  if (!length(out)) return(NULL)
  do.call(rbind, out)
}


# Parenthetical layout: a species summary states its length with no anchor word,
# in the bracket immediately after the name ("Entomobrya lanuginosa (2.0 mm;
# ...)"). The numbered names sit mid-line, after key-couplet dotted leaders, so
# the text is read whole rather than by line, and the first measurement inside
# each name's bracket is taken.
.monograph_parenthetical <- function(lines, spec) {
  txt <- gsub("\\s+", " ", paste(lines, collapse = " "))
  pat <- paste0("([A-Z][A-Za-z]+)\\s+([A-Za-z]{3,})\\s*",
                "\\(([^)]*?(?:mm|\u00b5m|\u03bcm|um)[^)]*?)\\)")
  blocks <- regmatches(txt, gregexpr(pat, txt, perl = TRUE))[[1L]]
  if (!length(blocks)) return(NULL)

  out <- list()
  for (b in blocks) {
    caps <- regmatches(b, regexec(pat, b, perl = TRUE))[[1L]]
    if (length(caps) < 4L) next
    genus <- paste0(toupper(substr(caps[2L], 1L, 1L)),
                    tolower(substr(caps[2L], 2L, nchar(caps[2L]))))
    if (genus %in% .monograph_stop_genus) next
    nm <- paste(genus, tolower(caps[3L]))
    if (!.is_binomial(nm)) next
    # a species summary leads its bracket with the body length ("(2.0 mm; ...)"),
    # optionally behind a qualifier; a bracket whose measurement sits later
    # ("(setae fused, 0.1 mm)") is not a body length, which drops the handful of
    # "Word word (... mm)" false positives that pass the binomial shape test.
    lead <- sub("^\\s*(up to|about|c[.a]?\\.?|ca\\.|~|<|>|=)?\\s*", "", caps[4L],
                perl = TRUE, ignore.case = TRUE)
    if (!grepl(paste0("^", .plazi_meas), lead, perl = TRUE)) next
    mc <- regmatches(caps[4L], regexec(.plazi_meas, caps[4L], perl = TRUE))[[1L]]
    if (length(mc) < 4L) next
    vals <- c(.plazi_to_num(mc[2L]), .plazi_to_num(mc[3L]))
    vals <- vals[!is.na(vals)]
    if (!length(vals)) next
    if (tolower(mc[4L]) != "mm") vals <- vals / 1000
    out[[length(out) + 1L]] <- data.frame(
      canonical_name = nm, lo = min(vals), hi = max(vals),
      stringsAsFactors = FALSE)
  }
  if (!length(out)) return(NULL)
  do.call(rbind, out)
}


#' Mine body length (mm) from an OCR'd taxonomic monograph
#'
#' Reads the plain text of one Collembola monograph and returns a body length
#' per species, using the source's layout spec to attach each measurement to a
#' name. The measurement classification is the same core the
#' `plazi_collembola_body_length` enrichment uses. `<prefix>_body_length_mm` is the median of the recorded
#' values, `<prefix>_body_length_min_mm` / `<prefix>_body_length_max_mm` bound
#' them, and `<prefix>_body_length_n` is the number of measurements behind the
#' species' value.
#'
#' @param path Character. Path to the monograph text file (an OCR export), or a
#'   directory holding one `.txt`.
#' @param source Character. Which monograph, one of `names(.monograph_specs)`:
#'   `"stach_1957"`, `"hopkin_2007"`, `"bretfeld_1999"`.
#' @return data.frame with `canonical_name` and the `<prefix>_body_length_*`
#'   columns, one row per species.
#' @export
parse_monograph_body_length <- function(path, source) {
  spec <- .monograph_specs[[source]]
  if (is.null(spec)) {
    stop("parse_monograph_body_length: unknown source '", source,
         "'. Known: ", paste(names(.monograph_specs), collapse = ", "),
         call. = FALSE)
  }
  file <- if (dir.exists(path)) {
    f <- list.files(path, pattern = "(?i)\\.txt$", full.names = TRUE)
    if (!length(f)) stop("No .txt found in: ", path, call. = FALSE)
    f[[1L]]
  } else {
    path
  }
  txt <- readLines(file, warn = FALSE, encoding = "UTF-8")
  # OCR / pdftotext output carries stray non-UTF-8 bytes; drop them so the
  # regex passes cleanly (keeps valid UTF-8 such as the micrometre sign).
  txt <- iconv(txt, from = "UTF-8", to = "UTF-8", sub = "")

  hits <- if (identical(spec$strategy, "parenthetical")) {
    .monograph_parenthetical(txt, spec)
  } else {
    .monograph_anchored(txt, spec)
  }
  if (is.null(hits) || !NROW(hits)) {
    stop("parse_monograph_body_length: no body lengths extracted for ", source,
         ".", call. = FALSE)
  }
  hits <- hits[hits$lo >= .plazi_bounds[1L] & hits$hi <= .plazi_bounds[2L],
               , drop = FALSE]
  hits <- hits[.is_binomial(hits$canonical_name), , drop = FALSE]
  if (!NROW(hits)) {
    stop("parse_monograph_body_length: no measurement resolved to a binomial.",
         call. = FALSE)
  }

  p <- spec$prefix
  by <- split(seq_len(NROW(hits)), hits$canonical_name)
  out <- data.frame(canonical_name = names(by), stringsAsFactors = FALSE,
                    row.names = NULL)
  out[[paste0(p, "_body_length_mm")]] <- vapply(by, function(i)
    round(stats::median(c(hits$lo[i], hits$hi[i])), 3L), 0)
  out[[paste0(p, "_body_length_min_mm")]] <- vapply(by, function(i)
    min(hits$lo[i]), 0)
  out[[paste0(p, "_body_length_max_mm")]] <- vapply(by, function(i)
    max(hits$hi[i]), 0)
  out[[paste0(p, "_body_length_n")]] <- vapply(by, length, 0L)
  out[order(out$canonical_name), , drop = FALSE]
}


#' Aggregate the frozen monograph body-length extraction to one row per species
#'
#' Reads the long-format frozen extraction (`canonical_name`, `source`, and the
#' `body_length_*` columns; one row per species per monograph, as
#' [parse_monograph_body_length()] produces for each book) and aggregates across
#' the source monographs to one row per species. This is the build reader for the
#' `monograph_collembola_body_length` enrichment: the source monographs are
#' copyrighted, so the mining runs once and only the extracted values -- facts,
#' not the book text -- are frozen and redistributed. `monograph_body_length_mm`
#' is the median of the per-monograph medians, `monograph_body_length_min_mm` /
#' `monograph_body_length_max_mm` bound the recorded values,
#' `monograph_body_length_n` is the total measurement count and
#' `monograph_body_length_sources` the number of monographs behind the value.
#'
#' @param path Character. Path to the frozen extraction `.csv`, or a directory
#'   containing one `.csv`.
#' @return data.frame with `canonical_name` and the `monograph_body_length_*`
#'   columns, one row per species.
#' @export
parse_monograph_collembola <- function(path) {
  file <- if (dir.exists(path)) {
    f <- list.files(path, pattern = "(?i)\\.csv$", full.names = TRUE)
    if (!length(f)) stop("No .csv found in: ", path, call. = FALSE)
    f[[1L]]
  } else {
    path
  }
  d <- utils::read.csv(file, stringsAsFactors = FALSE)
  need <- c("canonical_name", "source", "body_length_mm",
            "body_length_min_mm", "body_length_max_mm", "body_length_n")
  miss <- setdiff(need, names(d))
  if (length(miss)) {
    stop("parse_monograph_collembola: missing column(s): ",
         paste(miss, collapse = ", "), call. = FALSE)
  }
  d <- d[.is_binomial(d$canonical_name), , drop = FALSE]
  if (!nrow(d)) {
    stop("parse_monograph_collembola: no binomial rows.", call. = FALSE)
  }

  by <- split(seq_len(nrow(d)), d$canonical_name)
  out <- data.frame(
    canonical_name                = names(by),
    monograph_body_length_mm      = vapply(by, function(i)
      round(stats::median(d$body_length_mm[i]), 3L), 0),
    monograph_body_length_min_mm  = vapply(by, function(i)
      min(d$body_length_min_mm[i]), 0),
    monograph_body_length_max_mm  = vapply(by, function(i)
      max(d$body_length_max_mm[i]), 0),
    monograph_body_length_n       = vapply(by, function(i)
      sum(d$body_length_n[i]), 0L),
    monograph_body_length_sources = vapply(by, function(i)
      length(unique(d$source[i])), 0L),
    stringsAsFactors = FALSE, row.names = NULL)
  out[order(out$canonical_name), , drop = FALSE]
}
