# Body length mined from Plazi taxonomic treatments.
#
# Plazi (TreatmentBank) marks up the taxonomic treatments of published papers
# and republishes each paper as a Darwin Core Archive through GBIF. The archive
# carries taxa.txt (one row per treated taxon) and description.txt (the prose of
# the treatment, split into typed sections: description, diagnosis, discussion,
# materials_examined and so on). The morphological sections state a body length
# in mm or micrometres for most newly described species, so the archives are a
# per-species body-length source that grows with the primary literature rather
# than with the compendia.
#
# One trait of one class, so the enrichment is named
# `plazi_collembola_body_length`, not `plazi_collembola`: a Plazi treatment
# states far more than body length, and none of the rest is extracted here.
#
# It complements `betsi_collembola_body_length`, the body-length slice of a
# fixed 2017 export of the European compendia. The two barely overlap: BETSI
# covers 1,258 species drawn from Gisin, Stach, Fjellberg, Bretfeld, Hopkin and
# Thibaud, while these treatments are dominated by recent descriptions and by
# faunas outside Europe.
#
# Extraction is the hard part, because a treatment measures the antenna, furca,
# manubrium, mucro, claw, postantennal organ and individual setae in exactly the
# sentence style it uses for the whole animal. Every candidate number is
# therefore classified by the words immediately in front of it: an organ term
# rejects it, a body term accepts it, and an unanchored number is dropped. Three
# further guards remove numbers that are real body lengths but not this
# species'  adult body length: a comparison with another taxon ("0.9-1.3 mm in
# O. cincta"), the trunk-length convention ("body length without head and
# furca", a different quantity), and juvenile or inequality qualifiers.
#
# Validated against the BETSI export on the 35 species the two share: Pearson
# r = 0.906, Spearman 0.955, median difference -0.050 mm, median absolute
# difference 0.150 mm. The residual scatter is definitional rather than a
# parsing failure -- the sources disagree about whether the furca and the head
# count -- which was checked by reading the clause behind every one of the ten
# largest disagreements.


# One number, or a range, followed by a unit. Ranges use any of the dash
# characters the literature mixes freely, or the word "to".
.plazi_num  <- "[0-9]+(?:[.,][0-9]+)?"
.plazi_sep  <- "\\s*(?:[-\u2010\u2011\u2012\u2013\u2014\u2015\u2212~]|to)\\s*"
.plazi_unit <- "\\s*(mm|\u00b5m|\u03bcm|um)\\b"
.plazi_meas <- sprintf("(%s)(?:%s(%s))?%s", .plazi_num, .plazi_sep,
                       .plazi_num, .plazi_unit)

# Organ stems long enough to be matched as substrings ("macrosetae" has no word
# boundary before "seta", so an anchored pattern would miss it).
.plazi_organ_sub <- paste0(
  "antenn|furc|manubri|dentes|mucro|unguic|ungui|chaet|seta|sensill|",
  "postantennal|tenacul|tibiotars|empodi|vesicl|ocell|tubercl|papill|",
  "retinacul|trichobothri|bothriotri|collophor|granul|denticl|maxill|",
  "mandib|clyp|labr|labi|tergit|sternit|femur|femora|thorax|abdomen|",
  "diamet|microsens|macrosens|elater|corniculi|filament")
# Short or ambiguous terms, matched as whole words only.
.plazi_organ_word <- paste0(
  "dens|leg|legs|eye|eyes|lobe|lobes|neck|pore|pores|hair|hairs|claw|claws|",
  "scale|scales|spine|spines|head|width|wide|apical|basal|rami|ant|pao|",
  "abd|th|ventral tube|anal|dorsal|blade|tooth|teeth|segment|segments")
.plazi_organ <- sprintf("(?i)(%s)|(?i)\\b(%s)\\b",
                        .plazi_organ_sub, .plazi_organ_word)

.plazi_body <- paste0(
  "(?i)(",
  "bod(y|ies)\\s*(length|size)",
  "|length\\s+of\\s+(the\\s+)?bod(y|ies)",
  "|total\\s+length",
  "|maximum\\s+length",
  "|(^|[.;:,]\\s*)(size|length)\\b",
  "|(holotype|paratype|syntype|female|male|adult|specimen)s?\\s*length",
  "|bod(y|ies)\\s*$",
  ")")

# A discussion section compares the treated species with its congeners, so a
# number sitting in such a clause may belong to a different taxon.
.plazi_compare <- paste0(
  "(?i)(versus|instead of|differ(s|ing)\\b|compared (to|with)|whereas|",
  "the new species|other species|congener|similar species|they mention|",
  "unlike\\b|in contrast)")
# An abbreviated genus directly after the number ("0.9 - 1.3 mm in O.") means
# the value is another taxon's. Anchored to the text following the measurement
# so a trailing figure reference ("1 mm, habitus as in Fig. 2") is left alone,
# and case-sensitive because the capital is the whole signal.
.plazi_compare_after <- "^\\s*(in|than|vs\\.?|versus)\\s+[A-Z]\\."
# "without head" is trunk length by the Entomobryidae convention, a different
# quantity. Antennae are excluded by convention everywhere, so "without
# antennae" is not a disqualifier.
.plazi_not_body <- "(?i)(without|excluding|excl\\.?|minus)\\s+(the\\s+)?head"
# What a body length is stated to exclude qualifies the measurement; it is not
# itself a measurement of that part.
.plazi_excl <- paste0(
  "(?i)\\b(without|excluding|excl\\.?|minus|not including|apart from)\\s+",
  "(the\\s+)?[a-z]+(\\s+(and|nor|or)\\s+[a-z]+)?")
.plazi_loose <- paste0(
  "(?i)([<>\u2264\u2265]\\s*$|",
  "\\b(juvenile|subadult|immature|larva)\\w*\\s*[,:]?\\s*$)")

# Plausibility window for a springtail, in mm. The largest known Collembola
# (Tetrodontophora bielanensis) reaches about 9 mm.
.plazi_bounds <- c(0.1, 12)

# Collembola is filed as a class in some treatments and as an order under
# Entognatha in others, so both ranks have to be checked.
.plazi_coll_orders <- c("poduromorpha", "entomobryomorpha", "symphypleona",
                        "neelipleona", "arthropleona", "metaxypleona",
                        "collembola")


.plazi_strip_parens <- function(x) {
  x <- gsub("\\([^()]*\\)", " ", x, perl = TRUE)
  x <- gsub("\\[[^][]*\\]", " ", x, perl = TRUE)
  gsub("\\s+", " ", x)
}

# A comma with exactly three trailing digits is a thousands separator
# ("1,250 um"); with one or two it is a decimal comma ("0,78 mm").
.plazi_to_num <- function(s) {
  s <- gsub("\\s", "", s)
  if (grepl("^[0-9]+,[0-9]{3}$", s)) {
    return(suppressWarnings(as.numeric(sub(",", "", s, fixed = TRUE))))
  }
  if (grepl("^[0-9]+,[0-9]{1,2}$", s)) {
    return(suppressWarnings(as.numeric(sub(",", ".", s, fixed = TRUE))))
  }
  suppressWarnings(as.numeric(s))
}


#' Pull body lengths (mm) out of one treatment section
#'
#' @param txt Character(1). The text of a `description.txt` section.
#' @param guards Logical. Apply the comparison, trunk-length and juvenile
#'   guards. `FALSE` keeps every body-anchored number, which is useful for
#'   measuring what the guards remove.
#' @return data.frame with `lo`, `hi` (mm) and the source `clause`, or `NULL`.
#' @noRd
.plazi_body_length <- function(txt, guards = TRUE) {
  if (length(txt) != 1L || is.na(txt) || !nzchar(txt)) return(NULL)
  clauses <- unlist(strsplit(txt, "(?<=[.;])\\s+", perl = TRUE))
  out <- list()

  for (cl in clauses) {
    if (guards && grepl(.plazi_compare, cl, perl = TRUE)) next
    m <- gregexpr(.plazi_meas, cl, perl = TRUE)[[1L]]
    if (m[1L] == -1L) next
    starts <- as.integer(m)
    lens <- attr(m, "match.length")
    prev_end <- 1L

    for (k in seq_along(starts)) {
      ctx <- substr(cl, prev_end, starts[k] - 1L)
      tok <- substr(cl, starts[k], starts[k] + lens[k] - 1L)
      prev_end <- starts[k] + lens[k]

      if (guards && grepl(.plazi_not_body, ctx, perl = TRUE)) next
      if (guards && grepl(.plazi_loose, ctx, perl = TRUE)) next
      if (guards && grepl(.plazi_compare_after,
                          substr(cl, prev_end, prev_end + 30L), perl = TRUE)) next

      ctx_s <- .plazi_strip_parens(ctx)
      # "body length without antennae 2.2 mm" measures the body, not the
      # antenna; drop the exclusion clause before the organ terms are read, so
      # it reads the same whether or not the paper parenthesised it. The
      # head/trunk case was already refused above, on the unstripped context.
      ctx_s <- gsub(.plazi_excl, " ", ctx_s, perl = TRUE)
      ctx_s <- substr(ctx_s, max(1L, nchar(ctx_s) - 70L), nchar(ctx_s))
      if (grepl(.plazi_organ, ctx_s, perl = TRUE)) next
      if (!grepl(.plazi_body, ctx_s, perl = TRUE)) next

      caps <- regmatches(tok, regexec(.plazi_meas, tok, perl = TRUE))[[1L]]
      if (length(caps) < 4L) next
      vals <- c(.plazi_to_num(caps[2L]), .plazi_to_num(caps[3L]))
      vals <- vals[!is.na(vals)]
      if (!length(vals)) next
      if (tolower(caps[4L]) != "mm") vals <- vals / 1000  # micrometres

      out[[length(out) + 1L]] <- data.frame(
        lo = min(vals), hi = max(vals), clause = substr(cl, 1L, 240L),
        stringsAsFactors = FALSE)
    }
  }
  if (!length(out)) return(NULL)
  do.call(rbind, out)
}


#' Read one Darwin Core Archive member as a data.frame
#' @noRd
.plazi_member <- function(zip, name, cols) {
  con <- unz(zip, name, open = "rb")
  on.exit(close(con), add = TRUE)
  raw <- readBin(con, "raw", n = 40e6)
  if (!length(raw)) return(NULL)
  txt <- rawToChar(raw)
  Encoding(txt) <- "UTF-8"
  d <- tryCatch(
    utils::read.delim(text = txt, sep = "\t", quote = "", comment.char = "",
                      stringsAsFactors = FALSE, check.names = FALSE,
                      colClasses = "character"),
    error = function(e) NULL)
  if (is.null(d) || !NROW(d)) return(NULL)
  for (k in setdiff(cols, names(d))) d[[k]] <- NA_character_
  d[, cols, drop = FALSE]
}


#' Read every harvested Plazi archive into taxa + description tables
#' @noRd
.plazi_read_archives <- function(dir) {
  zips <- list.files(dir, pattern = "\\.zip$", full.names = TRUE)
  if (!length(zips)) {
    stop("No Plazi Darwin Core archives found in: ", dir, call. = FALSE)
  }
  tcol <- c("taxonID", "canonicalName", "scientificName", "kingdom", "phylum",
            "class", "order", "family", "genus", "taxonRank")
  dcol <- c("taxonID", "type", "description")

  tl <- dl <- vector("list", length(zips))
  for (i in seq_along(zips)) {
    z <- zips[i]
    mem <- tryCatch(utils::unzip(z, list = TRUE)$Name,
                    error = function(e) character(0L))
    if (!all(c("taxa.txt", "description.txt") %in% mem)) next
    tx <- .plazi_member(z, "taxa.txt", tcol)
    de <- .plazi_member(z, "description.txt", dcol)
    if (is.null(tx) || is.null(de)) next
    tx$dataset <- basename(z)
    de$dataset <- basename(z)
    tl[[i]] <- tx
    dl[[i]] <- de
  }
  tax <- do.call(rbind, Filter(Negate(is.null), tl))
  desc <- do.call(rbind, Filter(Negate(is.null), dl))
  if (is.null(tax) || is.null(desc)) {
    stop("No readable taxa.txt/description.txt in: ", dir, call. = FALSE)
  }
  list(tax = tax, desc = desc)
}


#' Parse Plazi Collembola treatments into per-species body length
#'
#' Reads the Darwin Core archives harvested by [harvest_plazi_dwca()], keeps the
#' Collembola taxa, and extracts body length (mm) from the prose of their
#' treatments. `plazi_body_length_mm` is the median of the recorded values,
#' `plazi_body_length_min_mm` / `plazi_body_length_max_mm` bound them, and
#' `plazi_body_length_n` / `plazi_body_length_sources` are the number of
#' measurements and of distinct treatment papers behind the species' value.
#'
#' @param path Character. Directory holding the harvested `.zip` archives.
#' @param sections Character. Treatment section types to read. The default set
#'   is every section that was measured to carry body lengths; narrowing it to
#'   `c("description", "diagnosis")` raises precision slightly at the cost of
#'   about 6 percent of the species.
#' @return data.frame with `canonical_name` and the `plazi_body_length_*`
#'   columns, one row per species.
#' @export
parse_plazi_collembola_body_length <- function(
    path,
    sections = c("description", "diagnosis", "type_taxon", "discussion",
                 "biology_ecology", "materials_examined", "distribution",
                 "etymology")) {
  tabs <- .plazi_read_archives(path)
  tax <- tabs$tax
  desc <- tabs$desc

  is_coll <- (tolower(tax$class) %in% "collembola") |
    (tolower(tax$order) %in% .plazi_coll_orders) |
    (tolower(tax$phylum) %in% "collembola")
  is_coll[is.na(is_coll)] <- FALSE
  if (!any(is_coll)) {
    stop("parse_plazi_collembola: no Collembola taxa in the archives.",
         call. = FALSE)
  }

  keys <- paste(tax$dataset, tax$taxonID)[is_coll]
  cd <- desc[paste(desc$dataset, desc$taxonID) %in% keys &
               tolower(desc$type) %in% tolower(sections), , drop = FALSE]

  res <- vector("list", NROW(cd))
  for (i in seq_len(NROW(cd))) {
    e <- .plazi_body_length(cd$description[i])
    if (!is.null(e)) {
      e$dataset <- cd$dataset[i]
      e$taxonID <- cd$taxonID[i]
      res[[i]] <- e
    }
  }
  hits <- do.call(rbind, Filter(Negate(is.null), res))
  if (is.null(hits) || !NROW(hits)) {
    stop("parse_plazi_collembola: no body lengths extracted.", call. = FALSE)
  }
  hits <- hits[hits$lo >= .plazi_bounds[1L] &
                 hits$hi <= .plazi_bounds[2L], , drop = FALSE]

  tx <- tax[is_coll, , drop = FALSE]
  tx$k <- paste(tx$dataset, tx$taxonID)
  tx <- tx[!duplicated(tx$k), , drop = FALSE]
  hits$k <- paste(hits$dataset, hits$taxonID)
  hits <- merge(hits, tx[, c("k", "canonicalName", "scientificName")],
                by = "k", all.x = TRUE)

  nm <- trimws(ifelse(is.na(hits$canonicalName) | !nzchar(hits$canonicalName),
                      hits$scientificName, hits$canonicalName))
  hits$canonical_name <- ifelse(.is_binomial(nm), nm, NA_character_)
  hits <- hits[!is.na(hits$canonical_name), , drop = FALSE]
  if (!NROW(hits)) {
    stop("parse_plazi_collembola: no measurement resolved to a binomial.",
         call. = FALSE)
  }

  by <- split(seq_len(NROW(hits)), hits$canonical_name)
  out <- data.frame(
    canonical_name = names(by),
    plazi_body_length_mm = vapply(by, function(i)
      round(stats::median(c(hits$lo[i], hits$hi[i])), 3L), 0),
    plazi_body_length_min_mm = vapply(by, function(i) min(hits$lo[i]), 0),
    plazi_body_length_max_mm = vapply(by, function(i) max(hits$hi[i]), 0),
    plazi_body_length_n = vapply(by, length, 0L),
    plazi_body_length_sources = vapply(by, function(i)
      length(unique(hits$dataset[i])), 0L),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  out[order(out$canonical_name), , drop = FALSE]
}
