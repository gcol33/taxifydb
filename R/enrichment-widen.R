# Shared widening primitives for enrichment parsers.
#
# taxify keeps every column a source provides: parsers never drop trait columns
# at build time. Two source shapes need two primitives, both additive on top of
# a parser's curated (nicely-named, unit-converted, recoded) columns:
#
#   * wide  (one row per record, columns = fields) -> .append_all_cols()
#   * long  (name, trait, value rows)             -> .pivot_species_traits(keep_all=)
#
# Curated columns a parser builds by hand stay byte-identical (the trait
# registry references them by exact name); these primitives only *add* the
# remaining source columns, sanitized-named and aggregated to one row per
# species. To later exclude a class of columns (e.g. free-text references),
# filter in one place here rather than in every parser.


#' Coerce character data to valid UTF-8 without erroring
#'
#' Widening reads source columns the curated parsers never touched; some carry
#' latin1 or otherwise invalid-UTF-8 bytes that make gsub/table/sort throw
#' "invalid UTF-8" in a UTF-8 locale. Valid UTF-8 is kept as-is; strings that
#' are invalid as UTF-8 are reinterpreted as latin1 (correct for the many
#' latin1 trait sources); anything still unmappable is dropped byte-wise.
#' @noRd
.to_utf8 <- function(x) {
  x <- as.character(x)
  y <- iconv(x, "UTF-8", "UTF-8", sub = NA)          # NA where not valid UTF-8
  bad <- is.na(y) & !is.na(x)
  if (any(bad)) y[bad] <- iconv(x[bad], "latin1", "UTF-8", sub = "")
  still <- is.na(y) & !is.na(x)
  if (any(still)) y[still] <- iconv(x[still], "UTF-8", "UTF-8", sub = "")
  y
}

#' Read a delimited file whose lines are not all in one encoding
#'
#' `read.csv(fileEncoding = )` applies a single encoding to the whole file, so
#' a source that is UTF-8 except for a run of latin1 lines cannot be read
#' correctly by any one setting: UTF-8 fails on the latin1 lines, latin1 turns
#' every real UTF-8 sequence into mojibake. The bytes are therefore read raw,
#' split into lines, and normalized per line by [.to_utf8()] before any
#' parsing, which decides the encoding for each line on its own evidence.
#'
#' @param path Character. File to read.
#' @param ... Passed to [utils::read.csv()] (`sep`, `quote`, ...).
#' @return data.frame, all character columns.
#' @noRd
.read_delim_utf8 <- function(path, ...) {
  raw <- readBin(path, "raw", file.size(path))
  if (any(raw == as.raw(0L))) {
    stop("The file contains a null byte, which no delimited reader carries.",
         call. = FALSE)
  }
  lines <- strsplit(rawToChar(raw), "\n", fixed = TRUE, useBytes = TRUE)[[1L]]
  lines <- .to_utf8(sub("\r$", "", lines, useBytes = TRUE))
  utils::read.csv(text = lines, colClasses = "character",
                  check.names = FALSE, ...)
}

#' First column present, in candidate order
#'
#' Parsers name the column they want as a list of spellings, most specific
#' first, because sources rename fields between releases. That list is a
#' preference order and has to be read as one: `intersect(names(df), cands)`
#' returns its matches in the order of its FIRST argument, so it yields
#' whichever candidate the source happens to put leftmost, not the preferred
#' one. A table carrying both its own row id and a foreign key then joins on
#' the row id, which matches partially, silently, and wrongly.
#'
#' @param x A data.frame, or a character vector of available names.
#' @param candidates Character. Column names in order of preference.
#' @param fallback Value returned when no candidate is present.
#' @return The preferred candidate present in `x`, else `fallback`.
#' @noRd
.first_col <- function(x, candidates, fallback = NULL) {
  nms <- if (is.data.frame(x)) names(x) else as.character(x)
  hit <- candidates[candidates %in% nms]
  if (length(hit) == 0L) fallback else hit[[1L]]
}

#' Sanitize a raw column/trait label to a snake_case identifier
#' @noRd
.sanitize_col <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x[is.na(x) | !nzchar(x)] <- "col"
  x
}

#' Are most non-missing values numeric? (drives num-vs-char inference)
#' @noRd
.mostly_numeric <- function(v, thresh = 0.8) {
  v <- .to_utf8(v)
  v <- v[!is.na(v) & nzchar(trimws(v))]
  if (!length(v)) return(FALSE)
  ok <- suppressWarnings(!is.na(as.numeric(gsub("[^0-9eE.+-]", "", v))))
  mean(ok) >= thresh
}

#' Is a sanitized column/trait name pure bookkeeping (not species data)?
#'
#' Excluded from auto-widening: free-text references/citations, URLs/DOIs,
#' internal row and join IDs, and the raw name columns replaced by
#' `canonical_name`. Trait, measurement, flag, distribution and taxonomy
#' columns are kept. This is the single place to adjust what "no dropping"
#' means; a parser's curated columns are added before this runs and are never
#' subject to it.
#'
#' Darwin Core writes its identifiers in camel case (`taxonID`, `locationID`),
#' which sanitizes to a bare `taxonid` with no separator for `.*_id` to match,
#' so those spellings are named here too. They are listed rather than caught by
#' a general `.*id` because ordinary trait values end in the same two letters
#' (`hybrid`, `diploid`, `humid`, `fluid`).
#' @noRd
.is_bookkeeping_col <- function(sane) {
  dwc_entity <- paste0(
    "taxon|location|event|occurrence|dataset|record|organism|",
    "collection|institution|catalog|catalogue|resource|material_?sample|",
    "measurement|identification|gbif|",
    "(accepted_?|original_?|parent_?)?name_?usage")
  grepl(paste0(
    "^(id[0-9]*|.*_id[0-9]*|(", dwc_entity, ")id[0-9]*|",
    "references?|refs?|citations?|source_refs?|",
    "bibliography|literature|doi|url|weblink|web_link|link|",
    "species|genus|subgenus|scientific_?name|taxon_?name|species_?name|",
    "binomial|canonical_?name|accepted_?name|name|taxon)$"), sane) |
    grepl("(^|_)url(_|$)|https?|www_", sane)
}

#' First free output name given a set of taken names (append _2, _3, ...)
#' @noRd
.uniq_colname <- function(nm, taken) {
  base <- nm
  i <- 1L
  while (nm %in% taken) {
    i <- i + 1L
    nm <- paste0(base, "_", i)
  }
  nm
}

#' Coerce a raw character vector of numeric-ish tokens to numeric
#' @noRd
.as_num_loose <- function(v) {
  suppressWarnings(as.numeric(gsub("[^0-9eE.+-]", "", .to_utf8(v))))
}

# ---- within-source numeric spread ------------------------------------------
#
# Where a source has several records per species (population/individual/life-
# stage measurements), collapsing to a single median discards the range. These
# helpers keep the median as the headline value (byte-identical to the old
# aggregate) AND, only where some species shows a genuine range (max > min), add
# <col>_min / <col>_max / <col>_n so the spread survives to the runtime and
# add_trait() can report it. Gating on the range (not merely the record count)
# keeps 1-row-per-species sources lean and also drops degenerate columns where
# repeated records only restate one value (e.g. an imputed value duplicated
# across accidental duplicate rows) -- there min==max carries nothing beyond the
# median. taxify's add_trait() looks for these companion columns and widens its
# reported range to them automatically.

#' Per-group numeric spread: headline value, min, max, count of finite values
#'
#' Returns a data.frame keyed by the sorted unique groups, with `val`, `min`,
#' `max`, `n`. Non-finite values are dropped before every statistic.
#'
#' `reduce` picks the statistic behind `val`. The median suits a continuous
#' quantity measured repeatedly (populations, accessions, life stages), where a
#' value between two records is itself a valid value. It does not suit a
#' discrete count whose records are cytotype variants of one species, because it
#' interpolates: a diploid 2n = 10 and a tetraploid 2n = 20 give 15, which
#' neither record reports. `min`, `max` and `mean` differ in which record they
#' name, but only `mean` shares the median's habit of inventing one; `min` and
#' `max` always return a value some record actually carries.
#' @noRd
.num_group_spread <- function(value, group,
                              reduce = c("median", "min", "max", "mean")) {
  reduce <- match.arg(reduce)
  fn <- switch(reduce, median = stats::median, min = min, max = max, mean = mean)
  v  <- suppressWarnings(as.numeric(value))
  g  <- as.character(group)
  ok <- is.finite(v) & !is.na(g) & nzchar(g)
  v  <- v[ok]; g <- g[ok]
  us <- sort(unique(g))
  if (!length(us)) {
    return(data.frame(group = character(0), val = numeric(0),
                      min = numeric(0), max = numeric(0), n = integer(0),
                      stringsAsFactors = FALSE))
  }
  idx  <- split(seq_along(v), factor(g, levels = us))
  stat <- function(f) vapply(idx, function(i) f(v[i]), numeric(1L))
  data.frame(group = us,
             val = stat(fn), min = stat(min), max = stat(max),
             n   = vapply(idx, length, integer(1L)),
             stringsAsFactors = FALSE)
}

#' Attach a collapsed numeric column (headline + gated spread) to a curated output
#'
#' `spread` is a `.num_group_spread()` result; `keys` aligns its groups to the
#' rows of `out`. Always sets `out[[oc]]` to the headline value; adds `<oc>_min`,
#' `<oc>_max`, `<oc>_n` only when some species shows a genuine range
#' (`max > min`). Gating on the range, not merely on the record count, keeps out
#' degenerate columns where several records repeat one value (e.g. an imputed
#' value duplicated across accidental duplicate rows) -- there the spread carries
#' no information beyond the headline.
#' @noRd
.attach_num_spread <- function(out, oc, spread, keys) {
  mi <- match(keys, spread$group)
  out[[oc]] <- spread$val[mi]
  if (any(spread$max > spread$min, na.rm = TRUE)) {
    out[[paste0(oc, "_min")]] <- spread$min[mi]
    out[[paste0(oc, "_max")]] <- spread$max[mi]
    out[[paste0(oc, "_n")]]   <- as.integer(spread$n[mi])
  }
  out
}

#' Collapse a data.frame's numeric columns by key to median + gated spread
#'
#' Drop-in replacement for the recurring `aggregate(df[cols], by = key,
#' FUN = median)` idiom that de-duplicates it and, wherever a column shows a
#' genuine within-key range (`max > min` for some key), also emits `<col>_min` /
#' `<col>_max` / `<col>_n`. Columns whose repeated records only ever restate one
#' value get the median alone -- their spread would be pure noise. Returns a
#' data.frame keyed by the unique `key` values.
#' @noRd
.aggregate_spread <- function(df, cols, key = "canonical_name") {
  k    <- as.character(df[[key]])
  keep <- !is.na(k) & nzchar(k)
  k    <- k[keep]; df <- df[keep, , drop = FALSE]
  us   <- sort(unique(k))
  out  <- stats::setNames(data.frame(us, stringsAsFactors = FALSE), key)
  if (!length(us)) return(out)
  idx  <- split(seq_along(k), factor(k, levels = us))
  for (cc in cols) {
    v   <- suppressWarnings(as.numeric(df[[cc]]))
    fin <- lapply(idx, function(i) { z <- v[i]; z[is.finite(z)] })
    n   <- vapply(fin, length, integer(1L))
    mn  <- vapply(fin, function(z) if (!length(z)) NA_real_ else min(z), numeric(1L))
    mx  <- vapply(fin, function(z) if (!length(z)) NA_real_ else max(z), numeric(1L))
    out[[cc]] <- vapply(fin, function(z) if (!length(z)) NA_real_ else stats::median(z),
                        numeric(1L))
    if (any(mx > mn, na.rm = TRUE)) {
      out[[paste0(cc, "_min")]] <- mn
      out[[paste0(cc, "_max")]] <- mx
      out[[paste0(cc, "_n")]]   <- n
    }
  }
  out
}

#' Resolve squish-matched target labels to their actual `df` column names
#'
#' `.squish_pick()` selects a column by stripping every non-alphanumeric
#' character (case-insensitive) from both the target label and the column names.
#' A parser that consumed columns that way cannot list exact names for `used`
#' (the real header may punctuate differently); this returns the real column
#' names a set of such targets resolves to, so `.append_all_cols()` skips them.
#' @noRd
.squish_used <- function(df, targets) {
  sq <- function(s) gsub("[^a-z0-9]", "", tolower(s))
  names(df)[sq(names(df)) %in% sq(targets)]
}

#' Append every un-consumed source column to a curated wide output
#'
#' `out` is a parser's curated data.frame (one row per species, keyed on
#' `canonical_name`). `df` is the raw wide source and `name` is the cleaned
#' species name for each of its rows (same cleaning used to build
#' `out$canonical_name`). Every `df` column whose sanitized name is not already
#' in `out` (and not listed in `used`, the source columns the parser already
#' consumed) is appended: numeric columns aggregated to the per-species median,
#' the rest to the per-species mode. `num_cols` / `cat_cols` force a column's
#' type; anything else is inferred.
#' @noRd
.append_all_cols <- function(out, df, name, used = character(0),
                             num_cols = character(0),
                             cat_cols = character(0),
                             group = NULL, group_row = NULL) {
  stopifnot("canonical_name" %in% names(out))
  name <- trimws(.to_utf8(name))
  keep <- !is.na(name) & nzchar(name)

  # Group-keyed enrichments (griis by country, wcvp by tdwg, ...) have several
  # rows per species; extra columns must aggregate on (name, group), not name
  # alone, and rejoin on the same pair.
  if (!is.null(group)) {
    stopifnot(group %in% names(out), length(group_row) == length(name))
    gr    <- .to_utf8(group_row)[keep]
    src_k <- paste(name[keep], gr, sep = "\r")
    out_k <- paste(out$canonical_name, as.character(out[[group]]), sep = "\r")
  } else {
    src_k <- name[keep]
    out_k <- out$canonical_name
  }
  df <- df[keep, , drop = FALSE]

  used_sane     <- .sanitize_col(used)
  existing_sane <- .sanitize_col(names(out))
  num_sane      <- .sanitize_col(num_cols)
  cat_sane      <- .sanitize_col(cat_cols)

  for (j in seq_along(df)) {
    col_sane <- .sanitize_col(names(df)[j])
    if (col_sane %in% c(used_sane, existing_sane, "canonical_name")) next
    if (.is_bookkeeping_col(col_sane)) next

    v  <- df[[j]]
    oc <- .uniq_colname(col_sane, names(out))
    as_num <- if (col_sane %in% num_sane) TRUE
              else if (col_sane %in% cat_sane) FALSE
              else .mostly_numeric(v)

    if (as_num) {
      spread <- .num_group_spread(.as_num_loose(v), src_k)
      out    <- .attach_num_spread(out, oc, spread, out_k)
    } else {
      vv <- trimws(.to_utf8(v))
      vv[!nzchar(vv) | vv == "NA"] <- NA_character_
      agg <- tapply(vv, src_k, .cat_mode)
      out[[oc]] <- as.character(agg[out_k])
    }
  }
  out
}
