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
#' @noRd
.is_bookkeeping_col <- function(sane) {
  grepl(paste0(
    "^(id|.*_id|references?|refs?|citations?|source_refs?|",
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
      vv  <- .as_num_loose(v)
      agg <- tapply(vv, src_k, function(z) {
        z <- z[is.finite(z)]
        if (!length(z)) NA_real_ else stats::median(z)
      })
      out[[oc]] <- as.numeric(agg[out_k])
    } else {
      vv <- trimws(.to_utf8(v))
      vv[!nzchar(vv) | vv == "NA"] <- NA_character_
      agg <- tapply(vv, src_k, .cat_mode)
      out[[oc]] <- as.character(agg[out_k])
    }
  }
  out
}
