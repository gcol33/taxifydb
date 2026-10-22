# Controlled trait vocabularies and their enrichment crosswalks.
#
# Each vocabulary is a frozen concept table plus a crosswalk that maps
# enrichment columns (the trait axis) and categorical values onto its concepts.
# The crosswalk stores concept keys only; identifiers are resolved from the
# concept table, so a key can never drift from its identifier. A trait axis
# with no faithful concept is left out of the crosswalk rather than forced onto
# a near-miss: the column keeps its own name and carries no identifier.
#
# Vocabularies, all under inst/extdata:
#   * tsita -- T-SITA (Pey et al. 2014, PLoS ONE, doi:10.1371/journal.pone.0108985,
#              CC BY), the soil-invertebrate thesaurus BETSI is built on: 71
#              traits + 24 ecological preferences with stable ARK URIs.
#              tsita_vocab.csv is produced by data-raw/tsita_vocab.R from the
#              live Opentheso scheme (ark:/66666/th558); concepts are keyed by
#              prefLabel and identified by URI (see gcol33/taxifydb#42).
#   * fwt   -- the freshwater trait vocabulary: trait axes for freshwater
#              organisms and, under each axis, the classes each definition
#              source uses (Tachet et al. 2010; Vieira et al. 2006, USGS DS-187;
#              Moog 2017; Barinova 2025; Balon 1975; ...). A class keeps its
#              source's own definition and, where the source gives one, its
#              bounds on the axis's canonical unit, so classes from different
#              sources are compared by overlap instead of being merged.
#              exact_match records a class another source defines identically;
#              broader records a coarser class. Concepts are keyed and
#              identified by id.

.vocab_schemes <- list(
  tsita = list(
    vocab      = "tsita_vocab.csv",
    crosswalk  = "tsita_crosswalk.csv",
    key_col    = "prefLabel",
    id_col     = "uri",
    regenerate = "data-raw/tsita_vocab.R",
    thesaurus  = paste0("T-SITA: A thesaurus for soil invertebrate trait-based ",
                        "approaches (Pey et al. 2014)"),
    scheme_uri = "https://ark.cefe.cnrs.fr/ark:/66666/th558",
    trait_fields = c(label = "trait_label", id = "trait_uri"),
    value_fields = c(label = "label", id = "uri")
  ),
  fwt = list(
    vocab      = "fwt_vocab.csv",
    crosswalk  = "fwt_crosswalk.csv",
    key_col    = "id",
    id_col     = "id",
    regenerate = "the OFT build_fwt_vocab.py script",
    thesaurus  = "Freshwater trait vocabulary (OFT)",
    scheme_uri = NULL,
    trait_fields = c(label = "trait_label"),
    value_fields = c(label = "label")
  )
)

.vocab_cache <- new.env(parent = emptyenv())

.vocab_scheme <- function(scheme) {
  s <- .vocab_schemes[[scheme]]
  if (is.null(s)) {
    stop("Unknown trait vocabulary '", scheme, "'. Known: ",
         paste(names(.vocab_schemes), collapse = ", "), call. = FALSE)
  }
  s
}

.vocab_extdata <- function(scheme, file) {
  f <- system.file("extdata", file, package = "taxifydb")
  if (!nzchar(f)) {
    stop("taxifydb data asset '", file, "' not found. Regenerate the '", scheme,
         "' vocabulary with ", .vocab_scheme(scheme)$regenerate, ".", call. = FALSE)
  }
  f
}


#' A controlled trait vocabulary as a flat concept table
#'
#' Loads a frozen trait vocabulary from `inst/extdata`, one row per concept.
#'
#' * `"tsita"`: the T-SITA thesaurus (Pey et al. 2014). Columns `uri`, `id`,
#'   `prefLabel`, `top` (`Trait` or `Ecological_preference`), `depth`,
#'   `broader`, `definition`, `scopeNote`. `prefLabel` is unique and is the key
#'   the crosswalk uses.
#' * `"fwt"`: the freshwater trait vocabulary. Columns `id` (the key), `label`,
#'   `kind` (`axis` or `class`), `axis`, `scheme` (the definition source of a
#'   class), `scope` (`cross_group` or `within_group`), `groups` (organism
#'   groups the axis applies to, `;`-separated), `value_type`, `unit`, `lower`
#'   and `upper` (a class's bounds on its axis's unit, `NA` when the source
#'   gives none), `broader`, `exact_match`, `definition`, `source`.
#'
#' The result is cached for the session.
#'
#' @param scheme Character. Vocabulary name: `"tsita"` or `"fwt"`.
#' @return A data.frame, one row per concept.
#' @seealso [trait_crosswalk()]
#' @export
trait_vocab <- function(scheme) {
  s <- .vocab_scheme(scheme)
  key <- paste0(scheme, ":vocab")
  if (is.null(.vocab_cache[[key]])) {
    v <- utils::read.csv(.vocab_extdata(scheme, s$vocab),
                         stringsAsFactors = FALSE, colClasses = "character",
                         na.strings = "")
    for (col in intersect(c("depth"), names(v))) {
      v[[col]] <- suppressWarnings(as.integer(v[[col]]))
    }
    for (col in intersect(c("lower", "upper"), names(v))) {
      v[[col]] <- suppressWarnings(as.numeric(v[[col]]))
    }
    if (anyDuplicated(v[[s$key_col]])) {
      stop("Vocabulary '", scheme, "' has duplicated ", s$key_col, " values.",
           call. = FALSE)
    }
    .vocab_cache[[key]] <- v
  }
  .vocab_cache[[key]]
}


#' The crosswalk from enrichment columns to a trait vocabulary
#'
#' Loads a vocabulary's crosswalk and resolves each concept key against
#' [trait_vocab()], failing loudly on a key the vocabulary does not hold. A row
#' with an empty `raw_value` maps the column (the trait axis); a row with a
#' `raw_value` maps one categorical value of that column onto a concept.
#'
#' @param scheme Character. Vocabulary name: `"tsita"` or `"fwt"`.
#' @param enrichment Character or `NULL`. Restrict to one or more enrichment
#'   names; `NULL` (default) returns the whole crosswalk.
#' @return A data.frame with columns `enrichment`, `column`, `raw_value` (`NA`
#'   for an axis mapping), `concept` (the vocabulary key), `concept_id` (its
#'   identifier), and any further columns the crosswalk carries (`unit` for
#'   `"fwt"`: the unit a numeric column is recorded in).
#' @seealso [trait_vocab()]
#' @export
trait_crosswalk <- function(scheme, enrichment = NULL) {
  s <- .vocab_scheme(scheme)
  key <- paste0(scheme, ":xwalk")
  if (is.null(.vocab_cache[[key]])) {
    x <- utils::read.csv(.vocab_extdata(scheme, s$crosswalk),
                         stringsAsFactors = FALSE, colClasses = "character",
                         na.strings = "")
    v <- trait_vocab(scheme)
    bad <- setdiff(unique(x$concept), v[[s$key_col]])
    if (length(bad)) {
      stop("The '", scheme, "' crosswalk references concept(s) absent from ",
           "trait_vocab(\"", scheme, "\"): ", paste(bad, collapse = ", "),
           call. = FALSE)
    }
    x$concept_id <- v[[s$id_col]][match(x$concept, v[[s$key_col]])]
    .vocab_cache[[key]] <- x
  }
  x <- .vocab_cache[[key]]
  if (!is.null(enrichment)) x <- x[x$enrichment %in% enrichment, , drop = FALSE]
  x
}


#' Build one vocabulary's block for an enrichment's meta.json
#'
#' Assembles, from the crosswalk rows for `name`, the entry each mapped column
#' gets: its trait concept (and identifier where the vocabulary has one apart
#' from the key), the unit of a numeric column, and for a categorical column a
#' `values` map from each raw value to its concept. Returns `NULL` when the
#' enrichment has no crosswalk rows, so [drop_empty_fields()] omits the field.
#'
#' @param scheme Character. Vocabulary name.
#' @param name Character. Enrichment identifier.
#' @param columns Character or `NULL`. The columns actually present in the built
#'   data. When given, only mapped columns present in the build are emitted, and
#'   a crosswalk column missing from the build raises a warning (a stale
#'   crosswalk). `NULL` emits every mapped column.
#' @return A named list for the meta field, or `NULL`.
#' @noRd
.vocab_enrichment_meta <- function(scheme, name, columns = NULL) {
  s <- .vocab_scheme(scheme)
  x <- trait_crosswalk(scheme, name)
  if (!nrow(x)) return(NULL)

  if (!is.null(columns)) {
    absent <- setdiff(unique(x$column), columns)
    if (length(absent)) {
      warning("[", scheme, "] enrichment '", name, "': crosswalk column(s) not ",
              "in the built data: ", paste(absent, collapse = ", "),
              ". Update inst/extdata/", s$crosswalk, ".", call. = FALSE)
    }
    x <- x[x$column %in% columns, , drop = FALSE]
    if (!nrow(x)) return(NULL)
  }

  concept_fields <- function(row, fields) {
    out <- list()
    out[[fields[["label"]]]] <- row$concept[[1L]]
    if (!is.na(fields["id"])) out[[fields[["id"]]]] <- row$concept_id[[1L]]
    out
  }

  cols <- list()
  for (col in unique(x$column)) {
    xi   <- x[x$column == col, , drop = FALSE]
    axis <- xi[is.na(xi$raw_value), , drop = FALSE]
    vals <- xi[!is.na(xi$raw_value), , drop = FALSE]
    if (!nrow(axis)) {
      warning("[", scheme, "] column '", col, "' in '", name, "' has value ",
              "mappings but no axis mapping; skipped.", call. = FALSE)
      next
    }
    entry <- concept_fields(axis[1L, , drop = FALSE], s$trait_fields)
    if ("unit" %in% names(axis) && !is.na(axis$unit[[1L]])) {
      entry$unit <- axis$unit[[1L]]
    }
    if (nrow(vals)) {
      vv <- list()
      for (k in seq_len(nrow(vals))) {
        vv[[vals$raw_value[[k]]]] <- concept_fields(vals[k, , drop = FALSE],
                                                    s$value_fields)
      }
      entry$values <- vv
    }
    cols[[col]] <- entry
  }
  if (!length(cols)) return(NULL)

  list(thesaurus = s$thesaurus, scheme_uri = s$scheme_uri, columns = cols)
}


# Every vocabulary's meta block for one enrichment, named by scheme; a scheme
# with no crosswalk rows for it is absent.
.vocab_meta_blocks <- function(name, columns = NULL) {
  blocks <- lapply(names(.vocab_schemes),
                   function(s) .vocab_enrichment_meta(s, name, columns))
  names(blocks) <- names(.vocab_schemes)
  blocks[!vapply(blocks, is.null, logical(1))]
}
