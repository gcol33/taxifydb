# T-SITA controlled-vocabulary crosswalk for the soil-fauna enrichments.
#
# BETSI and every soil-invertebrate trait database built around it speak T-SITA
# (Pey et al. 2014, PLoS ONE, doi:10.1371/journal.pone.0108985, CC BY): a
# thesaurus of 71 traits + 24 ecological preferences, each a concept with a
# definition and a stable ARK identifier, and for a categorical trait a fixed
# set of attribute-value concepts beneath it. taxifydb's Collembola / soil-fauna
# enrichments are assembled from open analogues (Ellers, ecomorphosis, the mined
# monographs, the INRAE matrices) that each name their traits their own way. This
# module gives them one shared vocabulary: it maps every enrichment column, and
# every categorical value, onto its T-SITA concept, so the built assets are
# interoperable with BETSI without carrying BETSI's data (see gcol33/taxifydb#42).
#
# Two data assets back it, both under inst/extdata:
#   * tsita_vocab.csv     -- the frozen thesaurus, one row per concept, produced
#                            by data-raw/tsita_vocab.R from the live Opentheso
#                            scheme (ark:/66666/th558). The single source of URIs.
#   * tsita_crosswalk.csv -- enrichment column (and value) -> T-SITA prefLabel.
#                            Stores labels only; URIs are resolved from the vocab
#                            so a label can never drift from its identifier.
#
# A trait axis with no faithful T-SITA concept (Ellers' biogeographic
# temperature-zone class, its thermal-niche breadth, pigmentation, pseudocelli)
# is deliberately left out of the crosswalk rather than forced onto a near-miss:
# the column keeps its own name and simply carries no T-SITA identifier.


.tsita_cache <- new.env(parent = emptyenv())

.tsita_extdata <- function(file) {
  f <- system.file("extdata", file, package = "taxifydb")
  if (!nzchar(f)) {
    stop("taxifydb data asset '", file, "' not found. For the vocabulary, run ",
         "data-raw/tsita_vocab.R to (re)generate it.", call. = FALSE)
  }
  f
}


#' The T-SITA thesaurus as a flat concept table
#'
#' Loads the frozen T-SITA vocabulary (`inst/extdata/tsita_vocab.csv`): one row
#' per concept, with its stable ARK URI, `prefLabel`, the top concept it sits
#' under (`Trait` or `Ecological_preference`), its `depth` below that top, the
#' `broader` parent URI, and the `definition` / `scopeNote`. `prefLabel` is
#' unique across the thesaurus, which is what lets the crosswalk key on labels
#' and resolve URIs from here. The result is cached for the session.
#'
#' @return A data.frame with columns `uri`, `id`, `prefLabel`, `top`, `depth`,
#'   `broader`, `definition`, `scopeNote`.
#' @seealso [tsita_crosswalk()]
#' @export
tsita_vocab <- function() {
  if (is.null(.tsita_cache$vocab)) {
    v <- utils::read.csv(.tsita_extdata("tsita_vocab.csv"),
                         stringsAsFactors = FALSE, colClasses = "character")
    v$depth <- suppressWarnings(as.integer(v$depth))
    .tsita_cache$vocab <- v
  }
  .tsita_cache$vocab
}


# Resolve a single T-SITA prefLabel to its vocabulary row, or stop.
.tsita_concept <- function(label) {
  v <- tsita_vocab()
  i <- match(label, v$prefLabel)
  if (is.na(i)) {
    stop("Unknown T-SITA concept label: '", label,
         "'. It is not a prefLabel in tsita_vocab().", call. = FALSE)
  }
  v[i, , drop = FALSE]
}


#' The enrichment-to-T-SITA crosswalk
#'
#' Loads `inst/extdata/tsita_crosswalk.csv` and resolves each `tsita_label` to
#' its concept URI against [tsita_vocab()], failing loudly if a label is not a
#' known T-SITA concept. Each row maps one enrichment column onto a T-SITA
#' concept: a row with an empty `raw_value` maps the column (the trait axis); a
#' row with a `raw_value` maps one categorical value of that column onto an
#' attribute concept.
#'
#' @param enrichment Character or `NULL`. Restrict to one or more enrichment
#'   names; `NULL` (default) returns the whole crosswalk.
#' @return A data.frame with columns `enrichment`, `column`, `raw_value`
#'   (`NA` for an axis mapping), `tsita_label`, `tsita_uri`.
#' @seealso [tsita_vocab()]
#' @export
tsita_crosswalk <- function(enrichment = NULL) {
  if (is.null(.tsita_cache$xwalk)) {
    x <- utils::read.csv(.tsita_extdata("tsita_crosswalk.csv"),
                         stringsAsFactors = FALSE, colClasses = "character",
                         na.strings = "")
    v <- tsita_vocab()
    bad <- setdiff(unique(x$tsita_label), v$prefLabel)
    if (length(bad)) {
      stop("tsita_crosswalk references label(s) absent from tsita_vocab(): ",
           paste(bad, collapse = ", "), call. = FALSE)
    }
    x$tsita_uri <- v$uri[match(x$tsita_label, v$prefLabel)]
    .tsita_cache$xwalk <- x
  }
  x <- .tsita_cache$xwalk
  if (!is.null(enrichment)) x <- x[x$enrichment %in% enrichment, , drop = FALSE]
  x
}


#' Build the `tsita` block for an enrichment's meta.json
#'
#' Assembles the T-SITA metadata an enrichment's `meta.json` sidecar carries,
#' from the crosswalk rows for `name`. Each mapped column becomes an entry with
#' its `trait_label` / `trait_uri`; a categorical column also gets a `values`
#' map from each raw value to its attribute `label` / `uri`. Returns `NULL` when
#' the enrichment has no crosswalk (so [drop_empty_fields()] omits the field).
#'
#' @param name Character. Enrichment identifier.
#' @param columns Character or `NULL`. The columns actually present in the built
#'   data. When given, only mapped columns present in the build are emitted, and
#'   a crosswalk column missing from the build raises a warning (a stale
#'   crosswalk). `NULL` emits every mapped column.
#' @return A named list for the `tsita` meta field, or `NULL`.
#' @noRd
.tsita_enrichment_meta <- function(name, columns = NULL) {
  x <- tsita_crosswalk(name)
  if (!nrow(x)) return(NULL)

  if (!is.null(columns)) {
    absent <- setdiff(unique(x$column), columns)
    if (length(absent)) {
      warning("[tsita] enrichment '", name, "': crosswalk column(s) not in the ",
              "built data: ", paste(absent, collapse = ", "),
              ". Update inst/extdata/tsita_crosswalk.csv.", call. = FALSE)
    }
    x <- x[x$column %in% columns, , drop = FALSE]
    if (!nrow(x)) return(NULL)
  }

  cols <- list()
  for (col in unique(x$column)) {
    xi   <- x[x$column == col, , drop = FALSE]
    axis <- xi[is.na(xi$raw_value), , drop = FALSE]
    vals <- xi[!is.na(xi$raw_value), , drop = FALSE]
    if (!nrow(axis)) {
      warning("[tsita] column '", col, "' in '", name, "' has value mappings but ",
              "no axis mapping; skipped.", call. = FALSE)
      next
    }
    entry <- list(trait_label = axis$tsita_label[[1L]],
                  trait_uri   = axis$tsita_uri[[1L]])
    if (nrow(vals)) {
      vv <- list()
      for (k in seq_len(nrow(vals))) {
        vv[[vals$raw_value[[k]]]] <- list(label = vals$tsita_label[[k]],
                                          uri   = vals$tsita_uri[[k]])
      }
      entry$values <- vv
    }
    cols[[col]] <- entry
  }
  if (!length(cols)) return(NULL)

  list(
    thesaurus  = paste0("T-SITA: A thesaurus for soil invertebrate trait-based ",
                        "approaches (Pey et al. 2014)"),
    scheme_uri = "https://ark.cefe.cnrs.fr/ark:/66666/th558",
    columns    = cols
  )
}
