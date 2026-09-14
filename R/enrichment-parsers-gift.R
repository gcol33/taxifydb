# Build-time parser for the GIFT enrichment.
#
# GIFT (Global Inventory of Floras and Traits) is served from a live REST API.
# The API exposes only the data GIFT is licensed to redistribute (its output is
# CC BY 4.0); references whose underlying source has restrictions carry a
# `restricted` flag and are excluded from the default, unauthenticated call.
# parse_gift() fetches that redistributable subset ONCE at build time and writes
# it into a `.vtr`, so the taxify runtime joins it offline and never calls the
# GIFT API per query.


# Turn a GIFT trait label (e.g. "Plant_height_max") into an output column name
# (e.g. "gift_plant_height_max"). Mirrors the runtime naming in taxify.
.gift_colname <- function(trait_label) {
  x <- tolower(trait_label)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  paste0("gift_", x)
}


#' Parse GIFT species-level plant traits (via the GIFT package)
#'
#' Downloads GIFT's full catalogue of species-level traits from the live API
#' (batched over all trait IDs) and pivots them to one row per species. GIFT
#' aggregates records to a single value per species (mean for numeric traits,
#' most frequent entry for categorical ones). Only the redistributable subset
#' the API returns is kept; restricted references are excluded by the default
#' call. The species name is GIFT's `work_species`.
#'
#' Every trait column `<col>` has a `<col>_source` column holding the GIFT
#' reference ids (`ref_ID`) GIFT lists for the aggregated value (the
#' `references` field [GIFT::GIFT_traits()] returns beside it). For a
#' categorical trait these are the references that state the reported value;
#' for a numeric trait the references entering the mean. GIFT prefixes some ids
#' with `-` or `+`, a marking its documentation does not define. The sign is
#' dropped from the cell and kept per reference in the table as
#' `listed_negative` / `listed_positive`. A negatively listed reference never
#' appears among the records `GIFT_traits_raw(bias_ref = FALSE)` returns (the
#' tree-only GlobalTreeSearch is one), which fits GIFT's `bias_ref` flag. The
#' reference table comes from [GIFT::GIFT_references()]; an id GIFT lists but
#' does not serve there has `NA` citation and a `note` saying so. It is attached
#' with [attach_references()].
#'
#' @param path Ignored; the GIFT package fetches data directly. Present so the
#'   interface matches the file-based parsers.
#' @param agreement Numeric in `[0, 1]`. Minimum agreement among source records
#'   for a categorical value to be reported. Default `0.66`.
#' @param batch_size Integer. Number of trait IDs per API request. Default `12`.
#' @param verbose Logical. Default `TRUE`.
#' @return data.frame keyed on `canonical_name` with one `gift_<trait>` column
#'   per GIFT trait. Numeric traits are doubles; the rest are character.
#' @export
parse_gift <- function(path = NULL, agreement = 0.66, batch_size = 12L,
                       verbose = TRUE) {
  if (!requireNamespace("GIFT", quietly = TRUE)) {
    stop("Package 'GIFT' is required to build the gift enrichment. ",
         "Install with: install.packages('GIFT')", call. = FALSE)
  }

  meta <- GIFT::GIFT_traits_meta()
  meta <- meta[!is.na(meta$Lvl3) & nzchar(as.character(meta$Lvl3)), , drop = FALSE]
  ids     <- as.character(meta$Lvl3)
  colname <- make.unique(.gift_colname(meta$Trait2), sep = "_")
  is_num  <- as.character(meta$type) == "numeric"
  names(colname) <- ids
  names(is_num)  <- ids

  batches <- split(ids, ceiling(seq_along(ids) / batch_size))
  acc <- NULL
  for (i in seq_along(batches)) {
    b <- batches[[i]]
    if (verbose) {
      message(sprintf("  GIFT batch %d/%d (%d traits)...",
                      i, length(batches), length(b)))
    }
    tr <- GIFT::GIFT_traits(trait_IDs = b, agreement = agreement,
                            bias_ref = FALSE, bias_deriv = FALSE)
    keep <- c("work_ID", "work_species",
              grep("^trait_value_", names(tr), value = TRUE),
              grep("^references_", names(tr), value = TRUE))
    tr <- tr[, intersect(keep, names(tr)), drop = FALSE]
    acc <- if (is.null(acc)) tr else
      merge(acc, tr, by = c("work_ID", "work_species"), all = TRUE)
  }

  out <- data.frame(
    canonical_name = trimws(as.character(acc$work_species)),
    stringsAsFactors = FALSE
  )
  neg <- character(0)
  pos <- character(0)
  for (id in ids) {
    src <- paste0("trait_value_", id)
    if (!src %in% names(acc)) next
    oc <- colname[[id]]
    if (isTRUE(is_num[[id]])) {
      out[[oc]] <- suppressWarnings(as.numeric(acc[[src]]))
    } else {
      v <- as.character(acc[[src]])
      v[v %in% c("", "NA", "NaN")] <- NA_character_
      out[[oc]] <- v
    }
    refs <- .gift_ref_cells(acc[[paste0("references_", id)]])
    refs$cell[is.na(out[[oc]])] <- NA_character_
    out[[.source_colname(oc, colname)]] <- refs$cell
    neg <- union(neg, refs$negative)
    pos <- union(pos, refs$positive)
  }

  out <- out[!is.na(out$canonical_name) & nzchar(out$canonical_name), ,
             drop = FALSE]
  trait_cols <- setdiff(names(out),
                        c("canonical_name", .reference_cols(names(out))))
  keep <- rowSums(!is.na(out[, trait_cols, drop = FALSE])) > 0L
  out <- out[keep, , drop = FALSE]
  rownames(out) <- NULL

  gr <- GIFT::GIFT_references()
  references <- unique(data.frame(
    ref_id   = as.character(gr$ref_ID),
    citation = as.character(gr$ref_long),
    doi      = .extract_doi(gr$ref_long),
    ref_type = as.character(gr$type),
    note     = NA_character_,
    stringsAsFactors = FALSE
  ))
  prov  <- .reference_cols(names(out))
  cells <- unlist(lapply(prov, function(p) out[[p]]), use.names = FALSE)
  used  <- unique(unlist(strsplit(cells[!is.na(cells)], "|", fixed = TRUE)))
  unserved <- setdiff(used, references$ref_id)
  if (length(unserved)) {
    references <- rbind(references, data.frame(
      ref_id = unserved, citation = NA_character_, doi = NA_character_,
      ref_type = NA_character_,
      note = "Listed by GIFT in a trait aggregate; not served by GIFT_references().",
      stringsAsFactors = FALSE))
  }
  references$listed_negative <- references$ref_id %in% neg
  references$listed_positive <- references$ref_id %in% pos
  attach_references(out, references)
}


#' Turn GIFT `references_<trait>` strings into provenance cells
#'
#' GIFT lists the reference ids behind an aggregated value comma-separated,
#' some prefixed with `-` or `+`. The cells come out as `.ref_join()` would
#' build them row by row, from one C-locale sort over every token of the column.
#' @return list with `cell` (the `|`-joined unsigned ids per row), `negative`
#'   and `positive` (the distinct ids that carried each sign).
#' @noRd
.gift_ref_cells <- function(x) {
  x <- as.character(x)
  n <- length(x)
  if (!n) {
    return(list(cell = character(0), negative = character(0),
                positive = character(0)))
  }
  has   <- which(!is.na(x))
  parts <- strsplit(x[has], ",", fixed = TRUE)
  row   <- rep(has, lengths(parts))
  tok   <- trimws(unlist(parts, use.names = FALSE))
  neg   <- unique(substring(tok[startsWith(tok, "-")], 2L))
  pos   <- unique(substring(tok[startsWith(tok, "+")], 2L))
  id    <- sub("^[-+]", "", tok)
  keep  <- nzchar(id)
  row   <- row[keep]
  id    <- id[keep]
  o     <- order(row, id, method = "radix")
  row   <- row[o]
  id    <- id[o]
  first <- c(TRUE, row[-1L] != row[-length(row)] | id[-1L] != id[-length(id)])
  row   <- row[first]
  id    <- id[first]
  cell  <- rep(NA_character_, n)
  if (length(id)) {
    grp <- split(id, row)
    cell[as.integer(names(grp))] <- vapply(grp, paste, character(1L),
                                           collapse = "|")
  }
  list(cell = cell, negative = neg, positive = pos)
}
