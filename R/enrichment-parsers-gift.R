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
              grep("^trait_value_", names(tr), value = TRUE))
    tr <- tr[, intersect(keep, names(tr)), drop = FALSE]
    acc <- if (is.null(acc)) tr else
      merge(acc, tr, by = c("work_ID", "work_species"), all = TRUE)
  }

  out <- data.frame(
    canonical_name = trimws(as.character(acc$work_species)),
    stringsAsFactors = FALSE
  )
  for (id in ids) {
    src <- paste0("trait_value_", id)
    if (!src %in% names(acc)) next
    if (isTRUE(is_num[[id]])) {
      out[[colname[[id]]]] <- suppressWarnings(as.numeric(acc[[src]]))
    } else {
      v <- as.character(acc[[src]])
      v[v %in% c("", "NA", "NaN")] <- NA_character_
      out[[colname[[id]]]] <- v
    }
  }

  out <- out[!is.na(out$canonical_name) & nzchar(out$canonical_name), ,
             drop = FALSE]
  trait_cols <- setdiff(names(out), "canonical_name")
  keep <- rowSums(!is.na(out[, trait_cols, drop = FALSE])) > 0L
  out <- out[keep, , drop = FALSE]
  rownames(out) <- NULL
  out
}
