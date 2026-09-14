# Per-value provenance for enrichment builds.
#
# A trait database aggregates several records into the one value an enrichment
# reports for a species, and each record names the reference it came from. A
# parser that keeps those references writes, beside every value column `<col>`,
# a `<col>_source` column holding the ids of the references behind the reported
# value, `|`-delimited: for a categorical value the records that state that
# value, for a numeric value the records entering the aggregate. The ids resolve
# against a reference table (`ref_id`, `citation`, `doi`, plus any columns the
# source adds) that the parser attaches with [attach_references()] and
# [build_enrichment_vtr()] publishes as `<name>_references.vtr` next to the
# enrichment, declared in `meta.json` under `references`.


#' Join reference ids into one provenance cell
#'
#' Distinct non-empty ids, sorted in C-locale order so the cell does not depend
#' on record order or on the build machine's locale.
#' @noRd
.ref_join <- function(ids) {
  ids <- trimws(as.character(ids))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  if (!length(ids)) return(NA_character_)
  paste(sort(unique(ids), method = "radix"), collapse = "|")
}


#' Reference ids behind each group's reported value
#'
#' `group`, `value` and `ref` describe one record each; `keys` are the output
#' rows and `reported` the value reported for each key. For `type = "num"` the
#' records are those with a finite numeric value (the records a median or mean
#' aggregates); with `reduce = "min"` or `"max"` only those equal to the reported
#' value. For `type = "cat"` the records whose value equals the reported value,
#' compared exactly, so a caller passes values prepared the way its reducer saw
#' them; with `reduce = "join"` every record with a non-empty value.
#' @return Character vector aligned to `keys`.
#' @noRd
.value_refs <- function(group, value, ref, reported, keys,
                        type = c("num", "cat"), reduce = NULL) {
  type <- match.arg(type)
  g    <- as.character(group)
  ref  <- as.character(ref)
  at   <- match(g, keys)
  sel  <- !is.na(g) & nzchar(g) & !is.na(at)
  if (type == "num") {
    v   <- suppressWarnings(as.numeric(value))
    sel <- sel & is.finite(v)
    if (!is.null(reduce) && reduce %in% c("min", "max")) {
      sel <- sel & !is.na(reported[at]) & v == reported[at]
    }
  } else {
    v <- as.character(value)
    if (identical(reduce, "join")) {
      sel <- sel & !is.na(v) & nzchar(trimws(v))
    } else {
      sel <- sel & !is.na(v) & !is.na(reported[at]) & v == reported[at]
    }
  }
  sel <- sel & !is.na(ref) & nzchar(trimws(ref))
  out <- rep(NA_character_, length(keys))
  if (!any(sel)) return(out)
  agg <- tapply(ref[sel], factor(at[sel], levels = seq_along(keys)), .ref_join)
  out[as.integer(names(agg))] <- as.character(agg)
  out
}


#' First DOI in a citation string
#' @noRd
.extract_doi <- function(x) {
  x   <- as.character(x)
  pos <- regexpr("10\\.[0-9]{4,9}/[^][:space:]()<>\",;]+", x, perl = TRUE)
  out <- rep(NA_character_, length(x))
  hit <- !is.na(pos) & pos > 0L
  out[hit] <- sub("[.]+$", "", regmatches(x, pos))
  out
}


#' Attach a reference table to a parsed enrichment
#'
#' Marks `df` as carrying per-value provenance: its `<col>_source` columns hold
#' reference ids that resolve against `references`. [build_enrichment()] carries
#' the table through name resolution and [build_enrichment_vtr()] publishes it.
#'
#' @param df The parser's data.frame, keyed on `canonical_name`.
#' @param references data.frame with a unique character `ref_id`, a non-empty
#'   `citation` and a `doi` (`NA` where the reference has none). Further columns
#'   are kept.
#' @return `df`, with the table in its `references` attribute.
#' @export
attach_references <- function(df, references) {
  attr(df, "references") <- .validate_references(references)
  df
}


#' Check and normalise a reference table
#' @noRd
.validate_references <- function(references) {
  if (!is.data.frame(references)) {
    stop("references must be a data.frame.", call. = FALSE)
  }
  miss <- setdiff(c("ref_id", "citation", "doi"), names(references))
  if (length(miss)) {
    stop(sprintf("references is missing column(s): %s.",
                 paste(miss, collapse = ", ")), call. = FALSE)
  }
  references$ref_id   <- trimws(.to_utf8(as.character(references$ref_id)))
  references$citation <- trimws(.to_utf8(as.character(references$citation)))
  references$doi      <- as.character(references$doi)
  if (anyNA(references$ref_id) || any(!nzchar(references$ref_id))) {
    stop("references$ref_id has missing or empty ids.", call. = FALSE)
  }
  # A source may list one reference once per scope it covers (GIFT repeats a
  # checklist for every family it spans); rows agreeing in every kept column are
  # one reference.
  references <- unique(references)
  if (anyDuplicated(references$ref_id)) {
    d <- unique(references$ref_id[duplicated(references$ref_id)])
    stop(sprintf("references$ref_id is not unique: %s.",
                 paste(utils::head(d, 5L), collapse = ", ")), call. = FALSE)
  }
  if (any(grepl("|", references$ref_id, fixed = TRUE))) {
    stop("references$ref_id may not contain '|', the provenance delimiter.",
         call. = FALSE)
  }
  if (anyNA(references$citation) || any(!nzchar(references$citation))) {
    stop("references$citation has missing or empty citations.", call. = FALSE)
  }
  rownames(references) <- NULL
  references
}


#' Provenance columns of an enrichment
#'
#' The `<col>_source` columns whose value column `<col>` is also present.
#' @param columns Character vector of column names.
#' @return Character vector of provenance column names.
#' @noRd
.reference_cols <- function(columns) {
  columns <- as.character(columns)
  s <- columns[endsWith(columns, "_source")]
  s[sub("_source$", "", s) %in% columns]
}


#' Name of the provenance column for a value column, refusing a collision
#' @noRd
.source_colname <- function(col, taken) {
  sc <- paste0(col, "_source")
  if (sc %in% taken) {
    stop(sprintf(
      "Provenance column '%s' collides with an existing column.", sc),
      call. = FALSE)
  }
  sc
}


#' Wrap a row-selecting reducer so provenance columns do not count as traits
#'
#' [.dedup_keep_richest()] keeps the row carrying the most non-missing trait
#' cells. A provenance cell is present exactly where its value is, so counting
#' it would weigh a row by its number of value columns a second time and could
#' change which row wins. The wrapper ranks rows on everything else and returns
#' the chosen rows whole.
#' @noRd
.reducer_ignoring <- function(reducer, ignore) {
  force(reducer)
  force(ignore)
  function(df, group_cols) {
    drop <- intersect(ignore, names(df))
    if (!length(drop)) return(reducer(df, group_cols))
    probe <- df[setdiff(names(df), drop)]
    probe$.__row__ <- seq_len(nrow(df))
    picked <- reducer(probe, group_cols)
    out <- df[picked$.__row__, , drop = FALSE]
    rownames(out) <- NULL
    out
  }
}


#' Write the reference table beside an enrichment `.vtr`
#'
#' Keeps the references some provenance cell names, errors on an id that has no
#' reference, and writes `<name>_references.vtr` sorted by `ref_id`.
#' @return The `references` block for `meta.json`.
#' @noRd
.write_references_vtr <- function(df, references, vtr_path, name) {
  references <- .validate_references(references)
  prov <- .reference_cols(names(df))
  if (!length(prov)) {
    stop(sprintf(
      "[enrichment/%s] references given but no <col>_source column present.",
      name), call. = FALSE)
  }
  cells <- unlist(lapply(prov, function(p) df[[p]]), use.names = FALSE)
  cells <- cells[!is.na(cells)]
  used  <- unique(unlist(strsplit(cells, "|", fixed = TRUE), use.names = FALSE))
  miss  <- setdiff(used, references$ref_id)
  if (length(miss)) {
    stop(sprintf(
      "[enrichment/%s] %d reference id(s) have no entry in the reference table: %s",
      name, length(miss), paste(utils::head(miss, 10L), collapse = ", ")),
      call. = FALSE)
  }
  refs <- references[references$ref_id %in% used, , drop = FALSE]
  refs <- refs[order(refs$ref_id, method = "radix"), , drop = FALSE]
  rownames(refs) <- NULL

  ref_path <- file.path(dirname(vtr_path), paste0(name, "_references.vtr"))
  vectra::write_vtr(refs, ref_path)
  vectra::create_index(ref_path, "ref_id")
  message(sprintf("[enrichment/%s] %d references behind %d provenance columns",
                  name, nrow(refs), length(prov)))
  list(
    file       = basename(ref_path),
    nrow       = nrow(refs),
    content_id = unname(tools::md5sum(ref_path))
  )
}
