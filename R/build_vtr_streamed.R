# Streaming build pipeline: chunks of normalized rows -> .vtr backbone.
#
# build_vtr() takes a whole normalized backbone as one data.frame, so the peak
# is the entire table in R. The largest backbones are millions of rows over
# twenty-odd character columns, which is the biggest single allocation any
# build makes.
#
# vectra is a larger-than-RAM engine, so the store never needed to be assembled
# in R: write_vtr() followed by append_vtr(along = "rows") grows a .vtr a batch
# at a time, indexes survive a row append, and arrange() is an external sort
# that spills past vectra_mem(). What kept the whole table in memory was the
# two steps of taxifydb's own pipeline that are not row-local:
#
#   embed_accepted() resolves a synonym to its accepted row, which can sit
#   anywhere in the table, and follows synonym chains.
#
#   build_vtr() sorts by genus so a genus filter prunes row groups.
#
# Both are done here without the table. The sort becomes a vectra arrange() on
# the staged store, which spills rather than materializing. The embedding
# becomes two joins against a links table built from a projection of three
# columns: the chain resolution needs only (id, accepted id, status), so it
# runs on that projection through the same embed_accepted() the non-streaming
# path uses, rather than a second copy of the chain logic.
#
# The peak is therefore one chunk plus that three-column link projection, not
# the backbone. It is bounded by the number of identifiers rather than by the
# table's width, so it does not grow as columns are added to a backbone.


#' Build a backbone .vtr from a stream of normalized chunks
#'
#' The streaming counterpart to [build_vtr()], for a source too large to
#' assemble in memory. Rows arrive a chunk at a time from `feed`; the finished
#' store is identical to what [build_vtr()] would have written from the
#' concatenation of those chunks.
#'
#' @param feed A function of no arguments returning the next chunk of rows as a
#'   normalized data.frame (the schema [normalize_backbone()] produces), or
#'   `NULL` when the source is exhausted.
#' @param vtr_path Character. Output path for the .vtr file.
#' @param backend_name Character. Backend identifier (e.g. `"colxr"`).
#' @param version Character. Version string.
#' @param source_url Character. URL the source data was downloaded from.
#' @param synonym_pattern Character. Regex matching the statuses that count as
#'   a synonym, as passed to [precompute_backbone()].
#' @param batch_size Integer. Row group size for vectra (default 50000).
#' @param work_dir Character. Directory for the staging store. Defaults to a
#'   session temporary directory, removed on exit.
#' @param verbose Logical.
#' @return The path to the .vtr file (invisibly).
#' @export
build_vtr_streamed <- function(feed, vtr_path, backend_name, version,
                               source_url, synonym_pattern = "SYNONYM",
                               batch_size = 50000L, work_dir = NULL,
                               verbose = TRUE) {
  if (!is.function(feed)) stop("`feed` must be a function.", call. = FALSE)

  own_work_dir <- is.null(work_dir)
  if (own_work_dir) work_dir <- tempfile("vtr_stream_")
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  if (own_work_dir) on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  staging_path <- file.path(work_dir, "staging.vtr")
  links_path   <- file.path(work_dir, "links.vtr")

  # ---- Pass 1: stage the rows, keeping only the link projection in memory ----

  n_rows <- 0L
  n_chunks <- 0L
  link_parts <- list()
  genus_col <- NULL

  repeat {
    chunk <- feed()
    if (is.null(chunk) || nrow(chunk) == 0L) break

    chunk <- precompute_backbone_rowwise(chunk)
    if (is.null(genus_col)) {
      genus_col <- if ("genus" %in% names(chunk)) "genus" else "resolved_genus"
    }

    n_chunks <- n_chunks + 1L
    link_parts[[n_chunks]] <- data.frame(
      taxon_id               = chunk$taxon_id,
      accepted_name_usage_id = chunk$accepted_name_usage_id,
      taxonomic_status       = chunk$taxonomic_status,
      stringsAsFactors       = FALSE
    )

    if (n_chunks == 1L) {
      dir.create(dirname(staging_path), recursive = TRUE, showWarnings = FALSE)
      vectra::write_vtr(chunk, staging_path, batch_size = batch_size)
    } else {
      vectra::append_vtr(chunk, staging_path)
    }

    n_rows <- n_rows + nrow(chunk)
    if (verbose && n_chunks %% 10L == 0L) {
      message(sprintf("  staged %s rows", format(n_rows, big.mark = ",")))
    }
  }

  if (n_rows == 0L) stop("`feed` yielded no rows.", call. = FALSE)
  if (verbose) {
    message(sprintf("Staged %s rows in %d chunks.",
                    format(n_rows, big.mark = ","), n_chunks))
  }

  # ---- Resolve synonym chains on the link projection ----

  if (verbose) message("Resolving accepted targets...")
  links <- do.call(rbind, link_parts)
  rm(link_parts)

  # Every column the chain resolution does not need is pointed at the id, so
  # this runs the same embed_accepted() the non-streaming path uses rather than
  # a second implementation of the chain walk. Only the resolved target and the
  # synonym flag are kept.
  resolved <- taxify::embed_accepted(
    links,
    id_col          = "taxon_id",
    acc_id_col      = "accepted_name_usage_id",
    name_col        = "taxon_id",
    family_col      = "taxon_id",
    genus_col       = "taxon_id",
    status_col      = "taxonomic_status",
    synonym_pattern = synonym_pattern
  )
  links <- data.frame(
    taxon_id          = resolved$taxon_id,
    accepted_taxon_id = resolved$accepted_taxon_id,
    is_synonym        = resolved$is_synonym,
    stringsAsFactors  = FALSE
  )
  rm(resolved)

  vectra::write_vtr(links, links_path, batch_size = batch_size)
  vectra::create_index(links_path, "taxon_id")
  n_links <- nrow(links)
  rm(links)

  vectra::create_index(staging_path, "taxon_id")

  # ---- Pass 2: embed accepted info, sort by genus, stream out ----

  if (verbose) message("Embedding accepted names and sorting by genus...")

  # The accepted side is renamed as it is selected, so the two sides never
  # collide on family/genus/authorship and no suffixing is applied.
  accepted <- vectra::select(
    vectra::tbl(staging_path),
    accepted_taxon_id   = "taxon_id",
    accepted_name       = "canonical_name",
    accepted_family     = "family",
    accepted_genus      = "genus",
    accepted_authorship = "authorship"
  )

  node <- vectra::tbl(staging_path) |>
    vectra::left_join(vectra::tbl(links_path), by = "taxon_id") |>
    vectra::left_join(accepted, by = "accepted_taxon_id")

  # arrange() captures its sort columns unevaluated, so the column name is
  # substituted into the call rather than passed as a string.
  node <- eval(bquote(vectra::arrange(node, .(as.name(genus_col)))))

  dir.create(dirname(vtr_path), recursive = TRUE, showWarnings = FALSE)
  vectra::write_vtr(node, vtr_path, batch_size = batch_size)

  if (n_links != n_rows) {
    stop(sprintf(
      "Link table has %s rows against %s staged rows; the accepted join would be ambiguous.",
      format(n_links, big.mark = ","), format(n_rows, big.mark = ",")),
      call. = FALSE)
  }

  index_backbone_vtr(vtr_path, genus_col)
  write_backbone_meta(vtr_path, backend_name, version, source_url, n_rows)
  report_built_backbone(vtr_path, backend_name, n_rows)

  invisible(vtr_path)
}


#' Apply the row-local half of precompute_backbone()
#'
#' [precompute_backbone()] does two things: derive the matching keys, which
#' depends only on the row, and embed the accepted name, which depends on the
#' whole table. The streaming build needs the first on each chunk and does the
#' second across the staged store, so the row-local half is named here and
#' called from both paths.
#'
#' @param df A normalized backbone data.frame or chunk.
#' @return `df` with the aggregate name folded and the matching keys added.
#' @noRd
precompute_backbone_rowwise <- function(df) {
  df$canonical_name <- taxify::normalize_aggregate_name(
    df$canonical_name, df$taxon_rank
  )
  taxify::precompute_keys(
    df,
    name_col    = "canonical_name",
    genus_col   = "genus",
    epithet_col = "specific_epithet"
  )
}


#' Feed a delimited file to [build_vtr_streamed()] a chunk at a time
#'
#' Reads a header once, then hands back successive blocks of rows, so a file
#' larger than memory can be normalized and staged block by block.
#'
#' `quote` and `encoding` must be set to whatever the backbone's whole-file
#' reader uses. They decide how the text is parsed, not just how fast it is
#' read: WoRMS ships its TSV with every field wrapped in double quotes, so
#' reading it with quoting disabled leaves the quote characters inside the
#' names and breaks every downstream join, while a source carrying genuine
#' embedded quotes in informal names needs quoting disabled to keep them.
#'
#' @param path Character. Path to the delimited file.
#' @param normalize A function taking one raw chunk (a data.frame with the
#'   file's own column names) and returning it in the normalized schema.
#' @param chunk_rows Integer. Rows to read per chunk.
#' @param sep Character. Field separator.
#' @param quote Character. Quoting character, or `""` to disable quoting.
#' @param encoding Character. Passed to `data.table::fread()`.
#' @param select Character vector of columns to read, or `NULL` for all.
#' @param na_strings Character vector read as `NA`.
#' @param verbose Logical.
#' @return A function of no arguments suitable as the `feed` of
#'   [build_vtr_streamed()].
#' @export
delim_chunk_feed <- function(path, normalize, chunk_rows = 500000L,
                             sep = "\t", quote = "", encoding = "UTF-8",
                             select = NULL, na_strings = c("", "NA"),
                             verbose = TRUE) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required to stream a delimited file.",
         call. = FALSE)
  }
  header <- names(data.table::fread(path, sep = sep, quote = quote,
                                    nrows = 0L, encoding = encoding,
                                    showProgress = FALSE))
  keep <- if (is.null(select)) NULL else intersect(select, header)
  offset <- 0L
  done <- FALSE

  function() {
    if (done) return(NULL)
    args <- list(
      path, sep = sep, quote = quote, skip = offset + 1L, nrows = chunk_rows,
      col.names = header, na.strings = na_strings, encoding = encoding,
      colClasses = "character", showProgress = FALSE, header = FALSE
    )
    raw <- do.call(data.table::fread, args)
    if (nrow(raw) == 0L) {
      done <<- TRUE
      return(NULL)
    }
    if (nrow(raw) < chunk_rows) done <<- TRUE
    offset <<- offset + nrow(raw)
    if (verbose) {
      message(sprintf("  read %s rows", format(offset, big.mark = ",")))
    }
    raw <- as.data.frame(raw, stringsAsFactors = FALSE)
    if (!is.null(keep)) raw <- raw[, keep, drop = FALSE]
    normalize(raw)
  }
}
