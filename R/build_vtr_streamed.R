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
#
# A backbone's own reader can be non-row-local too, when it denormalizes a
# higher classification held as foreign keys. delim_fk_lookup() handles that
# case the same way: only the referenced ids are ever looked up, so the lookup
# is the size of the ranks the keys point at rather than of the table, and the
# normalization becomes row-local once it is in hand.


#' Build a backbone .vtr from a stream of normalized chunks
#'
#' The streaming counterpart to [build_vtr()], for a source too large to
#' assemble in memory. Rows arrive a chunk at a time from `feed`; the finished
#' store is identical to what [build_vtr()] would have written from the
#' concatenation of those chunks.
#'
#' @param feed A function of no arguments returning the next chunk of rows as a
#'   normalized data.frame (the schema [normalize_backbone()] produces), or
#'   `NULL` when the source is exhausted. A zero-row data.frame means the block
#'   was filtered out entirely, not that the source has ended.
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
  n_blocks <- 0L
  link_parts <- list()
  genus_col <- NULL

  repeat {
    chunk <- feed()
    # Only NULL ends the feed. A block that normalizes to zero rows is one the
    # source filtered away entirely -- GBIF drops every KINGDOM, PHYLUM, CLASS
    # and ORDER row, which are contiguous in the file -- and reading that as the
    # end of the source would truncate the backbone silently.
    if (is.null(chunk)) break
    n_blocks <- n_blocks + 1L
    if (nrow(chunk) == 0L) next

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
    message(sprintf("Staged %s rows from %d blocks.",
                    format(n_rows, big.mark = ","), n_blocks))
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
  #
  # This sorts by byte order where build_vtr()'s order() follows the build
  # machine's LC_COLLATE, so the two builders lay a non-ASCII genus down in a
  # different place. What the sort is for survives either way: each genus still
  # occupies one contiguous run, which is what lets a genus filter prune row
  # groups. Byte order is the more useful of the two here, since it matches the
  # collation vectra compares with and does not vary with the locale of the
  # machine that ran the build.
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
#' `quote`, `encoding` and `file_encoding` must be set to whatever the
#' backbone's whole-file reader uses. They decide how the text is parsed, not
#' just how fast it is read: WoRMS ships its TSV with every field wrapped in
#' double quotes, so reading it with quoting disabled leaves the quote
#' characters inside the names and breaks every downstream join, while a source
#' carrying genuine embedded quotes in informal names needs quoting disabled to
#' keep them.
#'
#' `quote` also decides how the file is cut. With quoting disabled a newline
#' always ends a record, so blocks are taken by row offset and
#' `data.table::fread()` reads each directly, which is the faster path and the
#' one the unquoted sources use.
#'
#' With quoting enabled that stops holding: a field may contain a newline, and
#' then a line offset is not a record offset. WoRMS carries 4,626 newlines
#' inside quoted `namePublishedIn` fields, and every reader's row argument
#' counts lines -- `fread()`'s `skip` does, and so does `read.delim(nrows=)`
#' through `scan()`'s `nlines` -- so a line-offset read is 445 records adrift by
#' record 200,000 and silently repeats rows. A quoted source is therefore cut
#' only where the running count of quote characters is even, which is the one
#' place a record can end, and the number of records that implies is checked
#' against the number the parser returns.
#'
#' Lines are taken at the byte level, split on `LF` alone. R's own line reader
#' recognizes a lone `CR` as a terminator as well, and WoRMS holds 59 of them
#' inside fields: read as text it manufactures 57 lines the file does not have
#' and drops 49 quote characters, which flips the parity and is why
#' [utils::read.delim()] gives up on that file partway through.
#'
#' A field may also contain `""`, which RFC 4180 defines as one literal quote.
#' [utils::read.delim()] collapses it and `fread()` returns it doubled
#' (Rdatatable/data.table#1109); WFO has 4,117 such fields, and leaving them
#' doubled would put stray quote characters in the published values, which is
#' the defect worms-2026.07 was cut to fix. Rather than let the choice of reader
#' decide that, the blocks are parsed with `fread()` and the unescaping is done
#' here, so it holds whichever source is being read.
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
#' @param col_names Character vector naming the columns of a file that carries
#'   no header row, in file order, or `NULL` when the first line is a header.
#' @param file_encoding Character or `NULL`. The encoding of the file's bytes,
#'   meaning what `fileEncoding` means to [utils::read.delim()]: character
#'   columns are converted from it once the block is parsed. WFO is the case,
#'   whose reader decodes as `"latin1"`. A file needing this is read block by
#'   block whether or not it is quoted, since the conversion happens there.
#' @param verbose Logical.
#' @return A function of no arguments suitable as the `feed` of
#'   [build_vtr_streamed()].
#' @export
delim_chunk_feed <- function(path, normalize, chunk_rows = 500000L,
                             sep = "\t", quote = "", encoding = "UTF-8",
                             select = NULL, na_strings = c("", "NA"),
                             col_names = NULL, file_encoding = NULL,
                             verbose = TRUE) {
  header <- delim_header(path, sep, quote, encoding, col_names)
  keep <- if (is.null(select)) seq_along(header) else which(header %in% select)
  if (length(keep) == 0L) {
    stop("None of `select` matched a column of the file.", call. = FALSE)
  }
  keep_names <- header[keep]
  # A header row is skipped once; a headerless file starts at its first line.
  head_rows <- if (is.null(col_names)) 1L else 0L

  n_seen <- 0L
  emit <- function(raw) {
    raw <- as.data.frame(raw, stringsAsFactors = FALSE)
    names(raw) <- keep_names
    n_seen <<- n_seen + nrow(raw)
    if (verbose) {
      message(sprintf("  read %s rows", format(n_seen, big.mark = ",")))
    }
    normalize(raw)
  }

  # `file_encoding` picks the block path too: the conversion is applied to a
  # parsed block, so a file needing one is cut rather than read by row offset.
  if (identical(quote, "") && is.null(file_encoding)) {
    args <- list(sep = sep, quote = quote, encoding = encoding,
                 na.strings = na_strings, colClasses = "character",
                 showProgress = FALSE, header = FALSE)
    delim_line_feed(path, emit, keep, chunk_rows, head_rows, args)
  } else {
    delim_block_feed(path, emit, keep, chunk_rows, head_rows, sep,
                     quote, na_strings, file_encoding)
  }
}


#' Chunk a delimited file by row offset
#'
#' Correct only where a newline always ends a record, which quoting disabled
#' guarantees.
#'
#' @param path Character. Path to the file.
#' @param emit Function applied to each parsed block.
#' @param keep Integer column positions to read.
#' @param chunk_rows,head_rows Integer.
#' @param args List of further arguments for `data.table::fread()`.
#' @return A feed function.
#' @noRd
delim_line_feed <- function(path, emit, keep, chunk_rows, head_rows, args) {
  # fread() errors rather than returning nothing when `skip` reaches the end of
  # the file, which a row count that divides evenly by `chunk_rows` walks into.
  # Knowing the total up front stops the feed on the boundary instead.
  total <- count_data_lines(path, head_rows)
  offset <- 0
  done <- FALSE

  function() {
    if (done) return(NULL)
    remaining <- total - offset
    if (remaining <= 0) {
      done <<- TRUE
      return(NULL)
    }
    raw <- do.call(data.table::fread, c(
      list(path, skip = offset + head_rows,
           nrows = min(chunk_rows, remaining), select = keep),
      args
    ))
    if (nrow(raw) == 0L) {
      done <<- TRUE
      return(NULL)
    }
    if (nrow(raw) < chunk_rows) done <<- TRUE
    offset <<- offset + nrow(raw)
    emit(raw)
  }
}


#' Read a file as lines, splitting on LF or CRLF but never a lone CR
#'
#' R's own line readers recognize a lone `CR` as a terminator as well. WoRMS
#' holds 59 of them inside fields, and reading them that way both invents lines
#' the file does not have and loses bytes: its 15,419,038 quote characters come
#' back as 15,418,989, which is enough to flip the running parity the record
#' boundary is found from.
#'
#' Splitting the bytes on `LF` and then dropping one trailing `CR` per line
#' keeps a `CRLF` file's lines clean while leaving a `CR` that is not at the end
#' of a line where the file put it.
#'
#' @param path Character. Path to the file.
#' @param chunk_bytes Integer. Bytes to pull from the connection at a time.
#' @return A list of `read(n)`, returning up to `n` further lines and a
#'   zero-length vector at end of file, and `close()`.
#' @noRd
delim_lf_reader <- function(path, chunk_bytes = 33554432L) {
  con <- file(path, "rb")
  pending <- character(0)
  tail <- ""
  eof <- FALSE

  # A CR immediately before the LF is that line's terminator and comes off with
  # it. One anywhere else is a character of the field, so this is applied only
  # to lines already known to be complete -- never to the tail, whose CR may yet
  # turn out to be followed by the LF in the next chunk.
  drop_cr <- function(x) sub("\r$", "", x, useBytes = TRUE)

  fill <- function(n) {
    while (length(pending) < n && !eof) {
      raw <- readBin(con, "raw", chunk_bytes)
      if (length(raw) == 0L) {
        eof <<- TRUE
        # A final line with no trailing newline still holds a record.
        if (nzchar(tail)) {
          pending <<- c(pending, drop_cr(tail))
          tail <<- ""
        }
        break
      }
      if (any(raw == as.raw(0L))) {
        stop("The file contains a null byte, which no delimited reader carries.",
             call. = FALSE)
      }
      parts <- strsplit(paste0(tail, rawToChar(raw)), "\n",
                        fixed = TRUE, useBytes = TRUE)[[1L]]
      if (length(parts) == 0L) {
        tail <<- ""
        next
      }
      # strsplit() drops the empty piece after a trailing separator, so whether
      # the last piece is a whole line is read off the bytes rather than it.
      if (identical(raw[length(raw)], as.raw(10L))) {
        pending <<- c(pending, drop_cr(parts))
        tail <<- ""
      } else {
        pending <<- c(pending, drop_cr(parts[-length(parts)]))
        tail <<- parts[length(parts)]
      }
    }
  }

  list(
    read = function(n) {
      fill(n)
      take <- min(n, length(pending))
      if (take == 0L) return(character(0))
      out <- pending[seq_len(take)]
      pending <<- pending[-seq_len(take)]
      out
    },
    close = function() close(con)
  )
}


#' Collapse the doubled quotes of a quoted field
#'
#' RFC 4180 writes a literal quote inside a quoted field as two, which
#' [utils::read.delim()] collapses on the way in and `data.table::fread()`
#' returns as it found (Rdatatable/data.table#1109). Doing it here rather than
#' by choosing a reader keeps the published values the same whichever parser a
#' backbone is read with.
#'
#' @param df A data.frame of parsed fields.
#' @param quote Character. The quoting character, or `""` when quoting is off,
#'   in which case nothing was escaped and `df` is returned unchanged.
#' @return `df` with each character column unescaped.
#' @noRd
unescape_quotes <- function(df, quote = "\"") {
  if (!nzchar(quote)) return(df)
  doubled <- paste0(quote, quote)
  for (j in seq_along(df)) {
    v <- df[[j]]
    if (is.character(v)) {
      df[[j]] <- gsub(doubled, quote, v, fixed = TRUE)
    }
  }
  df
}


#' Chunk a delimited file on record boundaries
#'
#' Cuts the file itself, so a record spanning several lines is never split.
#' Every reader's row argument counts lines rather than records --
#' `fread(skip=)` does, and `read.delim(nrows=)` does through `scan()`'s
#' `nlines` -- so the cut cannot be delegated to one. `read.delim()` reading the
#' whole of WoRMS is the same fault at file scale: it stops at the first record
#' whose quoting it cannot close and returns 1,363,240 of 1,562,065 rows with
#' only a warning.
#'
#' Blocks end only where the running count of quote characters is even, which is
#' the one place a record can end, and the number of records that implies is
#' checked against the number the parser returns, so a file that escapes quotes
#' some other way stops the build rather than staging shifted rows.
#'
#' @param path Character. Path to the file.
#' @param emit Function applied to each parsed block.
#' @param keep Integer column positions to keep.
#' @param chunk_rows,head_rows Integer.
#' @param sep,quote Character. Field separator and quoting character.
#' @param na_strings Character vector read as `NA`.
#' @param file_encoding Character or `NULL`. Encoding of the file's bytes,
#'   applied to the parsed block exactly as `read.delim()` applies
#'   `fileEncoding` to what it read.
#' @return A feed function.
#' @noRd
delim_block_feed <- function(path, emit, keep, chunk_rows, head_rows,
                             sep, quote, na_strings, file_encoding) {
  reader <- delim_lf_reader(path)
  if (head_rows > 0L) reader$read(head_rows)

  # Each block is handed to the parser as a file rather than as text. fread()
  # splits a `text` argument into lines itself, which is the one thing a record
  # spanning lines cannot survive; from a file it reads the record whole. The
  # same scratch file is rewritten for every block.
  block_file <- tempfile("vtr_block_", fileext = ".tsv")

  carry <- character(0)
  eof <- FALSE
  done <- FALSE

  # Counted over bytes, not characters. WFO holds thousands of lines that are
  # invalid in the session's encoding, and a character-wise match reports
  # nothing for those rather than failing, which would leave the parity -- and
  # so the record boundary -- quietly wrong. The quoting character is ASCII and
  # every encoding read here is ASCII-transparent, so bytes are exact.
  quote_counts <- function(lines) {
    m <- gregexpr(quote, lines, fixed = TRUE, useBytes = TRUE)
    vapply(m, function(p) if (p[1L] == -1L) 0L else length(p), integer(1))
  }

  pull <- function(n) {
    more <- reader$read(n)
    if (length(more) == 0L) eof <<- TRUE
    more
  }

  function() {
    if (done) return(NULL)

    lines <- carry
    carry <<- character(0)
    ends <- integer(0)
    repeat {
      short <- chunk_rows - length(lines)
      if (short > 0L && !eof) {
        lines <- c(lines, pull(short))
        next
      }
      if (length(lines) > 0L) {
        ends <- which(cumsum(quote_counts(lines)) %% 2L == 0L)
      }
      # No even-parity line means one record spans the whole block, so the
      # block has to grow before it can be cut.
      if (length(ends) > 0L || eof) break
      lines <- c(lines, pull(chunk_rows))
    }

    if (length(lines) == 0L) {
      reader$close()
      unlink(block_file)
      done <<- TRUE
      return(NULL)
    }
    if (length(ends) == 0L) {
      reader$close()
      unlink(block_file)
      stop("The file ends inside a quoted field; its quoting is unbalanced.",
           call. = FALSE)
    }

    last <- ends[length(ends)]
    block <- lines[seq_len(last)]
    if (last < length(lines)) {
      carry <<- lines[(last + 1L):length(lines)]
    } else if (eof) {
      reader$close()
      done <<- TRUE
    }

    out <- file(block_file, "wb")
    writeLines(block, out, sep = "\n", useBytes = TRUE)
    close(out)

    # colClasses fixes the types instead of letting each block infer its own.
    # Without it a block in which some column happened to be empty throughout
    # would come back logical while the rest came back character, and the
    # staged store would take whichever block was written first.
    raw <- data.table::fread(
      block_file, header = FALSE, sep = sep,
      quote = quote, na.strings = na_strings, colClasses = "character",
      select = keep, encoding = "unknown", showProgress = FALSE,
      data.table = FALSE
    )
    unlink(block_file)
    if (nrow(raw) != length(ends)) {
      stop(sprintf(
        "Read %d records from a block the quote scan counted %d in; the file does not quote the way `quote` says.",
        nrow(raw), length(ends)), call. = FALSE)
    }
    if (!is.null(file_encoding)) {
      for (j in seq_along(raw)) {
        if (is.character(raw[[j]])) {
          raw[[j]] <- iconv(raw[[j]], from = file_encoding, to = "UTF-8")
        }
      }
    }
    emit(unescape_quotes(raw, quote))
  }
}


#' Count the data lines of a delimited file
#'
#' Counts newlines rather than parsed records, which is the same line-equals-
#' record assumption the chunked read already makes when it skips by row offset.
#'
#' @param path Character. Path to the file.
#' @param head_rows Integer. Header lines to discount.
#' @return The number of data lines.
#' @noRd
count_data_lines <- function(path, head_rows = 0L) {
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  newline <- as.raw(10L)
  n <- 0
  last <- newline
  repeat {
    block <- readBin(con, "raw", 16777216L)
    if (length(block) == 0L) break
    n <- n + sum(block == newline)
    last <- block[length(block)]
  }
  # A final line with no trailing newline still holds a record.
  if (!identical(last, newline)) n <- n + 1
  max(0, n - head_rows)
}


#' Column names of a delimited file
#'
#' @param path,sep,quote,encoding As in [delim_chunk_feed()].
#' @param col_names Character vector for a headerless file, or `NULL`.
#' @return Character vector of column names in file order.
#' @noRd
delim_header <- function(path, sep, quote, encoding, col_names) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required to stream a delimited file.",
         call. = FALSE)
  }
  if (!is.null(col_names)) return(col_names)
  names(data.table::fread(path, sep = sep, quote = quote, nrows = 0L,
                          encoding = encoding, showProgress = FALSE))
}


#' Build an id -> value lookup for the ids a file's foreign keys reference
#'
#' A backbone that denormalizes its higher classification stores foreign keys
#' rather than names, so normalizing a row needs a name that lives in some other
#' row. GBIF is the case: kingdom, phylum, class, order and family all arrive as
#' `*_key` columns pointing at rows that the build then drops.
#'
#' The whole id -> name map would be as large as the backbone, but only the ids
#' actually referenced are ever looked up, and those are the few thousand rows
#' at the ranks the keys point to. Two scans find them without holding the map:
#' the first collects the distinct keys, the second keeps the `id`/`value` pair
#' of each row a key names. What comes back is small enough to sit beside a
#' chunk, which makes the rest of the normalization row-local.
#'
#' The result is exactly the lookup a whole-table self-join would produce; it is
#' restricted by which ids are referenced, never by an assumption about the rank
#' a key points at.
#'
#' @param path Character. Path to the delimited file.
#' @param id_col Character. Column holding the row's own identifier.
#' @param value_col Character. Column whose value the keys resolve to.
#' @param key_cols Character vector of foreign-key columns.
#' @param chunk_rows Integer. Rows to read per scan chunk.
#' @param sep,quote,encoding,na_strings,col_names As in [delim_chunk_feed()].
#' @param verbose Logical.
#' @return A named character vector of `value`, named by `id`.
#' @export
delim_fk_lookup <- function(path, id_col, value_col, key_cols,
                            chunk_rows = 1000000L, sep = "\t", quote = "",
                            encoding = "UTF-8", na_strings = c("", "NA"),
                            col_names = NULL, verbose = TRUE) {
  scan_feed <- function(cols) {
    delim_chunk_feed(path, normalize = identity, chunk_rows = chunk_rows,
                     sep = sep, quote = quote, encoding = encoding,
                     select = cols, na_strings = na_strings,
                     col_names = col_names, verbose = FALSE)
  }

  if (verbose) message("  scanning for referenced keys...")
  wanted <- character(0)
  feed <- scan_feed(key_cols)
  repeat {
    chunk <- feed()
    if (is.null(chunk)) break
    keys <- unlist(chunk, use.names = FALSE)
    wanted <- unique(c(wanted, keys[!is.na(keys)]))
  }

  if (verbose) {
    message(sprintf("  resolving %s referenced keys...",
                    format(length(wanted), big.mark = ",")))
  }
  ids <- list()
  vals <- list()
  n <- 0L
  feed <- scan_feed(c(id_col, value_col))
  repeat {
    chunk <- feed()
    if (is.null(chunk)) break
    hit <- chunk[[id_col]] %in% wanted
    if (!any(hit)) next
    n <- n + 1L
    ids[[n]] <- chunk[[id_col]][hit]
    vals[[n]] <- chunk[[value_col]][hit]
  }

  lookup <- stats::setNames(unlist(vals, use.names = FALSE),
                            unlist(ids, use.names = FALSE))
  if (verbose) {
    message(sprintf("  resolved %s of %s keys",
                    format(length(lookup), big.mark = ","),
                    format(length(wanted), big.mark = ",")))
  }
  lookup
}


#' Decompress a gzipped file to a plain file
#'
#' A chunked reader seeks by row offset, and every seek into a `.gz` restarts
#' the decompression, so a gzipped source is expanded once up front instead.
#'
#' @param gz_path Character. Path to the `.gz` file.
#' @param out_path Character. Path to write the decompressed file to.
#' @param verbose Logical.
#' @return `out_path`, invisibly.
#' @export
gunzip_file <- function(gz_path, out_path, verbose = TRUE) {
  if (verbose) message(sprintf("Decompressing %s...", basename(gz_path)))
  con <- gzfile(gz_path, "rb")
  on.exit(close(con), add = TRUE)
  out <- file(out_path, "wb")
  on.exit(close(out), add = TRUE)
  repeat {
    block <- readBin(con, "raw", 16777216L)
    if (length(block) == 0L) break
    writeBin(block, out)
  }
  if (verbose) {
    message(sprintf("  %.1f MB", file.size(out_path) / 1048576))
  }
  invisible(out_path)
}
