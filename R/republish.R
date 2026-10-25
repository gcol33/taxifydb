# Re-publish every published store in the current store format.
#
# A store format change (vectra's tdc container) leaves the data of every
# backbone and enrichment as it was, so a republish re-encodes the published
# bytes rather than rebuilding from source: a rebuild would re-read sources
# that have moved on since the build, and the republished asset would no longer
# be the data its tag names. Each store is taken from the asset its manifest
# entry records, checked against the content id recorded there, re-encoded,
# and compared column by column with the store it came from before anything is
# uploaded.


#' Re-encode and republish every store a manifest records
#'
#' For each store asset named by the manifests (the main `.vtr` of every
#' backbone and enrichment, a backbone's extras, an enrichment's reference
#' table), the legacy bytes are taken from `cache_dir` when a copy there hashes
#' to the recorded checksum and downloaded otherwise; re-encoded with
#' `vectra::write_vtr(compress = .published_vtr_compress)`, keeping the row
#' groups of the store it came from; compared with that store column by column;
#' and uploaded under the same release tag with its format-qualified asset name
#' ([.published_asset_name()]) plus a content-addressed copy. The legacy asset is
#' left in place for installs that read the legacy manifest.
#'
#' Every finished store writes a record under `work_dir/done/`, so a run that
#' stops part way resumes at the first store without one. The manifests are
#' written only once every store has a record.
#'
#' @param manifests Named character vector: names are the legacy manifests,
#'   values the manifests to write.
#' @param work_dir Character. Directory for downloads, re-encoded stores and
#'   the per-store records.
#' @param cache_dir Character or NULL. A taxify data directory to take legacy
#'   bytes from before downloading them.
#' @param only Character or NULL. Restrict to these manifest entry names.
#' @param drop_cols Named list of columns to leave out of a backbone's
#'   republished store (`list(wfo = "normalizedName")`), for a column the
#'   current build no longer writes.
#' @param upload Logical. `FALSE` re-encodes and verifies without uploading.
#' @param repo Character. GitHub repo.
#' @return The per-store records, invisibly.
#' @export
republish_store_format <- function(manifests, work_dir, cache_dir = NULL,
                                   only = NULL,
                                   drop_cols = list(wfo = "normalizedName"),
                                   upload = TRUE,
                                   repo = "gcol33/taxifydb") {
  stopifnot(is.character(manifests), length(names(manifests)) == length(manifests))
  legacy <- lapply(names(manifests), jsonlite::read_json, simplifyVector = FALSE)
  assets <- unique(do.call(rbind, lapply(legacy, .manifest_store_assets)))
  assets <- assets[!duplicated(assets$url), , drop = FALSE]
  if (!is.null(only)) assets <- assets[assets$owner %in% only, , drop = FALSE]

  done_dir <- file.path(work_dir, "done")
  dir.create(done_dir, recursive = TRUE, showWarnings = FALSE)

  records <- list()
  for (i in seq_len(nrow(assets))) {
    a <- assets[i, , drop = FALSE]
    rec_path <- file.path(done_dir, sprintf("%s__%s.json", a$tag, a$file))
    if (file.exists(rec_path)) {
      records[[a$url]] <- jsonlite::read_json(rec_path, simplifyVector = TRUE)
      next
    }
    message(sprintf("[%d/%d] %s %s (%s)", i, nrow(assets), a$tag, a$file,
                    format(Sys.time(), "%H:%M:%S")))
    rec <- .republish_one(a, work_dir, cache_dir,
                          drop = if (a$kind == "backbone") drop_cols[[a$owner]],
                          upload = upload, repo = repo)
    write_json_lf(rec, rec_path, pretty = TRUE, auto_unbox = TRUE)
    records[[a$url]] <- rec
  }

  if (is.null(only) && upload) {
    for (k in seq_along(manifests)) {
      m <- .republished_manifest(legacy[[k]], records)
      write_json_lf(m, manifests[[k]], pretty = TRUE, auto_unbox = TRUE)
      message("Wrote ", manifests[[k]])
    }
  }
  invisible(records)
}


#' The store assets a manifest records, one row each
#'
#' @return A data.frame with `owner` (the manifest entry), `kind` (`backbone`,
#'   `extra`, `enrichment`, `references`), `url`, `tag`, `file` (the local
#'   file name), `md5` and `sha256` (whichever the manifest records, else NA).
#' @noRd
.manifest_store_assets <- function(manifest) {
  row <- function(owner, kind, url, md5 = NA_character_, sha256 = NA_character_) {
    data.frame(owner = owner, kind = kind, url = url,
               tag = basename(dirname(url)), file = basename(url),
               md5 = md5 %||% NA_character_, sha256 = sha256 %||% NA_character_,
               stringsAsFactors = FALSE)
  }
  out <- list()
  for (nm in names(manifest$backends)) {
    e <- manifest$backends[[nm]]
    out[[length(out) + 1L]] <- row(nm, "backbone", e$full_url, e$content_id,
                                   e$full_sha256)
    for (x in e$extras %||% list()) {
      out[[length(out) + 1L]] <- row(nm, "extra", x$url, sha256 = x$sha256)
    }
  }
  for (nm in names(manifest$enrichments)) {
    e <- manifest$enrichments[[nm]]
    out[[length(out) + 1L]] <- row(nm, "enrichment", e$full_url, e$content_id)
    if (!is.null(e$references)) {
      out[[length(out) + 1L]] <- row(nm, "references", e$references$url,
                                     e$references$content_id)
    }
  }
  do.call(rbind, out)
}


#' Re-encode, verify and upload one store asset
#' @return The store's record: legacy `url`, `tag`, `file`, and the
#'   republished store's `size`, `sha256`, `md5` and `nrow`.
#' @noRd
.republish_one <- function(a, work_dir, cache_dir, drop, upload, repo) {
  src <- .legacy_store(a, work_dir, cache_dir)
  out <- file.path(work_dir, "tdcf", a$tag, a$file)
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)

  node <- vectra::tbl(src)
  if (length(drop)) {
    keep <- setdiff(.vtr_columns(src), drop)
    node <- do.call(vectra::select, c(list(node), lapply(keep, as.name)))
  }
  t <- system.time(
    vectra::write_vtr(node, out, compress = .published_vtr_compress)
  )[["elapsed"]]
  nrow <- .verify_reencoded(src, out)
  message(sprintf("  %s: %.1f MiB -> %.1f MiB in %.0f s, %s rows identical",
                  a$file, file.size(src) / 2^20, file.size(out) / 2^20, t,
                  format(nrow, big.mark = ",")))

  if (upload) {
    staged <- .stage_published(out)
    on.exit(unlink(dirname(staged), recursive = TRUE), add = TRUE)
    .upload_assets(a$tag, staged, repo, clobber = TRUE)
    if (a$kind != "extra") .upload_content_addressed(a$tag, out, repo)
  }
  if (startsWith(normalizePath(src, winslash = "/"),
                 normalizePath(file.path(work_dir, "legacy"), winslash = "/",
                               mustWork = FALSE))) {
    unlink(src)
  }
  list(url = a$url, tag = a$tag, file = a$file, size = file.size(out),
       sha256 = sha256(out), md5 = unname(tools::md5sum(out)), nrow = nrow)
}


#' The legacy bytes of a store asset, verified against the manifest
#'
#' A copy in `cache_dir` (the asset's own backbone or enrichment directory, at
#' any build) is used when it hashes to the recorded checksum; otherwise the
#' asset is downloaded.
#' @noRd
.legacy_store <- function(a, work_dir, cache_dir) {
  matches <- function(p) {
    if (!is.na(a$md5)) identical(unname(tools::md5sum(p)), a$md5)
    else if (!is.na(a$sha256)) identical(sha256(p), a$sha256)
    else FALSE
  }
  if (!is.null(cache_dir)) {
    dirs <- file.path(cache_dir, c(a$owner, file.path("enrichment", a$owner)))
    cands <- unlist(lapply(dirs[dir.exists(dirs)], list.files,
                           pattern = sprintf("^%s$", gsub(".", "\\.", a$file,
                                                          fixed = TRUE)),
                           recursive = TRUE, full.names = TRUE))
    for (p in cands) if (matches(p)) return(p)
  }
  dest <- file.path(work_dir, "legacy", a$tag, a$file)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  curl::curl_download(a$url, dest, quiet = TRUE)
  if (!matches(dest)) {
    stop(sprintf("%s: downloaded bytes do not match the manifest's checksum.",
                 a$url), call. = FALSE)
  }
  dest
}


#' Column names of a `.vtr`
#' @noRd
.vtr_columns <- function(path) {
  names(vectra::collect(utils::head(vectra::tbl(path), 1L)))
}


#' Check a re-encoded store against the store it came from
#'
#' Every column of the re-encoded store must be identical to the same column
#' of the source, read one column at a time so a multi-gigabyte backbone is
#' never held whole. The source may carry columns the re-encoded store leaves
#' out (`drop_cols`); the re-encoded store may carry none the source lacks.
#' @return The row count, invisibly.
#' @noRd
.verify_reencoded <- function(src, out) {
  cols_src <- .vtr_columns(src)
  cols_out <- .vtr_columns(out)
  extra <- setdiff(cols_out, cols_src)
  if (length(extra)) {
    stop(sprintf("%s: columns not in the source: %s", out,
                 paste(extra, collapse = ", ")), call. = FALSE)
  }
  if (!identical(cols_out, cols_src[cols_src %in% cols_out])) {
    stop(sprintf("%s: column order differs from the source.", out), call. = FALSE)
  }
  col_of <- function(p, col) {
    vectra::collect(do.call(vectra::select,
                            list(vectra::tbl(p), as.name(col))))[[1L]]
  }
  n <- NA_integer_
  for (col in cols_out) {
    a <- col_of(src, col)
    b <- col_of(out, col)
    if (!identical(a, b)) {
      stop(sprintf("%s: column %s differs from the source.", out, col),
           call. = FALSE)
    }
    n <- length(a)
  }
  invisible(n)
}


#' A manifest with every store entry pointing at its republished asset
#'
#' Changes only what the store bytes determine -- the asset URL, size,
#' checksums and content-addressed URL -- and drops delta fields, since a delta
#' cut against a legacy store cannot patch a republished one. Every other field
#' is carried over as it stands.
#' @param manifest A legacy manifest (list, as read by `jsonlite::read_json()`).
#' @param records Per-store records keyed by legacy URL.
#' @noRd
.republished_manifest <- function(manifest, records) {
  rec <- function(url) {
    r <- records[[url]]
    if (is.null(r)) stop("No republished store for ", url, call. = FALSE)
    r
  }
  ca_url <- function(r) {
    sprintf("%s/%s-%s.vtr", dirname(r$url), tools::file_path_sans_ext(r$file),
            r$md5)
  }
  asset_url <- function(r) {
    sprintf("%s/%s", dirname(r$url), .published_asset_name(r$file))
  }

  for (nm in names(manifest$backends)) {
    e <- manifest$backends[[nm]]
    r <- rec(e$full_url)
    e$full_url <- asset_url(r)
    e$full_size <- r$size
    e$full_sha256 <- r$sha256
    e$content_id <- r$md5
    e$content_url <- ca_url(r)
    e$nrow <- r$nrow
    e[c("delta_from", "delta_from_content_id", "delta_url", "delta_size")] <- NULL
    if (!is.null(e$extras)) {
      e$extras <- lapply(e$extras, function(x) {
        rx <- rec(x$url)
        x$url <- asset_url(rx)
        x$size <- rx$size
        x$sha256 <- rx$sha256
        x
      })
    }
    manifest$backends[[nm]] <- e
  }
  for (nm in names(manifest$enrichments)) {
    e <- manifest$enrichments[[nm]]
    r <- rec(e$full_url)
    e$full_url <- asset_url(r)
    e$content_id <- r$md5
    e$content_url <- ca_url(r)
    if (!is.null(e$references)) {
      rr <- rec(e$references$url)
      e$references$url <- asset_url(rr)
      e$references$content_id <- rr$md5
      e$references$content_url <- ca_url(rr)
    }
    manifest$enrichments[[nm]] <- e
  }
  manifest$schema_version <- 3L
  manifest
}
