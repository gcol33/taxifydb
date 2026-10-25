# republish_store_format() re-encodes the stores a manifest records and points
# a new manifest at them. gh is replaced by a fake that records each call; the
# legacy bytes come from a cache directory, so nothing is downloaded.

republish_gh <- function() {
  calls <- list()
  fn <- function(args, stdout = "", stderr = "") {
    calls[[length(calls) + 1L]] <<- args
    if (args[[2L]] == "view") return(character(0L))
    if (args[[2L]] == "upload") return(character(0L))
    stop("unexpected gh call: ", paste(args, collapse = " "))
  }
  list(fn = fn, calls = function() calls)
}

upload_names <- function(calls) {
  up <- Filter(function(a) identical(a[[2L]], "upload"), calls)
  unlist(lapply(up, function(a) {
    paths <- a[-(1:3)]
    basename(gsub("^[\"']|[\"']$", "", paths[seq_len(match("--repo", paths) - 1L)]))
  }))
}

republish_fixture <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  cache <- file.path(dir, "cache")
  bb <- file.path(cache, "wfo", "latest")
  en <- file.path(cache, "enrichment", "leda", "latest")
  dir.create(bb, recursive = TRUE)
  dir.create(en, recursive = TRUE)

  wfo <- file.path(bb, "wfo.vtr")
  vectra::write_vtr(data.frame(canonical_name = c("Abies alba", "Pinus nigra"),
                               normalizedName = c("abies alba", "pinus nigra"),
                               n = c(1L, NA)), wfo)
  leda <- file.path(en, "leda.vtr")
  vectra::write_vtr(data.frame(canonical_name = "Abies alba", sla = 12.5), leda)
  refs <- file.path(en, "leda_references.vtr")
  vectra::write_vtr(data.frame(ref_id = "r1", citation = "Kleyer 2008"), refs)

  rel <- "https://github.com/o/r/releases/download"
  md5 <- function(p) unname(tools::md5sum(p))
  manifest <- list(
    schema_version = 2L,
    backends = list(wfo = list(
      latest = "2026.06",
      full_url = sprintf("%s/wfo-2026.06/wfo.vtr", rel),
      full_size = file.size(wfo), full_sha256 = taxifydb:::sha256(wfo),
      nrow = 2L, content_id = md5(wfo),
      delta_from = "2025.12", delta_url = sprintf("%s/wfo-2026.06/wfo.xdelta", rel),
      delta_size = 10L, citation = list(text = "WFO (2026)"))),
    enrichments = list(leda = list(
      latest = "2026.08",
      full_url = sprintf("%s/enrichment-2026.08/leda.vtr", rel),
      content_id = md5(leda), nrow = 1L, trait_cols = list("sla"),
      references = list(
        url = sprintf("%s/enrichment-2026.08/leda_references.vtr", rel),
        content_id = md5(refs), nrow = 1L))))
  legacy <- file.path(dir, "manifest.json")
  taxifydb:::write_json_lf(manifest, legacy, pretty = TRUE, auto_unbox = TRUE)
  list(dir = dir, cache = cache, legacy = legacy,
       out = file.path(dir, "manifest_v3.json"), wfo = wfo)
}

test_that("every store is re-encoded, uploaded under its format name, and recorded", {
  fx <- republish_fixture()
  gh <- republish_gh()
  local_mocked_bindings(.gh = gh$fn)

  recs <- suppressMessages(republish_store_format(
    stats::setNames(fx$out, fx$legacy), file.path(fx$dir, "work"),
    cache_dir = fx$cache, repo = "o/r"))
  expect_length(recs, 3L)

  up <- upload_names(gh$calls())
  expect_true(all(c("wfo.tdcf.vtr", "leda.tdcf.vtr",
                    "leda_references.tdcf.vtr") %in% up))
  wfo_rec <- recs[[grep("wfo.vtr$", names(recs))]]
  expect_true(sprintf("wfo-%s.vtr", wfo_rec$md5) %in% up)
  expect_false(any(c("wfo.vtr", "leda.vtr") %in% up))

  # The dropped column is gone; the rest reads back as written.
  new_wfo <- file.path(fx$dir, "work", "tdcf", "wfo-2026.06", "wfo.vtr")
  back <- vectra::collect(vectra::tbl(new_wfo))
  expect_named(back, c("canonical_name", "n"))
  expect_equal(back$n, c(1, NA))
})

test_that("the new manifest changes only what the store bytes determine", {
  fx <- republish_fixture()
  local_mocked_bindings(.gh = republish_gh()$fn)
  recs <- suppressMessages(republish_store_format(
    stats::setNames(fx$out, fx$legacy), file.path(fx$dir, "work"),
    cache_dir = fx$cache, repo = "o/r"))

  old <- jsonlite::read_json(fx$legacy, simplifyVector = FALSE)
  new <- jsonlite::read_json(fx$out, simplifyVector = FALSE)
  expect_identical(new$schema_version, 3L)

  w <- new$backends$wfo
  rec <- recs[[old$backends$wfo$full_url]]
  expect_identical(w$full_url,
                   "https://github.com/o/r/releases/download/wfo-2026.06/wfo.tdcf.vtr")
  expect_identical(w$content_id, rec$md5)
  expect_identical(w$content_url, sprintf(
    "https://github.com/o/r/releases/download/wfo-2026.06/wfo-%s.vtr", rec$md5))
  expect_identical(w$full_sha256, rec$sha256)
  expect_null(w$delta_url)
  expect_null(w$delta_from)
  expect_identical(w$citation, old$backends$wfo$citation)
  expect_identical(w$latest, "2026.06")

  l <- new$enrichments$leda
  expect_match(l$full_url, "enrichment-2026.08/leda\\.tdcf\\.vtr$")
  expect_match(l$references$url, "enrichment-2026.08/leda_references\\.tdcf\\.vtr$")
  expect_match(l$references$content_url,
               sprintf("leda_references-%s\\.vtr$", l$references$content_id))
  expect_identical(l$trait_cols, list("sla"))
})

test_that("a finished store is not redone when the run is resumed", {
  fx <- republish_fixture()
  local_mocked_bindings(.gh = republish_gh()$fn)
  work <- file.path(fx$dir, "work")
  suppressMessages(republish_store_format(
    stats::setNames(fx$out, fx$legacy), work, cache_dir = fx$cache, repo = "o/r"))

  gh <- republish_gh()
  local_mocked_bindings(.gh = gh$fn)
  suppressMessages(republish_store_format(
    stats::setNames(fx$out, fx$legacy), work, cache_dir = fx$cache, repo = "o/r"))
  expect_length(upload_names(gh$calls()), 0L)
})

test_that("a cached copy that does not hash to the recorded id is not used", {
  fx <- republish_fixture()
  vectra::write_vtr(data.frame(canonical_name = "Other"), fx$wfo)
  local_mocked_bindings(.gh = republish_gh()$fn)
  local_mocked_bindings(curl_download = function(url, destfile, ...) {
    writeLines("not the published bytes", destfile)
  }, .package = "curl")
  expect_error(
    suppressMessages(republish_store_format(
      stats::setNames(fx$out, fx$legacy), file.path(fx$dir, "work"),
      cache_dir = fx$cache, only = "wfo", repo = "o/r")),
    "do not match the manifest's checksum")
})

test_that("a re-encoded column that differs from the source is caught", {
  dir <- withr::local_tempdir()
  a <- file.path(dir, "a.vtr")
  b <- file.path(dir, "b.vtr")
  vectra::write_vtr(data.frame(x = c("p", "q"), y = 1:2), a)
  vectra::write_vtr(data.frame(x = c("p", "r"), y = 1:2), b)
  expect_error(taxifydb:::.verify_reencoded(a, b), "column x differs")
  vectra::write_vtr(data.frame(x = c("p", "q"), z = 1:2), b)
  expect_error(taxifydb:::.verify_reencoded(a, b), "columns not in the source: z")
})
