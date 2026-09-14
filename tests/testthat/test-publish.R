# A manifest entry's content_url names `<name>-<content_id>.vtr` on the release
# it was cut under. Whatever publishes the release has to upload that copy, or
# the URL 404s while every other field of the entry is correct. gh is replaced
# by a fake that records each call and answers from a scripted release state.

fake_gh <- function(exists = FALSE, assets = character(0L), upload_status = 0L,
                    download_bytes = "published bytes") {
  calls <- list()
  fn <- function(args, stdout = "", stderr = "") {
    calls[[length(calls) + 1L]] <<- args
    verb <- args[[2L]]
    if (verb == "view" && "--json" %in% args) return(assets)
    if (verb == "view") return(if (exists) 0L else 1L)
    if (verb == "create") return(0L)
    if (verb == "download") {
      dir <- gsub("^[\"']|[\"']$", "", args[[match("--dir", args) + 1L]])
      writeLines(download_bytes, file.path(dir, args[[match("--pattern", args) + 1L]]))
      return(character(0L))
    }
    if (verb == "upload") {
      out <- character(0L)
      if (upload_status != 0L) attr(out, "status") <- upload_status
      return(out)
    }
    stop("unexpected gh call: ", paste(args, collapse = " "))
  }
  list(fn = fn, calls = function() calls)
}

uploads <- function(calls) {
  Filter(function(a) identical(a[[2L]], "upload"), calls)
}

# Asset names an upload call carries, with the shell quoting removed.
uploaded_names <- function(call) {
  paths <- call[-(1:3)]
  paths <- paths[seq_len(match("--repo", paths) - 1L)]
  basename(gsub("^[\"']|[\"']$", "", paths))
}

touch <- function(dir, name, text = name) {
  path <- file.path(dir, name)
  writeLines(text, path)
  path
}

test_that("the artifact set is every file the build wrote for its backbone", {
  dir <- withr::local_tempdir()
  touch(dir, "worms.vtr")
  touch(dir, "worms.xdelta")
  touch(dir, "worms.meta")
  touch(dir, "worms_species_profile.vtr")
  touch(dir, "col_species_profile.vtr")
  touch(dir, "worms_notes.txt")

  a <- taxifydb:::.backbone_artifacts(dir, "worms")
  expect_equal(a$vtr, file.path(dir, "worms.vtr"))
  expect_equal(a$delta, file.path(dir, "worms.xdelta"))
  expect_equal(a$meta, file.path(dir, "worms.meta"))
  expect_equal(basename(a$extras), "worms_species_profile.vtr")
})

test_that("the artifact set reads the content id a patch was cut against", {
  dir <- withr::local_tempdir()
  touch(dir, "worms.vtr")
  touch(dir, "worms.xdelta")
  expect_null(taxifydb:::.backbone_artifacts(dir, "worms")$delta_from_content_id)

  touch(dir, "worms.xdelta.base", "0123456789abcdef0123456789abcdef")
  a <- taxifydb:::.backbone_artifacts(dir, "worms")
  expect_equal(a$delta_from_content_id, "0123456789abcdef0123456789abcdef")
  expect_length(a$extras, 0L)
})

test_that("absent optional artifacts come back empty and a missing .vtr stops", {
  dir <- withr::local_tempdir()
  touch(dir, "wfo.vtr")

  a <- taxifydb:::.backbone_artifacts(dir, "wfo")
  expect_null(a$delta)
  expect_null(a$delta_from_content_id)
  expect_null(a$meta)
  expect_length(a$extras, 0L)

  expect_error(taxifydb:::.backbone_artifacts(dir, "ncbi"), "Build first")
})

test_that("publish_release uploads the content-addressed copy content_url names", {
  dir <- withr::local_tempdir()
  vtr  <- touch(dir, "wfo.vtr", "wfo bytes")
  meta <- touch(dir, "wfo.meta")
  side <- touch(dir, "wfo_profile.vtr")
  gh <- fake_gh(exists = FALSE)
  local_mocked_bindings(.gh = gh$fn)

  expect_message(
    publish_release("wfo", "2026.09", vtr, meta_path = meta, extras = side,
                    repo = "o/r", notes = "n"),
    "Published release: wfo-2026.09")

  calls <- gh$calls()
  expect_true(any(vapply(calls, function(a) identical(a[[2L]], "create"),
                         logical(1L))))
  up <- uploads(calls)
  expect_length(up, 2L)

  expect_setequal(uploaded_names(up[[1L]]),
                  c("wfo.vtr", "wfo.meta", "wfo_profile.vtr"))
  expect_true("--clobber" %in% up[[1L]])

  cid <- unname(tools::md5sum(vtr))
  expect_equal(uploaded_names(up[[2L]]), sprintf("wfo-%s.vtr", cid))
  expect_false("--clobber" %in% up[[2L]])
})

test_that("re-publishing an existing tag keeps it and skips a copy already there", {
  dir <- withr::local_tempdir()
  vtr <- touch(dir, "wfo.vtr", "wfo bytes")
  cid <- unname(tools::md5sum(vtr))
  gh <- fake_gh(exists = TRUE,
                assets = c("wfo.vtr", sprintf("wfo-%s.vtr", cid)))
  local_mocked_bindings(.gh = gh$fn)

  expect_message(
    publish_release("wfo", "2026.09", vtr, repo = "o/r", notes = "n"),
    "already exists")

  verbs <- vapply(gh$calls(), function(a) a[[2L]], character(1L))
  expect_false(any(verbs %in% c("create", "delete")))
  up <- uploads(gh$calls())
  expect_length(up, 1L)
  expect_equal(uploaded_names(up[[1L]]), "wfo.vtr")
})

test_that("a re-cut under an existing tag adds its own content-addressed copy", {
  dir <- withr::local_tempdir()
  vtr <- touch(dir, "wfo.vtr", "new wfo bytes")
  gh <- fake_gh(exists = TRUE,
                assets = c("wfo.vtr", "wfo-0123456789abcdef0123456789abcdef.vtr"))
  local_mocked_bindings(.gh = gh$fn)

  suppressMessages(publish_release("wfo", "2026.09", vtr, repo = "o/r",
                                   notes = "n"))

  up <- uploads(gh$calls())
  expect_length(up, 2L)
  expect_equal(uploaded_names(up[[2L]]),
               sprintf("wfo-%s.vtr", unname(tools::md5sum(vtr))))
})

test_that("a rolling .vtr with no content-addressed copy is preserved before it is replaced", {
  # wfo-2026.06 was cut as a June build of WFO 2024-12, before content-addressed
  # copies existed; the WFO June 2026 edition publishes under the same tag.
  dir <- withr::local_tempdir()
  vtr <- touch(dir, "wfo.vtr", "wfo 2026-06 edition")
  gh <- fake_gh(exists = TRUE, assets = c("wfo.vtr", "wfo.meta"),
                download_bytes = "wfo 2024-12 bytes")
  local_mocked_bindings(.gh = gh$fn)

  suppressMessages(publish_release("wfo", "2026.06", vtr, repo = "o/r",
                                   notes = "n"))

  calls <- gh$calls()
  verbs <- vapply(calls, function(a) a[[2L]], character(1L))
  up <- uploads(calls)
  expect_length(up, 3L)

  old <- withr::local_tempfile()
  writeLines("wfo 2024-12 bytes", old)
  preserved <- sprintf("wfo-%s.vtr", unname(tools::md5sum(old)))
  expect_equal(uploaded_names(up[[1L]]), preserved)
  expect_false("--clobber" %in% up[[1L]])
  # The old bytes are fetched and kept before the clobbering upload runs.
  expect_lt(match("download", verbs),
            which(verbs == "upload" & vapply(calls, function(a) "--clobber" %in% a,
                                             logical(1L))))
  expect_equal(uploaded_names(up[[2L]]), "wfo.vtr")
  expect_true("--clobber" %in% up[[2L]])
})

test_that("a release with no rolling .vtr yet fetches nothing", {
  dir <- withr::local_tempdir()
  vtr <- touch(dir, "wfo.vtr")
  gh <- fake_gh(exists = TRUE, assets = "wfo.meta")
  local_mocked_bindings(.gh = gh$fn)

  suppressMessages(publish_release("wfo", "2026.06", vtr, repo = "o/r",
                                   notes = "n"))
  verbs <- vapply(gh$calls(), function(a) a[[2L]], character(1L))
  expect_false("download" %in% verbs)
})

test_that("a publish must use the version its build recorded", {
  dir <- withr::local_tempdir()
  vtr <- touch(dir, "wfo.vtr")
  writeLines(c("backend=wfo", "version=2026.06"), file.path(dir, "wfo.meta"))
  gh <- fake_gh(exists = FALSE)
  local_mocked_bindings(.gh = gh$fn)

  expect_error(
    publish_release("wfo", "2026.09", vtr, repo = "o/r", notes = "n"),
    "publishing as version 2026.09, but the build recorded version 2026.06")
  expect_length(gh$calls(), 0L)
})

test_that("the release version is read from the build's .meta and validated", {
  dir <- withr::local_tempdir()
  touch(dir, "gbif.vtr")
  expect_error(taxifydb:::.backbone_release_version(dir, "gbif"),
               "no version recorded")

  writeLines(c("backend=gbif", "version=current"), file.path(dir, "gbif.meta"))
  expect_equal(taxifydb:::.backbone_artifacts(dir, "gbif")$version, "current")
  expect_error(taxifydb:::.backbone_release_version(dir, "gbif"),
               "cannot name a release")

  writeLines(c("backend=gbif", "version=2023.08"), file.path(dir, "gbif.meta"))
  expect_equal(taxifydb:::.backbone_release_version(dir, "gbif"), "2023.08")
})

test_that("publish_enrichment_release uploads a content-addressed copy per .vtr", {
  dir <- withr::local_tempdir()
  a <- touch(dir, "anage.vtr", "anage bytes")
  b <- touch(dir, "leda.vtr", "leda bytes")
  gh <- fake_gh(exists = TRUE)
  local_mocked_bindings(.gh = gh$fn)

  suppressMessages(publish_enrichment_release("2026.09", c(a, b), repo = "o/r"))

  up <- uploads(gh$calls())
  expect_length(up, 2L)
  expect_setequal(uploaded_names(up[[1L]]), c("anage.vtr", "leda.vtr"))
  expect_setequal(uploaded_names(up[[2L]]), c(
    sprintf("anage-%s.vtr", unname(tools::md5sum(a))),
    sprintf("leda-%s.vtr", unname(tools::md5sum(b)))))
})

test_that("a failed upload stops the publish", {
  dir <- withr::local_tempdir()
  vtr <- touch(dir, "wfo.vtr")
  gh <- fake_gh(exists = TRUE, upload_status = 1L)
  local_mocked_bindings(.gh = gh$fn)

  expect_error(
    suppressMessages(publish_release("wfo", "2026.09", vtr, repo = "o/r",
                                     notes = "n")),
    "gh release upload failed for wfo-2026.09")
})
