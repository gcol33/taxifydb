# A manifest entry's content_url names `<name>-<content_id>.vtr` on the release
# it was cut under. Whatever publishes the release has to upload that copy, or
# the URL 404s while every other field of the entry is correct. gh is replaced
# by a fake that records each call and answers from a scripted release state.

fake_gh <- function(exists = FALSE, assets = character(0L), upload_status = 0L) {
  calls <- list()
  fn <- function(args, stdout = "", stderr = "") {
    calls[[length(calls) + 1L]] <<- args
    verb <- args[[2L]]
    if (verb == "view" && "--json" %in% args) return(assets)
    if (verb == "view") return(if (exists) 0L else 1L)
    if (verb == "create") return(0L)
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

test_that("absent optional artifacts come back empty and a missing .vtr stops", {
  dir <- withr::local_tempdir()
  touch(dir, "wfo.vtr")

  a <- taxifydb:::.backbone_artifacts(dir, "wfo")
  expect_null(a$delta)
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
  gh <- fake_gh(exists = TRUE, assets = c("wfo.vtr", "wfo-0123abcd.vtr"))
  local_mocked_bindings(.gh = gh$fn)

  suppressMessages(publish_release("wfo", "2026.09", vtr, repo = "o/r",
                                   notes = "n"))

  up <- uploads(gh$calls())
  expect_length(up, 2L)
  expect_equal(uploaded_names(up[[2L]]),
               sprintf("wfo-%s.vtr", unname(tools::md5sum(vtr))))
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
