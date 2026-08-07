# vtr_changed() is the release gate: a build whose bytes match the published
# asset must not cut a version, because taxify's runtime refetches on a fresh
# version and the user would download a file they already hold. It fails open,
# so every case it cannot decide still publishes.

vtr_fixture <- function(dir, bytes = as.raw(1:200)) {
  p <- file.path(dir, "demo.vtr")
  writeBin(bytes, p)
  p
}

manifest_fixture <- function(dir, tag, backends) {
  p <- file.path(dir, paste0("manifest-", tag, ".json"))
  jsonlite::write_json(
    list(schema_version = 2L, backends = backends, enrichments = list()),
    p, pretty = TRUE, auto_unbox = TRUE
  )
  p
}

test_that("identical bytes are not a change", {
  dd <- withr::local_tempdir()
  vtr <- vtr_fixture(dd)
  mf <- manifest_fixture(dd, "same", list(
    demo = list(latest = "2026.07", full_sha256 = taxifydb::sha256(vtr))
  ))
  expect_false(vtr_changed(mf, "demo", vtr))
})

test_that("different bytes are a change", {
  dd <- withr::local_tempdir()
  vtr <- vtr_fixture(dd)
  mf <- manifest_fixture(dd, "diff", list(
    demo = list(latest = "2026.07", full_sha256 = "deadbeef")
  ))
  expect_true(vtr_changed(mf, "demo", vtr))
})

test_that("a rebuild that really changed is caught", {
  dd <- withr::local_tempdir()
  vtr <- vtr_fixture(dd)
  mf <- manifest_fixture(dd, "rebuilt", list(
    demo = list(latest = "2026.07", full_sha256 = taxifydb::sha256(vtr))
  ))
  expect_false(vtr_changed(mf, "demo", vtr))
  writeBin(as.raw(c(1:199, 255L)), vtr)          # one byte differs
  expect_true(vtr_changed(mf, "demo", vtr))
})

test_that("it fails open on anything undecidable", {
  dd <- withr::local_tempdir()
  vtr <- vtr_fixture(dd)

  # No hash recorded for this backbone.
  no_hash <- manifest_fixture(dd, "nosha", list(demo = list(latest = "2026.07")))
  expect_true(vtr_changed(no_hash, "demo", vtr))

  # Backbone absent from the manifest (a first-ever build).
  absent <- manifest_fixture(dd, "absent", list(other = list(latest = "1")))
  expect_true(vtr_changed(absent, "demo", vtr))

  # An empty hash string is no hash at all.
  empty <- manifest_fixture(dd, "empty", list(
    demo = list(latest = "2026.07", full_sha256 = "")
  ))
  expect_true(vtr_changed(empty, "demo", vtr))

  # No manifest, and no built .vtr.
  expect_true(vtr_changed(file.path(dd, "nope.json"), "demo", vtr))
  expect_true(vtr_changed(no_hash, "demo", file.path(dd, "nope.vtr")))
})

test_that("unparseable manifest publishes rather than erroring", {
  dd <- withr::local_tempdir()
  vtr <- vtr_fixture(dd)
  bad <- file.path(dd, "broken.json")
  writeLines("{ not json", bad)
  expect_true(vtr_changed(bad, "demo", vtr))
})
