# Resuming a publish reuses whatever .vtr sits in the output directory. That
# file carries no marker of which source release produced it, so the sidecar the
# build wrote is what has to agree with the registry before the bytes go up.

make_build <- function(dir, meta) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  vtr <- file.path(dir, "x.vtr")
  file.create(vtr)
  jsonlite::write_json(meta, file.path(dir, "meta.json"), auto_unbox = TRUE)
  vtr
}

test_that("a build matching the registry is reusable", {
  reg <- .enrichment_build_registry[["glonaf"]]
  vtr <- make_build(withr::local_tempdir(),
                    list(source_url = reg$source_url, version = reg$version))

  expect_true(assert_built_matches_registry("glonaf", vtr))
})

test_that("a build from a superseded source release is refused", {
  reg <- .enrichment_build_registry[["glonaf"]]
  vtr <- make_build(withr::local_tempdir(),
                    list(source_url = "https://zenodo.org/api/records/13235357",
                         version = "2024.1"))

  expect_error(assert_built_matches_registry("glonaf", vtr),
               "predates the current registry entry")
  expect_error(assert_built_matches_registry("glonaf", vtr), "13235357")
  expect_error(assert_built_matches_registry("glonaf", vtr), reg$source_url,
               fixed = TRUE)
})

test_that("a build with no sidecar is refused rather than trusted", {
  dir <- withr::local_tempdir()
  vtr <- file.path(dir, "x.vtr")
  file.create(vtr)

  expect_error(assert_built_matches_registry("glonaf", vtr), "no meta.json")
})
