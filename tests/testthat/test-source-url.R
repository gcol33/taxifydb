# Provenance guard for manifest source_url values.
#
# A manifest entry's source_url is the link back to the original data. A value
# whose host cannot resolve reads as provenance while pointing nowhere, so the
# two manifest writers reject it rather than serializing it.

test_that("a real download URL passes through unchanged", {
  url <- paste0("https://github.com/mammaldiversity/mammaldiversity.github.io/",
                "raw/refs/heads/master/assets/data/MDD.zip")
  expect_identical(check_source_url(url, "mdd"), url)
})

test_that("a host with no dot is rejected", {
  expect_error(check_source_url("https://example", "mdd"),
               "no resolvable host")
  expect_error(check_source_url("http://localhost/data.zip", "x"),
               "no resolvable host")
})

test_that("an absent or empty source_url is left alone", {
  expect_null(check_source_url(NULL, "x"))
  expect_identical(check_source_url("", "x"), "")
})

test_that("a non-URL provenance string is not treated as a URL", {
  derived <- "derived from: wfo, col, gbif"
  expect_identical(check_source_url(derived, "genus_register"), derived)
})

test_that("each URL of a multi-source field is checked", {
  ok <- paste("https://hosted-datasets.gbif.org/datasets/backbone/current/backbone.zip",
              "https://ftp.ncbi.nlm.nih.gov/pub/taxonomy/new_taxdump/new_taxdump.tar.gz",
              sep = " ; ")
  expect_identical(check_source_url(ok, "common_names"), ok)

  mixed <- paste("https://zenodo.org/records/1/files/a.csv", "https://example",
                 sep = " ; ")
  expect_error(check_source_url(mixed, "common_names"), "https://example")
})

test_that("every entry of the build-side manifest carries a resolvable source", {
  path <- test_path("..", "..", "manifest", "manifest.json")
  skip_if_not(file.exists(path), "manifest.json not in the checked tree")

  manifest <- jsonlite::read_json(path, simplifyVector = TRUE)
  entries <- c(manifest$backends, manifest$enrichments)

  for (name in names(entries)) {
    url <- entries[[name]]$source_url
    if (is.null(url)) next
    expect_identical(check_source_url(url, name), url)
  }
})
