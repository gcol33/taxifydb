test_that("the newest COL XR without an export gets one requested", {
  local_mocked_bindings(readLines = function(...) '{"result":[
    {"key":2,"alias":"COL26.9 XR","issued":"2026-09-25"},
    {"key":1,"alias":"COL26.8 XR","issued":"2026-08-26"},
    {"key":9,"alias":"Other XR","issued":"2026-09-30"}]}', .package = "base")
  requested <- character()
  rel <- colxr_latest_release(
    verbose = FALSE, export_ready = function(key) FALSE,
    request_export = function(key) {
      requested <<- c(requested, key)
      TRUE
    }
  )
  expect_equal(rel$alias, "COL26.9 XR")
  expect_equal(requested, "2")

  requested <- character()
  rel <- colxr_latest_release(
    verbose = FALSE, export_ready = function(key) key == "1",
    request_export = function(key) {
      requested <<- c(requested, key)
      FALSE
    }
  )
  expect_equal(rel$key, "1")
  expect_equal(requested, "2")

  expect_error(colxr_latest_release(verbose = FALSE,
                                    export_ready = function(key) FALSE,
                                    request_export = function(key) FALSE),
               "downloadable export")
})

test_that("an export request without a GBIF account returns FALSE", {
  old <- Sys.getenv(c("GBIF_USER", "GBIF_PWD"), unset = NA)
  on.exit(for (v in names(old)) {
    if (is.na(old[[v]])) Sys.unsetenv(v) else do.call(Sys.setenv, as.list(old[v]))
  })
  Sys.unsetenv(c("GBIF_USER", "GBIF_PWD"))
  expect_false(colxr_request_export("1", verbose = FALSE))
})

test_that("COL XR takes a publication reference only where ID, name and author agree", {
  out <- data.frame(taxon_id = c("8MN6", "8MN7", "X1", "Z9"),
                    canonical_name = c("Absinthium vulgare", "Absinthium vulgare",
                                       "Aaptos nigra", "Nova species"),
                    authorship = c("(L.) Dulac", "Lam.", "Bowerbank", NA),
                    stringsAsFactors = FALSE)
  pub <- data.frame(taxon_id = c("8MN6", "8MN7", "X1"),
                    canonical_name = c("Absinthium vulgare", "Absinthium vulgare",
                                       "Aaptos niger"),
                    authorship = c("(L.) Dulac", "Lam.", "Bowerbank"),
                    name_published_in = c("Dulac. Fl. Hautes-Pyrenees 502 (1867).",
                                          "Lam. Fl. Franc.", "Ref"),
                    year = c(1867L, NA, 1873L), stringsAsFactors = FALSE)
  got <- colxr_attach_publications(out, pub)
  expect_equal(got$name_published_in,
               c("Dulac. Fl. Hautes-Pyrenees 502 (1867).", "Lam. Fl. Franc.",
                 NA, NA))
  expect_identical(got$year, c(1867L, NA, NA, NA))
})
