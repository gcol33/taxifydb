test_that("a COL XR release without an export is skipped for the previous one", {
  local_mocked_bindings(readLines = function(...) '{"result":[
    {"key":2,"alias":"COL26.9 XR","issued":"2026-09-25"},
    {"key":1,"alias":"COL26.8 XR","issued":"2026-08-26"},
    {"key":9,"alias":"Other XR","issued":"2026-09-30"}]}', .package = "base")
  rel <- colxr_latest_release(verbose = FALSE,
                              export_ready = function(key) key == "1")
  expect_equal(rel$key, "1")
  expect_equal(rel$alias, "COL26.8 XR")
  expect_error(colxr_latest_release(verbose = FALSE,
                                    export_ready = function(key) FALSE),
               "downloadable export")
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
