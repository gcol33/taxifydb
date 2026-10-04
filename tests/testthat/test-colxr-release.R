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
