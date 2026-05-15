# Build-time GBIF status mapping. Internal (`@noRd`) so accessed via `:::`.

test_that("gbif_status_to_standard maps correctly", {
  expect_equal(taxifydb:::gbif_status_to_standard("ACCEPTED"), "ACCEPTED")
  expect_equal(taxifydb:::gbif_status_to_standard("DOUBTFUL"), "ACCEPTED")
  expect_equal(taxifydb:::gbif_status_to_standard("PROVISIONALLY_ACCEPTED"),
               "ACCEPTED")
  expect_equal(taxifydb:::gbif_status_to_standard("SYNONYM"), "SYNONYM")
  expect_equal(taxifydb:::gbif_status_to_standard("HOMOTYPIC_SYNONYM"),
               "SYNONYM")
  expect_equal(taxifydb:::gbif_status_to_standard("HETEROTYPIC_SYNONYM"),
               "SYNONYM")
  expect_equal(taxifydb:::gbif_status_to_standard("PROPARTE_SYNONYM"),
               "SYNONYM")
  expect_equal(taxifydb:::gbif_status_to_standard("MISAPPLIED"), "SYNONYM")
  expect_equal(taxifydb:::gbif_status_to_standard("AMBIGUOUS_SYNONYM"),
               "SYNONYM")
})
