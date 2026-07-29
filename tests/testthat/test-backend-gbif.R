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

test_that("gbif_resolve_higher resolves higher-rank keys to names (#24)", {
  # read_gbif() feeds kingdom_key/phylum_key/class_key/order_key through this
  # helper (as it already does for family_key) so each row carries denormalized
  # ancestor names before the KINGDOM..ORDER rows are dropped.
  df <- data.frame(
    id             = c("1", "2", "3", "4", "5"),
    canonical_name = c("Animalia", "Chordata", "Mammalia", "Carnivora",
                       "Canidae"),
    stringsAsFactors = FALSE
  )
  keys <- c("1", "3", "4", NA, "999")
  expect_equal(taxifydb:::gbif_resolve_higher(df, keys),
               c("Animalia", "Mammalia", "Carnivora", NA, NA))
})
