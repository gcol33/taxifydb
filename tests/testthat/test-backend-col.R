# Build-time COL helpers. Internal (`@noRd`) so accessed via `:::`.

test_that("col_strip_authorship removes authorship correctly", {
  sci <- c("Quercus robur L.", "Pinus sylvestris L.", "Festulolium")
  auth <- c("L.", "L.", NA)
  result <- taxifydb:::col_strip_authorship(sci, auth)
  expect_equal(result, c("Quercus robur", "Pinus sylvestris", "Festulolium"))
})

test_that("col_strip_authorship handles complex authorship", {
  sci <- "Quercus petraea (Matt.) Liebl."
  auth <- "(Matt.) Liebl."
  result <- taxifydb:::col_strip_authorship(sci, auth)
  expect_equal(result, "Quercus petraea")
})

test_that("col_strip_authorship handles NA values", {
  sci <- c("Quercus robur L.", NA, "Pinus sylvestris L.")
  auth <- c("L.", NA, "L.")
  result <- taxifydb:::col_strip_authorship(sci, auth)
  expect_equal(result, c("Quercus robur", NA, "Pinus sylvestris"))
})
