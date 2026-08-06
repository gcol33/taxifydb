test_that("a binomial splits into genus and specific epithet", {
  p <- split_scientific_name("Quercus robur")
  expect_equal(p$genus, "Quercus")
  expect_equal(p$specific, "robur")
  expect_true(is.na(p$infraspecific))
})


test_that("a name at genus rank yields no epithets", {
  p <- split_scientific_name("Quercus")
  expect_equal(p$genus, "Quercus")
  expect_true(is.na(p$specific))
  expect_true(is.na(p$infraspecific))
})


test_that("a rank marker and a bare trinomial reach the same epithet", {
  marked <- split_scientific_name("Poa annua subsp. exilis")
  bare   <- split_scientific_name("Larus fuscus graellsii")

  expect_equal(marked$specific, "annua")
  expect_equal(marked$infraspecific, "exilis")
  expect_equal(bare$specific, "fuscus")
  expect_equal(bare$infraspecific, "graellsii")
})


test_that("a supplied genus anchors the split", {
  # Without the anchor the second word would be read as the epithet.
  p <- split_scientific_name("Adansonia digitata", genus = "Adansonia")
  expect_equal(p$genus, "Adansonia")
  expect_equal(p$specific, "digitata")
})


test_that("a supplied genus that does not open the name is ignored", {
  p <- split_scientific_name("Quercus robur", genus = "Betula")
  expect_equal(p$genus, "Quercus")
  expect_equal(p$specific, "robur")
})


test_that("a free-standing hybrid sign is not read as the epithet", {
  p <- split_scientific_name("Salix \u00d7 fragilis")
  expect_equal(p$genus, "Salix")
  expect_equal(p$specific, "fragilis")
  expect_true(is.na(p$infraspecific))
})


test_that("empty, NA and whitespace-only names give NA throughout", {
  p <- split_scientific_name(c(NA, "", "   "))
  expect_true(all(is.na(p$genus)))
  expect_true(all(is.na(p$specific)))
  expect_true(all(is.na(p$infraspecific)))
})


test_that("irregular whitespace does not change the split", {
  p <- split_scientific_name("  Poa   annua  ")
  expect_equal(p$genus, "Poa")
  expect_equal(p$specific, "annua")
})


test_that("the split is vectorized and length-preserving", {
  nm <- c("Quercus robur", "Poa", NA, "Larus fuscus graellsii")
  p <- split_scientific_name(nm)
  expect_length(p$genus, 4L)
  expect_length(p$specific, 4L)
  expect_length(p$infraspecific, 4L)
  expect_equal(p$genus, c("Quercus", "Poa", NA, "Larus"))
  expect_equal(p$specific, c("robur", NA, NA, "fuscus"))
  expect_equal(p$infraspecific, c(NA, NA, NA, "graellsii"))
})


test_that("a one-word name never reports itself as its own epithet", {
  # The pre-refactor fishbase synonym split returned the genus as the
  # specific epithet here, which would key a genus row as a species.
  p <- split_scientific_name("Gobius")
  expect_equal(p$genus, "Gobius")
  expect_true(is.na(p$specific))
})
