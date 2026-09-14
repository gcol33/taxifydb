test_that("parse_ecoflora writes Ecoflora's cm heights in mm", {
  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines(c(
    '"species","h_max","h_min","seed_wght"',
    '"Quercus robur","3000",NA,"3378;3681;3853"',
    '"Bellis perennis","12","3","0.09;0.1"',
    '"Urtica dioica","150","30","0.2"'
  ), csv)

  out <- parse_ecoflora(csv)
  row <- function(sp) out[out$canonical_name == sp, , drop = FALSE]

  expect_equal(row("Quercus robur")$height_max_mm_uk, 30000)
  expect_true(is.na(row("Quercus robur")$height_min_mm_uk))
  expect_equal(row("Bellis perennis")$height_max_mm_uk, 120)
  expect_equal(row("Bellis perennis")$height_min_mm_uk, 30)
  expect_equal(row("Urtica dioica")$height_max_mm_uk, 1500)

  expect_equal(row("Quercus robur")$seed_weight_mg_uk, 3681)
  expect_equal(row("Bellis perennis")$seed_weight_mg_uk, 0.095)
})
