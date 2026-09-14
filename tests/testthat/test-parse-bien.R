bien_long <- function() {
  data.frame(
    name  = c("Acer campestre", "Acer campestre", "Zea mays",
              "Acer campestre", "Zea mays"),
    trait = c("leaf dry mass", "leaf dry mass", "leaf dry mass",
              "leaf area", "whole plant woodiness"),
    value = c("0.333315893", "0.0351", "0.4185", "4741.5", "herbaceous"),
    unit  = c("g", "g", "g", "mm2", NA),
    stringsAsFactors = FALSE
  )
}

test_that("BIEN leaf dry mass is recorded in g and written in mg", {
  spec <- .bien_trait_spec()
  expect_identical(spec$leaf_dry_mass_mg$unit, "g")

  out <- .pivot_species_traits(.bien_convert_units(bien_long(), spec), spec,
                               keep_all = FALSE)
  acer <- out[out$canonical_name == "Acer campestre", ]
  zea  <- out[out$canonical_name == "Zea mays", ]
  expect_equal(acer$leaf_dry_mass_mg, (333.315893 + 35.1) / 2)
  expect_equal(zea$leaf_dry_mass_mg, 418.5)
  expect_equal(acer$leaf_area_mm2, 4741.5)
  expect_identical(zea$woodiness, "herbaceous")
})

test_that("a BIEN record in an undeclared unit stops the build", {
  long <- bien_long()
  long$unit[2] <- "mg"
  expect_error(.bien_convert_units(long, .bien_trait_spec()),
               "leaf dry mass.*'mg'.*expected 'g'")

  long <- bien_long()
  long$unit <- NULL
  expect_error(.bien_convert_units(long, .bien_trait_spec()), "no `unit`")
})

test_that("every curated numeric BIEN column declares its record unit", {
  spec <- .bien_trait_spec()
  num  <- Filter(function(s) identical(s$type, "num"), spec)
  expect_true(all(vapply(num, function(s) is.character(s$unit), logical(1))))
})
