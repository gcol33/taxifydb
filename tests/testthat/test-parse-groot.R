# The GRooT aggregate specific_root_area column mixes scales: three source papers
# sit ~1000x below GRooT's cm2 g-1 standard (a compilation unit error, grounded by
# Mokany & Ash 2008's own SRA-SLA regression). parse_groot repairs it from the full
# per-record version by rescaling those papers x1000, with standardized sources
# winning per species and the rescaled papers filling gaps only. .groot_fix_sra()
# is the repair; test it offline.

test_that(".groot_fix_sra rescales flagged papers x1000, clean sources win per species", {
  skip_if_not_installed("data.table")

  full <- tempfile(fileext = ".csv")
  on.exit(unlink(full), add = TRUE)
  # Species covered ONLY by a flagged paper; a species with BOTH a flagged and a
  # clean record; a species with only a clean record.
  utils::write.csv(data.frame(
    genusTNRS  = c("Adinandra", "Hypochaeris", "Hypochaeris", "Quercus"),
    speciesTNRS = c("millettii", "radicata", "radicata", "robur"),
    traitName   = "Specific_root_area",
    traitValue  = c(0.20, 0.06, 1449, 300),
    referencesAbbreviated = c("Quanquan 2011", "Mokany and Ash 2008",
                              "Valverde-Barrantes et al 2015",
                              "Valverde-Barrantes et al 2015"),
    stringsAsFactors = FALSE
  ), full, row.names = FALSE)

  out <- data.frame(
    canonical_name     = c("Adinandra millettii", "Hypochaeris radicata",
                           "Quercus robur"),
    specific_root_area = c(0.22, 725, 300),
    stringsAsFactors   = FALSE
  )

  fixed <- .groot_fix_sra(out, full)
  # Flagged-only species -> rescaled x1000 (0.20 -> 200), recovered not dropped.
  expect_equal(fixed$specific_root_area[fixed$canonical_name == "Adinandra millettii"],
               200)
  # Mixed species -> clean record wins, not the rescaled pot-grown 0.06 (-> 60).
  expect_equal(fixed$specific_root_area[fixed$canonical_name == "Hypochaeris radicata"],
               1449)
  # Clean-only species is unchanged.
  expect_equal(fixed$specific_root_area[fixed$canonical_name == "Quercus robur"],
               300)
  # No physically impossible sub-1 cm2/g values remain.
  expect_true(all(fixed$specific_root_area >= 1, na.rm = TRUE))
})

test_that(".groot_fix_sra is a no-op when the full file is missing or has no SRA", {
  skip_if_not_installed("data.table")

  out <- data.frame(canonical_name = "Quercus robur",
                    specific_root_area = 300, stringsAsFactors = FALSE)

  empty <- tempfile(fileext = ".csv")
  on.exit(unlink(empty), add = TRUE)
  utils::write.csv(data.frame(
    genusTNRS = "Quercus", speciesTNRS = "robur",
    traitName = "Root_diameter", traitValue = 0.3,
    referencesAbbreviated = "Some 2019", stringsAsFactors = FALSE
  ), empty, row.names = FALSE)

  expect_identical(.groot_fix_sra(out, empty), out)                 # no SRA rows
  expect_identical(.groot_fix_sra(out, tempfile(fileext = ".csv")), out) # absent
})
