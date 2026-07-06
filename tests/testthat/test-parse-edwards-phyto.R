# parse_edwards_phyto reduces the per-culture Edwards et al. (2015) nutrient
# trait table to one row per species: numeric traits by median, taxon/system by
# mode. Experimental conditions are dropped, the "fresh" system label is
# normalized, and trait columns left entirely empty after reduction are removed.

make_table1 <- function() {
  dir <- file.path(tempdir(), paste0("edw_", as.integer(stats::runif(1, 1, 1e9))))
  dir.create(dir, showWarnings = FALSE)
  d <- data.frame(
    species     = c("Aaa bbb", "Aaa bbb", "Ccc ddd"),
    isolate     = c("i1", "i2", "i3"),
    taxon       = c("diatom", "diatom", "green"),
    system      = c("marine", "marine", "fresh"),
    temperature = c(15, 25, 20),          # experimental condition, must be dropped
    synonym     = c(NA, NA, NA),
    volume      = c(100, 300, NA),         # -> cell_volume, median
    c_per_cell  = c(NA, NA, NA),           # -> carbon_per_cell, all NA -> dropped
    mu_inf_nit  = c(1.0, 3.0, 2.0),        # numeric trait, median
    k_p         = c(NA, NA, 0.5),
    qmax_amm_c  = c(NA, NA, NA),           # entirely empty -> dropped
    citation    = c(1, 1, 2),
    stringsAsFactors = FALSE
  )
  f <- file.path(dir, "Table1.csv")
  utils::write.csv(d, f, row.names = FALSE)
  dir
}

test_that("parse_edwards_phyto reduces measurements to species medians", {
  dir <- make_table1()
  out <- parse_edwards_phyto(dir)

  expect_setequal(out$canonical_name, c("Aaa bbb", "Ccc ddd"))
  # median of two Aaa bbb measurements: mu_inf_nit median(1, 3) = 2, volume median(100,300)=200.
  a <- out[out$canonical_name == "Aaa bbb", ]
  expect_equal(a$mu_inf_nit, 2)
  expect_equal(a$cell_volume, 200)
  # categorical mode, renamed columns.
  expect_equal(a$taxon_group, "diatom")
  expect_equal(a$habitat_system, "marine")
})

test_that("parse_edwards_phyto normalizes 'fresh' and drops empty/condition cols", {
  dir <- make_table1()
  out <- parse_edwards_phyto(dir)

  # "fresh" -> "freshwater".
  expect_equal(out$habitat_system[out$canonical_name == "Ccc ddd"], "freshwater")
  # experimental conditions and per-measurement metadata are not carried.
  expect_false(any(c("temperature", "isolate", "citation", "synonym") %in% names(out)))
  # entirely empty trait columns after reduction are dropped.
  expect_false("qmax_amm_c" %in% names(out))   # all NA in the fixture
  expect_false("carbon_per_cell" %in% names(out))  # c_per_cell all NA
  # a partially-populated trait survives.
  expect_true("k_p" %in% names(out))
})
