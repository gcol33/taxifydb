# POPLW length-weight selection (gcol33/taxifydb#25). The pure ranking half of
# .rfishbase_lw_table() is testable without a network fetch.

test_that(".rfishbase_lw_select returns NULL for empty or malformed input", {
  expect_null(taxifydb:::.rfishbase_lw_select(NULL))
  expect_null(taxifydb:::.rfishbase_lw_select(data.frame(SpecCode = 1)))
  # Rows with no usable a/b are dropped, leaving nothing.
  lw <- data.frame(SpecCode = "1", a = NA_real_, b = NA_real_)
  expect_null(taxifydb:::.rfishbase_lw_select(lw))
})

test_that(".rfishbase_lw_select keeps one row per species with the lw_* schema", {
  lw <- data.frame(
    SpecCode = c("2", "2", "5"),
    a = c(0.01, 0.02, 0.03), b = c(3.0, 3.1, 2.9),
    Type = c("TL", "SL", "TL"),
    Method = rep("type I linear regression", 3),
    Sex = c("unsexed", "unsexed", "mixed"),
    Number = c(100, 50, 20),
    CoeffDetermination = c(0.98, 0.95, 0.9),
    stringsAsFactors = FALSE
  )
  out <- taxifydb:::.rfishbase_lw_select(lw)
  expect_equal(sort(out$SpecCode), c("2", "5"))
  expect_setequal(
    names(out),
    c("SpecCode", "lw_a", "lw_b", "lw_type", "lw_method", "lw_sex", "lw_n",
      "lw_r2"))
})

test_that("the species' max-length type wins over sex/method/sample size", {
  # Row 1 is SL, general, strong, big-n; row 2 is TL but sex-specific, weak,
  # small-n. With the species' max length recorded as TL, row 2 still wins,
  # because a coefficient must match the length column it will be applied to.
  lw <- data.frame(
    SpecCode = c("2", "2"),
    a = c(0.010, 0.020), b = c(3.0, 3.2),
    Type = c("SL", "TL"),
    Method = c("type I linear regression", "single L-W pair with b=3"),
    Sex = c("unsexed", "female"),
    Number = c(500, 5),
    CoeffDetermination = c(0.99, 0.5),
    stringsAsFactors = FALSE
  )
  out <- taxifydb:::.rfishbase_lw_select(lw, spec_ltype = c("2" = "TL"))
  expect_equal(out$lw_type, "TL")
  expect_equal(out$lw_a, 0.020)
})

test_that("with no type preference, sex then method then sample size decide", {
  lw <- data.frame(
    SpecCode = c("2", "2", "2", "2"),
    a = c(0.01, 0.02, 0.03, 0.04), b = c(3, 3, 3, 3),
    Type = c("TL", "TL", "TL", "TL"),
    Method = c("single L-W pair with b=3", "type I linear regression",
               "type I linear regression", "type I linear regression"),
    Sex = c("unsexed", "female", "unsexed", "unsexed"),
    Number = c(999, 999, 10, 900),
    stringsAsFactors = FALSE
  )
  out <- taxifydb:::.rfishbase_lw_select(lw)
  # unsexed + regression beats unsexed + weak (row 1) and female + regression
  # (row 2); among the two unsexed regressions, larger n (row 4) wins.
  expect_equal(out$lw_sex, "unsexed")
  expect_true(grepl("regression", out$lw_method))
  expect_equal(out$lw_a, 0.04)
})
