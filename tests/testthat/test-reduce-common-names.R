# The build keeps one common name per taxon and language. Every candidate is
# equally populated, so without a rule of its own the pick follows row order,
# which moves with every lookup rebuild and churned 209,701 of 1.2M names
# between two builds of identical sources.

cands <- data.frame(
  canonical_name = c("Abaeis nicippe", "Abaeis nicippe", "Abaeis nicippe",
                     "Abaeis nicippe", "Abaeis nicippe"),
  lang = c("en", "en", "en", "es", NA),
  common_name = c("Sleepy Orange", "Orange-tip", "sleepy orange",
                  "Naranja dormilona", "sleepy orange"),
  source = c("gbif", "gbif", "gbif", "gbif", "ncbi"),
  n_support = c(3L, 1L, 3L, 1L, 1L),
  n_spelling = c(2L, 1L, 1L, 1L, 1L),
  stringsAsFactors = FALSE
)

test_that("the most supported name wins, in its most frequent spelling", {
  out <- .reduce_common_names(cands, "lang")
  expect_equal(nrow(out), 3L)
  expect_equal(out$common_name[out$lang %in% "en"], "Sleepy Orange")
  expect_equal(out$common_name[out$lang %in% "es"], "Naranja dormilona")
  expect_equal(out$common_name[is.na(out$lang)], "sleepy orange")
  expect_false(any(c("n_support", "n_spelling") %in% names(out)))
})

test_that("the pick does not depend on row order", {
  ref <- .reduce_common_names(cands, "lang")
  for (seed in 1:20) {
    set.seed(seed)
    got <- .reduce_common_names(cands[sample(nrow(cands)), ], "lang")
    expect_identical(got, ref)
  }
})

test_that("equal support falls back to byte order, not position", {
  tie <- data.frame(canonical_name = "X y", lang = "en",
                    common_name = c("Beta", "Alpha"), source = "gbif",
                    n_support = 1L, n_spelling = 1L, stringsAsFactors = FALSE)
  expect_equal(.reduce_common_names(tie, "lang")$common_name, "Alpha")
  expect_equal(.reduce_common_names(tie[2:1, ], "lang")$common_name, "Alpha")
})

test_that("rows without counts (a second reducer pass) still reduce", {
  out <- .reduce_common_names(cands[, 1:4], "lang")
  expect_equal(nrow(out), 3L)
})
