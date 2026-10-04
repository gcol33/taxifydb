# The build keeps every common name a taxon carries in a language, ranked.
# Every candidate is equally populated, so without a rule of its own the order
# follows rows, which move with every lookup rebuild: two builds of identical
# sources once differed in 209,701 of 1.2M names.

cands <- data.frame(
  canonical_name = rep("Abaeis nicippe", 5L),
  lang = c("en", "en", "en", "es", NA),
  common_name = c("Sleepy Orange", "Orange-tip", "sleepy orange",
                  "Naranja dormilona", "sleepy orange"),
  source = c("gbif", "gbif", "gbif", "gbif", "ncbi"),
  n_support = c(3L, 1L, 3L, 1L, 1L),
  n_spelling = c(2L, 1L, 1L, 1L, 1L),
  stringsAsFactors = FALSE
)

test_that("every distinct name is kept, the best supported ranked first", {
  out <- .reduce_common_names(cands, "lang")
  en <- out[out$lang %in% "en", ]
  expect_equal(en$common_name[order(en$name_rank)], c("Sleepy Orange", "Orange-tip"))
  expect_equal(out$name_rank[out$lang %in% "es"], 1L)
  expect_equal(out$common_name[is.na(out$lang)], "sleepy orange")
  expect_false(any(c("n_support", "n_spelling") %in% names(out)))
})

test_that("case variants of one name collapse to its most frequent spelling", {
  out <- .reduce_common_names(cands, "lang")
  expect_equal(sum(tolower(out$common_name[out$lang %in% "en"]) == "sleepy orange"), 1L)
})

test_that("the ranking does not depend on row order", {
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
  for (d in list(tie, tie[2:1, ])) {
    out <- .reduce_common_names(d, "lang")
    expect_equal(out$common_name[out$name_rank == 1L], "Alpha")
  }
})

test_that("a second pass keeps the ranks the first pass gave", {
  first <- .reduce_common_names(cands, "lang")
  again <- .reduce_common_names(first[nrow(first):1, ], "lang")
  key <- function(d) paste(d$lang, d$common_name, d$name_rank)
  expect_setequal(key(again), key(first))
})
