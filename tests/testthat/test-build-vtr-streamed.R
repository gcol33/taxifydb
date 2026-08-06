# build_vtr_streamed() must produce the store build_vtr() would have produced
# from the same rows. The streaming path resolves synonym chains on a
# three-column projection and does the accepted embedding and the genus sort
# inside vectra, so an equivalence test against the in-memory path is what
# holds those three substitutions honest.

make_backbone <- function() {
  # Chains on purpose: 3 -> 2 -> 1 exercises the multi-hop walk, 6 points at an
  # id that does not exist, and rows are deliberately out of genus order.
  data.frame(
    taxon_id               = c("1", "2", "3", "4", "5", "6", "7"),
    canonical_name         = c("Quercus robur", "Quercus pedunculata",
                               "Quercus racemosa", "Betula pendula",
                               "Betula alba", "Poa nomatch", "Abies alba"),
    taxon_rank             = rep("SPECIES", 7L),
    taxonomic_status       = c("ACCEPTED", "SYNONYM", "SYNONYM", "ACCEPTED",
                               "SYNONYM", "SYNONYM", "ACCEPTED"),
    accepted_name_usage_id = c(NA, "1", "2", NA, "4", "999", NA),
    family                 = c("Fagaceae", "Fagaceae", "Fagaceae",
                               "Betulaceae", "Betulaceae", "Poaceae",
                               "Pinaceae"),
    genus                  = c("Quercus", "Quercus", "Quercus", "Betula",
                               "Betula", "Poa", "Abies"),
    specific_epithet       = c("robur", "pedunculata", "racemosa", "pendula",
                               "alba", "nomatch", "alba"),
    authorship             = c("L.", "Ehrh.", "Lam.", "Roth", "L.", NA, "Mill."),
    infraspecific_epithet  = rep(NA_character_, 7L),
    stringsAsFactors       = FALSE
  )
}

feed_in_chunks <- function(df, size) {
  starts <- seq(1L, nrow(df), by = size)
  i <- 0L
  function() {
    i <<- i + 1L
    if (i > length(starts)) return(NULL)
    df[starts[i]:min(starts[i] + size - 1L, nrow(df)), , drop = FALSE]
  }
}

# `dir` comes from the calling test so the stores outlive this helper's frame.
build_both <- function(df, chunk_size, dir) {
  direct <- file.path(dir, "direct.vtr")
  streamed <- file.path(dir, "streamed.vtr")

  build_vtr(precompute_backbone(df), direct, "test", "1.0", "http://example")
  build_vtr_streamed(feed_in_chunks(df, chunk_size), streamed, "test", "1.0",
                     "http://example", verbose = FALSE)

  list(direct = vectra::collect(vectra::tbl(direct)),
       streamed = vectra::collect(vectra::tbl(streamed)),
       direct_path = direct, streamed_path = streamed)
}

# Row order within a genus is not fixed by the sort, so compare on a stable key.
canonical_order <- function(d) d[order(d$taxon_id), names(d)[order(names(d))],
                                 drop = FALSE]


test_that("a streamed build matches the in-memory build row for row", {
  skip_if_not_installed("withr")
  b <- build_both(make_backbone(), chunk_size = 3L, withr::local_tempdir())

  expect_setequal(names(b$streamed), names(b$direct))
  expect_equal(nrow(b$streamed), nrow(b$direct))

  s <- canonical_order(b$streamed)
  d <- canonical_order(b$direct)
  rownames(s) <- NULL
  rownames(d) <- NULL
  expect_equal(s, d)
})


test_that("the chunk size does not change the result", {
  skip_if_not_installed("withr")
  df <- make_backbone()
  one <- canonical_order(
    build_both(df, chunk_size = 7L, withr::local_tempdir())$streamed)
  many <- canonical_order(
    build_both(df, chunk_size = 2L, withr::local_tempdir())$streamed)
  rownames(one) <- NULL
  rownames(many) <- NULL
  expect_equal(one, many)
})


test_that("synonym chains resolve to the terminal accepted name", {
  skip_if_not_installed("withr")
  s <- build_both(make_backbone(), chunk_size = 2L,
                  withr::local_tempdir())$streamed

  # 3 -> 2 -> 1, so the two-hop synonym must land on Quercus robur.
  three <- s[s$taxon_id == "3", ]
  expect_equal(three$accepted_name, "Quercus robur")
  expect_equal(three$accepted_taxon_id, "1")
  expect_true(three$is_synonym)

  one <- s[s$taxon_id == "1", ]
  expect_equal(one$accepted_name, "Quercus robur")
  expect_false(one$is_synonym)
})


test_that("a synonym pointing at a missing id stays a synonym of itself", {
  skip_if_not_installed("withr")
  s <- build_both(make_backbone(), chunk_size = 3L,
                  withr::local_tempdir())$streamed

  six <- s[s$taxon_id == "6", ]
  expect_true(six$is_synonym)
  expect_equal(six$accepted_taxon_id, "6")
  expect_equal(six$accepted_name, "Poa nomatch")
})


test_that("the store is sorted by genus and carries its indexes", {
  skip_if_not_installed("withr")
  b <- build_both(make_backbone(), chunk_size = 2L, withr::local_tempdir())

  g <- b$streamed$genus
  expect_false(is.unsorted(g[!is.na(g)]))

  # An index makes a genus filter return the same rows a scan would.
  hit <- vectra::collect(vectra::filter(vectra::tbl(b$streamed_path),
                                        genus == "Quercus"))
  expect_equal(nrow(hit), 3L)
})


test_that("the meta sidecar records the streamed row count", {
  skip_if_not_installed("withr")
  b <- build_both(make_backbone(), chunk_size = 2L, withr::local_tempdir())
  meta <- readLines(sub("\\.vtr$", ".meta", b$streamed_path))
  expect_true("nrow=7" %in% meta)
  expect_true("backend=test" %in% meta)
})


test_that("an empty feed is an error rather than an empty store", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  expect_error(
    build_vtr_streamed(function() NULL, file.path(dir, "x.vtr"),
                       "test", "1.0", "http://example", verbose = FALSE),
    "yielded no rows"
  )
})
