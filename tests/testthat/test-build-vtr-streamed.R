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


test_that("the store groups each genus into one run and carries its indexes", {
  skip_if_not_installed("withr")
  b <- build_both(make_backbone(), chunk_size = 2L, withr::local_tempdir())

  # The genus sort exists so a genus filter can prune row groups, which needs
  # every row of a genus to sit in one contiguous run. It does NOT need the two
  # builders to agree on the order of the genera themselves: build_vtr() sorts
  # with order(), which follows the build machine's LC_COLLATE, while the
  # streamed path sorts inside vectra, which compares bytes. The two disagree
  # wherever a genus is non-ASCII -- Achmaeops written with the ligature sorts
  # beside "ae" under a locale and after "o" under byte order -- so runs, not
  # is.unsorted(), is the invariant to assert.
  runs <- function(g) {
    g[is.na(g)] <- "\001NA"
    length(rle(g)$lengths)
  }
  expect_equal(runs(b$streamed$genus), length(unique(b$streamed$genus)))
  expect_equal(runs(b$streamed$genus), runs(b$direct$genus))

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


# The parsing arguments a chunk feed uses have to match the whole-file reader
# it replaces. WoRMS ships every TSV field wrapped in double quotes, and
# reading that with quoting disabled leaves the quote characters inside the
# names, which is what broke every marine enrichment join before worms-2026.07
# (taxifydb#2). Other sources carry genuine embedded quotes in informal names
# and need quoting off. The feed must be able to express both.

test_that("a chunk feed honours the quoting its source needs", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  path <- file.path(dir, "quoted.tsv")
  writeLines(c("taxonID\tscientificName",
               "\"1\"\t\"Aglaophamus malmgreni\"",
               "\"2\"\t\"Gyrodactylus barbatuli\""), path)

  seen <- NULL
  feed <- delim_chunk_feed(path, normalize = function(ch) { seen <<- ch; ch },
                           quote = "\"", verbose = FALSE)
  feed()
  expect_equal(seen$scientificName, c("Aglaophamus malmgreni",
                                      "Gyrodactylus barbatuli"))
  expect_false(any(grepl('"', seen$scientificName, fixed = TRUE)))
})


test_that("a chunk feed can keep genuine embedded quotes", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  path <- file.path(dir, "embedded.tsv")
  writeLines(c("taxonID\tscientificName",
               "1\tGyrodactylus sp. \"A\""), path)

  seen <- NULL
  feed <- delim_chunk_feed(path, normalize = function(ch) { seen <<- ch; ch },
                           quote = "", verbose = FALSE)
  feed()
  expect_equal(seen$scientificName, 'Gyrodactylus sp. "A"')
})


test_that("a chunk feed reads only the selected columns", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  path <- file.path(dir, "wide.tsv")
  writeLines(c("a\tb\tc", "1\t2\t3", "4\t5\t6"), path)

  seen <- NULL
  feed <- delim_chunk_feed(path, normalize = function(ch) { seen <<- ch; ch },
                           select = c("a", "c"), verbose = FALSE)
  feed()
  expect_equal(names(seen), c("a", "c"))
  expect_equal(seen$c, c("3", "6"))
})


test_that("a chunk feed splits a file into the expected blocks", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  path <- file.path(dir, "rows.tsv")
  writeLines(c("a", as.character(1:10)), path)

  got <- c()
  feed <- delim_chunk_feed(path, normalize = identity, chunk_rows = 4L,
                           verbose = FALSE)
  repeat {
    ch <- feed()
    if (is.null(ch)) break
    got <- c(got, ch$a)
  }
  expect_equal(got, as.character(1:10))
})
