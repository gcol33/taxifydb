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


# A quoted field may hold a newline, and then a line offset is not a record
# offset. WoRMS carries 4,626 of them in its reference column, and fread()'s
# skip counts lines, so chunking that by row offset re-reads rows it has
# already emitted -- silently, since the rows are real ones. These pin the cut
# to record boundaries.

test_that("a record spanning several lines is read as one row", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  path <- file.path(dir, "wrapped.tsv")
  writeLines(c("taxonID\tscientificName\treference",
               "1\tAglaophamus malmgreni\t\"Carter, J. G. (2000).",
               "Cladistic notes.\"",
               "2\tGyrodactylus barbatuli\t\"Short ref.\"",
               "3\tNuculana pernula\t\"Line one",
               "line two",
               "line three\""), path)

  seen <- list()
  feed <- delim_chunk_feed(path, normalize = identity, quote = "\"",
                           verbose = FALSE)
  repeat {
    ch <- feed()
    if (is.null(ch)) break
    seen[[length(seen) + 1L]] <- ch
  }
  got <- do.call(rbind, seen)

  expect_equal(nrow(got), 3L)
  expect_equal(got$scientificName,
               c("Aglaophamus malmgreni", "Gyrodactylus barbatuli",
                 "Nuculana pernula"))
  expect_equal(got$reference[1L], "Carter, J. G. (2000).\nCladistic notes.")
  expect_equal(got$reference[3L], "Line one\nline two\nline three")
})


test_that("chunking a line-wrapped file does not repeat or drop records", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  path <- file.path(dir, "wrapped.tsv")

  # 40 records, every third one wrapped over two physical lines, so a
  # line-offset cut drifts further out of step with each block.
  rows <- vapply(1:40, function(i) {
    if (i %% 3L == 0L) {
      sprintf("%d\tName %d\t\"ref %d\nsecond line\"", i, i, i)
    } else {
      sprintf("%d\tName %d\t\"ref %d\"", i, i, i)
    }
  }, character(1))
  writeLines(c("taxonID\tscientificName\treference", rows), path)

  collect <- function(chunk_rows) {
    parts <- list()
    feed <- delim_chunk_feed(path, normalize = identity, quote = "\"",
                             chunk_rows = chunk_rows, verbose = FALSE)
    repeat {
      ch <- feed()
      if (is.null(ch)) break
      parts[[length(parts) + 1L]] <- ch
    }
    do.call(rbind, parts)
  }

  full <- collect(1000L)
  expect_equal(nrow(full), 40L)
  expect_equal(full$taxonID, as.character(1:40))

  for (n in c(2L, 3L, 5L, 7L, 13L)) {
    got <- collect(n)
    expect_equal(nrow(got), 40L)
    expect_equal(got$taxonID, as.character(1:40))
    expect_equal(anyDuplicated(got$taxonID), 0L)
  }
})


test_that("a doubled quote inside a quoted field is unescaped once", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  path <- file.path(dir, "escaped.tsv")

  # RFC 4180 writes a literal quote inside a quoted field as "". read.delim
  # collapses it, fread returns it doubled (Rdatatable/data.table#1109). WFO has
  # 4,117 of these, and reading them the other way leaves stray quote characters
  # in published values -- the defect worms-2026.07 was cut to fix.
  writeLines(c("taxonID\tnamePublishedIn",
               "1\t\"Index Seminum 9: \"\"32, 80\"\" 1843\"",
               "2\tFlora Europaea 3: 12"), path)

  seen <- NULL
  feed <- delim_chunk_feed(path, normalize = function(ch) { seen <<- ch; ch },
                           quote = "\"", na_strings = "", verbose = FALSE)
  feed()

  direct <- utils::read.delim(path, stringsAsFactors = FALSE, na.strings = "",
                              colClasses = "character")
  expect_equal(seen$namePublishedIn, direct$namePublishedIn)
  expect_equal(seen$namePublishedIn[1L], "Index Seminum 9: \"32, 80\" 1843")
})


test_that("a block read fixes column types instead of inferring them", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  path <- file.path(dir, "sparse.tsv")

  # acceptedNameUsageID is empty for the whole first block and populated in the
  # second. Left to infer, the first block would come back logical and the
  # staged store would take that type for the column.
  writeLines(c("taxonID\tacceptedNameUsageID",
               "1\t", "2\t", "3\t7", "4\t8"), path)

  types <- character(0)
  feed <- delim_chunk_feed(path, normalize = function(ch) {
    types <<- c(types, class(ch$acceptedNameUsageID)); ch
  }, quote = "\"", na_strings = "", chunk_rows = 2L, verbose = FALSE)
  repeat if (is.null(feed())) break

  expect_equal(types, c("character", "character"))
})


test_that("file_encoding decodes the bytes the way read.delim does", {
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  path <- file.path(dir, "latin.tsv")

  # UTF-8 bytes for the multiplication sign, which WFO's reader decodes as
  # latin1 on purpose and then repairs.
  con <- file(path, "wb")
  writeBin(charToRaw("taxonID\tscientificName\n"), con)
  writeBin(c(charToRaw("1\tQuercus "), as.raw(c(0xC3, 0x97)),
             charToRaw(" rosacea\n")), con)
  close(con)

  seen <- NULL
  feed <- delim_chunk_feed(path, normalize = function(ch) { seen <<- ch; ch },
                           quote = "\"", na_strings = "",
                           file_encoding = "latin1", verbose = FALSE)
  feed()

  direct <- utils::read.delim(path, fileEncoding = "latin1",
                              stringsAsFactors = FALSE, na.strings = "")
  expect_equal(seen$scientificName, direct$scientificName)
  expect_equal(charToRaw(seen$scientificName),
               charToRaw(direct$scientificName))
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
