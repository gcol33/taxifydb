# WFO's classification file is UTF-8. Each non-ASCII character must reach the
# normalized table as the bytes the file holds, in every text column, whether
# the file is read whole or streamed a block at a time (#51).

write_wfo_utf8_fixture <- function(path) {
  u <- function(...) as.raw(c(...))
  s <- charToRaw
  header <- paste(c("taxonID", "scientificName", "taxonRank",
                    "taxonomicStatus", "acceptedNameUsageID", "family",
                    "genus", "specificEpithet", "scientificNameAuthorship",
                    "infraspecificEpithet", "namePublishedIn",
                    "taxonRemarks"), collapse = "\t")
  row1 <- c(
    s("wfo-0000743304\t\"Aeonium arboreum subsp. holochrysum\"\tsubspecies\t"),
    s("Accepted\t\tCrassulaceae\tAeonium\tarboreum\t\"(H.Y.Liu) Ba"),
    u(0xC3, 0xB1),
    s("ares\"\tholochrysum\t\"Bull. Acad. Imp. Sci. Saint-P"),
    u(0xC3, 0xA9),
    s("tersbourg\"\t\"Updated from Dostal to Dost"),
    u(0xC3, 0xA1),
    s("l, cut at M"),
    u(0xC3),
    s("\"\n")
  )
  row2 <- c(
    s("wfo-2\t\"Quercus "), u(0xC3, 0x97),
    s(" rosacea\"\tnothospecies\tAccepted\t\tFagaceae\tQuercus\trosacea\t"),
    s("Bechst.\t\t\t\n")
  )
  con <- file(path, "wb")
  on.exit(close(con), add = TRUE)
  writeBin(c(s(header), u(0x0A), row1, row2), con)
}

test_that("read_wfo decodes the UTF-8 source once in every text column", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  path <- file.path(dir, "classification.csv")
  write_wfo_utf8_fixture(path)

  df <- read_wfo(path, verbose = FALSE)

  expect_equal(charToRaw(df$authorship[1L]), c(
    charToRaw("(H.Y.Liu) Ba"), as.raw(c(0xC3, 0xB1)), charToRaw("ares")))
  expect_equal(charToRaw(df$namePublishedIn[1L]), c(
    charToRaw("Bull. Acad. Imp. Sci. Saint-P"), as.raw(c(0xC3, 0xA9)),
    charToRaw("tersbourg")))
  expect_equal(charToRaw(df$canonical_name[2L]), c(
    charToRaw("Quercus "), as.raw(c(0xC3, 0x97)), charToRaw(" rosacea")))

  # The remark is cut inside a character; the lone lead byte is dropped and
  # the rest of the field keeps its own UTF-8.
  expect_equal(charToRaw(df$taxonRemarks[1L]), c(
    charToRaw("Updated from Dostal to Dost"), as.raw(c(0xC3, 0xA1)),
    charToRaw("l, cut at M")))

  text <- unlist(df[vapply(df, is.character, logical(1))], use.names = FALSE)
  text <- text[!is.na(text)]
  expect_true(all(validUTF8(text)))
  expect_false(any(grepl("Ã", text, fixed = TRUE)))
})

test_that("the streamed WFO read matches the whole-file read byte for byte", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("withr")
  dir <- withr::local_tempdir()
  path <- file.path(dir, "classification.csv")
  write_wfo_utf8_fixture(path)

  whole <- read_wfo(path, verbose = FALSE)

  blocks <- list()
  feed <- wfo_feed(path, normalize = function(ch) normalize_wfo(ch, FALSE),
                   chunk_rows = 1L, verbose = FALSE)
  repeat {
    b <- feed()
    if (is.null(b)) break
    blocks[[length(blocks) + 1L]] <- b
  }
  streamed <- do.call(rbind, blocks)

  expect_identical(lapply(streamed, function(v) lapply(v, charToRaw)),
                   lapply(whole, function(v) lapply(v, charToRaw)))
})
