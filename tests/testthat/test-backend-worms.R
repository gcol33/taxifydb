# WoRMS canonical-name construction. Internal helpers via `:::`.

test_that("strip_authorship does not truncate when authorship is not a suffix", {
  # WoRMS data quirk: scientificName does not end with scientificNameAuthorship.
  # A length-based truncation would cut "Abatus cavernosus" down to "A".
  sci <- "Abatus cavernosus"
  auth <- "(Philippi, 1845)"
  result <- taxifydb:::strip_authorship(sci, auth)
  expect_equal(result, "Abatus cavernosus")
})

test_that("strip_authorship leaves genus-only names intact under mismatch", {
  sci <- c("Abatus", "Foo", "Quercus robur L.")
  auth <- c("(Philippi, 1845)", "Linnaeus", "L.")
  result <- taxifydb:::strip_authorship(sci, auth)
  expect_equal(result, c("Abatus", "Foo", "Quercus robur"))
})

test_that("strip_authorship strips a genuine trailing authorship", {
  sci <- "Abatus cavernosus (Philippi, 1845)"
  auth <- "(Philippi, 1845)"
  result <- taxifydb:::strip_authorship(sci, auth)
  expect_equal(result, "Abatus cavernosus")
})

test_that("strip_authorship never yields a 1-3 char canonical from a binomial", {
  sci <- c("Abatus cavernosus", "Gibbula cineraria", "Mytilus edulis")
  auth <- c("(Philippi, 1845)", "(Linnaeus, 1758)", "Linnaeus, 1758")
  result <- taxifydb:::strip_authorship(sci, auth)
  expect_true(all(nchar(result) > 3L))
  expect_false(any(result %in% LETTERS))
})

test_that("read_worms builds full canonical names from a DwC-A sample", {
  dir <- tempfile("worms_test_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  taxon <- data.frame(
    `dwc:taxonID` = c("160762", "200001", "200002"),
    `dwc:scientificName` = c(
      "Abatus cavernosus",                 # authorship NOT a suffix (the bug)
      "Gibbula cineraria (Linnaeus, 1758)",  # genuine trailing authorship
      "Mytilus edulis Linnaeus, 1758"        # genuine trailing authorship
    ),
    `dwc:scientificNameAuthorship` = c(
      "(Philippi, 1845)", "(Linnaeus, 1758)", "Linnaeus, 1758"
    ),
    `dwc:taxonRank` = c("species", "species", "species"),
    `dwc:taxonomicStatus` = c("accepted", "accepted", "accepted"),
    `dwc:acceptedNameUsageID` = c("160762", "200001", "200002"),
    `dwc:family` = c("Schizasteridae", "Trochidae", "Mytilidae"),
    `dwc:genus` = c("Abatus", "Gibbula", "Mytilus"),
    `dwc:specificEpithet` = c("cavernosus", "cineraria", "edulis"),
    `dwc:kingdom` = c("Animalia", "Animalia", "Animalia"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  taxon_path <- file.path(dir, "Taxon.tsv")
  utils::write.table(taxon, taxon_path, sep = "\t", quote = FALSE,
                     row.names = FALSE, fileEncoding = "UTF-8")

  out <- read_worms(dir, verbose = FALSE)

  row1 <- out[out$taxon_id == "160762", ]
  expect_equal(row1$canonical_name, "Abatus cavernosus")
  expect_equal(out$canonical_name[out$taxon_id == "200001"],
               "Gibbula cineraria")
  expect_equal(out$canonical_name[out$taxon_id == "200002"],
               "Mytilus edulis")
  expect_true(all(nchar(out$canonical_name) > 3L))
})
