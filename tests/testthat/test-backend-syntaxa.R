# Syntaxon backbones: EuroVegChecklist and the Czech national classification.

# A unit list in the FloraVeg.EU layout: one class, one order, two alliances,
# the second renamed since the 2016 checklist.
.write_evc_units <- function(path) {
  units <- data.frame(
    "Code" = c("CM (MOL)", "CM01 (MOL-01)", "CM01A (MOL-01A)",
               "CM01C (MOL-01C)"),
    "Syntaxon_name (original EVC, Mucina et al. 2016)" = c(
      "Molinio-Arrhenatheretea", "Arrhenatheretalia elatioris",
      "Arrhenatherion elatioris", "Not included in EuroVegChecklist "),
    "Author (original EVC, Mucina et al. 2016)" = c(
      "Tx. 1937", "Tx. 1931", "Luquet 1926", "NULL"),
    "Syntaxon_name (EVC, version 2025-06-12)" = c(
      "Molinio-Arrhenatheretea", "Arrhenatheretalia elatioris",
      "Arrhenatherion elatioris", "Cynosurion cristati"),
    "Author (EVC, version 2025-06-12)" = c(
      "Tx. 1937", "Tx. 1931", "Koch 1926", "Tx. 1947"),
    "Description" = c("Meadows", "Mown meadows", "Mesic mown meadows",
                      "Mesic pastures"),
    check.names = FALSE, stringsAsFactors = FALSE)
  openxlsx2::write_xlsx(units, path)
}

.write_evc_synonyms <- function(path) {
  recs <- list(
    list(code = "CM (MOL)", name = "Molinio-Arrhenatheretea", synonyms = list(),
         truncated = FALSE),
    list(code = "CM01 (MOL-01)", name = "Arrhenatheretalia elatioris",
         synonyms = list(), truncated = FALSE),
    list(code = "CM01A (MOL-01A)", name = "Arrhenatherion elatioris",
         truncated = TRUE, synonyms = list(
           list(text = "Arrhenatherion Br.-Bl. 1931", status = "2b",
                remark = NULL),
           list(text = "Arrhenatherion elatioris Luquet 1926", status = "31",
                remark = NULL))),
    list(code = "CM01C (MOL-01C)", name = "Cynosurion cristati",
         truncated = FALSE, synonyms = list(
           list(text = "Lolion perennis Felfoldy 1942", status = "36",
                remark = "mol06 A proposal to conserve"),
           list(text = "Plantagini-Lolion sensu auct. non Tx. 1950",
                status = NULL, remark = NULL))))
  writeLines(vapply(recs, jsonlite::toJSON, character(1L), auto_unbox = TRUE,
                    null = "null"), path, useBytes = TRUE)
}

.write_evc2016 <- function(path) {
  writeLines(c("mnemonic\ttext\tstatus",
               "MOL-01A\tArrhenatherion Br.-Bl. 1931\t2b",
               "MOL-01A\tTriseto-Arrhenatherion Passarge 1969\tsyntax.syn.",
               "MOL-01C\tAchilleo-Cynosurion Passarge 1969\tsyntax.syn."),
             path, useBytes = TRUE)
}


test_that("read_evc assembles units, synonyms and lineage", {
  skip_if_not_installed("openxlsx2")
  dir <- withr::local_tempdir()
  units <- file.path(dir, "units.xlsx")
  syn <- file.path(dir, "syn.jsonl")
  p16 <- file.path(dir, "evc2016.tsv")
  .write_evc_units(units)
  .write_evc_synonyms(syn)
  .write_evc2016(p16)

  df <- suppressMessages(read_evc(syn, units, p16, verbose = FALSE))
  acc <- df[df$taxonomic_status == "ACCEPTED", ]
  expect_setequal(acc$taxon_id, c("CM", "CM01", "CM01A", "CM01C"))
  expect_equal(acc$taxon_rank[match("CM01A", acc$taxon_id)], "ALLIANCE")
  expect_equal(df$class, rep("Molinio-Arrhenatheretea", nrow(df)))
  expect_true(all(is.na(df$association)))

  by_name <- function(n) df[df$canonical_name == n, , drop = FALSE]
  # a page synonym, its author split off
  expect_equal(by_name("Arrhenatherion")$authorship, "Br.-Bl. 1931")
  expect_equal(by_name("Arrhenatherion")$alliance, "Arrhenatherion elatioris")
  # the unit renamed since 2016 keeps its old author as a synonym
  old <- by_name("Arrhenatherion elatioris")
  expect_setequal(old$authorship, c("Koch 1926", "Luquet 1926"))
  # the paper fills a truncated page only, and only with what it lacks
  expect_equal(nrow(by_name("Triseto-Arrhenatherion")), 1L)
  expect_equal(nrow(by_name("Achilleo-Cynosurion")), 0L)
  expect_equal(nrow(by_name("Arrhenatherion")), 1L)
  # a name cited in another author's sense is a misapplication
  expect_equal(by_name("Plantagini-Lolion")$taxonomic_status, "MISAPPLIED")
  expect_equal(by_name("Lolion perennis")$remark,
               "mol06 A proposal to conserve")
  expect_equal(attr(df, "truncated_units"), "CM01A")
  expect_false(anyDuplicated(df$taxon_id) > 0L)
})


test_that("read_vegcz places associations under their alliances", {
  skip_if_not_installed("openxlsx2")
  dir <- withr::local_tempdir()
  units <- file.path(dir, "units.xlsx")
  syn <- file.path(dir, "syn.jsonl")
  openxlsx2::write_xlsx(data.frame(
    "Rank EN" = c("Class", "Alliance", "Association"),
    "Rank CZ" = c("Třída", "Svaz", "Asociace"),
    "Code" = c("TD", "TDA", "TDA01"),
    "Name" = c("Molinio-Arrhenatheretea", "Arrhenatherion elatioris",
               "Pastinaco sativae-Arrhenatheretum elatioris"),
    "Author citation" = c("Tx. 1937", "Luquet 1926", "Passarge 1964"),
    "Name with author citation" = NA,
    "Czech descriptor" = c("Louky", "Ovsikove louky", "Mezofilni louky"),
    "English descriptor" = c("Meadows", NA, NA),
    check.names = FALSE), units)
  writeLines(c(
    jsonlite::toJSON(list(code = "TD", name = "Molinio-Arrhenatheretea",
                          synonyms = list(), original = NULL),
                     auto_unbox = TRUE, null = "null"),
    jsonlite::toJSON(list(code = "TDA", name = "Arrhenatherion elatioris",
                          synonyms = list(list(name = "Arrhenatherion",
                                               rest = "Br.-Bl. 1931 (§ 2b, nomen nudum)")),
                          original = NULL), auto_unbox = TRUE, null = "null"),
    jsonlite::toJSON(list(code = "TDA01",
                          name = "Pastinaco sativae-Arrhenatheretum elatioris",
                          synonyms = list(list(name = "Arrhenatheretum elatioris",
                                               rest = "Braun 1915 (§ 36, nomen ambiguum)"),
                                          list(name = "Arrhenatheretum",
                                               rest = "sensu Kuhn 1937 non Braun 1915")),
                          original = NULL), auto_unbox = TRUE, null = "null")),
    syn, useBytes = TRUE)

  df <- suppressMessages(read_vegcz(syn, units, verbose = FALSE))
  by_name <- function(n) df[df$canonical_name == n, , drop = FALSE]
  ass <- by_name("Pastinaco sativae-Arrhenatheretum elatioris")
  expect_equal(ass$taxon_rank, "ASSOCIATION")
  expect_equal(ass$alliance, "Arrhenatherion elatioris")
  expect_equal(ass$class, "Molinio-Arrhenatheretea")
  expect_true(is.na(ass$order))
  expect_equal(ass$association, ass$canonical_name)

  syn1 <- by_name("Arrhenatheretum elatioris")
  expect_equal(syn1$authorship, "Braun 1915")
  expect_equal(syn1$nomenclatural_note, "(§ 36, nomen ambiguum)")
  expect_equal(syn1$accepted_name_usage_id, "TDA01")
  expect_equal(syn1$association, "Pastinaco sativae-Arrhenatheretum elatioris")
  expect_equal(by_name("Arrhenatheretum")$taxonomic_status, "MISAPPLIED")
})


test_that("syntaxon backbones stay out of the enrichment name lookups", {
  expect_false(any(c("evc", "vegcz") %in% list_backends()))
  expect_true(all(c("evc", "vegcz") %in% names(.all_builders())))
})
