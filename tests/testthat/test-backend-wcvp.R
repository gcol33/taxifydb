# WCVP names reader: status derivation, canonical passthrough, literal quotes.

# A pipe-delimited wcvp_names.csv sample covering the cases the reader must get
# right: an accepted self-pointing name, a synonym pointing at it, an unplaced
# name with no accepted target, a hybrid, an infraspecific taxon, and an
# informal name carrying a genuine embedded double-quote.
.write_wcvp_sample <- function(path) {
  header <- paste(
    "plant_name_id", "taxon_rank", "taxon_status", "family", "genus",
    "species", "infraspecies", "taxon_name", "taxon_authors",
    "accepted_plant_name_id", "ipni_id", "powo_id", "lifeform_description",
    "climate_description", "geographic_area", "first_published",
    sep = "|"
  )
  rows <- c(
    # accepted species (accepted_plant_name_id == plant_name_id)
    "1|Species|Accepted|Lardizabalaceae|Stauntonia|conspicua||Stauntonia conspicua|R.H.Chang|1|932629-1|urn:1|climber|temperate|SE. China|(1987)",
    # synonym pointing at id 1
    "2|Species|Synonym|Lardizabalaceae|Stauntonia|maculata||Stauntonia maculata|Merr.|1|111-1|urn:2|||China|(1922)",
    # unplaced name (empty accepted_plant_name_id) -> its own accepted concept
    "3|Species|Unplaced|Foaceae|Foo|bar||Foo bar|L.||222-1|urn:3|||World|(1800)",
    # nothospecies hybrid: taxon_name already carries the multiplication sign
    "4|Species|Accepted|Lentibulariaceae|Utricularia|japonica||Utricularia × japonica|Makino|4|333-1|urn:4|aquatic|temperate|Japan|(1914)",
    # infraspecific taxon
    "5|Variety|Accepted|Lentibulariaceae|Utricularia|inflexa|tenuifolia|Utricularia inflexa var. tenuifolia|P.Taylor|5|444-1|urn:5|aquatic|tropical|Africa|(1964)",
    # informal name with a genuine embedded double-quote (literal, not wrapping)
    "6|Species|Accepted|Lentibulariaceae|Utricularia|A||Utricularia sp. \"A\"|Anon.|6|555-1|urn:6|||Brazil|(2000)"
  )
  con <- file(path, encoding = "UTF-8")
  on.exit(close(con))
  writeLines(c(header, rows), con)
}

test_that("read_wcvp derives status from the accepted_plant_name_id link", {
  path <- tempfile("wcvp_names_", fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  .write_wcvp_sample(path)

  out <- read_wcvp(path, verbose = FALSE)

  status <- stats::setNames(out$taxonomic_status, out$taxon_id)
  expect_equal(unname(status["1"]), "ACCEPTED")  # self-pointing
  expect_equal(unname(status["2"]), "SYNONYM")   # points at 1
  expect_equal(unname(status["3"]), "ACCEPTED")  # unplaced, no target
  expect_equal(unname(status["4"]), "ACCEPTED")

  acc <- stats::setNames(out$accepted_name_usage_id, out$taxon_id)
  expect_equal(unname(acc["2"]), "1")
  expect_true(is.na(acc["1"]))
  expect_true(is.na(acc["3"]))
})

test_that("read_wcvp passes taxon_name through as the canonical name", {
  path <- tempfile("wcvp_names_", fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  .write_wcvp_sample(path)

  out <- read_wcvp(path, verbose = FALSE)
  name <- stats::setNames(out$canonical_name, out$taxon_id)

  # hybrid multiplication sign preserved
  expect_equal(unname(name["4"]), "Utricularia × japonica")
  # infraspecific marker preserved
  expect_equal(unname(name["5"]), "Utricularia inflexa var. tenuifolia")
  # genuine embedded double-quote kept literal (quote = "")
  expect_true(grepl('"A"', name["6"], fixed = TRUE))

  expect_equal(unname(stats::setNames(out$genus, out$taxon_id)["1"]),
               "Stauntonia")
  expect_equal(unname(stats::setNames(out$specific_epithet,
                                      out$taxon_id)["1"]), "conspicua")
  expect_equal(unname(stats::setNames(out$infraspecific_epithet,
                                      out$taxon_id)["5"]), "tenuifolia")
})

test_that("read_wcvp produces the full unified backbone schema", {
  path <- tempfile("wcvp_names_", fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  .write_wcvp_sample(path)

  out <- read_wcvp(path, verbose = FALSE)
  required <- c("taxon_id", "canonical_name", "taxon_rank", "taxonomic_status",
                "accepted_name_usage_id", "family", "genus", "specific_epithet",
                "authorship", "infraspecific_epithet")
  expect_true(all(required %in% names(out)))
  expect_true(all(c("lifeform_description", "powo_id") %in% names(out)))
  expect_equal(nrow(out), 6L)
})
