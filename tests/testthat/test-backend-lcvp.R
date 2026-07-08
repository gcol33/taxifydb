# LCVP reader: canonical assembly from Input.* parts, status/link mapping.

# A tab_lcvp sample saved as an .rda (the reader loads it with base load()),
# covering an accepted species, a synonym linked by globalId.of.Output.Taxon,
# an unresolved name, and infraspecific taxa at var. and forma rank.
.write_lcvp_sample <- function(path) {
  tab_lcvp <- data.frame(
    global.Id                  = c(1L, 3L, 5L, 6L, 7L),
    Input.Genus                = c("Aa", "Aa", "Bar", "Utricularia",
                                   "Utricularia"),
    Input.Epitheton            = c("argyrolepis", "brevis", "foo", "inflexa",
                                   "humboldtii"),
    Rank                       = c("species", "species", "species", "var.",
                                   "forma"),
    Input.Subspecies.Epitheton = c("nil", "nil", "nil", "tenuifolia",
                                    "albiflora"),
    Input.Authors              = c("(Rchb.f.) Rchb.f.", "Schltr.", "L.",
                                   "P.Taylor", "Steyerm."),
    Status                     = c("accepted", "synonym", "unresolved",
                                   "accepted", "accepted"),
    globalId.of.Output.Taxon   = c(1L, 100L, 999L, 6L, 7L),
    Output.Taxon               = c("Aa argyrolepis (Rchb.f.) Rchb.f. ",
                                   "Myrosmodes breve (Schltr.) Garay ",
                                   "Bar foo L. ",
                                   "Utricularia inflexa var. tenuifolia ",
                                   "Utricularia humboldtii f. albiflora "),
    Family                     = c("Orchidaceae", "Orchidaceae", "Baraceae",
                                   "Lentibulariaceae", "Lentibulariaceae"),
    Order                      = c("Asparagales", "Asparagales", "Barales",
                                   "Lamiales", "Lamiales"),
    Literature                 = rep("", 5),
    Comments                   = rep("", 5),
    stringsAsFactors           = FALSE
  )
  save(tab_lcvp, file = path)
}

test_that("read_lcvp assembles canonical names from the input parts", {
  path <- tempfile("tab_lcvp_", fileext = ".rda")
  on.exit(unlink(path), add = TRUE)
  .write_lcvp_sample(path)

  out <- read_lcvp(path, verbose = FALSE)
  name <- stats::setNames(out$canonical_name, out$taxon_id)

  expect_equal(unname(name["1"]), "Aa argyrolepis")
  # var. marker taken from Rank, subspecies epithet appended
  expect_equal(unname(name["6"]), "Utricularia inflexa var. tenuifolia")
  # forma rank renders as the standard "f." abbreviation
  expect_equal(unname(name["7"]), "Utricularia humboldtii f. albiflora")

  infra <- stats::setNames(out$infraspecific_epithet, out$taxon_id)
  expect_equal(unname(infra["6"]), "tenuifolia")
  expect_true(is.na(infra["1"]))  # "nil" sentinel dropped for species rank
})

test_that("read_lcvp maps Status and the accepted-name link", {
  path <- tempfile("tab_lcvp_", fileext = ".rda")
  on.exit(unlink(path), add = TRUE)
  .write_lcvp_sample(path)

  out <- read_lcvp(path, verbose = FALSE)
  status <- stats::setNames(out$taxonomic_status, out$taxon_id)
  acc    <- stats::setNames(out$accepted_name_usage_id, out$taxon_id)

  expect_equal(unname(status["1"]), "ACCEPTED")
  expect_equal(unname(status["3"]), "SYNONYM")
  # unresolved is kept as its own accepted concept, not asserted as a synonym
  expect_equal(unname(status["5"]), "ACCEPTED")

  expect_equal(unname(acc["3"]), "100")
  expect_true(is.na(acc["1"]))
  expect_true(is.na(acc["5"]))
})

test_that("read_lcvp produces the full unified backbone schema", {
  path <- tempfile("tab_lcvp_", fileext = ".rda")
  on.exit(unlink(path), add = TRUE)
  .write_lcvp_sample(path)

  out <- read_lcvp(path, verbose = FALSE)
  required <- c("taxon_id", "canonical_name", "taxon_rank", "taxonomic_status",
                "accepted_name_usage_id", "family", "genus", "specific_epithet",
                "authorship", "infraspecific_epithet")
  expect_true(all(required %in% names(out)))
  expect_true("order" %in% names(out))
  expect_equal(unname(stats::setNames(out$genus, out$taxon_id)["6"]),
               "Utricularia")
  expect_equal(nrow(out), 5L)
})
