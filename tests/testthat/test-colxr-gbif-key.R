gbif_fixture <- function() {
  path <- tempfile(fileext = ".vtr")
  vectra::write_vtr(data.frame(
    taxon_id       = c("2878688", "7911626", "8206510", "4087167", "100", "101", "102"),
    canonical_name = c("Quercus robur", "Quercus robur", "Quercus robur",
                       "Entoloma truncatum", "Corona loroisiana",
                       "Homonymus alpha", "Homonymus alpha"),
    authorship     = c("L.", "Asso", "A.DC.", "Noordel. & Co-David", NA,
                       "Smith", "Smith"),
    taxon_rank     = "SPECIES",
    stringsAsFactors = FALSE
  ), path)
  path
}

test_that("authorship is compared without years, brackets, case or accents", {
  au <- crosswalk_authorship(c("(Hupé, 1857)", "(Romagn.) Noordel.", NA))
  expect_equal(au$full, c("hupe", "romagnnoordel", ""))
  expect_equal(au$outer, c("", "noordel", ""))
})

test_that("a COL XR usage takes the GBIF key of the same name and author", {
  cw <- colxr_gbif_crosswalk(gbif_fixture())
  key <- colxr_gbif_lookup(
    cw,
    canonical_name = c("Quercus robur", "Quercus robur", "Entoloma truncatum",
                       "Quercus robur"),
    authorship     = c("L.", "Asso", "(Romagn.) Noordel. & Co-David", "Mill."),
    taxon_rank     = "SPECIES"
  )
  expect_equal(key, c("2878688", "7911626", "4087167", NA))
})

test_that("a name GBIF records without an author matches on name and rank", {
  cw <- colxr_gbif_crosswalk(gbif_fixture())
  expect_equal(
    colxr_gbif_lookup(cw, "Corona loroisiana", "(Hupé, 1857)", "SPECIES"),
    "100"
  )
})

test_that("a homonym maps to the ascending set of its GBIF keys", {
  cw <- colxr_gbif_crosswalk(gbif_fixture())
  expect_equal(
    colxr_gbif_lookup(cw, "Homonymus alpha", "Smith", "SPECIES"),
    "101|102"
  )
})

test_that("normalize_colxr writes gbif_key beside the COL XR identifier", {
  raw <- data.frame(
    taxonID = "4R5YN", taxonomicStatus = "accepted", taxonRank = "species",
    scientificName = "Quercus robur", scientificNameAuthorship = "L.",
    family = "Fagaceae", genus = "Quercus", stringsAsFactors = FALSE
  )
  cw <- colxr_gbif_crosswalk(gbif_fixture())
  out <- normalize_colxr(raw, gbif_crosswalk = cw, verbose = FALSE)
  expect_equal(out$taxon_id, "4R5YN")
  expect_equal(out$gbif_key, "2878688")
  expect_true(is.na(normalize_colxr(raw, verbose = FALSE)$gbif_key))
})
