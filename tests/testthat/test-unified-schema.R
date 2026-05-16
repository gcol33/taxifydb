# Build-side schema contract.
#
# Each per-backend reader feeds taxifydb::precompute_backbone() and the
# pre-built .vtr that taxify (the runtime) reads via its `col_map`. The
# unified snake_case schema is the contract between the two packages — if
# any reader emits source-native names by accident (e.g., camelCase DwC), a
# rebuilt .vtr will silently fail to match in taxify::*_backend() col_maps.
#
# These tests assert that the read_*() functions used by build_*() emit
# the column names the runtime expects. Regression guard for the build-
# pipeline divergence resolved 2026-05-16.

unified_main_cols <- c(
  "taxon_id", "canonical_name", "taxon_rank", "taxonomic_status",
  "accepted_name_usage_id", "family", "genus", "specific_epithet",
  "authorship", "infraspecific_epithet"
)

write_wfo_fixture <- function(path) {
  df <- data.frame(
    taxonID = c("wfo-1", "wfo-2"),
    scientificName = c("Quercus robur", "Quercus petraea"),
    taxonRank = c("species", "species"),
    taxonomicStatus = c("Accepted", "Accepted"),
    acceptedNameUsageID = c(NA, NA),
    family = c("Fagaceae", "Fagaceae"),
    genus = c("Quercus", "Quercus"),
    specificEpithet = c("robur", "petraea"),
    scientificNameAuthorship = c("L.", "(Matt.) Liebl."),
    infraspecificEpithet = c(NA, NA),
    scientificNameID = c("urn:lsid:wfo:1", "urn:lsid:wfo:2"),
    parentNameUsageID = c(NA, NA),
    namePublishedIn = c(NA, NA),
    nomenclaturalStatus = c(NA, NA),
    taxonRemarks = c(NA, NA),
    subfamily = c(NA, NA),
    tribe = c(NA, NA),
    subtribe = c(NA, NA),
    subgenus = c(NA, NA),
    stringsAsFactors = FALSE
  )
  con <- file(path, "w", encoding = "latin1")
  on.exit(close(con), add = TRUE)
  utils::write.table(df, con, sep = "\t", quote = FALSE, row.names = FALSE,
                     na = "")
}

test_that("read_wfo emits unified-schema column names", {
  txt <- tempfile(fileext = ".txt")
  on.exit(unlink(txt), add = TRUE)
  write_wfo_fixture(txt)

  df <- read_wfo(txt, verbose = FALSE)

  expect_true(all(unified_main_cols %in% names(df)),
              info = paste("missing:",
                           paste(setdiff(unified_main_cols, names(df)),
                                 collapse = ", ")))
  # WFO-specific extras must survive the normalize step verbatim.
  expect_true(all(c("scientificNameID", "parentNameUsageID",
                    "nomenclaturalStatus", "taxonRemarks") %in% names(df)))
})


write_col_fixture <- function(col_dir) {
  dir.create(col_dir, recursive = TRUE, showWarnings = FALSE)
  df <- data.frame(
    `dwc:taxonID` = c("col-1", "col-2"),
    `dwc:parentNameUsageID` = c(NA, NA),
    `dwc:acceptedNameUsageID` = c(NA, NA),
    `dwc:scientificName` = c("Quercus robur L.",
                              "Quercus petraea (Matt.) Liebl."),
    `dwc:scientificNameAuthorship` = c("L.", "(Matt.) Liebl."),
    `dwc:taxonRank` = c("species", "species"),
    `dwc:taxonomicStatus` = c("Accepted", "Accepted"),
    `dwc:family` = c("Fagaceae", "Fagaceae"),
    `dwc:genericName` = c("Quercus", "Quercus"),
    `dwc:specificEpithet` = c("robur", "petraea"),
    `dwc:infraspecificEpithet` = c(NA, NA),
    `dwc:nomenclaturalCode` = c("ICN", "ICN"),
    `dwc:kingdom` = c("Plantae", "Plantae"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  utils::write.table(df, file.path(col_dir, "Taxon.tsv"),
                     sep = "\t", quote = FALSE, row.names = FALSE, na = "",
                     fileEncoding = "UTF-8")
}

test_that("read_col emits unified-schema column names", {
  col_dir <- tempfile("col_")
  on.exit(unlink(col_dir, recursive = TRUE), add = TRUE)
  write_col_fixture(col_dir)

  df <- read_col(col_dir, verbose = FALSE)

  expect_true(all(unified_main_cols %in% names(df)),
              info = paste("missing:",
                           paste(setdiff(unified_main_cols, names(df)),
                                 collapse = ", ")))
  # The original COL scientificName (with authorship) must survive as an
  # extra alongside the authorship-free canonical_name.
  expect_true("scientificName" %in% names(df))
})


write_gbif_fixture <- function(gz_path) {
  cols <- c(
    "id", "parent_key", "basionym_key", "is_synonym", "status",
    "rank", "nom_status", "constituent_key", "origin", "source_taxon_key",
    "kingdom_key", "phylum_key", "class_key", "order_key", "family_key",
    "genus_key", "species_key", "name_id", "scientific_name",
    "canonical_name", "genus_or_above", "specific_epithet",
    "infra_specific_epithet", "notho_type", "authorship", "year",
    "bracket_authorship", "bracket_year", "name_published_in", "issues"
  )
  rows <- list(
    list(id = "1", parent_key = "10", basionym_key = "\\N",
         is_synonym = "f", status = "ACCEPTED",
         rank = "SPECIES", nom_status = "\\N", constituent_key = "\\N",
         origin = "SOURCE", source_taxon_key = "\\N",
         kingdom_key = "\\N", phylum_key = "\\N", class_key = "\\N",
         order_key = "\\N", family_key = "1",
         genus_key = "\\N", species_key = "\\N", name_id = "\\N",
         scientific_name = "Quercus robur L.",
         canonical_name = "Quercus robur", genus_or_above = "Quercus",
         specific_epithet = "robur", infra_specific_epithet = "\\N",
         notho_type = "\\N", authorship = "L.", year = "1753",
         bracket_authorship = "\\N", bracket_year = "\\N",
         name_published_in = "\\N", issues = "\\N"),
    list(id = "1", parent_key = "0", basionym_key = "\\N",
         is_synonym = "f", status = "ACCEPTED",
         rank = "FAMILY", nom_status = "\\N", constituent_key = "\\N",
         origin = "SOURCE", source_taxon_key = "\\N",
         kingdom_key = "\\N", phylum_key = "\\N", class_key = "\\N",
         order_key = "\\N", family_key = "1",
         genus_key = "\\N", species_key = "\\N", name_id = "\\N",
         scientific_name = "Fagaceae",
         canonical_name = "Fagaceae", genus_or_above = "\\N",
         specific_epithet = "\\N", infra_specific_epithet = "\\N",
         notho_type = "\\N", authorship = "\\N", year = "\\N",
         bracket_authorship = "\\N", bracket_year = "\\N",
         name_published_in = "\\N", issues = "\\N")
  )
  lines <- vapply(rows, function(r) {
    paste(unlist(r[cols]), collapse = "\t")
  }, character(1L))

  con <- gzfile(gz_path, "w")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con)
}

test_that("read_gbif emits unified-schema column names", {
  gz_path <- tempfile(fileext = ".txt.gz")
  on.exit(unlink(gz_path), add = TRUE)
  write_gbif_fixture(gz_path)

  df <- read_gbif(gz_path, verbose = FALSE)

  expect_true(all(unified_main_cols %in% names(df)),
              info = paste("missing:",
                           paste(setdiff(unified_main_cols, names(df)),
                                 collapse = ", ")))
  # parent_key must survive as an extra — taxify::resolve_kingdom_via_gbif()
  # needs it for the genus-to-kingdom walk.
  expect_true("parent_key" %in% names(df))
})
