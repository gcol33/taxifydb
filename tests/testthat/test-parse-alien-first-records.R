# The Seebens first-record release is keyed on location wording, is delimited
# with semicolons, and is UTF-8 apart from a run of latin1 lines. Each of those
# is a way for the table to come back smaller or mis-decoded rather than to
# fail, so each is pinned here.

HDR <- paste(
  "locationID", "location", "verbatimLocation", "taxonID", "taxon", "habitat",
  "firstRecordEvent", "verbatimFirstRecordEvent", "confidenceFirstRecordEvent",
  "occurrenceStatus", "establishmentMeans", "degreeOfEstablishment", "pathway",
  "datasetName", "bibliographicCitation", "accessRights", sep = ";")

rec <- function(location, taxon, year, status = "present", verbatim = NULL,
                loc_id = "1", tax_id = "1") {
  paste(
    sprintf('"%s"', c(loc_id, location, location, tax_id, taxon, "",
                      as.character(year),
                      if (is.null(verbatim)) as.character(year) else verbatim,
                      "high confidence", status, "introduced", "established",
                      "", "SomeDataset", "Author et al (2020)", "Free")),
    collapse = ";")
}

# Written as bytes so the fixture's encoding is exactly what is asserted;
# writeLines would re-encode in the session's own encoding.
write_fixture <- function(lines, path) {
  writeBin(charToRaw(paste0(paste(lines, collapse = "\n"), "\n")), path)
  path
}


test_that("locations are mapped through accent folding and renamed spellings", {
  f <- tempfile(fileext = ".csv")
  write_fixture(c(HDR,
                  rec("United States of America", "Aus unus", 1900),
                  rec("Timor-Leste", "Aus duo", 1910),
                  rec("Hawaii", "Aus tres", 1920)), f)

  out <- parse_alien_first_records(f)
  expect_equal(nrow(out), 3L)
  expect_equal(out$country_code[out$canonical_name == "Aus unus"], "US")
  expect_equal(out$country_code[out$canonical_name == "Aus duo"], "TL")
  expect_equal(out$country_code[out$canonical_name == "Aus tres"], "US")
})


test_that("an accented respelling maps without its own map entry", {
  # "Reunion" is what the map holds; the release writes it with the acute.
  reunion <- rawToChar(as.raw(c(0x52, 0xc3, 0xa9, 0x75, 0x6e, 0x69,
                               0x6f, 0x6e)))
  f <- tempfile(fileext = ".csv")
  write_fixture(c(HDR, rec(reunion, "Aus unus", 1900)), f)

  out <- parse_alien_first_records(f)
  expect_equal(nrow(out), 1L)
  expect_equal(out$country_code, "RE")
})


test_that("a location the map does not hold is an error, not a dropped row", {
  f <- tempfile(fileext = ".csv")
  write_fixture(c(HDR,
                  rec("France", "Aus unus", 1900),
                  rec("Republic of Longitude", "Aus duo", 1910)), f)

  expect_error(parse_alien_first_records(f), "Republic of Longitude")
})


test_that("records with no location are dropped rather than erroring", {
  # Sub- and supra-national entries ("Aegean Sea") carry a verbatim location
  # but no location, and have no country to be keyed on.
  f <- tempfile(fileext = ".csv")
  write_fixture(c(HDR,
                  rec("France", "Aus unus", 1900),
                  rec("", "Aus duo", 1910)), f)

  out <- parse_alien_first_records(f)
  expect_equal(nrow(out), 1L)
  expect_equal(out$canonical_name, "Aus unus")
})


test_that("a present record wins over an earlier non-present one", {
  f <- tempfile(fileext = ".csv")
  write_fixture(c(HDR,
                  rec("France", "Aus unus", 1200, status = "absent"),
                  rec("France", "Aus unus", 1900, status = "present")), f)

  out <- parse_alien_first_records(f)
  expect_equal(nrow(out), 1L)
  expect_equal(out$alien_first_record, 1900L)
  # The status published is the retained record's own, not the mode over both:
  # a tie between "absent" and "present" reduces to "absent" alphabetically.
  expect_equal(out$alien_first_record_status, "present")
})


test_that("the earliest year still wins among present records", {
  f <- tempfile(fileext = ".csv")
  write_fixture(c(HDR,
                  rec("France", "Aus unus", 1950),
                  rec("France", "Aus unus", 1900)), f)

  out <- parse_alien_first_records(f)
  expect_equal(nrow(out), 1L)
  expect_equal(out$alien_first_record, 1900L)
})


test_that("a species with only non-present records is kept", {
  f <- tempfile(fileext = ".csv")
  write_fixture(c(HDR, rec("France", "Aus unus", 1200, status = "absent")), f)

  out <- parse_alien_first_records(f)
  expect_equal(nrow(out), 1L)
  expect_equal(out$alien_first_record_status, "absent")
})


test_that("latin1 lines decode beside utf-8 ones in the same file", {
  # o-circumflex twice: once as utf-8 (c3 b4), once as latin1 (f4).
  utf8_name  <- rawToChar(as.raw(c(0x43, 0xc3, 0xb4, 0x74, 0x65)))
  latin1_name <- rawToChar(as.raw(c(0x43, 0xf4, 0x74, 0x65)))
  f <- tempfile(fileext = ".csv")
  write_fixture(c(HDR,
                  rec("France", paste("Aus", utf8_name), 1900),
                  rec("Spain", paste("Aus", latin1_name), 1910)), f)

  out <- parse_alien_first_records(f)
  expect_equal(nrow(out), 2L)
  # Both lines name the same taxon; only the encoding of the file differed.
  expect_equal(length(unique(out$canonical_name)), 1L)
  expect_false(any(is.na(iconv(out$canonical_name, "UTF-8", "UTF-8"))))
})


test_that("the verbatim year keeps spans and decades as written", {
  f <- tempfile(fileext = ".csv")
  write_fixture(c(HDR,
                  rec("France", "Aus unus", -2500, verbatim = "-3000 - -2000"),
                  rec("Spain", "Aus duo", 1855, verbatim = "1850s")), f)

  out <- parse_alien_first_records(f)
  expect_equal(sort(out$verbatimfirstrecordevent), c("-3000 - -2000", "1850s"))
  expect_equal(out$alien_first_record[out$country_code == "FR"], -2500L)
})


test_that("internal join ids are not published as trait columns", {
  f <- tempfile(fileext = ".csv")
  write_fixture(c(HDR, rec("France", "Aus unus", 1900)), f)

  out <- parse_alien_first_records(f)
  expect_false(any(grepl("^(location|taxon)id", names(out))))
})


test_that("the 1500 and 1492 convention markers are not served as a year", {
  f <- tempfile(fileext = ".csv")
  write_fixture(c(HDR,
                  rec("Italy", "Aus unus", 1500),
                  rec("Germany", "Aus duo", 1492)), f)

  out <- parse_alien_first_records(f)
  expect_equal(nrow(out), 2L)
  expect_true(all(is.na(out$alien_first_record)))
  # The raw marker is still kept, verbatim.
  expect_setequal(out$verbatimfirstrecordevent, c("1500", "1492"))
})


test_that("a real year wins over a convention marker for the same pair", {
  f <- tempfile(fileext = ".csv")
  write_fixture(c(HDR,
                  rec("Italy", "Aus unus", 1500),
                  rec("Italy", "Aus unus", 1679)), f)

  out <- parse_alien_first_records(f)
  expect_equal(nrow(out), 1L)
  expect_equal(out$alien_first_record, 1679L)
})


test_that("the concept-grain reducer keeps the earliest present-preferred year", {
  # Two synonyms collapse onto one accepted name per country; the marker year
  # (already NA after parsing) must not win, and the earliest real year does.
  df <- data.frame(
    canonical_name            = rep("Aus unus", 4L),
    country_code              = c("SE", "SE", "IT", "IT"),
    alien_first_record        = c(1838L, 1726L, NA_integer_, 1679L),
    alien_first_record_status = rep("present", 4L),
    stringsAsFactors = FALSE)

  out <- .keep_earliest_first_record(df, c("canonical_name", "country_code"))
  expect_equal(out$alien_first_record[out$country_code == "SE"], 1726L)
  expect_equal(out$alien_first_record[out$country_code == "IT"], 1679L)
})


test_that("the reducer prefers a present record to an earlier non-present one", {
  df <- data.frame(
    canonical_name            = rep("Aus unus", 2L),
    country_code              = rep("FR", 2L),
    alien_first_record        = c(1850L, 1900L),
    alien_first_record_status = c("absent", "present"),
    stringsAsFactors = FALSE)

  out <- .keep_earliest_first_record(df, c("canonical_name", "country_code"))
  expect_equal(out$alien_first_record, 1900L)
  expect_equal(out$alien_first_record_status, "present")
})


test_that("region names folding to one key must agree on the country", {
  # The lookup is only safe while no two wordings fold together and disagree.
  keys <- .norm_region_key(names(.seebens_region_map))
  by_key <- split(unname(.seebens_region_map), keys)
  clash <- vapply(by_key, function(v) length(unique(v[!is.na(v)])) > 1L,
                  logical(1))
  expect_equal(names(by_key)[clash], character(0))
})
