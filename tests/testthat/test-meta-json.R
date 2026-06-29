# Serialization guard for meta.json and manifest entries.
#
# Absent optional fields (no DOI, no attribution, no group column) must be
# omitted from the written JSON, never rendered as `null` or an empty object
# `{}`. jsonlite::write_json(auto_unbox = TRUE) turns a zero-length list into
# `{}` and an NA into `null`; downstream readers parse both back to a
# zero-length list, which slips past `%||%` guards and breaks `&&`/`is.na()`.

test_that("drop_empty_fields removes null/NA/empty-string/empty-list fields", {
  x <- list(
    name        = "baseflor",
    source_doi  = NULL,
    attribution = NA_character_,
    group_col   = "",
    empty_list  = list(),
    nrow        = 10L,
    groups      = c("AT", "DE")
  )

  clean <- drop_empty_fields(x)

  expect_false("source_doi"  %in% names(clean))
  expect_false("attribution" %in% names(clean))
  expect_false("group_col"   %in% names(clean))
  expect_false("empty_list"  %in% names(clean))
  expect_equal(clean$nrow, 10L)
  expect_equal(clean$groups, c("AT", "DE"))
})

test_that("an absent citation doi is omitted, not written as {} or null", {
  meta <- list(
    name     = "baseflor",
    version  = "2023.10",
    license  = "ODbL 1.0 / CC BY-SA 2.0",
    citation = list(
      key     = "julve1998baseflor",
      type    = "misc",
      authors = "Julve P",
      year    = "1998",
      title   = "baseflor. Index botanique de la Flore de France.",
      doi     = list()
    )
  )

  clean <- drop_empty_fields(meta)

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  jsonlite::write_json(clean, tmp, pretty = TRUE, auto_unbox = TRUE,
                       null = "null")

  txt <- paste(readLines(tmp), collapse = "\n")
  expect_false(grepl('"doi"', txt, fixed = TRUE))
  expect_false(grepl("{}", txt, fixed = TRUE))
  expect_false(grepl("null", txt, fixed = TRUE))

  rt <- jsonlite::read_json(tmp, simplifyVector = FALSE)
  expect_false("doi" %in% names(rt$citation))
  expect_null(rt$citation$doi)
  expect_equal(rt$citation$key, "julve1998baseflor")
})

test_that("a NULL source_doi is omitted from a serialized meta block", {
  meta <- list(
    type        = "enrichment",
    name        = "ecoflora",
    version     = "2026.06",
    source_url  = "https://example.org/ecoflora.csv",
    source_doi  = NULL,
    license     = "CC BY-NC-SA 4.0",
    attribution = "Fitter & Peat (1994)",
    group_col   = NULL,
    nrow        = 5L
  )

  clean <- drop_empty_fields(meta)

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  jsonlite::write_json(clean, tmp, pretty = TRUE, auto_unbox = TRUE,
                       null = "null")

  rt <- jsonlite::read_json(tmp, simplifyVector = FALSE)
  expect_false("source_doi" %in% names(rt))
  expect_false("group_col"  %in% names(rt))
  expect_equal(rt$attribution, "Fitter & Peat (1994)")
})
