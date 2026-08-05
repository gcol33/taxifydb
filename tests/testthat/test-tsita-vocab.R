# T-SITA vocabulary + enrichment crosswalk.
#
# The frozen thesaurus is the single source of trait URIs; the crosswalk keys on
# prefLabels and must resolve every one of them. Every mapped column has to name
# a real enrichment and carry an axis mapping before any per-value mapping, and
# the mapping has to reach meta.json when an enrichment is built.

test_that("the frozen T-SITA vocabulary loads with unique prefLabels", {
  v <- tsita_vocab()
  expect_true(nrow(v) > 300L)
  expect_true(all(c("uri", "prefLabel", "top", "depth", "broader") %in% names(v)))
  expect_false(any(duplicated(v$prefLabel)))
  expect_true(all(v$top %in% c("Trait", "Ecological_preference")))
  expect_true(all(grepl("^https://ark\\.cefe\\.cnrs\\.fr/ark:", v$uri)))
})

test_that("every crosswalk label resolves to a T-SITA concept URI", {
  x <- tsita_crosswalk()
  v <- tsita_vocab()
  expect_true(all(x$tsita_label %in% v$prefLabel))
  expect_true(all(nzchar(x$tsita_uri)))
  expect_identical(x$tsita_uri, v$uri[match(x$tsita_label, v$prefLabel)])
})

test_that("crosswalk enrichment names are all registered enrichments", {
  x <- tsita_crosswalk()
  expect_true(all(unique(x$enrichment) %in% list_enrichments()))
})

test_that("every value mapping sits under an axis mapping for its column", {
  x <- tsita_crosswalk()
  keyed <- split(x, list(x$enrichment, x$column), drop = TRUE)
  for (g in keyed) {
    if (any(!is.na(g$raw_value))) {
      expect_true(any(is.na(g$raw_value)),
                  info = paste("value mapping without an axis mapping:",
                               g$enrichment[1], g$column[1]))
    }
  }
})

test_that(".tsita_enrichment_meta maps axis and value, leaves the rest alone", {
  m <- taxifydb:::.tsita_enrichment_meta(
    "ecomorphosis",
    columns = c("ecomorphosis", "ecomorphosis_area", "ecomorphosis_reference"))
  expect_equal(m$columns$ecomorphosis$trait_label, "Ecomorphosis")
  expect_match(m$columns$ecomorphosis$trait_uri, "TSITA_")
  expect_equal(m$columns$ecomorphosis$values$`TRUE`$label, "Ecomorphosis_presence")
  # a provenance column is not a trait and stays unmapped
  expect_null(m$columns$ecomorphosis_area)

  # an axis with no faithful T-SITA value set keeps the axis, no forced values
  me <- taxifydb:::.tsita_enrichment_meta("ellers_collembola")
  expect_equal(me$columns$ellers_moisture_pref$trait_label, "Humidity_preference")
  expect_null(me$columns$ellers_moisture_pref$values)
  expect_null(me$columns$ellers_vertical_distribution)

  expect_null(taxifydb:::.tsita_enrichment_meta("woodiness"))
})

test_that("a built enrichment carries the T-SITA block in meta.json", {
  td <- file.path(tempdir(), "tsita_meta_test")
  dir.create(td, showWarnings = FALSE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  vp <- file.path(td, "ecomorphosis.vtr")
  df <- data.frame(
    canonical_name = c("Isotoma viridis", "Orchesella cincta"),
    ecomorphosis = c(TRUE, TRUE),
    ecomorphosis_area = "Europe",
    ecomorphosis_reference = "Bonfanti 2022",
    stringsAsFactors = FALSE)
  suppressMessages(build_enrichment_vtr(
    df, vp, "ecomorphosis", "test",
    source_url = "https://zenodo.org/record/7194559", license = "CC BY 4.0"))

  meta <- jsonlite::read_json(file.path(td, "meta.json"))
  expect_false(is.null(meta$tsita))
  expect_equal(meta$tsita$columns$ecomorphosis$trait_label, "Ecomorphosis")
  expect_match(meta$tsita$scheme_uri, "ark:/66666/th558")
})

test_that("an enrichment with no crosswalk writes no tsita field", {
  td <- file.path(tempdir(), "tsita_nowalk_test")
  dir.create(td, showWarnings = FALSE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  vp <- file.path(td, "woodiness.vtr")
  df <- data.frame(canonical_name = "Quercus robur", woody = TRUE,
                   stringsAsFactors = FALSE)
  suppressMessages(build_enrichment_vtr(df, vp, "woodiness", "test",
    source_url = "x", license = "CC0"))
  meta <- jsonlite::read_json(file.path(td, "meta.json"))
  expect_null(meta$tsita)
})
