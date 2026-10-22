# Controlled trait vocabularies + enrichment crosswalks.
#
# Each frozen vocabulary is the single source of concept identifiers; its
# crosswalk keys on concepts and must resolve every one of them. Every mapped
# column has to name a real enrichment and carry an axis mapping before any
# per-value mapping, and the mapping has to reach meta.json when an enrichment
# is built.

test_that("an unknown vocabulary name fails loudly", {
  expect_error(trait_vocab("nope"), "Unknown trait vocabulary")
})

test_that("the frozen T-SITA vocabulary loads with unique prefLabels", {
  v <- trait_vocab("tsita")
  expect_true(nrow(v) > 300L)
  expect_true(all(c("uri", "prefLabel", "top", "depth", "broader") %in% names(v)))
  expect_false(any(duplicated(v$prefLabel)))
  expect_true(all(v$top %in% c("Trait", "Ecological_preference")))
  expect_true(all(grepl("^https://ark\\.cefe\\.cnrs\\.fr/ark:", v$uri)))
})

test_that("every T-SITA crosswalk concept resolves to its URI", {
  x <- trait_crosswalk("tsita")
  v <- trait_vocab("tsita")
  expect_true(all(x$concept %in% v$prefLabel))
  expect_true(all(nzchar(x$concept_id)))
  expect_identical(x$concept_id, v$uri[match(x$concept, v$prefLabel)])
})

test_that("the freshwater vocabulary is internally consistent", {
  v <- trait_vocab("fwt")
  expect_false(any(duplicated(v$id)))
  expect_true(all(v$kind %in% c("axis", "class")))
  axes <- v[v$kind == "axis", ]
  classes <- v[v$kind == "class", ]
  expect_true(all(axes$scope %in% c("cross_group", "within_group")))
  expect_true(all(classes$axis %in% axes$id))
  expect_true(all(!is.na(classes$scheme) & !is.na(classes$source)))

  links <- c(classes$broader, unlist(strsplit(stats::na.omit(classes$exact_match), ";")))
  links <- links[!is.na(links)]
  expect_true(all(links %in% v$id))
  # a class and what it links to sit on the same axis
  same_axis <- function(from, to) v$axis[match(from, v$id)] == v$axis[match(to, v$id)]
  b <- classes[!is.na(classes$broader), ]
  expect_true(all(same_axis(b$id, b$broader)))
  e <- classes[!is.na(classes$exact_match), ]
  expect_true(all(same_axis(e$id, e$exact_match)))

  bounded <- !is.na(classes$lower) & !is.na(classes$upper)
  expect_true(all(classes$lower[bounded] <= classes$upper[bounded]))

  groups <- unique(unlist(strsplit(axes$groups, ";")))
  expect_true(all(groups %in% c("macroinvertebrates", "fish", "diatoms",
                                "phytoplankton", "phytobenthos", "macrophytes",
                                "amphibians", "zooplankton")))
  # amphibians are freshwater-dependent: no cross-group axis applies to them
  cross <- axes[axes$scope == "cross_group", ]
  expect_false(any(grepl("amphibians", cross$groups)))
})

test_that("bounded classes from different sources line up on the axis unit", {
  v <- trait_vocab("fwt")
  exact <- v[v$kind == "class" & !is.na(v$exact_match) & !is.na(v$lower), ]
  target <- v[match(exact$exact_match, v$id), ]
  expect_equal(exact$lower, target$lower)
  expect_equal(exact$upper, target$upper)
})

test_that("every crosswalk names a registered enrichment and resolves", {
  for (scheme in c("tsita", "fwt")) {
    x <- trait_crosswalk(scheme)
    expect_true(all(unique(x$enrichment) %in% list_enrichments()), info = scheme)
    expect_false(anyNA(x$concept_id), info = scheme)
  }
})

test_that("every value mapping sits under an axis mapping for its column", {
  for (scheme in c("tsita", "fwt")) {
    x <- trait_crosswalk(scheme)
    keyed <- split(x, list(x$enrichment, x$column), drop = TRUE)
    for (g in keyed) {
      if (any(!is.na(g$raw_value))) {
        expect_true(any(is.na(g$raw_value)),
                    info = paste(scheme, "value mapping without an axis mapping:",
                                 g$enrichment[1], g$column[1]))
      }
    }
  }
})

test_that("an fwt value maps to a class on its column's axis", {
  x <- trait_crosswalk("fwt")
  v <- trait_vocab("fwt")
  axis_of <- x[is.na(x$raw_value), c("enrichment", "column", "concept")]
  vals <- x[!is.na(x$raw_value), ]
  key <- paste(vals$enrichment, vals$column)
  col_axis <- axis_of$concept[match(key, paste(axis_of$enrichment, axis_of$column))]
  expect_true(all(v$kind[match(vals$concept, v$id)] == "class"))
  expect_identical(v$axis[match(vals$concept, v$id)], col_axis)
  expect_true(all(v$kind[match(axis_of$concept, v$id)] == "axis"))
})

test_that("the T-SITA meta block maps axis and value, leaves the rest alone", {
  m <- taxifydb:::.vocab_enrichment_meta(
    "tsita", "ecomorphosis",
    columns = c("ecomorphosis", "ecomorphosis_area", "ecomorphosis_reference"))
  expect_equal(m$columns$ecomorphosis$trait_label, "Ecomorphosis")
  expect_match(m$columns$ecomorphosis$trait_uri, "TSITA_")
  expect_equal(m$columns$ecomorphosis$values$`TRUE`$label, "Ecomorphosis_presence")
  expect_match(m$columns$ecomorphosis$values$`TRUE`$uri, "TSITA_")
  # a provenance column is not a trait and stays unmapped
  expect_null(m$columns$ecomorphosis_area)

  # an axis with no faithful T-SITA value set keeps the axis, no forced values
  me <- taxifydb:::.vocab_enrichment_meta("tsita", "ellers_collembola")
  expect_equal(me$columns$ellers_moisture_pref$trait_label, "Humidity_preference")
  expect_null(me$columns$ellers_moisture_pref$values)
  expect_null(me$columns$ellers_vertical_distribution)

  expect_null(taxifydb:::.vocab_enrichment_meta("tsita", "woodiness"))
})

test_that("the fwt meta block carries axis, unit and class per value", {
  m <- taxifydb:::.vocab_enrichment_meta("fwt", "epa_freshwater")
  expect_equal(m$columns$voltinism$trait_label, "voltinism")
  expect_equal(m$columns$voltinism$values$`1 Generation per year`$label,
               "voltinism/ds187/1_generation_per_year")
  expect_null(m$columns$voltinism$trait_uri)
  expect_null(m$scheme_uri)
  expect_null(m$columns$body_shape)

  f <- taxifydb:::.vocab_enrichment_meta("fwt", "fishtraits")
  expect_equal(f$columns$max_length_cm$unit, "cm")
  expect_equal(f$columns$max_length_cm$trait_label, "body_size")
})

test_that("a built enrichment carries each vocabulary block in meta.json", {
  td <- file.path(tempdir(), "vocab_meta_test")
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
  expect_null(meta$fwt)

  vp2 <- file.path(td, "epa_freshwater.vtr")
  df2 <- data.frame(canonical_name = "Baetis rhodani",
                    voltinism = "> 1 Generation per year",
                    body_shape = "Streamlined / fusiform", stringsAsFactors = FALSE)
  suppressMessages(suppressWarnings(build_enrichment_vtr(
    df2, vp2, "epa_freshwater", "test", source_url = "x", license = "Public domain")))
  meta2 <- jsonlite::read_json(file.path(td, "meta.json"))
  expect_null(meta2$tsita)
  expect_equal(meta2$fwt$columns$voltinism$values$`> 1 Generation per year`$label,
               "voltinism/ds187/gt_1_generation_per_year")
})

test_that("an enrichment with no crosswalk writes no vocabulary field", {
  td <- file.path(tempdir(), "vocab_nowalk_test")
  dir.create(td, showWarnings = FALSE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  vp <- file.path(td, "woodiness.vtr")
  df <- data.frame(canonical_name = "Quercus robur", woody = TRUE,
                   stringsAsFactors = FALSE)
  suppressMessages(build_enrichment_vtr(df, vp, "woodiness", "test",
    source_url = "x", license = "CC0"))
  meta <- jsonlite::read_json(file.path(td, "meta.json"))
  expect_null(meta$tsita)
  expect_null(meta$fwt)
})
