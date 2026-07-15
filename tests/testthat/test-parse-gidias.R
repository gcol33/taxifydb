# GIDIAS records what a species impacts in Affected.native.species.Taxon (a
# controlled 5-term vocabulary), so parse_gidias aggregates each species once
# over all its records (affected_taxon = "Any") and once per affected taxon.
# The two grains have to stay distinct: "Any" is the only row carrying SEICAT
# and the negative records with no affected taxon recorded, and it must keep
# answering exactly what it answered before the per-taxon rows existed.

# One species impacting two taxa at different severities (a cat-shaped case:
# vertebrates driven to extinction, invertebrate populations only reduced),
# plus a negative record with no affected taxon, plus a CWB record.
gidias_fixture <- function() {
  f <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(
    Verified.Name.GBIF.Taxon      = c(rep("Felis catus", 4L), "Bufo marinus"),
    Affected.native.species.Taxon = c("Vertebrate", "Invertebrate", "", "", ""),
    direction.Nature              = c("Negative", "Negative", "Negative", "", "Negative"),
    magnitude.Nature              = c(3L, 2L, 1L, NA, 2L),
    global.extinction             = c("TRUE", "FALSE", "FALSE", "FALSE", "FALSE"),
    mechanism.Nature.clean        = c("Predation", "Predation; Competition", "Predation", "", "Competition"),
    direction.CWB                 = c("", "", "", "negative", ""),
    magnitude.CWB                 = c(NA, NA, NA, 2L, NA),
    affected.CWB.clean            = c("", "", "", "Health", ""),
    IAS.Taxon                     = c(rep("Vertebrate", 4L), "Vertebrate"),
    Kingdom                       = "Animalia",
    Realm                         = c(rep("terrestrial", 4L), "terrestrial"),
    DOI                           = c("10.1/a", "10.1/b", "10.1/a", "10.1/c", "10.1/d"),
    Reference                     = "ref",
    stringsAsFactors              = FALSE
  ), f, row.names = FALSE)
  f
}

# parse_gidias resolves names internally (registry: resolve_names = FALSE), so
# stub the resolver to keep the test offline and about the aggregation.
local_stub_resolver <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    resolve_name_map = function(names, verbose = FALSE) {
      data.frame(input_name = names, accepted_name = names,
                 stringsAsFactors = FALSE)
    },
    .env = env
  )
}

test_that("each species gets one all-records aggregate plus a row per affected taxon", {
  local_stub_resolver()
  f <- gidias_fixture()
  on.exit(unlink(f), add = TRUE)

  out <- parse_gidias(f)

  expect_true(all(c("canonical_name", "affected_taxon") %in% names(out)))
  expect_setequal(out$affected_taxon[out$canonical_name == "Felis catus"],
                  c("Any", "Vertebrate", "Invertebrate"))
  # A species with no affected taxon recorded still gets its aggregate.
  expect_identical(out$affected_taxon[out$canonical_name == "Bufo marinus"], "Any")
  # Exactly one aggregate per species, so the default grain stays a lookup.
  expect_false(anyDuplicated(out$canonical_name[out$affected_taxon == "Any"]) > 0L)
})

test_that("the aggregate summarises every record, the per-taxon rows only their own", {
  local_stub_resolver()
  f <- gidias_fixture()
  on.exit(unlink(f), add = TRUE)

  out <- parse_gidias(f)
  pick <- function(taxon, col) {
    out[[col]][out$canonical_name == "Felis catus" & out$affected_taxon == taxon]
  }

  # Most severe across all records: magnitude 3 + global extinction -> Massive.
  expect_identical(pick("Any", "gidias_eicat_category"), "MV")
  expect_identical(pick("Any", "gidias_n_records"), 4L)
  expect_identical(pick("Any", "gidias_n_negative"), 3L)

  # The collapse this grain exists to undo: the cat is MV overall but only
  # Moderate for invertebrates.
  expect_identical(pick("Vertebrate", "gidias_eicat_category"), "MV")
  expect_identical(pick("Invertebrate", "gidias_eicat_category"), "MO")
  expect_identical(pick("Invertebrate", "gidias_n_records"), 1L)
  expect_false(pick("Invertebrate", "gidias_global_extinction"))

  # Mechanisms are per-grain too, and compound cells are split.
  expect_identical(pick("Invertebrate", "gidias_eicat_mechanism"),
                   "Competition; Predation")
  expect_identical(pick("Vertebrate", "gidias_eicat_mechanism"), "Predation")
})

test_that("the affected-taxon axis slices the environmental block only", {
  local_stub_resolver()
  f <- gidias_fixture()
  on.exit(unlink(f), add = TRUE)

  out <- parse_gidias(f)
  grp <- out[out$affected_taxon != "Any", , drop = FALSE]

  # SEICAT measures impact on people's activities, so it is not a question the
  # affected-native-taxon column can answer: it lives on the aggregate alone.
  expect_true(all(is.na(grp$gidias_seicat_category)))
  expect_true(all(is.na(grp$gidias_seicat_magnitude)))
  expect_true(all(is.na(grp$gidias_seicat_affected)))
  expect_identical(
    out$gidias_seicat_category[out$canonical_name == "Felis catus" &
                                 out$affected_taxon == "Any"],
    "MO"
  )
})

test_that("a negative record with no affected taxon lands in the aggregate only", {
  local_stub_resolver()
  f <- gidias_fixture()
  on.exit(unlink(f), add = TRUE)

  out <- parse_gidias(f)
  cat_rows <- out[out$canonical_name == "Felis catus", , drop = FALSE]

  # The untagged magnitude-1 record is counted by the aggregate ...
  expect_identical(cat_rows$gidias_n_negative[cat_rows$affected_taxon == "Any"], 3L)
  # ... and by no per-taxon row, which between them see only 2 of the 3.
  expect_identical(
    sum(cat_rows$gidias_n_negative[cat_rows$affected_taxon != "Any"]), 2L
  )
})
