# The build-only catalog records candidate trait sources whose live licence
# forbids redistribution (freshwaterecology.info, NEMAPLEX). taxifydb must build
# no .vtr and place nothing in the enrichment build registry or the manifest for
# them, so the key invariants are: the licence-blocked names are catalogued,
# none of them leaks into list_enrichments(), and every entry carries the fields
# (licence, reason, attribution, issue) that justify the exclusion.

test_that("list_build_only_enrichments() catalogs the licence-blocked sources", {
  bo <- list_build_only_enrichments()

  expect_s3_class(bo, "data.frame")
  expect_true(all(c(
    "name", "source", "organism_groups", "candidate_trait", "source_url",
    "license", "access", "reason", "attribution", "issue"
  ) %in% names(bo)))

  expect_true(all(c("freshwaterecology", "nemaplex") %in% bo$name))
})

test_that("BETSI is no longer a refuse-to-redistribute source", {
  # BETSI was reclassified out of build-only (#42): the decision is to serve it
  # as informed-risk redistribution, so it must NOT be catalogued here as a
  # refuse-to-redistribute source. It is not in list_enrichments() yet either --
  # that build is blocked on data acquisition, tracked in #42.
  expect_false("betsi" %in% list_build_only_enrichments()$name)
})

test_that("build-only sources are never in the build registry or manifest", {
  bo <- list_build_only_enrichments()

  # No build-only source may also be a buildable enrichment: that is the whole
  # point -- taxifydb writes no .vtr for them.
  expect_length(intersect(bo$name, list_enrichments()), 0L)
})

test_that("every build-only entry carries a licence, reason, and issue", {
  bo <- list_build_only_enrichments()

  expect_true(all(nzchar(bo$license)))
  expect_true(all(nzchar(bo$reason)))
  expect_true(all(nzchar(bo$attribution)))
  expect_type(bo$issue, "integer")
  expect_true(all(bo$issue > 0L))
})

test_that("earthworms (sWorm) stayed a buildable enrichment, not build-only", {
  # sWorm is CC BY 4.0, so issue #31's earthworm layer is a real build, not a
  # catalog entry -- guard against it being demoted into the build-only list.
  expect_true("sworm" %in% list_enrichments())
  expect_false("sworm" %in% list_build_only_enrichments()$name)
})
