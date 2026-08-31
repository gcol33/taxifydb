# update_manifest() rewrites one backbone entry in place. What it must not do
# is take anything with it that it was not asked to change: the manifest is the
# only record of where a published artifact lives, so a field dropped here is
# an artifact the runtime stops downloading.

fake_vtr <- function(dir, name, rows = 3L) {
  path <- file.path(dir, paste0(name, ".vtr"))
  vectra::write_vtr(
    data.frame(canonical_name = paste0("Genus species", seq_len(rows)),
               taxon_id = as.character(seq_len(rows))),
    path)
  path
}

write_manifest <- function(path, entry) {
  jsonlite::write_json(
    list(schema_version = 2L,
         backends = stats::setNames(list(entry), "worms"),
         enrichment = list()),
    path, pretty = TRUE, auto_unbox = TRUE)
  path
}

test_that("a release without sidecars keeps the ones already recorded", {
  dir <- withr::local_tempdir()
  vtr <- fake_vtr(dir, "worms")

  # The recorded sidecar names an older tag than the release being written,
  # which is the normal case: it is published on its own cadence.
  extras <- list(list(
    name = "worms_species_profile.vtr",
    url = paste0("https://github.com/gcol33/taxifydb/releases/download/",
                 "worms-2026.05/worms_species_profile.vtr"),
    size = 78609521, sha256 = "ade818"))
  mf <- write_manifest(file.path(dir, "manifest.json"),
                       list(latest = "2026.07", extras = extras,
                            citation = "WoRMS Editorial Board"))

  update_manifest(mf, "worms", "2026.08", vtr, source_url = "https://example.org/x")
  got <- jsonlite::read_json(mf, simplifyVector = FALSE)$backends$worms

  expect_equal(got$latest, "2026.08")
  expect_length(got$extras, 1L)
  expect_equal(got$extras[[1L]]$name, "worms_species_profile.vtr")
  expect_match(got$extras[[1L]]$url, "worms-2026.05", fixed = TRUE)
  # Curated fields are preserved on the same principle.
  expect_equal(got$citation, "WoRMS Editorial Board")
})

test_that("a release carrying sidecars records them against its own tag", {
  dir <- withr::local_tempdir()
  vtr <- fake_vtr(dir, "worms")
  sidecar <- fake_vtr(dir, "worms_species_profile", rows = 2L)

  mf <- write_manifest(file.path(dir, "manifest.json"), list(latest = "2026.07"))
  update_manifest(mf, "worms", "2026.08", vtr, extras = sidecar,
                  source_url = "https://example.org/x")
  got <- jsonlite::read_json(mf, simplifyVector = FALSE)$backends$worms

  expect_length(got$extras, 1L)
  expect_match(got$extras[[1L]]$url, "worms-2026.08/worms_species_profile.vtr",
               fixed = TRUE)
  expect_equal(got$extras[[1L]]$size, file.size(sidecar))
})

test_that("an empty extras vector is how a sidecar block is removed", {
  dir <- withr::local_tempdir()
  vtr <- fake_vtr(dir, "worms")
  extras <- list(list(name = "worms_species_profile.vtr", url = "u",
                      size = 1L, sha256 = "a"))
  mf <- write_manifest(file.path(dir, "manifest.json"),
                       list(latest = "2026.07", extras = extras))

  update_manifest(mf, "worms", "2026.08", vtr, extras = character(0),
                  source_url = "https://example.org/x")
  got <- jsonlite::read_json(mf, simplifyVector = FALSE)$backends$worms

  expect_null(got$extras)
})

test_that("a content-addressed url is recorded for the built bytes", {
  # full_url is a moving pointer a re-cut overwrites in place; content_url names
  # the immutable copy keyed on content_id, so a recorded content_id resolves
  # back to bytes after the tag has moved on (taxifydb#47).
  dir <- withr::local_tempdir()
  vtr <- fake_vtr(dir, "worms")
  mf <- write_manifest(file.path(dir, "manifest.json"), list(latest = "2026.07"))

  update_manifest(mf, "worms", "2026.08", vtr, source_url = "https://example.org/x")
  got <- jsonlite::read_json(mf, simplifyVector = FALSE)$backends$worms

  cid <- unname(tools::md5sum(vtr))
  expect_equal(got$content_id, cid)
  expect_equal(got$content_url, sprintf(
    "https://github.com/gcol33/taxifydb/releases/download/worms-2026.08/worms-%s.vtr",
    cid))
})

test_that("a delta is dropped when the release has none, unlike a sidecar", {
  dir <- withr::local_tempdir()
  vtr <- fake_vtr(dir, "worms")
  # A delta URL names the release being written, so a stale one would 404.
  mf <- write_manifest(file.path(dir, "manifest.json"),
                       list(latest = "2026.07", delta_from = "2026.06",
                            delta_url = "https://example.org/old.xdelta",
                            delta_size = 10L))

  update_manifest(mf, "worms", "2026.08", vtr, source_url = "https://example.org/x")
  got <- jsonlite::read_json(mf, simplifyVector = FALSE)$backends$worms

  expect_null(got$delta_url)
  expect_null(got$delta_from)
  expect_null(got$delta_size)
})


# A runtime citation is curated text, often a structured block. It is rewritten
# from the build only when it no longer names the work being served.

write_enrichment_manifest <- function(path, entry) {
  jsonlite::write_json(
    list(schema_version = 2L, backends = list(),
         enrichments = stats::setNames(list(entry), "glonaf")),
    path, pretty = TRUE, auto_unbox = TRUE)
  path
}

enrichment_meta <- function(dir, source_url, source_doi) {
  jsonlite::write_json(
    list(name = "glonaf", version = "2.02", nrow = 3L,
         source_url = source_url, source_doi = source_doi,
         license = "CC BY 4.0",
         attribution = "van Kleunen M et al. (2019) ... Ecology 100:e02542."),
    file.path(dir, "meta.json"), auto_unbox = TRUE)
}

curated_citation <- list(
  key = "vankleunen2019glonaf", type = "article",
  authors = "van Kleunen M, Pysek P, Dawson W", year = "2019",
  title = "The Global Naturalized Alien Flora (GloNAF) database",
  journal = "Ecology", doi = "10.1002/ecy.2542"
)


# trait_cols names the columns the .vtr carries, so a source that renames its
# fields must not leave the runtime advertising the old ones.

enrichment_meta_cols <- function(dir, trait_cols, group_col = NULL) {
  jsonlite::write_json(
    c(list(name = "glonaf", version = "2.02", nrow = 3L,
           source_url = "https://zenodo.org/api/records/17105725",
           source_doi = "10.5281/zenodo.17105725",
           license = "CC BY 4.0", attribution = "van Kleunen M et al. (2019).",
           trait_cols = list(trait_cols)),
      if (!is.null(group_col)) list(group_col = group_col)),
    file.path(dir, "meta.json"), auto_unbox = TRUE)
}

test_that("a trait_cols naming only columns still built is kept, curation and all", {
  dir <- withr::local_tempdir()
  vtr <- fake_vtr(dir, "glonaf")
  enrichment_meta_cols(dir, c("alpha", "beta", "gamma"))
  # A curated subset, deliberately narrower and in its own order.
  mf <- write_enrichment_manifest(
    file.path(dir, "manifest.json"),
    list(latest = "2026.07", trait_cols = list("gamma", "alpha")))

  update_enrichment_manifest(mf, "glonaf", vtr, release_version = "2026.08",
                             runtime = TRUE)
  got <- jsonlite::read_json(mf, simplifyVector = FALSE)$enrichments$glonaf

  expect_equal(unlist(got$trait_cols), c("gamma", "alpha"))
})

test_that("a trait_cols naming a column no longer built is rewritten", {
  dir <- withr::local_tempdir()
  vtr <- fake_vtr(dir, "glonaf")
  enrichment_meta_cols(dir, c("alpha", "beta"))
  mf <- write_enrichment_manifest(
    file.path(dir, "manifest.json"),
    list(latest = "2026.07", trait_cols = list("alpha", "removed_by_upstream")))

  update_enrichment_manifest(mf, "glonaf", vtr, release_version = "2026.08",
                             runtime = TRUE)
  got <- jsonlite::read_json(mf, simplifyVector = FALSE)$enrichments$glonaf

  expect_equal(unlist(got$trait_cols), c("alpha", "beta"))
})

test_that("the group column keeps its place in a rewritten trait_cols", {
  dir <- withr::local_tempdir()
  vtr <- fake_vtr(dir, "glonaf")
  enrichment_meta_cols(dir, c("alpha", "beta"), group_col = "region_id")
  mf <- write_enrichment_manifest(
    file.path(dir, "manifest.json"),
    list(latest = "2026.07",
         trait_cols = list("region_id", "removed_by_upstream")))

  update_enrichment_manifest(mf, "glonaf", vtr, release_version = "2026.08",
                             runtime = TRUE)
  got <- jsonlite::read_json(mf, simplifyVector = FALSE)$enrichments$glonaf

  expect_equal(unlist(got$trait_cols), c("region_id", "alpha", "beta"))
})

test_that("an entry that never carried the group column does not gain one", {
  dir <- withr::local_tempdir()
  vtr <- fake_vtr(dir, "glonaf")
  enrichment_meta_cols(dir, c("alpha", "beta"), group_col = "region_id")
  mf <- write_enrichment_manifest(
    file.path(dir, "manifest.json"),
    list(latest = "2026.07", trait_cols = list("removed_by_upstream")))

  update_enrichment_manifest(mf, "glonaf", vtr, release_version = "2026.08",
                             runtime = TRUE)
  got <- jsonlite::read_json(mf, simplifyVector = FALSE)$enrichments$glonaf

  expect_equal(unlist(got$trait_cols), c("alpha", "beta"))
})

test_that("the group column alone does not count as a stale trait_cols", {
  # region_id is a real column of the built file, just not one meta lists as a
  # trait, so an entry naming it plus current traits must survive untouched.
  dir <- withr::local_tempdir()
  vtr <- fake_vtr(dir, "glonaf")
  enrichment_meta_cols(dir, c("alpha", "beta"), group_col = "region_id")
  mf <- write_enrichment_manifest(
    file.path(dir, "manifest.json"),
    list(latest = "2026.07", trait_cols = list("region_id", "beta")))

  update_enrichment_manifest(mf, "glonaf", vtr, release_version = "2026.08",
                             runtime = TRUE)
  got <- jsonlite::read_json(mf, simplifyVector = FALSE)$enrichments$glonaf

  expect_equal(unlist(got$trait_cols), c("region_id", "beta"))
})

test_that("a content-addressed url is recorded from the build's content_id", {
  dir <- withr::local_tempdir()
  vtr <- fake_vtr(dir, "glonaf")
  jsonlite::write_json(
    list(name = "glonaf", version = "2.02", nrow = 3L,
         content_id = "3e0017815cf679f4e7e28309f57700e5",
         source_url = "https://zenodo.org/api/records/17105725",
         source_doi = "10.5281/zenodo.17105725", license = "CC BY 4.0",
         attribution = "van Kleunen M et al. (2019)."),
    file.path(dir, "meta.json"), auto_unbox = TRUE)
  mf <- write_enrichment_manifest(file.path(dir, "manifest.json"),
                                  list(latest = "2026.07"))

  update_enrichment_manifest(mf, "glonaf", vtr, release_version = "2026.08")
  got <- jsonlite::read_json(mf, simplifyVector = FALSE)$enrichments$glonaf

  expect_equal(got$content_id, "3e0017815cf679f4e7e28309f57700e5")
  expect_equal(got$content_url, paste0(
    "https://github.com/gcol33/taxifydb/releases/download/",
    "enrichment-2026.08/glonaf-3e0017815cf679f4e7e28309f57700e5.vtr"))
})

test_that("a new deposit of the same work keeps the curated citation", {
  dir <- withr::local_tempdir()
  vtr <- fake_vtr(dir, "glonaf")
  enrichment_meta(dir, "https://zenodo.org/api/records/17105725", "10.1002/ecy.2542")
  mf <- write_enrichment_manifest(
    file.path(dir, "manifest.json"),
    list(latest = "2026.07", citation = curated_citation,
         source_url = "https://zenodo.org/api/records/13235357",
         source_doi = "10.1002/ecy.2542"))

  update_enrichment_manifest(mf, "glonaf", vtr, release_version = "2026.08",
                             runtime = TRUE)
  got <- jsonlite::read_json(mf, simplifyVector = FALSE)$enrichments$glonaf

  # the URL moved to the new record, the cited work did not
  expect_equal(got$source_url, "https://zenodo.org/api/records/17105725")
  expect_equal(got$citation$doi, "10.1002/ecy.2542")
  expect_equal(got$citation$journal, "Ecology")
  expect_equal(got$citation$key, "vankleunen2019glonaf")
})

test_that("a move to a different work rewrites the citation from the build", {
  dir <- withr::local_tempdir()
  vtr <- fake_vtr(dir, "glonaf")
  enrichment_meta(dir, "https://zenodo.org/api/records/17105725", "10.5281/zenodo.17105725")
  mf <- write_enrichment_manifest(
    file.path(dir, "manifest.json"),
    list(latest = "2026.07", citation = curated_citation,
         source_url = "https://zenodo.org/api/records/13235357",
         source_doi = "10.1002/ecy.2542"))

  update_enrichment_manifest(mf, "glonaf", vtr, release_version = "2026.08",
                             runtime = TRUE)
  got <- jsonlite::read_json(mf, simplifyVector = FALSE)$enrichments$glonaf

  expect_equal(got$source_doi, "10.5281/zenodo.17105725")
  expect_true(is.character(got$citation))
  expect_match(got$citation, "van Kleunen", fixed = TRUE)
})
