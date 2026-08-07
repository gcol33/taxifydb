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
