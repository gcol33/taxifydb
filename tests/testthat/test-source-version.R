# A backbone's version is the source release it was built from. It becomes the
# release tag, the manifest's `latest` and what taxify_lock() stamps, so a value
# naming the build month or an alias (GBIF's `current`) records a taxonomy date
# that is not the taxonomy's (#53, #54).

test_that("release dates normalize to the YYYY.MM version form", {
  f <- taxifydb:::release_version_from_date
  expect_equal(f("2026-06"), "2026.06")
  expect_equal(f("2026-09-11"), "2026.09")
  expect_equal(f("2026-08-26 XR"), "2026.08")
  expect_equal(f("2021"), "2021")
  expect_equal(f(as.Date("2024-12-21")), "2024.12")
  expect_equal(f(as.POSIXct("2023-08-28 13:58:40", tz = "UTC")), "2023.08")
  expect_error(f("current"), "Cannot read a release date")
  expect_error(f("Apr 2024"), "Cannot read a release date")
  expect_error(f(""), "Cannot read a release date")
})

test_that("only a release identifier can become a version", {
  f <- taxifydb:::check_release_version
  for (ok in c("2026.06", "3.7.3", "2025b", "2.5", "2021")) {
    expect_equal(f(ok, "demo"), ok)
  }
  for (bad in c("current", "2024-12", "2026-08-26 XR", "", "v3.0.1")) {
    expect_error(f(bad, "demo"), "cannot name a release")
  }
  expect_error(f(NA_character_, "demo"), "cannot name a release")
  expect_error(f(c("2026.06", "2026.07"), "demo"), "cannot name a release")
})

test_that("a build cannot write a meta whose version names no release", {
  dir <- withr::local_tempdir()
  vtr <- file.path(dir, "gbif.vtr")
  expect_error(
    taxifydb:::write_backbone_meta(vtr, "gbif", "current", "https://x.org", 1L),
    "gbif: version 'current' cannot name a release")
  expect_false(file.exists(file.path(dir, "gbif.meta")))

  taxifydb:::write_backbone_meta(vtr, "gbif", "2023.08", "https://x.org", 1L)
  expect_equal(unname(taxifydb:::read_meta(file.path(dir, "gbif.meta"))[["version"]]),
               "2023.08")
})

test_that("pinned backbones derive URL and version from one release constant", {
  expect_match(taxifydb:::.gbif_url, taxifydb:::.gbif_release, fixed = TRUE)
  expect_false(grepl("/current/", taxifydb:::.gbif_url, fixed = TRUE))
  expect_match(taxifydb:::.col_url, taxifydb:::.col_release, fixed = TRUE)
  expect_match(taxifydb:::.lcvp_url, taxifydb:::.lcvp_release, fixed = TRUE)
  expect_match(taxifydb:::.avilist_url, taxifydb:::.avilist_release, fixed = TRUE)
  expect_match(taxifydb:::.meow_url, taxifydb:::.meow_snapshot_version, fixed = TRUE)
  expect_match(taxifydb:::.euromed_snapshot_release,
               taxifydb:::.euromed_snapshot_version, fixed = TRUE)
  expect_match(taxifydb:::.wgsrpd_url, taxifydb:::.wgsrpd_commit, fixed = TRUE)
  expect_match(taxifydb:::.reptiledb_checklist_url,
               sub("-", "_", taxifydb:::.reptiledb_checklist_release, fixed = TRUE),
               fixed = TRUE)
})

test_that("the MDD version is read from the archive's species file", {
  dir <- withr::local_tempdir()
  inner <- file.path(dir, "MDD")
  dir.create(inner)
  file.create(file.path(inner, "MDD_v2.6_6900species.csv"))
  file.create(file.path(inner, "Species_Syn_Current_v2.6.csv"))
  dir.create(file.path(dir, "__MACOSX"))
  file.create(file.path(dir, "__MACOSX", "MDD_v9.9_1species.csv"))
  expect_equal(taxifydb:::mdd_archive_version(dir), "2.6")

  empty <- withr::local_tempdir()
  expect_error(taxifydb:::mdd_archive_version(empty), "cannot read the release")
})

test_that("the WFO edition, URL and version come from one Zenodo record", {
  skip_on_cran()
  skip_if_offline("zenodo.org")
  ed <- wfo_latest_edition(verbose = FALSE)
  expect_match(ed$edition, "^[0-9]{4}-[0-9]{2}$")
  expect_equal(ed$version, sub("-", ".", ed$edition, fixed = TRUE))
  expect_equal(ed$url, sprintf("https://zenodo.org/records/%s/files/_DwC_backbone_R.zip",
                               ed$record))
  expect_gte(as.numeric(ed$record), 20782718)
})

test_that("a ChecklistBank dataset is versioned by the release it serves", {
  skip_on_cran()
  skip_if_offline("api.checklistbank.org")
  rel <- taxifydb:::checklistbank_release(taxifydb:::.fungorum_dataset_key)
  expect_match(rel$issued, "^[0-9]{4}")
  expect_equal(rel$version, taxifydb:::release_version_from_date(rel$issued))
})
