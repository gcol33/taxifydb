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

test_that("release dates normalize to the source_date form", {
  f <- taxifydb:::source_date_from
  expect_equal(f("2026-09-11"), "2026-09-11")
  expect_equal(f("2026-08-26 XR"), "2026-08-26")
  expect_equal(f("2026-06-21T07:38:42Z"), "2026-06-21")
  expect_equal(f("20251220"), "2025-12-20")
  expect_equal(f("2026-06"), "2026-06")
  expect_equal(f("2021"), "2021")
  expect_equal(f(as.Date("2024-04-28")), "2024-04-28")
  expect_equal(f(as.POSIXct("2026-08-27 21:32:18", tz = "UTC")), "2026-08-27")
  expect_error(f("current"), "Cannot read a source date")
  expect_error(f("Apr 2024"), "Cannot read a source date")
  expect_error(f(""), "Cannot read a source date")
})

test_that("one release date gives both the version and the source_date", {
  expect_equal(taxifydb:::release_from_date("2026-09-11"),
               list(version = "2026.09", date = "2026-09-11"))
  expect_equal(taxifydb:::release_from_date("2021"),
               list(version = "2021", date = "2021"))
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

test_that("the meta records source_date, and only in the source_date form", {
  dir <- withr::local_tempdir()
  vtr <- file.path(dir, "gbif.vtr")
  taxifydb:::write_backbone_meta(vtr, "gbif", "2023.08", "https://x.org", 1L,
                                 source_date = "2023-08-28")
  expect_equal(unname(taxifydb:::read_meta(file.path(dir, "gbif.meta"))[["source_date"]]),
               "2023-08-28")

  for (bad in c("2023.08", "28 Aug 2023", "2023-08-28 13:58")) {
    expect_error(
      taxifydb:::write_backbone_meta(vtr, "gbif", "2023.08", "https://x.org", 1L,
                                     source_date = bad),
      "is not a date in the form")
  }

  reg <- file.path(dir, "genus_register.vtr")
  taxifydb:::write_backbone_meta(reg, "genus_register", "2026.08", "derived", 1L)
  expect_false("source_date" %in%
                 names(taxifydb:::read_meta(file.path(dir, "genus_register.meta"))))
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

test_that("the MDD source_date is read from the archive's release.toml", {
  dir <- withr::local_tempdir()
  inner <- file.path(dir, "MDD")
  dir.create(inner)
  writeLines(c("[metadata]", 'version = "v2.5"', 'release_date = "2026-07-28"'),
             file.path(inner, "release.toml"))
  dir.create(file.path(dir, "__MACOSX", "MDD"), recursive = TRUE)
  writeLines('release_date = "1999-01-01"',
             file.path(dir, "__MACOSX", "MDD", "release.toml"))
  expect_equal(taxifydb:::mdd_archive_release_date(dir), "2026-07-28")

  empty <- withr::local_tempdir()
  expect_error(taxifydb:::mdd_archive_release_date(empty), "cannot read release_date")
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
  expect_match(ed$date, "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")
})

test_that("a ChecklistBank dataset is versioned by the release it serves", {
  skip_on_cran()
  skip_if_offline("api.checklistbank.org")
  rel <- taxifydb:::checklistbank_release(taxifydb:::.fungorum_dataset_key)
  expect_match(rel$issued, "^[0-9]{4}")
  expect_equal(rel$version, taxifydb:::release_version_from_date(rel$issued))
  expect_equal(rel$date, taxifydb:::source_date_from(rel$issued))
})

test_that("the pinned OTT archive is dated by its properties.json", {
  skip_on_cran()
  skip_if_offline("files.opentreeoflife.org")
  expect_equal(taxifydb:::ott_release_date(), "2025-12-20")
})
