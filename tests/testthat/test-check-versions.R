# The weekly freshness check can only answer its question if both sides of the
# comparison speak the same language. The recorded `source_version` is this
# package's release string ("2020.1") and an upstream counter is whatever the
# host counts in ("6"), so comparing those reports every Figshare-sourced entry
# as outdated forever. What makes the comparison meaningful is the identity the
# host gave the version a build actually read, written down at build time.
#
# The probes themselves reach the network; everything below is the wiring around
# them, tested offline through the injectable `probe` argument.

test_that("a source host picks its probe, and api.gbif wins over gbif", {
  expect_equal(
    .upstream_probe_for("https://zenodo.org/api/records/16959762/files/x/content"),
    "check_zenodo_version")
  expect_equal(
    .upstream_probe_for("https://api.figshare.com/v2/file/download/12345"),
    "check_figshare_version")
  # api.gbif.org contains gbif.org, so the order of the table is load-bearing.
  expect_equal(
    .upstream_probe_for("https://api.gbif.org/v1/dataset/abc"),
    "check_gbif_api_version")
  expect_equal(
    .upstream_probe_for("https://hosted-datasets.gbif.org/eBird/x.zip"),
    "check_gbif_version")
  expect_null(.upstream_probe_for("https://example.org/traits.csv"))
  expect_null(.upstream_probe_for(NULL))
})

test_that("an unprobed host resolves to no identity at all", {
  expect_null(probe_upstream_identity("https://example.org/traits.csv"))
})

test_that("the recorded identity decides freshness", {
  entry <- list(source_url = "https://api.figshare.com/v2/file/download/1",
                source_version = "2020.1", upstream_id = "4")
  probe <- function(url) list(id = "6", version = "6", url = "u")

  res <- check_enrichment_source_version(entry, probe = probe)
  expect_true(res$outdated)
  expect_equal(res$built_id, "4")
  expect_equal(res$upstream_version, "6")

  entry$upstream_id <- "6"
  expect_false(check_enrichment_source_version(entry, probe = probe)$outdated)
})

test_that("a pinned source answers without a recorded identity", {
  # Zenodo names the record in the URL, so a build that predates upstream_id is
  # still checkable: the pinned record is the identity it read.
  entry <- list(source_url = "https://zenodo.org/api/records/14056760/files/x",
                source_version = "2024.1")
  probe <- function(url) list(id = "16959762", version = "2025-08-27",
                              pinned = "14056760", url = "u")

  res <- check_enrichment_source_version(entry, probe = probe)
  expect_true(res$outdated)
  expect_equal(res$built_id, "14056760")
})

test_that("an entry with nothing to compare reports unknown, not outdated", {
  entry <- list(source_url = "https://api.figshare.com/v2/file/download/1",
                source_version = "2020.1")
  probe <- function(url) list(id = "6", version = "6", url = "u")

  res <- check_enrichment_source_version(entry, probe = probe)
  expect_true(is.na(res$outdated))
  expect_match(res$note, "no upstream identity recorded")
  # The upstream side is still reported: it is the half that is known.
  expect_equal(res$upstream_version, "6")
})

test_that("a host with no probe is reported apart from one that failed", {
  # Two different jobs: the first needs a probe written, the second needs the
  # recorded URL looked at (a Figshare download id the API no longer knows).
  uncovered <- list(source_url = "https://example.org/traits.csv",
                    source_version = "2020.1", upstream_id = "4")
  res <- check_enrichment_source_version(uncovered, probe = function(url) NULL)
  expect_true(is.na(res$outdated))
  expect_match(res$note, "no version check for source host")
  expect_equal(res$built_id, "4")

  unreadable <- list(source_url = "https://ndownloader.figshare.com/files/8828578",
                     source_version = "1.0")
  res <- check_enrichment_source_version(unreadable, probe = function(url) NULL)
  expect_true(is.na(res$outdated))
  expect_match(res$note, "could not be read")
})

test_that("a static entry is skipped without reaching the network", {
  entry <- list(source_url = "https://zenodo.org/api/records/1/files/x",
                source_version = "2020.1", static = TRUE)
  probe <- function(url) stop("the network must not be reached here")

  res <- check_enrichment_source_version(entry, probe = probe)
  expect_false(res$outdated)
  expect_equal(res$note, "static dataset, skipped")
})

test_that("the built identity travels from meta.json into the manifest entry", {
  dir <- withr::local_tempdir()
  vtr <- file.path(dir, "thermofresh.vtr")

  build_enrichment_vtr(
    data.frame(canonical_name = c("Salmo trutta", "Perca fluviatilis"),
               ctmax = c(26.5, 33.1)),
    vtr, name = "thermofresh", version = "2025.1",
    source_url = "https://zenodo.org/api/records/16959762/files/x/content",
    upstream_id = "16959762", license = "CC BY 4.0")

  meta <- jsonlite::read_json(file.path(dir, "meta.json"),
                              simplifyVector = TRUE)
  expect_equal(meta$upstream_id, "16959762")

  mf <- file.path(dir, "manifest.json")
  jsonlite::write_json(list(schema_version = 2L, backends = list(),
                            enrichments = list()),
                       mf, pretty = TRUE, auto_unbox = TRUE)
  update_enrichment_manifest(mf, "thermofresh", vtr, release_version = "2026.08")

  entry <- jsonlite::read_json(mf, simplifyVector = FALSE)$enrichments$thermofresh
  expect_equal(entry$upstream_id, "16959762")
})

test_that("a runtime citation is rewritten when the build changes source", {
  dir <- withr::local_tempdir()
  vtr <- file.path(dir, "thermofresh.vtr")
  build_enrichment_vtr(
    data.frame(canonical_name = "Salmo trutta", ctmax = 26.5),
    vtr, name = "thermofresh", version = "2025.1",
    source_url = "https://zenodo.org/api/records/16959762/files/x/content",
    source_doi = "10.5281/zenodo.16959762", upstream_id = "16959762",
    license = "CC BY 4.0", attribution = "ThermoFresh v1.0, record 16959762.")

  mf <- file.path(dir, "manifest.json")
  jsonlite::write_json(
    list(schema_version = 2L, backends = list(),
         enrichments = list(thermofresh = list(
           source_url = "https://zenodo.org/api/records/14056760/files/x/content",
           source_doi = "10.5281/zenodo.14056760",
           species_col = "species",
           citation = "Freshwater thermal-tolerance database, record 14056760."))),
    mf, pretty = TRUE, auto_unbox = TRUE)

  update_enrichment_manifest(mf, "thermofresh", vtr, release_version = "2026.08",
                             runtime = TRUE)
  entry <- jsonlite::read_json(mf, simplifyVector = FALSE)$enrichments$thermofresh

  expect_equal(entry$citation, "ThermoFresh v1.0, record 16959762.")
  # Curation unrelated to the source is still preserved.
  expect_equal(entry$species_col, "species")
})

test_that("a curated citation survives a release that keeps its source", {
  dir <- withr::local_tempdir()
  vtr <- file.path(dir, "woodiness.vtr")
  build_enrichment_vtr(
    data.frame(canonical_name = "Quercus robur", woody = TRUE),
    vtr, name = "woodiness", version = "2026.08",
    source_url = "https://example.org/woodiness.csv", license = "CC0",
    attribution = "Generated attribution.")

  mf <- file.path(dir, "manifest.json")
  jsonlite::write_json(
    list(schema_version = 2L, backends = list(),
         enrichments = list(woodiness = list(
           source_url = "https://example.org/woodiness.csv",
           citation = "Zanne et al. (2014), curated by hand."))),
    mf, pretty = TRUE, auto_unbox = TRUE)

  update_enrichment_manifest(mf, "woodiness", vtr, release_version = "2026.08",
                             runtime = TRUE)
  entry <- jsonlite::read_json(mf, simplifyVector = FALSE)$enrichments$woodiness

  expect_equal(entry$citation, "Zanne et al. (2014), curated by hand.")
})

test_that("a build with no upstream identity writes no field to carry", {
  dir <- withr::local_tempdir()
  vtr <- file.path(dir, "local.vtr")

  build_enrichment_vtr(
    data.frame(canonical_name = "Salmo trutta", ctmax = 26.5),
    vtr, name = "local", version = "2026.08",
    source_url = "https://example.org/traits.csv", license = "CC0")

  meta <- jsonlite::read_json(file.path(dir, "meta.json"),
                              simplifyVector = FALSE)
  expect_false("upstream_id" %in% names(meta))
})
