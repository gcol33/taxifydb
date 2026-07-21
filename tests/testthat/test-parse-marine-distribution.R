# parse_marine_distribution rolls WoRMS MRGID distributions up to MEOW
# ecoregions: drops absences and doubtful records, explodes coarse MRGIDs to
# every ecoregion they cover, and lets a native record win over introduced for
# the same (species, ecoregion).

marine_fixture <- function(extra_recs = list()) {
  dir <- tempfile("marine_")
  dir.create(dir)

  recs <- list(
    # Native in the North Sea (mrgid 2350 -> ecoregion 20164)
    list(aphia_id = "107451", canonical_name = "Carcinus maenas", mrgid = "2350",
         locality = "North Sea", establishment_means = "Native",
         invasiveness = "None", occurrence = "Established", record_status = "valid"),
    # Alien in a basin (mrgid 9999 -> ecoregions 20164 + 20051)
    list(aphia_id = "107451", canonical_name = "Carcinus maenas", mrgid = "9999",
         locality = "Some basin", establishment_means = "Alien",
         invasiveness = "Invasive", occurrence = "Established", record_status = "valid"),
    # A recorded ABSENCE must be dropped, not counted as a range
    list(aphia_id = "107451", canonical_name = "Carcinus maenas", mrgid = "20051x",
         locality = "Elsewhere", establishment_means = "Alien",
         invasiveness = "None", occurrence = "Absent", record_status = "valid"),
    # A DOUBTFUL record must be dropped
    list(aphia_id = "140416", canonical_name = "Rapana venosa", mrgid = "2350",
         locality = "North Sea", establishment_means = "Alien",
         invasiveness = "Invasive", occurrence = "Reported", record_status = "doubtful"),
    # A clean introduced record for a second species
    list(aphia_id = "140416", canonical_name = "Rapana venosa", mrgid = "7777",
         locality = "Bay", establishment_means = "Alien",
         invasiveness = "Invasive", occurrence = "Established", record_status = "valid")
  )
  recs <- c(recs, extra_recs)
  jl <- file.path(dir, "worms_distributions.jsonl")
  writeLines(vapply(recs, function(r) jsonlite::toJSON(r, auto_unbox = TRUE),
                    character(1L)), jl)

  xw <- data.frame(
    mrgid     = c("2350", "9999", "9999", "7777"),
    eco_code  = c("20164", "20164", "20051", "20051"),
    ecoregion = c("North Sea", "North Sea", "Azores Canaries Madeira",
                  "Azores Canaries Madeira"),
    province  = c("Northern European Seas", "Northern European Seas",
                  "Lusitanian", "Lusitanian"),
    realm     = c("Temperate Northern Atlantic", "Temperate Northern Atlantic",
                  "Temperate Northern Atlantic", "Temperate Northern Atlantic"),
    stringsAsFactors = FALSE
  )
  utils::write.table(xw, file.path(dir, "mrgid_meow.tsv"), sep = "\t",
                     quote = FALSE, row.names = FALSE)
  dir
}

test_that("parse_marine_distribution emits the WCVP-analogue range shape", {
  dir <- marine_fixture()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  out <- parse_marine_distribution(dir)

  expect_setequal(names(out), c("canonical_name", "region_code", "ecoregion",
                                "province", "realm", "native_status"))
  expect_true(all(out$region_code %in% c("20164", "20051")))
})

test_that("a coarse MRGID explodes to every ecoregion it covers", {
  dir <- marine_fixture()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  out <- parse_marine_distribution(dir)
  cm <- out[out$canonical_name == "Carcinus maenas", ]
  # native North Sea record + basin record (20164 & 20051) -> both ecoregions
  expect_setequal(cm$region_code, c("20164", "20051"))
})

test_that("a native record wins over an introduced one for the same ecoregion", {
  dir <- marine_fixture()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  out <- parse_marine_distribution(dir)
  ns <- out[out$canonical_name == "Carcinus maenas" & out$region_code == "20164", ]
  expect_equal(nrow(ns), 1L)
  # 20164 has both a Native (North Sea) and an Alien (basin) record -> native
  expect_equal(ns$native_status, "native")
  # the basin-only ecoregion is introduced
  az <- out[out$canonical_name == "Carcinus maenas" & out$region_code == "20051", ]
  expect_equal(az$native_status, "introduced")
})

test_that("absences and doubtful records are dropped", {
  dir <- marine_fixture()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  out <- parse_marine_distribution(dir)
  # Rapana venosa's only surviving record is the clean mrgid 7777 (-> 20051);
  # its North Sea record was doubtful and must be gone.
  rv <- out[out$canonical_name == "Rapana venosa", ]
  expect_equal(rv$region_code, "20051")
  expect_equal(rv$native_status, "introduced")
  # the Absent record (mrgid 20051x, not in the crosswalk anyway) contributes nothing
  expect_false(any(out$canonical_name == "Carcinus maenas" &
                     out$region_code == "20051" & out$native_status == "native"))
})

test_that("every WoRMS Native variant counts as native", {
  # WoRMS qualifies nativeness where it can, so "Native" is a minority of the
  # native signal: the snapshot carries 16663 "Native - Endemic" and 4375
  # "Native - Non-endemic" records alongside 113140 bare "Native" ones. Matching
  # "Native" exactly scored all of those "unknown" and emptied range_mode =
  # "native" of a sixth of its evidence.
  dir <- marine_fixture(list(
    list(aphia_id = "1", canonical_name = "Mytilus edulis", mrgid = "2350",
         locality = "North Sea", establishment_means = "Native - Non-endemic",
         invasiveness = "None", occurrence = "Established",
         record_status = "valid"),
    list(aphia_id = "2", canonical_name = "Patella candei", mrgid = "7777",
         locality = "Bay", establishment_means = "Native - Endemic",
         invasiveness = "None", occurrence = "Established",
         record_status = "valid")
  ))
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  out <- parse_marine_distribution(dir)
  expect_equal(out$native_status[out$canonical_name == "Mytilus edulis"], "native")
  expect_equal(out$native_status[out$canonical_name == "Patella candei"], "native")
})

test_that("origin-unknown records stay unknown rather than becoming native", {
  # An unscored record carries no establishmentMeans at all, which is 77% of the
  # snapshot, so the unknown default has to survive a missing field as well as
  # the two "Origin ..." spellings.
  dir <- marine_fixture(list(
    list(aphia_id = "3", canonical_name = "Sabella spallanzanii", mrgid = "2350",
         locality = "North Sea", establishment_means = "Origin unknown",
         invasiveness = "None", occurrence = "Established",
         record_status = "valid"),
    list(aphia_id = "4", canonical_name = "Bugula neritina", mrgid = "7777",
         locality = "Bay", establishment_means = "Origin uncertain",
         invasiveness = "None", occurrence = "Established",
         record_status = "valid"),
    list(aphia_id = "12", canonical_name = "Ciona intestinalis", mrgid = "2350",
         locality = "North Sea", occurrence = "Established",
         record_status = "valid")
  ))
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  out <- parse_marine_distribution(dir)
  expect_equal(out$native_status[out$canonical_name == "Sabella spallanzanii"],
               "unknown")
  expect_equal(out$native_status[out$canonical_name == "Bugula neritina"],
               "unknown")
  expect_equal(out$native_status[out$canonical_name == "Ciona intestinalis"],
               "unknown")
})

test_that("non-presence occurrences and inaccurate records are dropped", {
  # An absence is not the only way WoRMS says "not here": a retracted record, a
  # population that is gone, and one held only under human care are all
  # non-presence, and an "inaccurate" record is no more trustworthy than a
  # doubtful one. Each species below has exactly one record, so surviving the
  # filter would put it in the output.
  dir <- marine_fixture(list(
    list(aphia_id = "5", canonical_name = "Anguilla anguilla", mrgid = "2350",
         locality = "North Sea", establishment_means = "Native",
         invasiveness = "None", occurrence = "Recorded in error",
         record_status = "valid"),
    list(aphia_id = "6", canonical_name = "Ostrea edulis", mrgid = "2350",
         locality = "North Sea", establishment_means = "Native",
         invasiveness = "None", occurrence = "Extirpated",
         record_status = "valid"),
    list(aphia_id = "7", canonical_name = "Amphiprion ocellaris", mrgid = "2350",
         locality = "North Sea", establishment_means = "Alien",
         invasiveness = "None", occurrence = "In captivity/cultivated",
         record_status = "valid"),
    list(aphia_id = "8", canonical_name = "Crassostrea gigas", mrgid = "2350",
         locality = "North Sea", establishment_means = "Alien",
         invasiveness = "Invasive", occurrence = "Eradicated",
         record_status = "valid"),
    list(aphia_id = "9", canonical_name = "Homarus gammarus", mrgid = "2350",
         locality = "North Sea", establishment_means = "Native",
         invasiveness = "None", occurrence = "Established",
         record_status = "inaccurate")
  ))
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  out <- parse_marine_distribution(dir)
  dropped <- c("Anguilla anguilla", "Ostrea edulis", "Amphiprion ocellaris",
               "Crassostrea gigas", "Homarus gammarus")
  expect_false(any(out$canonical_name %in% dropped))
})

test_that("a borderline occurrence still counts as presence", {
  # The filter is a soft disambiguation aid, so an uncertain or intermittent
  # record widens a species' range rather than narrowing it.
  dir <- marine_fixture(list(
    list(aphia_id = "10", canonical_name = "Sepia officinalis", mrgid = "2350",
         locality = "North Sea", establishment_means = "Native",
         invasiveness = "None", occurrence = "Uncertain",
         record_status = "valid"),
    list(aphia_id = "11", canonical_name = "Solea solea", mrgid = "7777",
         locality = "Bay", establishment_means = "Native",
         invasiveness = "None", occurrence = "Sometimes present",
         record_status = "valid")
  ))
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  out <- parse_marine_distribution(dir)
  expect_equal(out$region_code[out$canonical_name == "Sepia officinalis"], "20164")
  expect_equal(out$region_code[out$canonical_name == "Solea solea"], "20051")
})
