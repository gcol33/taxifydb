# parse_kew_sid reduces the per-record SER-SID tables to one row per species:
# numeric traits by median (seed weight filtered to strictly positive), storage
# behaviour by mode with qualifiers collapsed to the base class, and a positive
# seed-weight record count. Species with no trait at all are dropped.

make_sid <- function() {
  dir <- file.path(tempdir(), paste0("sid_", as.integer(stats::runif(1, 1, 1e9))))
  dir.create(dir, showWarnings = FALSE)
  saveRDS(data.frame(
    int_id  = c(1L, 2L, 3L, 4L),
    genus   = c("Quercus", "Zea", "Ghostus", "Nogenus"),
    epithet = c("robur", "mays", "absentia", ""),      # #4 has no epithet -> dropped
    lifeform = c("phan.", NA, "ther.", "phan."),
    stringsAsFactors = FALSE
  ), file.path(dir, "species.rds"))
  saveRDS(data.frame(
    species_id = c(1L, 1L, 1L, 2L, 2L),
    thousandseedweight = c(3000, 4000, 0, 200, 264.8),  # sp1 median(3000,4000)=3500 (0 dropped)
    stringsAsFactors = FALSE
  ), file.path(dir, "seed_weights.rds"))
  saveRDS(data.frame(
    species_id = c(1L, 1L, 2L),
    storage_behaviour = c("Recalcitrant", "Recalcitrant?", "Orthodox p"),
    stringsAsFactors = FALSE
  ), file.path(dir, "storage_behaviour.rds"))
  saveRDS(data.frame(
    species_id = c(2L, 2L),
    oil_content = c(5.0, 5.5),                           # median 5.25
    stringsAsFactors = FALSE
  ), file.path(dir, "oil_content.rds"))
  saveRDS(data.frame(species_id = integer(0), protein_content = numeric(0)),
          file.path(dir, "protein_content.rds"))
  saveRDS(data.frame(species_id = 3L, fruit_type = "capsule",
                     stringsAsFactors = FALSE),
          file.path(dir, "morphology.rds"))
  dir
}

test_that("parse_kew_sid reduces records to per-species medians and modes", {
  out <- parse_kew_sid(make_sid())

  # sp4 (no epithet) dropped; sp1/sp2 kept; sp3 kept via fruit_type only.
  expect_setequal(out$canonical_name,
                  c("Quercus robur", "Zea mays", "Ghostus absentia"))

  q <- out[out$canonical_name == "Quercus robur", ]
  expect_equal(q$thousand_seed_weight, 3500)            # 0 excluded from median
  expect_equal(q$n_seed_weight_records, 2L)             # only positive weights count
  expect_equal(q$storage_behaviour, "Recalcitrant")    # "?" qualifier collapsed

  z <- out[out$canonical_name == "Zea mays", ]
  expect_equal(z$thousand_seed_weight, 232.4)
  expect_equal(z$storage_behaviour, "Orthodox")        # "Orthodox p" -> "Orthodox"
  expect_equal(z$oil_content_pct, 5.25)
})

test_that("parse_kew_sid keeps trait-only species and zero-fills the weight count", {
  out <- parse_kew_sid(make_sid())

  g <- out[out$canonical_name == "Ghostus absentia", ]
  expect_equal(g$fruit_type, "capsule")
  expect_equal(g$n_seed_weight_records, 0L)             # survivor with no weights -> 0
  expect_true(is.na(g$thousand_seed_weight))
  # an entirely empty protein table contributes no column values (all NA).
  expect_true(all(is.na(out$protein_content_pct)))
})
