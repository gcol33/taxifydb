test_that("a genus reassignment survives the kingdom gate", {
  # Lasiurus cinereus and Aeorestes cinereus are one bat under two genera.
  # Unioning both is the whole point of cross-backbone resolution: the
  # enrichment has to join whichever name the user's taxify() returned.
  raw <- data.frame(
    key_ci        = rep("lasiurus cinereus", 3L),
    accepted_name = c("Lasiurus cinereus", "Aeorestes cinereus",
                      "Lasiurus cinereus"),
    kingdom       = rep("Animalia", 3L),
    stringsAsFactors = FALSE
  )
  out <- taxifydb:::.drop_cross_kingdom_names(raw, verbose = FALSE)
  expect_setequal(out$accepted_name,
                  c("Lasiurus cinereus", "Aeorestes cinereus"))
  expect_false("kingdom" %in% names(out))
})

test_that("a cross-kingdom homonym is dropped", {
  # Coronella austriaca is the smooth snake in six backbones and a fossil
  # foraminiferan in WoRMS, which files the binomial under Coronipora
  # austriaca. Unioning that writes the snake's traits onto a chromist.
  raw <- data.frame(
    key_ci        = rep("coronella austriaca", 7L),
    accepted_name = c(rep("Coronella austriaca", 6L), "Coronipora austriaca"),
    kingdom       = c(rep("Animalia", 6L), "Chromista"),
    stringsAsFactors = FALSE
  )
  out <- taxifydb:::.drop_cross_kingdom_names(raw, verbose = FALSE)
  expect_equal(unique(out$accepted_name), "Coronella austriaca")
})

test_that("a backbone recording no kingdom never contradicts", {
  # The vascular-plant backbones carry no kingdom column. An absent kingdom
  # must not be read as disagreement, or their mappings would all be dropped.
  raw <- data.frame(
    key_ci        = rep("quercus robur", 3L),
    accepted_name = c("Quercus robur", "Quercus robur", "Quercus pedunculata"),
    kingdom       = c("Plantae", NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  out <- taxifydb:::.drop_cross_kingdom_names(raw, verbose = FALSE)
  expect_setequal(out$accepted_name,
                  c("Quercus robur", "Quercus pedunculata"))
})

test_that("the gate needs a majority, not a single dissenting backbone", {
  # One backbone against one is not a contradiction anyone can adjudicate, so
  # ties keep the incoming order and nothing is dropped beyond the duplicate.
  raw <- data.frame(
    key_ci        = rep("ambiguous name", 2L),
    accepted_name = c("Animal version", "Plant version"),
    kingdom       = c("Animalia", "Plantae"),
    stringsAsFactors = FALSE
  )
  out <- taxifydb:::.drop_cross_kingdom_names(raw, verbose = FALSE)
  expect_equal(nrow(out), 1L)
})

test_that("rows with no kingdom information at all pass through untouched", {
  raw <- data.frame(
    key_ci        = c("a a", "b b"),
    accepted_name = c("A a", "B b"),
    kingdom       = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  out <- taxifydb:::.drop_cross_kingdom_names(raw, verbose = FALSE)
  expect_equal(nrow(out), 2L)
  expect_false("kingdom" %in% names(out))
})
