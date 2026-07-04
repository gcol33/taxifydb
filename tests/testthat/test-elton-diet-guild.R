# .elton_diet_guild derives one guild per species from the ten EltonTraits diet
# fractions: fractions are summed within guild (the four vertebrate/fish columns
# are all carnivory), the dominant guild wins at >=50%, else omnivore. The label
# agrees 93% with EltonTraits' own diet_5cat and 83% with AVONET on real data.

test_that(".elton_diet_guild sums within guild then takes the dominant one", {
  z <- function(...) { v <- c(...); v }
  out <- data.frame(
    canonical_name  = c("obligate_carn", "split_carn", "seed_eater",
                        "mixed_omni", "frugivore", "no_data"),
    diet_inv        = c(0,  0,  10, 40, 0,  0),
    diet_vend       = c(90, 30, 0,  0,  0,  0),
    diet_vect       = c(0,  0,  0,  0,  0,  0),
    diet_vfish      = c(0,  30, 0,  0,  0,  0),
    diet_vunk       = c(0,  10, 0,  0,  0,  0),
    diet_scav       = c(10, 0,  0,  0,  0,  0),
    diet_fruit      = c(0,  0,  0,  30, 100,0),
    diet_nect       = c(0,  0,  0,  0,  0,  0),
    diet_seed       = c(0,  0,  90, 20, 0,  0),
    diet_plantother = c(0,  0,  0,  10, 0,  0),
    stringsAsFactors = FALSE
  )
  out$diet_guild <- .elton_diet_guild(out)

  # 90% endotherm verts -> carnivore.
  expect_equal(out$diet_guild[out$canonical_name == "obligate_carn"], "carnivore")
  # 30 endo + 30 fish + 10 unk = 70% carnivory summed across four columns, even
  # though no single column reaches 50 -- must be carnivore, not omnivore.
  expect_equal(out$diet_guild[out$canonical_name == "split_carn"], "carnivore")
  # 90% seed -> granivore.
  expect_equal(out$diet_guild[out$canonical_name == "seed_eater"], "granivore")
  # no guild reaches 50% -> omnivore.
  expect_equal(out$diet_guild[out$canonical_name == "mixed_omni"], "omnivore")
  # 100% fruit -> frugivore.
  expect_equal(out$diet_guild[out$canonical_name == "frugivore"], "frugivore")
  # all-zero row -> NA, not a guild.
  expect_true(is.na(out$diet_guild[out$canonical_name == "no_data"]))
})

test_that(".elton_diet_guild returns all-NA when no diet columns are present", {
  out <- data.frame(canonical_name = c("a", "b"), body_mass_g = c(1, 2),
                    stringsAsFactors = FALSE)
  expect_true(all(is.na(.elton_diet_guild(out))))
})
