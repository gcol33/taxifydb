# The widening primitives guarantee no source columns are dropped at build
# time: wide parsers append every un-consumed column, long parsers pivot every
# distinct trait, both on top of byte-identical curated columns, with pure
# bookkeeping (references/URLs/IDs/raw-name) excluded.

test_that(".append_all_cols adds extra columns, keeps curated, drops bookkeeping", {
  df <- data.frame(
    class             = c("Aves", "Mammalia"),
    order             = c("Passeriformes", "Rodentia"),
    genus             = c("Corvus", "Mus"),
    species           = c("corax", "musculus"),
    adult_body_mass_g = c(1200, 20),
    female_svl_cm     = c(-999, 9.5),
    activity_cycle    = c("diurnal", "nocturnal"),
    record_id         = c(101, 202),
    reference         = c("Smith 2001", "Jones 1999"),
    source_url        = c("http://x", "http://y"),
    stringsAsFactors  = FALSE
  )
  df[df == -999] <- NA
  cname <- trimws(paste(df$genus, df$species))
  out <- data.frame(
    canonical_name    = cname,
    adult_body_mass_g = df$adult_body_mass_g,
    stringsAsFactors  = FALSE
  )
  out <- .append_all_cols(out, df, cname,
                          used = c("genus", "species", "class", "adult_body_mass_g"))

  # curated preserved, extra data + taxonomy appended
  expect_true(all(c("canonical_name", "adult_body_mass_g",
                    "order", "female_svl_cm", "activity_cycle") %in% names(out)))
  # bookkeeping + name-building + used columns dropped
  expect_false(any(c("record_id", "reference", "source_url",
                     "genus", "species", "class") %in% names(out)))
  # sentinel handled, values aligned by species
  expect_true(is.na(out$female_svl_cm[out$canonical_name == "Corvus corax"]))
  expect_equal(out$activity_cycle[out$canonical_name == "Mus musculus"], "nocturnal")
})

test_that(".append_all_cols aggregates numeric by median, character by mode per species", {
  df <- data.frame(
    sp    = c("Aus bus", "Aus bus", "Aus bus"),
    trait = c(1, 3, 100),
    cat   = c("x", "x", "y"),
    stringsAsFactors = FALSE
  )
  out <- data.frame(canonical_name = "Aus bus", stringsAsFactors = FALSE)
  out <- .append_all_cols(out, df, df$sp, used = "sp")
  expect_equal(out$trait, 3)          # median(1,3,100)
  expect_equal(out$cat, "x")          # mode
})

test_that(".append_all_cols group path keys extra columns on (name, group)", {
  df <- data.frame(
    sp      = c("Aus bus", "Aus bus"),
    country = c("DE", "FR"),
    status  = c("invasive", "native"),
    habitat = c("forest", "grassland"),
    stringsAsFactors = FALSE
  )
  out <- data.frame(
    canonical_name = c("Aus bus", "Aus bus"),
    country_code   = c("DE", "FR"),
    stringsAsFactors = FALSE
  )
  out <- .append_all_cols(out, df, df$sp, group = "country_code",
                          group_row = df$country, used = c("sp", "country"))
  expect_true("habitat" %in% names(out))
  expect_equal(out$habitat[out$country_code == "DE"], "forest")
  expect_equal(out$habitat[out$country_code == "FR"], "grassland")
})

test_that(".pivot_species_traits keep_all pivots every trait, curated names kept", {
  long <- data.frame(
    name  = c("Aus bus", "Aus bus", "Cus dus"),
    trait = c("Curated_One", "Novel_Trait", "Curated_One"),
    value = c("2.5", "9", "4.0"),
    stringsAsFactors = FALSE
  )
  curated <- list(nice_name = list(trait = "Curated_One", type = "num"))
  res <- .pivot_species_traits(long, curated)
  expect_true(all(c("canonical_name", "nice_name", "novel_trait") %in% names(res)))
  expect_equal(res$nice_name[res$canonical_name == "Aus bus"], 2.5)
  # curated-only mode still keeps every trait via keep_all default
  res2 <- .pivot_species_traits(long, list())
  expect_true(all(c("curated_one", "novel_trait") %in% names(res2)))
})

test_that(".is_bookkeeping_col flags references/URLs/IDs/names, keeps traits", {
  expect_true(all(.is_bookkeeping_col(c("reference", "references", "citation",
                                        "doi", "source_url", "record_id",
                                        "taxon_id", "species", "genus",
                                        "scientific_name", "id"))))
  expect_false(any(.is_bookkeeping_col(c("body_mass_g", "order", "family",
                                         "activity_cycle", "trophic_level",
                                         "leaf_area"))))
})


# ---- within-source numeric spread ------------------------------------------
#
# Where a source carries several records per species (population/life-stage
# measurements), the median stays the headline value but <col>_min/_max/_n keep
# the range visible. The companion columns appear only where some group has more
# than one finite record, so 1-row-per-species sources stay lean.

test_that(".num_group_spread returns median/min/max/n per group, dropping non-finite", {
  s <- .num_group_spread(c(1, 3, 100, NA, Inf, 7), c("a", "a", "a", "a", "a", "b"))
  expect_equal(s$group, c("a", "b"))
  expect_equal(s$med, c(3, 7))          # median(1,3,100) ; single value 7
  expect_equal(s$min, c(1, 7))
  expect_equal(s$max, c(100, 7))
  expect_equal(s$n, c(3L, 1L))          # NA and Inf dropped from group a
})

test_that(".attach_num_spread adds companions only when a group has >1 record", {
  # multiplicity present -> companions added
  sp  <- .num_group_spread(c(1, 3, 100), c("Aus bus", "Aus bus", "Aus bus"))
  out <- .attach_num_spread(data.frame(canonical_name = "Aus bus",
                                       stringsAsFactors = FALSE),
                            "trait", sp, "Aus bus")
  expect_equal(out$trait, 3)
  expect_equal(out$trait_min, 1)
  expect_equal(out$trait_max, 100)
  expect_equal(out$trait_n, 3L)

  # all singletons -> median only, no companions
  sp2  <- .num_group_spread(c(5, 9), c("Aus bus", "Cus dus"))
  out2 <- .attach_num_spread(data.frame(canonical_name = c("Aus bus", "Cus dus"),
                                        stringsAsFactors = FALSE),
                             "trait", sp2, c("Aus bus", "Cus dus"))
  expect_equal(out2$trait, c(5, 9))
  expect_false(any(c("trait_min", "trait_max", "trait_n") %in% names(out2)))
})

test_that(".aggregate_spread collapses by key to median + gated spread", {
  df <- data.frame(
    canonical_name = c("Aus bus", "Aus bus", "Aus bus", "Cus dus"),
    height = c(10, 100, 40, 7),
    mass   = c(1, 2, 3, 5),
    stringsAsFactors = FALSE
  )
  out <- .aggregate_spread(df, c("height", "mass"))
  expect_equal(out$canonical_name, c("Aus bus", "Cus dus"))
  expect_equal(out$height, c(40, 7))          # median(10,100,40)=40 ; 7
  expect_equal(out$height_min, c(10, 7))
  expect_equal(out$height_max, c(100, 7))
  expect_equal(out$height_n, c(3L, 1L))
  expect_equal(out$mass, c(2, 5))
})

test_that(".append_all_cols emits spread where a species has several records", {
  df <- data.frame(
    sp    = c("Aus bus", "Aus bus", "Aus bus", "Cus dus"),
    trait = c(1, 3, 100, 8),
    stringsAsFactors = FALSE
  )
  out <- .append_all_cols(data.frame(canonical_name = c("Aus bus", "Cus dus"),
                                     stringsAsFactors = FALSE),
                          df, df$sp, used = "sp")
  expect_equal(out$trait[out$canonical_name == "Aus bus"], 3)      # median
  expect_equal(out$trait_min[out$canonical_name == "Aus bus"], 1)
  expect_equal(out$trait_max[out$canonical_name == "Aus bus"], 100)
  expect_equal(out$trait_n[out$canonical_name == "Aus bus"], 3L)
  # the single-record species carries the value but NA spread (n == 1 there)
  expect_equal(out$trait[out$canonical_name == "Cus dus"], 8)
  expect_equal(out$trait_n[out$canonical_name == "Cus dus"], 1L)
})

test_that(".pivot_species_traits emits spread for a multiply-recorded trait", {
  long <- data.frame(
    name  = c("Aus bus", "Aus bus", "Aus bus", "Cus dus"),
    trait = c("Height", "Height", "Height", "Height"),
    value = c("10", "40", "100", "7"),
    stringsAsFactors = FALSE
  )
  res <- .pivot_species_traits(long, list(height = list(trait = "Height", type = "num")))
  expect_equal(res$height[res$canonical_name == "Aus bus"], 40)
  expect_equal(res$height_min[res$canonical_name == "Aus bus"], 10)
  expect_equal(res$height_max[res$canonical_name == "Aus bus"], 100)
  expect_equal(res$height_n[res$canonical_name == "Aus bus"], 3L)
})
