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
