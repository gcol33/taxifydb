# Parsers name the column they want as a list of spellings in preference order,
# because sources rename fields between releases. The list has to be read as an
# order, not as a set: a table that carries both its own row id and a foreign
# key must be joined on the foreign key whichever of the two sits leftmost.

test_that(".first_col reads its candidates as a preference order", {
  df <- data.frame(id = 1:2, taxon_wcvp_id = 3:4, stringsAsFactors = FALSE)

  # both candidates present, and the preferred one is NOT the leftmost column
  expect_equal(.first_col(df, c("taxon_wcvp_id", "taxon_id", "id")),
               "taxon_wcvp_id")
  # reversing the preference reverses the answer, on the same data frame
  expect_equal(.first_col(df, c("id", "taxon_wcvp_id")), "id")
})

test_that(".first_col falls back only when no candidate is present", {
  df <- data.frame(a = 1, b = 2, stringsAsFactors = FALSE)

  expect_null(.first_col(df, c("x", "y")))
  expect_equal(.first_col(df, c("x", "y"), fallback = names(df)[1L]), "a")
  expect_equal(.first_col(names(df), c("b", "a")), "b")
})

test_that("parse_glonaf joins on the foreign key, not the flora row id", {
  dir <- withr::local_tempdir()

  # The flora table's own row ids overlap the taxon table's ids, which is what
  # GloNAF itself looks like: joining on the wrong one still matches, so a
  # wrong join shows up as scrambled names rather than as an error.
  utils::write.csv(
    data.frame(
      id            = c(1L, 2L, 3L),
      taxon_wcvp_id = c(3L, 2L, 1L),
      region_id     = c(10L, 10L, 20L),
      status        = c("Naturalized", "Invasive", "Naturalized"),
      stringsAsFactors = FALSE
    ),
    file.path(dir, "glonaf_flora2.csv"), row.names = FALSE
  )
  utils::write.csv(
    data.frame(
      id            = c(1L, 2L, 3L),
      taxa_accepted = c("Abutilon theophrasti", "Acacia saligna",
                        "Lantana camara"),
      stringsAsFactors = FALSE
    ),
    file.path(dir, "glonaf_taxon_wcvp.csv"), row.names = FALSE
  )
  # OBJIDsic is the polygon's GIS object id; the flora table's region_id points
  # at `id`, and the two ranges overlap.
  utils::write.csv(
    data.frame(
      id       = c(10L, 20L),
      code     = c("AUT", "LUX"),
      OBJIDsic = c(20L, 30L),
      stringsAsFactors = FALSE
    ),
    file.path(dir, "glonaf_region.csv"), row.names = FALSE
  )

  out <- parse_glonaf(dir)

  expect_equal(nrow(out), 3L)
  expect_equal(
    out$canonical_name[order(out$canonical_name)],
    c("Abutilon theophrasti", "Acacia saligna", "Lantana camara")
  )
  # every record keeps its own region, and no region is lost to the GIS id
  expect_equal(sort(unique(out$region_id)), c("AUT", "LUX"))
  expect_equal(out$region_id[out$canonical_name == "Abutilon theophrasti"], "LUX")
  expect_equal(out$status[out$canonical_name == "Lantana camara"], "Naturalized")
})

test_that("parse_glonaf fails when a join key only partly resolves", {
  dir <- withr::local_tempdir()

  utils::write.csv(
    data.frame(id = 1:2, taxon_wcvp_id = 1:2, region_id = c(10L, 99L),
               stringsAsFactors = FALSE),
    file.path(dir, "glonaf_flora2.csv"), row.names = FALSE
  )
  utils::write.csv(
    data.frame(id = 1:2, taxa_accepted = c("Abutilon theophrasti", "Acacia saligna"),
               stringsAsFactors = FALSE),
    file.path(dir, "glonaf_taxon_wcvp.csv"), row.names = FALSE
  )
  utils::write.csv(
    data.frame(id = 10L, code = "AUT", stringsAsFactors = FALSE),
    file.path(dir, "glonaf_region.csv"), row.names = FALSE
  )

  expect_error(parse_glonaf(dir), "region join does not resolve")
})
