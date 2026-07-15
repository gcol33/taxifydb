# FishTraits codes missing values three ways. -999 and -555 are the documented
# ones. The third, -1, marks "no mapped native range in the conterminous US" and
# spans the whole range-derived block, so it cannot be stripped one column at a
# time: -1 is also a genuine January minimum for species that do have a range.
# The workbook also follows the SAS convention of "." for an absent text value.

# Build a workbook carrying every column parse_fishtraits touches, so the repair
# can be exercised offline.
fishtraits_fixture <- function(rows) {
  guild <- c("A_1_1", "A_1_2", "B_2_3", "C1_1")
  cols <- c("SID", "GENUS", "SPECIES", "ITISTSN", "COMMONNAME", "NATIVE",
            "NONFEED", "BENTHIC", "SURWCOL", "ALGPHYTO", "MACVASCU", "DETRITUS",
            "INVLVFSH", "FSHCRCRB", "BLOOD", "EGGS", "MAXTL", "MATUAGE",
            "LONGEVITY", "FECUNDITY", "SERIAL", "SEASON", "EURYHALINE",
            "MINTEMP", "MAXTEMP", "POTANADR", "PREFLOT", "PREFLEN", "LISTED",
            "EXTINCT", "AREAKM2", "PERIMETER", "PATCHES", "LATRANGE", "LONRANGE",
            guild)
  df <- as.data.frame(
    matrix(0, nrow = length(rows), ncol = length(cols), dimnames = list(NULL, cols)),
    stringsAsFactors = FALSE)
  df$GENUS <- ""; df$SPECIES <- ""; df$ITISTSN <- ""; df$COMMONNAME <- ""
  df$EXTINCT <- "U"
  for (i in seq_along(rows)) for (k in names(rows[[i]])) df[i, k] <- rows[[i]][[k]]
  f <- tempfile(fileext = ".xlsx")
  openxlsx2::write_xlsx(df, f)
  f
}

test_that("parse_fishtraits strips the -1 no-range sentinel but keeps real -1 minima", {
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("readxl")

  f <- fishtraits_fixture(list(
    # Introduced, no mapped US range: the whole range block reads -1.
    list(GENUS = "Carassius", SPECIES = "auratus", COMMONNAME = "goldfish",
         NATIVE = 0, MAXTL = 59, LONGEVITY = 10,
         MINTEMP = -1, MAXTEMP = -1, AREAKM2 = -1, PERIMETER = -1,
         PATCHES = -1, LATRANGE = -1, LONRANGE = -1, LISTED = -1),
    # Native, mapped range, January minimum that genuinely is -1 deg C.
    list(GENUS = "Cyprinodon", SPECIES = "elegans", COMMONNAME = "Comanche Springs pupfish",
         NATIVE = 1, MAXTL = 5, LONGEVITY = 1.5,
         MINTEMP = -1, MAXTEMP = 35.2, AREAKM2 = 15564, PATCHES = 1),
    # Native, ordinary row.
    list(GENUS = "Micropterus", SPECIES = "salmoides", COMMONNAME = "largemouth bass",
         NATIVE = 1, MAXTL = 97, LONGEVITY = 16,
         MINTEMP = -3.3, MAXTEMP = 32, AREAKM2 = 2e6, PATCHES = 3),
    # The source's documented missing codes, which were always handled.
    list(GENUS = "Esox", SPECIES = "lucius", COMMONNAME = "northern pike",
         NATIVE = 1, MAXTL = 133, LONGEVITY = 30,
         MINTEMP = -999, MAXTEMP = -555, AREAKM2 = 3e6, PATCHES = 2)
  ))
  on.exit(unlink(f), add = TRUE)

  d <- parse_fishtraits(f)

  # The sentinel row loses only its range-derived values.
  g <- d[d$canonical_name == "Carassius auratus", ]
  expect_true(is.na(g$min_temp_c))
  expect_true(is.na(g$max_temp_c))
  expect_equal(g$max_length_cm, 59)      # a non-range trait is untouched
  expect_equal(g$longevity_yr, 10)

  # A real -1 January minimum on a species that has a range survives.
  p <- d[d$canonical_name == "Cyprinodon elegans", ]
  expect_equal(p$min_temp_c, -1)
  expect_equal(p$max_temp_c, 35.2)

  # Ordinary row untouched; documented codes still stripped.
  expect_equal(d$min_temp_c[d$canonical_name == "Micropterus salmoides"], -3.3)
  expect_true(is.na(d$min_temp_c[d$canonical_name == "Esox lucius"]))
  expect_true(is.na(d$max_temp_c[d$canonical_name == "Esox lucius"]))

  # No July maximum may be negative once the sentinel is gone.
  expect_false(any(d$max_temp_c < 0, na.rm = TRUE))
})

test_that("parse_fishtraits treats '.' as absent and drops entries with no epithet", {
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("readxl")

  f <- fishtraits_fixture(list(
    # Undescribed fish: a family name in GENUS and no epithet, so no species key.
    list(GENUS = "Percidae", SPECIES = ".", COMMONNAME = ".", ITISTSN = ".",
         NATIVE = 1, MAXTL = 8.2, MINTEMP = -2.3, MAXTEMP = 31.8, AREAKM2 = 1000),
    list(GENUS = "Catostomidae", SPECIES = ".", COMMONNAME = "Salish sucker",
         ITISTSN = ".", NATIVE = 1, MAXTL = 25, MINTEMP = 0.9, MAXTEMP = 22.5,
         AREAKM2 = 900),
    # A real species whose common name and TSN happen to be absent.
    list(GENUS = "Micropterus", SPECIES = "salmoides", COMMONNAME = ".",
         ITISTSN = ".", NATIVE = 1, MAXTL = 97, MINTEMP = -3.3, MAXTEMP = 32,
         AREAKM2 = 2e6)
  ))
  on.exit(unlink(f), add = TRUE)

  d <- parse_fishtraits(f)

  expect_equal(d$canonical_name, "Micropterus salmoides")
  expect_false(any(grepl("\\.", d$canonical_name)))
  expect_false(any(grepl(" NA$", d$canonical_name)))
  expect_true(is.na(d$common_name))
  expect_true(is.na(d$itis_tsn))
})
