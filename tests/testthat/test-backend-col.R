# Build-time COL helpers. Internal (`@noRd`) so accessed via `:::`.

test_that("strip_authorship removes authorship correctly", {
  sci <- c("Quercus robur L.", "Pinus sylvestris L.", "Festulolium")
  auth <- c("L.", "L.", NA)
  result <- taxifydb:::strip_authorship(sci, auth)
  expect_equal(result, c("Quercus robur", "Pinus sylvestris", "Festulolium"))
})

test_that("strip_authorship handles complex authorship", {
  sci <- "Quercus petraea (Matt.) Liebl."
  auth <- "(Matt.) Liebl."
  result <- taxifydb:::strip_authorship(sci, auth)
  expect_equal(result, "Quercus petraea")
})

test_that("strip_authorship handles NA values", {
  sci <- c("Quercus robur L.", NA, "Pinus sylvestris L.")
  auth <- c("L.", NA, "L.")
  result <- taxifydb:::strip_authorship(sci, auth)
  expect_equal(result, c("Quercus robur", NA, "Pinus sylvestris"))
})

test_that("col_resolve_classification denormalizes the full lineage (#24)", {
  # Animalia > Chordata > Mammalia > Carnivora > Canidae > Vulpes > V. vulpes,
  # plus a species that links straight to the family (no genus row).
  df <- data.frame(
    taxonID           = c("1", "2", "3", "4", "5", "6", "7", "8"),
    parentNameUsageID = c(NA, "1", "2", "3", "4", "5", "6", "5"),
    taxonRank         = c("KINGDOM", "PHYLUM", "CLASS", "ORDER", "FAMILY",
                          "GENUS", "SPECIES", "SPECIES"),
    canonicalName     = c("Animalia", "Chordata", "Mammalia", "Carnivora",
                          "Canidae", "Vulpes", "Vulpes vulpes",
                          "Nyctereutes procyonoides"),
    genericName       = c(NA, NA, NA, NA, NA, "Vulpes", "Vulpes", "Nyctereutes"),
    stringsAsFactors  = FALSE
  )
  cls <- taxifydb:::col_resolve_classification(df)

  # A species carries its whole lineage.
  expect_equal(cls$kingdom[7], "Animalia")
  expect_equal(cls$phylum[7],  "Chordata")
  expect_equal(cls$class[7],   "Mammalia")
  expect_equal(cls$order[7],   "Carnivora")
  expect_equal(cls$family[7],  "Canidae")

  # A species linking straight to the family still resolves upward.
  expect_equal(cls$order[8],  "Carnivora")
  expect_equal(cls$family[8], "Canidae")

  # A family row carries its ancestors and seeds itself.
  expect_equal(cls$kingdom[5], "Animalia")
  expect_equal(cls$order[5],   "Carnivora")
  expect_equal(cls$family[5],  "Canidae")

  # A kingdom row has no family/order ancestor.
  expect_true(is.na(cls$family[1]))
  expect_true(is.na(cls$order[1]))
  expect_equal(cls$kingdom[1], "Animalia")
})

test_that("col fills higher ranks from the family's dominant placement (#24)", {
  # Subtree A places Canidae under Carnivora/Mammalia/Chordata/Animalia. Subtree
  # B is an orphan Canidae node with no chain above the family, so its members
  # get family via the tree but no higher ranks -- they should inherit A's.
  df <- data.frame(
    taxonID           = as.character(1:10),
    parentNameUsageID = c(NA, "1", "2", "3", "4", "5", "6",   # A: K>P>C>O>F>G>SP
                          NA, "8", "9"),                       # B: orphan F>G>SP
    taxonRank         = c("KINGDOM", "PHYLUM", "CLASS", "ORDER", "FAMILY",
                          "GENUS", "SPECIES",
                          "FAMILY", "GENUS", "SPECIES"),
    canonicalName     = c("Animalia", "Chordata", "Mammalia", "Carnivora",
                          "Canidae", "Vulpes", "Vulpes vulpes",
                          "Canidae", "Canis", "Canis lupus"),
    genericName       = c(NA, NA, NA, NA, NA, "Vulpes", "Vulpes",
                          NA, "Canis", "Canis"),
    stringsAsFactors  = FALSE
  )
  cls <- taxifydb:::col_resolve_classification(df)
  # Canis lupus (row 10) has family Canidae from the tree but no higher chain;
  # the fallback fills its order/class/kingdom from Canidae's known placement.
  expect_equal(cls$family[10],  "Canidae")
  expect_equal(cls$order[10],   "Carnivora")
  expect_equal(cls$class[10],   "Mammalia")
  expect_equal(cls$kingdom[10], "Animalia")
})

test_that("col family fallback fills from genericName on a broken parent link (#24)", {
  df <- data.frame(
    taxonID           = c("5", "6", "9"),
    parentNameUsageID = c("4", "5", "999"),   # 999 is absent -> chain breaks
    taxonRank         = c("FAMILY", "GENUS", "SPECIES"),
    canonicalName     = c("Canidae", "Vulpes", "Vulpes lagopus"),
    genericName       = c(NA, "Vulpes", "Vulpes"),
    stringsAsFactors  = FALSE
  )
  cls <- taxifydb:::col_resolve_classification(df)
  # Row 3's parent link is broken, but genericName Vulpes -> Canidae.
  expect_equal(cls$family[3], "Canidae")
})
