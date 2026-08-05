# A Plazi treatment measures the antenna, furca, mucro, claw and individual
# setae in the same sentence style it uses for the whole animal, so the value of
# this parser is entirely in which numbers it refuses. Every string below is
# taken verbatim from the harvested Collembola archives.

mm <- function(txt) taxifydb:::.plazi_body_length(txt)

test_that("body length is read from the phrasings the literature uses", {
  expect_equal(mm("Body length 0.48 mm.")$lo, 0.48)
  expect_equal(mm("Body length 1.3 - 1.6 mm.")[, c("lo", "hi")],
               data.frame(lo = 1.3, hi = 1.6))
  expect_equal(mm("Body length (without antennae) 2.75 – 4 mm.")$hi, 4)
  expect_equal(mm("Size 0.7 – 1.5 mm.")$lo, 0.7)
  expect_equal(mm("Body size up to 1.6 mm.")$hi, 1.6)
  expect_equal(mm("Total length (head + trunk) of holotype 2.51 mm.")$lo, 2.51)
  expect_equal(mm("Length, 1.6 mm (n = 4).")$lo, 1.6)
  expect_equal(mm("Maximum body length: up to 1.5 mm.")$hi, 1.5)
  expect_equal(mm("Length of body (stylet excluded): 400 - 450 µm.")$hi, 0.45)
  # the anchor may follow the number
  expect_equal(mm("Adult body 500 – 630 μm long and 100 μm wide.")$hi, 0.63)
})

test_that("organ measurements are refused", {
  expect_null(mm("Furcal length 635 μm (n = 3)."))
  expect_null(mm("Total length of antenna ~ 70 µm;"))
  expect_null(mm("Antennae length 1.36 mm, 2.8 times the length of the head."))
  expect_null(mm("The length of manubrium and dens 285 + 353 = 638 μm."))
  expect_null(mm("length of mucro: 37 µm, width in the middle part: 5 µm."))
  expect_null(mm("Cephalic diagonal length 158 µm."))
  expect_null(mm("Claw 20 – 25 μm long."))
  expect_null(mm("Anal spines 5 – 7 μm long."))
  expect_null(mm("Postantennal organ narrow, 25 – 35 μm long and 5 μm wide."))
  # "macrosetae" has no word boundary before "seta"
  expect_null(mm("Length of long macrosetae from 62 – 100 µm."))
  expect_null(mm("Length of Ant I to IV (30 µm, 40 µm, 40 µm, 65 µm)."))
})

test_that("a number with no length anchor at all is refused", {
  expect_null(mm("Head dorsally covered with round scales."))
  expect_null(mm("Body length."))
  expect_null(mm("Ratio of segments 1: 1.67: 1.48: 2.55."))
})

test_that("units and separators are converted", {
  # micrometres to mm
  expect_equal(mm("Body length: holotype 400 µm.")$lo, 0.4)
  # decimal comma
  expect_equal(mm("Maximum length 1,25 mm.")$lo, 1.25)
  # thousands separator inside a micrometre value
  expect_equal(mm("Body length 1,250 μm.")$lo, 1.25)
  # every dash the literature mixes
  for (dash in c("-", "–", "—", "‒", "~")) {
    expect_equal(mm(paste0("Body length 1.0 ", dash, " 2.0 mm."))$hi, 2.0,
                 info = dash)
  }
  expect_equal(mm("Body length 1.0 to 2.0 mm.")$hi, 2.0)
})

test_that("guards refuse values that are not this species' adult body length", {
  # trunk length: a different quantity from whole-body length
  expect_null(mm("Adult body length (without head and furca) 0.9 – 1.2 mm."))
  expect_null(mm("Holotype body length (without head nor furca) 2.6 mm."))
  # the number belongs to a congener
  expect_null(mm("They can be distinguished by body size (0.9 – 1.3 mm in O."))
  expect_null(mm("Smaller body size (1.5 mm instead of 2.0 mm), fewer setae."))
  expect_null(mm("bidentata differing by body length (1.1 - 1.3 mm in F."))
  # inequality and juveniles
  expect_null(mm("These three species have body length <3 mm and psx."))
  expect_null(mm("Length of available juveniles 0.33 – 0.42 mm."))

  # a trailing figure reference is not a comparison
  expect_equal(mm("Body size 0.7 - 0.9 mm, habitus as in Figs 1, 2.")$lo, 0.7)
  # "without antennae" is the universal convention, not a disqualifier
  expect_equal(mm("Body length without antennae 2.2 – 2.6 mm.")$hi, 2.6)
  # a within-species sex comparison is not a between-species one
  expect_equal(mm("Body length: 1.33 – 1.79 mm in adults.")$hi, 1.79)
})

test_that("guards can be switched off", {
  expect_null(mm("Adult body length (without head and furca) 0.9 – 1.2 mm."))
  expect_equal(
    taxifydb:::.plazi_body_length(
      "Adult body length (without head and furca) 0.9 – 1.2 mm.",
      guards = FALSE)$hi, 1.2)
})

test_that("one clause can yield several measurements, each judged separately", {
  r <- mm(paste("Body length 1.5 mm.",
                "Antennae length 0.8 mm.",
                "Body size 2.0 – 2.4 mm."))
  expect_equal(NROW(r), 2L)
  expect_equal(sort(r$lo), c(1.5, 2.0))
})

test_that("the parser aggregates archives to one row per species", {
  dir <- withr::local_tempdir()
  write_archive <- function(name, taxa, descs) {
    wd <- withr::local_tempdir()
    utils::write.table(taxa, file.path(wd, "taxa.txt"), sep = "\t",
                       row.names = FALSE, quote = FALSE, na = "")
    utils::write.table(descs, file.path(wd, "description.txt"), sep = "\t",
                       row.names = FALSE, quote = FALSE, na = "")
    old <- setwd(wd); on.exit(setwd(old), add = TRUE)
    utils::zip(file.path(dir, name), c("taxa.txt", "description.txt"),
               flags = "-q")
  }

  # Isotoma viridis is measured in both papers; the beetle must be ignored.
  write_archive("a.zip",
    data.frame(taxonID = c("t1", "t2"),
               canonicalName = c("Isotoma viridis", "Carabus auratus"),
               scientificName = c("Isotoma viridis Bourlet", "Carabus auratus L."),
               class = c("Collembola", "Insecta"),
               order = c("Entomobryomorpha", "Coleoptera"),
               taxonRank = "species", stringsAsFactors = FALSE),
    data.frame(taxonID = c("t1", "t2"), type = "description",
               description = c("Body length 4.0 – 5.0 mm.",
                               "Body length 20.0 mm."),
               stringsAsFactors = FALSE))
  # Collembola filed as an order under Entognatha, the other half of the corpus
  write_archive("b.zip",
    data.frame(taxonID = "t1", canonicalName = "Isotoma viridis",
               scientificName = "Isotoma viridis Bourlet",
               class = "Entognatha", order = "Collembola",
               taxonRank = "species", stringsAsFactors = FALSE),
    data.frame(taxonID = "t1", type = "diagnosis",
               description = "Body length 4.5 mm.", stringsAsFactors = FALSE))

  skip_if_not(length(list.files(dir, pattern = "\\.zip$")) == 2L,
              "zip utility unavailable")
  out <- parse_plazi_collembola_body_length(dir)

  expect_equal(out$canonical_name, "Isotoma viridis")
  expect_equal(out$plazi_body_length_min_mm, 4.0)
  expect_equal(out$plazi_body_length_max_mm, 5.0)
  expect_equal(out$plazi_body_length_mm, 4.5)
  expect_equal(out$plazi_body_length_n, 2L)
  expect_equal(out$plazi_body_length_sources, 2L)
  # the 20 mm beetle is out of the Collembola plausibility window anyway, but
  # it must be gone because it is not Collembola
  expect_false("Carabus auratus" %in% out$canonical_name)
})
