# The monograph adapter reuses the Plazi body-length core and adds only the
# per-source segmentation: which line is a species name, and where the length
# sits relative to it. The strings below are in the layout of the books the two
# strategies were written for -- Stach/Bretfeld (anchored) and Hopkin
# (parenthetical).

write_txt <- function(lines) {
  f <- tempfile(fileext = ".txt")
  writeLines(lines, f)
  f
}

test_that("anchored: 'Length X mm' attaches to the nearest species-name line", {
  f <- write_txt(c(
    "Neelus murinus Folsom, 1896",
    "Sensory areae well developed. Length 0,5 mm.",
    "Megalothorax incertus Borner, 1903",
    "Dental setae smooth. Length 0,39 mm."))
  out <- parse_monograph_body_length(f, "stach_1957")
  expect_setequal(out$canonical_name, c("Neelus murinus", "Megalothorax incertus"))
  expect_equal(out$stach1957_body_length_mm[out$canonical_name == "Neelus murinus"], 0.5)
  expect_equal(out$stach1957_body_length_mm[out$canonical_name == "Megalothorax incertus"], 0.39)
})

test_that("anchored: the decimal comma and dash ranges of the source are read", {
  f <- write_txt(c(
    "Dicyrtoma fusca Latzel, 1918",
    "Colour dark. Length about 0,9-1,4 mm."))
  out <- parse_monograph_body_length(f, "stach_1957")
  expect_equal(out$stach1957_body_length_min_mm, 0.9)
  expect_equal(out$stach1957_body_length_max_mm, 1.4)
})

test_that("anchored: a sentence opening with a capitalised word is not a species", {
  f <- write_txt(c(
    "The genus Megalothorax is erected by Willem, in 1900.",
    "Total length up to 0,5 mm.",
    "Neelus murinus Folsom, 1896",
    "Length 0,4 mm."))
  out <- parse_monograph_body_length(f, "stach_1957")
  expect_false("The genus" %in% out$canonical_name)
  expect_true("Neelus murinus" %in% out$canonical_name)
})

test_that("parenthetical: the leading bracket measurement is the body length", {
  f <- write_txt(c(
    "186. Entomobrya lanuginosa (2.0 mm; greenish to greyish blue; Fig. 57)",
    "196. Entomobryoides myrmecophilus",
    "(3.0 mm; brownish yellow, antennae darker; Fig. 56)"))
  out <- parse_monograph_body_length(f, "hopkin_2007")
  expect_equal(out$hopkin2007_body_length_mm[out$canonical_name == "Entomobrya lanuginosa"], 2.0)
  # the bracket may sit on the line after the name
  expect_equal(out$hopkin2007_body_length_mm[out$canonical_name == "Entomobryoides myrmecophilus"], 3.0)
})

test_that("parenthetical: a bracket whose measurement is not leading is refused", {
  f <- write_txt(c(
    "Stout mandibles present (basal tooth 0.1 mm long).",
    "Petri dish observation (specimen 2 mm from edge).",
    "Allacma fusca (3.5 mm; brown; Fig. 1)"))
  out <- parse_monograph_body_length(f, "hopkin_2007")
  expect_equal(out$canonical_name, "Allacma fusca")
  expect_equal(out$hopkin2007_body_length_mm, 3.5)
})

test_that("parenthetical: an all-caps terminal name is normalised", {
  f <- write_txt("178. STENAPHORURA QUADRISPINA (1.1 mm; white; Fig. 21)")
  out <- parse_monograph_body_length(f, "hopkin_2007")
  expect_equal(out$canonical_name, "Stenaphorura quadrispina")
  expect_equal(out$hopkin2007_body_length_mm, 1.1)
})

test_that("micrometre body lengths convert to mm", {
  f <- write_txt(c(
    "Megalothorax laevis Denis, 1948",
    "Length up to 400 um."))
  out <- parse_monograph_body_length(f, "stach_1957")
  expect_equal(out$stach1957_body_length_mm, 0.4)
})

test_that("an unknown source is an error, not a silent empty result", {
  f <- write_txt("Neelus murinus Folsom, 1896")
  expect_error(parse_monograph_body_length(f, "gisin_1960"), "unknown source")
})

test_that("a directory of one .txt is accepted", {
  d <- withr::local_tempdir()
  writeLines(c("Neelus murinus Folsom, 1896", "Length 0,5 mm."),
             file.path(d, "mono.txt"))
  out <- parse_monograph_body_length(d, "stach_1957")
  expect_equal(out$canonical_name, "Neelus murinus")
})


# parse_monograph_collembola: aggregation of the frozen per-book extraction
write_csv <- function(df) {
  f <- tempfile(fileext = ".csv")
  utils::write.csv(df, f, row.names = FALSE)
  f
}

test_that("reader aggregates a species across monographs", {
  f <- write_csv(data.frame(
    canonical_name     = c("Sminthurus viridis", "Sminthurus viridis",
                           "Neelus murinus"),
    source             = c("hopkin_2007", "bretfeld_1999", "stach_1957"),
    body_length_mm     = c(2.0, 3.0, 0.5),
    body_length_min_mm = c(1.8, 2.5, 0.4),
    body_length_max_mm = c(2.2, 3.4, 0.6),
    body_length_n      = c(3L, 2L, 1L),
    stringsAsFactors   = FALSE))
  out <- parse_monograph_collembola(f)
  sv <- out[out$canonical_name == "Sminthurus viridis", ]
  expect_equal(sv$monograph_body_length_mm, 2.5)       # median(2.0, 3.0)
  expect_equal(sv$monograph_body_length_min_mm, 1.8)   # min of mins
  expect_equal(sv$monograph_body_length_max_mm, 3.4)   # max of maxes
  expect_equal(sv$monograph_body_length_n, 5L)         # sum of counts
  expect_equal(sv$monograph_body_length_sources, 2L)   # two monographs
})

test_that("reader passes a single-monograph species through unchanged", {
  f <- write_csv(data.frame(
    canonical_name     = "Neelus murinus",
    source             = "stach_1957",
    body_length_mm     = 0.5, body_length_min_mm = 0.4,
    body_length_max_mm = 0.6, body_length_n = 1L,
    stringsAsFactors   = FALSE))
  out <- parse_monograph_collembola(f)
  expect_equal(out$monograph_body_length_mm, 0.5)
  expect_equal(out$monograph_body_length_sources, 1L)
})

test_that("reader errors on a missing column, not a silent empty result", {
  f <- write_csv(data.frame(canonical_name = "Neelus murinus",
                            body_length_mm = 0.5))
  expect_error(parse_monograph_collembola(f), "missing column")
})
