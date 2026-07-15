# The C-values search returns one prime estimate per database species entry, so
# a binomial collects several records only where entries differing by accession
# or cytotype share it. Chromosome number and ploidy level are discrete counts
# there: a median between cytotypes invents a count neither record reports. The
# 1C DNA amount is a continuous measurement and keeps the median.
#
# An odd 2n is not an error to be filtered: a triploid carries one by definition
# (Tahiti lime 2n = 3x = 27), as do aneuploid hybrids and the gametophytic
# counts of haploid-dominant bryophytes. The reduction must leave those alone.

kew_page <- function(rows) {
  cell <- function(x) paste0("<td>", x, "</td>")
  body <- vapply(seq_len(nrow(rows)), function(i) {
    paste0("<tr>", paste0(vapply(rows[i, ], cell, character(1L)),
                          collapse = ""), "</tr>")
  }, character(1L))
  f <- tempfile(fileext = ".html")
  writeLines(c(
    "<html><body>",
    # A summary table the parser must ignore (no Genus/Species columns).
    "<table><tr><th>Mean</th><th>Min</th><th>Max</th></tr>",
    "<tr><td>1</td><td>2</td><td>3</td></tr></table>",
    "<table><tr>",
    paste0("<th>", names(rows), "</th>", collapse = ""),
    "</tr>", body, "</table>",
    "</body></html>"
  ), f)
  f
}

fixture <- function() {
  kew_page(data.frame(
    Family  = "Brassicaceae",
    Genus   = c("Arabidopsis", "Arabidopsis", "Zea", "Acer", "Acer", "Sedum",
                "Citrus"),
    Species = c("thaliana", "thaliana", "mays", "campestre", "campestre", "acre",
                "latifolia"),
    `Chromosome Number (2n)` = c("10", "20", "20", "26", "52", "-", "27"),
    `Ploidy Level (x)`       = c("2", "4", "2", "2", "4", "-", "3"),
    `Estimation Method`      = c("FC:PI", "Fe", "FC:PI", "FC:PI", "FC:PI",
                                 "FC:PI", "FC:PI"),
    `DNA Amount1C (pg)`      = c("0.20", "0.40", "2.70", "0.60", "1.20", "5.00",
                                 "0.61"),
    `Original Reference`     = c("Bennett and Smith,1976", "Zonneveld et al.,2005",
                                 "Rayburn et al.,1993", "Siljak-Yakovlev,2010",
                                 "Siljak-Yakovlev,2010", "Suda et al.,2005",
                                 "Pellicer et al.,2013"),
    check.names      = FALSE,
    stringsAsFactors = FALSE
  ))
}

test_that("a cytotype pair reduces to the base cytotype, not an odd median", {
  res <- parse_kew_cvalues(fixture())
  at  <- res[res$canonical_name == "Arabidopsis thaliana", ]

  # median(10, 20) would be 15 -- an odd somatic number no plant has
  expect_equal(at$chromosome_2n, 10)
  expect_equal(at$ploidy_x, 2)
  # the cytotype range survives on the companion columns
  expect_equal(at$chromosome_2n_min, 10)
  expect_equal(at$chromosome_2n_max, 20)
  expect_equal(at$chromosome_2n_n, 2L)

  ac <- res[res$canonical_name == "Acer campestre", ]
  expect_equal(ac$chromosome_2n, 26)      # median(26, 52) would be 39
  expect_equal(ac$chromosome_2n_max, 52)
})

test_that("a reduced count is always a count some record reported", {
  res <- parse_kew_cvalues(fixture())
  v   <- res$chromosome_2n[is.finite(res$chromosome_2n)]
  expect_true(all(abs(v - round(v)) < 1e-9))

  p <- res$ploidy_x[is.finite(res$ploidy_x)]
  expect_true(all(abs(p - round(p)) < 1e-9))

  # every reduced value appears among that species' own source records
  expect_true(all(res$chromosome_2n[is.finite(res$chromosome_2n)] %in%
                    c(10, 20, 26, 52, 27)))
})

test_that("provenance survives the reduction: every paper behind a value is kept", {
  res <- parse_kew_cvalues(fixture())

  # a species measured twice keeps both papers, so the reduced value is traceable
  at <- res[res$canonical_name == "Arabidopsis thaliana", ]
  expect_equal(at$original_reference,
               "Bennett and Smith,1976; Zonneveld et al.,2005")
  # the two records used different methods, and both are reported
  expect_equal(at$estimation_method, "FC:PI; Fe")

  # a single-record species carries its one reference, unjoined
  expect_equal(res$original_reference[res$canonical_name == "Zea mays"],
               "Rayburn et al.,1993")
  expect_equal(res$estimation_method[res$canonical_name == "Zea mays"], "FC:PI")

  # two records from the same paper collapse to one citation
  expect_equal(res$original_reference[res$canonical_name == "Acer campestre"],
               "Siljak-Yakovlev,2010")
})

test_that("a genuine triploid keeps its odd count", {
  res <- parse_kew_cvalues(fixture())
  cl  <- res[res$canonical_name == "Citrus latifolia", ]

  # Tahiti lime is a seedless triploid: 2n = 3x = 27. Odd is correct here, and
  # a reduction that filtered or evened odd counts would destroy it.
  expect_equal(cl$chromosome_2n, 27)
  expect_equal(cl$ploidy_x, 3)
})

test_that("the continuous 1C amount keeps its median, and '-' reads as absent", {
  res <- parse_kew_cvalues(fixture())
  at  <- res[res$canonical_name == "Arabidopsis thaliana", ]
  expect_equal(at$genome_size_1c_pg, 0.30)      # median(0.20, 0.40)

  # a single-record species passes through untouched
  expect_equal(res$genome_size_1c_pg[res$canonical_name == "Zea mays"], 2.70)
  expect_equal(res$chromosome_2n[res$canonical_name == "Zea mays"], 20)

  # "-" is the source's absent marker, not a value
  sa <- res[res$canonical_name == "Sedum acre", ]
  expect_true(is.na(sa$chromosome_2n))
  expect_equal(sa$genome_size_1c_pg, 5.00)
})
