# The C-values search returns one prime estimate per database species entry, so
# a binomial collects several records only where entries differing by accession
# or cytotype share it. Chromosome number and ploidy level are discrete counts
# there: a median between cytotypes invents a value no plant carries. The 1C DNA
# amount is a continuous measurement and keeps the median.

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
    Genus   = c("Arabidopsis", "Arabidopsis", "Zea", "Acer", "Acer", "Sedum"),
    Species = c("thaliana", "thaliana", "mays", "campestre", "campestre", "acre"),
    `Chromosome Number (2n)` = c("10", "20", "20", "26", "52", "-"),
    `Ploidy Level (x)`       = c("2", "4", "2", "2", "4", "-"),
    `DNA Amount1C (pg)`      = c("0.20", "0.40", "2.70", "0.60", "1.20", "5.00"),
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

test_that("every reduced chromosome number is an even, whole count", {
  res <- parse_kew_cvalues(fixture())
  v   <- res$chromosome_2n[is.finite(res$chromosome_2n)]
  expect_true(all(abs(v - round(v)) < 1e-9))
  expect_true(all(round(v) %% 2 == 0))

  p <- res$ploidy_x[is.finite(res$ploidy_x)]
  expect_true(all(abs(p - round(p)) < 1e-9))
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
