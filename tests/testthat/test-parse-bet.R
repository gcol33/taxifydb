bet_fixture <- function(rows) {
  path <- tempfile(fileext = ".txt")
  utils::write.table(rows, path, sep = " ", quote = TRUE, row.names = TRUE)
  path
}

test_that("parse_bet collapses the six substrate flags into a set", {
  rows <- data.frame(
    friendly_name = c("Grimmia pulvinata", "Orthotrichum affine",
                      "Polytrichum commune", "Buxbaumia viridis",
                      "Splachnum ampullaceum", "Radula complanata",
                      "Andreaea rupestris"),
    sub_so = c(0, 0, 1, 0, 0, 0, NA),
    sub_ro = c(1, 1, 0, 0, 0, 0, NA),
    sub_ba = c(0, 1, 0, 0, 0, 1, NA),
    sub_wo = c(0, 0, 0, 1, 0, 1, NA),
    sub_nw = c(0, 0, 0, 0, 0, 1, NA),
    sub_an = c(0, 0, 0, 0, 1, 0, NA),
    stringsAsFactors = FALSE
  )
  d <- parse_bet(bet_fixture(rows))
  got <- setNames(d$substrate, d$canonical_name)
  expect_identical(unname(got[rows$friendly_name]), c(
    "rock", "rock|bark", "soil", "wood", "dung_carcass",
    "bark|wood|living_plants", NA_character_
  ))
  expect_identical(d$substrate_rock[d$canonical_name == "Orthotrichum affine"], 1)
})

test_that("parse_bet substrate labels stay inside the documented set", {
  rows <- data.frame(
    friendly_name = c("A a", "B b"),
    sub_so = c(1, 1), sub_ro = c(1, 1), sub_ba = c(1, 0),
    sub_wo = c(1, 0), sub_nw = c(1, 0), sub_an = c(1, 0),
    stringsAsFactors = FALSE
  )
  d <- parse_bet(bet_fixture(rows))
  tokens <- unique(unlist(strsplit(d$substrate, "|", fixed = TRUE)))
  expect_setequal(tokens, c("soil", "rock", "bark", "wood", "living_plants",
                            "dung_carcass"))
})
