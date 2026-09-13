test_that("the subgenus is dropped from a species-group name (#50)", {
  nm <- c("Camponotus (Camponotus) herculeanus",
          "Lepturges (L.) sexvittatus",
          "Potamon (Potamon) atkinsonianum var. emphyseteum",
          "Anopheles (Nyssorhynchus) pristinus")
  g <- c("Camponotus", "Lepturges", "Potamon", "Anopheles")
  expect_equal(
    taxifydb:::drop_infrageneric(nm, g),
    c("Camponotus herculeanus", "Lepturges sexvittatus",
      "Potamon atkinsonianum var. emphyseteum", "Anopheles pristinus")
  )
})


test_that("every infrageneric group before the epithet is dropped (#50)", {
  nm <- c("Thoracostoma (Pseudocella) (Corythostoma) filipjevi",
          "Thoracostoma (Pseudocella) (Corythostoma)")
  g <- c("Thoracostoma", "Thoracostoma")
  expect_equal(taxifydb:::drop_infrageneric(nm, g),
               c("Thoracostoma filipjevi", "Thoracostoma (Corythostoma)"))
  p <- split_scientific_name(nm[1], g[1])
  expect_equal(p$specific, "filipjevi")
})


test_that("a subgenus keeps its own name (#50)", {
  nm <- c("Aaleniella (Danocythere)", "Aaleniella (Aaleniella)")
  expect_equal(taxifydb:::drop_infrageneric(nm, c("Aaleniella", "Aaleniella")),
               nm)
})


test_that("a parenthesis that is not a subgenus is left in place (#50)", {
  nm <- c("Valencia (Quatrefages, 1846) sensu Koehler, 1885",
          "Salix (caprea x viminalis) x (purpurea x viminalis)",
          "Quercus robur",
          "Mentha \u00d7 piperita",
          NA)
  g <- c("Valencia", "Salix", "Quercus", "Mentha", NA)
  expect_equal(taxifydb:::drop_infrageneric(nm, g), nm)
})


test_that("without a usable genus the first word anchors the group (#50)", {
  # ITIS can resolve the genus column to the subgenus itself.
  expect_equal(
    taxifydb:::drop_infrageneric(
      c("Hyposmocoma (Euperissus) exaltata", "Abax (Argutor) ater"),
      c("Euperissus", NA)
    ),
    c("Hyposmocoma exaltata", "Abax ater")
  )
})


test_that("normalize_backbone keys every backbone on the binomial (#50)", {
  df <- data.frame(
    id     = c("1", "2", "3"),
    name   = c("Camponotus (Camponotus) herculeanus",
               "Camponotus (Tanaemyrmex)", "Camponotus"),
    rank   = c("species", "subgenus", "genus"),
    status = c("accepted", "accepted", "accepted"),
    acc    = c(NA, NA, NA),
    fam    = "Formicidae",
    gen    = "Camponotus",
    sp     = c("herculeanus", NA, NA),
    stringsAsFactors = FALSE
  )
  out <- normalize_backbone(df, list(
    taxon_id = "id", canonical_name = "name", taxon_rank = "rank",
    taxonomic_status = "status", accepted_name_usage_id = "acc",
    family = "fam", genus = "gen", specific_epithet = "sp"
  ))
  expect_equal(out$canonical_name,
               c("Camponotus herculeanus", "Camponotus (Tanaemyrmex)",
                 "Camponotus"))
})
