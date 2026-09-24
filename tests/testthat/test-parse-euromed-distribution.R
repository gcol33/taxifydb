# parse_euromed_distribution(): status classes, retracted reports, conflict
# resolution, ISO codes and reference provenance.

el <- function(area, name, status, absent = FALSE, refs = character(0)) {
  list(area = area, area_name = name, area_level = "Euro+Med 2. area level",
       status = status, absent = absent,
       refs = lapply(refs, function(u) list(uuid = u, citation = paste("cit", u))))
}

snapshot <- function(dir = tempfile()) {
  dir.create(dir)
  rec <- list(uuid = "uuid-1", name = "Robinia pseudoacacia", distribution = list(
    el("Au(A)", "Austria", "naturalised", refs = c("r1", "r2")),
    el("Au(A)", "Austria", "introduced", refs = "r3"),
    el("Au(L)", "Liechtenstein", "native: reported in error", absent = TRUE,
       refs = "r1"),
    el("Ge", "Germany", "native: formerly native", absent = TRUE, refs = "r2"),
    el("Hs", "Spain, with Gibraltar and Andorra",
       "introduced: presence questionable", refs = "r1"),
    el("Ga(F)", "France", "endemic", refs = "r3")))
  empty <- list(uuid = "uuid-2", name = "Nothing here", distribution = list())
  path <- file.path(dir, "euromed_distribution.jsonl")
  writeLines(vapply(list(rec, empty), jsonlite::toJSON, "", auto_unbox = TRUE),
             path)
  path
}

test_that("status classes follow the Euro+Med vocabulary", {
  cls <- taxifydb:::.euromed_status_class
  expect_equal(
    cls(c("native", "endemic", "not endemic", "naturalised", "casual",
          "cultivated", "introduced", "introduced: uncertain degree of naturalisation",
          "native: presence questionable", "introduced: doubtfully introduced (perhaps cultivated only)",
          "undefined", "native: formerly native", "native: reported in error", ""),
        c(rep(FALSE, 11), TRUE, TRUE, FALSE)),
    c("native", "native", "native", "naturalised", "casual", "cultivated",
      "introduced", "introduced", "doubtful", "doubtful", "doubtful",
      "extinct", NA, NA))
})

test_that("the snapshot parses to one row per taxon and area", {
  out <- suppressMessages(parse_euromed_distribution(snapshot()))
  expect_equal(nrow(out), 4L)
  expect_false("Au(L)" %in% out$area_code)
  row <- function(a) out[out$area_code == a, ]
  expect_equal(row("Au(A)")$euromed_status, "naturalised")
  expect_equal(row("Au(A)")$euromed_status_source, "r1|r2")
  expect_equal(row("Au(A)")$iso2, "AT")
  expect_equal(row("Ge")$euromed_status, "extinct")
  expect_equal(row("Hs")$euromed_status, "doubtful")
  expect_true(is.na(row("Hs")$iso2))
  expect_equal(row("Ga(F)")$euromed_status_detail, "endemic")
  expect_equal(unique(out$taxon_id), "uuid-1")
})

test_that("the reference table resolves every cited id", {
  out  <- suppressMessages(parse_euromed_distribution(snapshot()))
  refs <- attr(out, "references")
  cited <- unique(unlist(strsplit(stats::na.omit(out$euromed_status_source),
                                  "|", fixed = TRUE)))
  expect_setequal(refs$ref_id, cited)
  expect_equal(refs$citation[refs$ref_id == "r1"], "cit r1")
})
