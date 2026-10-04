test_that("aggregate rows rejoin after a reducer that adds a column", {
  df <- data.frame(
    canonical_name = c("Abaeis nicippe", "Rubus fruticosus agg."),
    lang = "en",
    common_name = c("Sleepy Orange", "Bramble"),
    n_support = 1L, n_spelling = 1L,
    stringsAsFactors = FALSE
  )
  local_mocked_bindings(
    .resolve_species_names = function(df, group_cols, ..., reduce_fn) {
      reduce_fn(df, group_cols)
    }
  )
  out <- resolve_enrichment_names(df, group_cols = "lang", backends = "col",
                                  reduce_fn = .reduce_common_names)
  expect_equal(nrow(out), 2L)
  expect_true(all(out$name_rank == 1L))
  expect_true(any(grepl("aggr\\.$", out$canonical_name)))
})
