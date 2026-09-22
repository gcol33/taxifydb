# GBIF occurrence counts: the key selection, the batched count, and the
# n_occurrences column the GBIF build carries.

raw_gbif_rows <- function() {
  n <- 4L
  df <- as.data.frame(
    stats::setNames(rep(list(rep(NA_character_, n)),
                        length(taxifydb:::.gbif_col_names)),
                    taxifydb:::.gbif_col_names),
    stringsAsFactors = FALSE)
  df$id             <- c("3875069", "11399973", "5338804", "8078486")
  df$parent_key     <- c(NA, NA, NA, "5338866")
  df$is_synonym     <- c("f", "f", "f", "t")
  df$status         <- c("ACCEPTED", "DOUBTFUL", "ACCEPTED",
                         "HETEROTYPIC_SYNONYM")
  df$rank           <- rep("SPECIES", n)
  df$canonical_name <- c("Karwinskia mollis", "Karwinskia mollis",
                         "Houstonia pusilla", "Houstonia pusilla")
  df$scientific_name <- paste(df$canonical_name,
                              c("Schltdl.", "Standl.", "Schoepf", "J.F.Gmel."))
  df$genus_or_above <- c("Karwinskia", "Karwinskia", "Houstonia", "Houstonia")
  df$specific_epithet <- c("mollis", "mollis", "pusilla", "pusilla")
  df$authorship     <- c("Schltdl.", "Standl.", "Schoepf", "J.F.Gmel.")
  df
}

test_that("normalize_gbif() keeps DOUBTFUL and attaches the snapshot counts", {
  counts <- c("3875069" = 663, "11399973" = 0, "8078486" = 9)
  out <- taxifydb::normalize_gbif(raw_gbif_rows(), character(0),
                                  counts = counts, verbose = FALSE)
  expect_equal(out$taxonomic_status,
               c("ACCEPTED", "DOUBTFUL", "ACCEPTED", "SYNONYM"))
  # A key the snapshot does not hold is NA, not zero.
  expect_equal(out$n_occurrences, c(663, 0, NA, 9))
})

test_that("normalize_gbif() without counts adds no column", {
  out <- taxifydb::normalize_gbif(raw_gbif_rows(), character(0),
                                  verbose = FALSE)
  expect_false("n_occurrences" %in% names(out))
})

test_that("gbif_count_keys() selects colliding keys and their accepted targets", {
  skip_if_not_installed("withr")
  df <- data.frame(
    taxon_id          = c("1", "2", "3", "4", "5", "6"),
    key_ci            = c("aus bus", "aus bus", "cus dus", "cus dus",
                          "cus dusa", "eus fus"),
    key_normalized    = c("aus bus", "aus bus", "cus dus", "cus dus",
                          "cus dus", "eus fus"),
    accepted_taxon_id = c("1", "2", "3", "3", "9", "6"),
    stringsAsFactors  = FALSE
  )
  path <- withr::local_tempfile(fileext = ".vtr")
  vectra::write_vtr(df, path)
  # aus bus: two accepted taxa. cus dus: one accepted taxon under key_ci, but
  # key_normalized also folds in row 5 (-> 9), so rows 3-5 and target 9 count.
  # eus fus: a single record.
  expect_setequal(taxifydb::gbif_count_keys(path),
                  c("1", "2", "3", "4", "5", "9"))
})

test_that("count_gbif_occurrences() batches keys and re-counts a full facet", {
  calls <- character(0)
  local_mocked_bindings(
    .gbif_get_json = function(url, tries = 5L) {
      calls <<- c(calls, url)
      if (!grepl("facet=", url)) {
        return(list(count = 7))
      }
      keys <- regmatches(url, gregexpr("taxonKey=[0-9]+", url))[[1L]]
      keys <- sub("taxonKey=", "", keys)
      if ("30" %in% keys) {
        # A facet at its limit: the per-key recount must replace it.
        f <- data.frame(name = c("99", "98"), count = c(5, 4))
      } else {
        f <- data.frame(name = keys[keys != "11"], count = 3)
      }
      list(facets = data.frame(counts = I(list(f))))
    },
    .package = "taxifydb"
  )
  out <- taxifydb::count_gbif_occurrences(c("10", "11", "30", "31"),
                                          batch_size = 2L, facet_limit = 2L,
                                          verbose = FALSE)
  expect_equal(out$taxon_id, c("10", "11", "30", "31"))
  # 11 is absent from a facet that was not full: zero records.
  expect_equal(out$n_occurrences, c(3, 0, 7, 7))
  expect_equal(sum(!grepl("facet=", calls)), 2L)
})

test_that("an occurrence-count snapshot round-trips through the reader", {
  skip_if_not_installed("withr")
  path <- withr::local_tempfile(fileext = ".tsv.gz")
  con <- gzfile(path, "w")
  utils::write.table(data.frame(taxon_id = c("3875069", "11399973"),
                                n_occurrences = c(663, 0)),
                     con, sep = "\t", quote = FALSE, row.names = FALSE)
  close(con)
  expect_equal(taxifydb::read_gbif_occurrence_counts(path),
               c("3875069" = 663, "11399973" = 0))
})
