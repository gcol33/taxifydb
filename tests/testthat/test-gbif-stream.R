# The streaming GBIF build replaces read.delim() over the whole file with
# chunked fread() calls, and swaps a whole-table self-join for a lookup built
# from the referenced ids alone. Both substitutions are parser-level: \N is the
# NULL marker, quoting is off, there is no header row, and the rows the *_key
# columns point at are dropped by the same filter that runs per chunk. An
# equivalence test against read_gbif() on a GBIF-shaped file is what holds them
# honest, since a difference in how an empty field or a \N is read would
# otherwise surface only in a rebuilt 6.4M-row backbone.

# simple.txt is 30 positional columns with no header. Build a row by name so
# the fixture stays readable and the column order stays the source's.
#
# Every column of the published gbif.vtr is character, `year` and `bracket_year`
# included, so the fixture populates the passthrough columns the way the real
# file does -- `{}` for the two array-valued ones, a non-numeric year among the
# numeric ones. A fixture leaving them all \N would have read.delim() infer
# logical and integer, and the equivalence test would then be measuring the
# fixture rather than the 6.4M-row file it stands in for.
gbif_row <- function(id, rank, canonical_name, ...) {
  row <- stats::setNames(
    as.list(rep("\\N", length(taxifydb:::.gbif_col_names))),
    taxifydb:::.gbif_col_names
  )
  row$id <- as.character(id)
  row$rank <- rank
  row$canonical_name <- canonical_name
  row$is_synonym <- "f"
  row$status <- "ACCEPTED"
  row$nom_status <- "{}"
  row$issues <- "{}"
  row$origin <- "SOURCE"
  overrides <- list(...)
  row[names(overrides)] <- overrides
  unlist(row, use.names = FALSE)
}

gbif_fixture <- function() {
  rows <- list(
    # The four ranks whose rows are dropped after their names are read off.
    gbif_row(1, "KINGDOM", "Plantae", kingdom_key = "1"),
    gbif_row(2, "PHYLUM", "Tracheophyta", kingdom_key = "1", phylum_key = "2"),
    gbif_row(3, "CLASS", "Magnoliopsida", kingdom_key = "1", phylum_key = "2",
             class_key = "3"),
    gbif_row(4, "ORDER", "Fagales", kingdom_key = "1", phylum_key = "2",
             class_key = "3", order_key = "4"),
    gbif_row(5, "FAMILY", "Fagaceae", kingdom_key = "1", phylum_key = "2",
             class_key = "3", order_key = "4", family_key = "5"),
    gbif_row(6, "GENUS", "Quercus", kingdom_key = "1", phylum_key = "2",
             class_key = "3", order_key = "4", family_key = "5",
             genus_or_above = "Quercus"),
    gbif_row(7, "SPECIES", "Quercus robur", kingdom_key = "1",
             phylum_key = "2", class_key = "3", order_key = "4",
             family_key = "5", genus_or_above = "Quercus",
             specific_epithet = "robur", authorship = "L.", year = "1753",
             notho_type = "SPECIFIC", name_published_in = "Sp. Pl. 2: 996"),
    # A synonym: parent_key carries the accepted id, not acceptedNameUsageID.
    # Its year is non-numeric, which is why the real column is character.
    gbif_row(8, "SPECIES", "Quercus pedunculata", is_synonym = "t",
             status = "HETEROTYPIC_SYNONYM", parent_key = "7",
             kingdom_key = "1", phylum_key = "2", class_key = "3",
             order_key = "4", family_key = "5", genus_or_above = "Quercus",
             specific_epithet = "pedunculata", authorship = "Ehrh.",
             year = "1789?", bracket_authorship = "Michx.",
             bracket_year = "1801"),
    # An empty authorship field rather than \N, to pin how a blank is read.
    gbif_row(9, "SPECIES", "Quercus racemosa", kingdom_key = "1",
             family_key = "5", genus_or_above = "Quercus",
             specific_epithet = "racemosa", authorship = "",
             year = "1801", bracket_year = "1832?"),
    # Filtered: UNRANKED, and an empty canonical name.
    gbif_row(10, "UNRANKED", "Incertae sedis", kingdom_key = "1"),
    gbif_row(11, "SPECIES", "", kingdom_key = "1", family_key = "5"),
    # A key pointing at an id no row carries stays unresolved.
    gbif_row(12, "SPECIES", "Orphan name", kingdom_key = "1",
             family_key = "999", genus_or_above = "Orphanus",
             specific_epithet = "name")
  )
  vapply(rows, paste, character(1), collapse = "\t")
}

write_gbif_fixture <- function(dir) {
  txt <- file.path(dir, "simple.txt")
  writeLines(gbif_fixture(), txt)
  gz <- file.path(dir, "simple.txt.gz")
  raw <- readBin(txt, "raw", file.size(txt))
  con <- gzfile(gz, "wb")
  writeBin(raw, con)
  close(con)
  list(txt = txt, gz = gz)
}

# The streamed half of build_gbif(), without the download.
stream_gbif <- function(txt, chunk_rows) {
  higher <- taxifydb::delim_fk_lookup(
    txt, id_col = "id", value_col = "canonical_name",
    key_cols = c("kingdom_key", "phylum_key", "class_key", "order_key",
                 "family_key"),
    quote = "", na_strings = "\\N",
    col_names = taxifydb:::.gbif_col_names, verbose = FALSE
  )
  feed <- taxifydb::delim_chunk_feed(
    txt,
    normalize = function(chunk) {
      taxifydb::normalize_gbif(chunk, higher, verbose = FALSE)
    },
    chunk_rows = chunk_rows, quote = "", na_strings = "\\N",
    col_names = taxifydb:::.gbif_col_names, verbose = FALSE
  )
  parts <- list()
  repeat {
    chunk <- feed()
    if (is.null(chunk)) break
    parts[[length(parts) + 1L]] <- chunk
  }
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out
}


test_that("the streamed GBIF read matches read_gbif() column for column", {
  skip_if_not_installed("withr")
  skip_if_not_installed("data.table")
  dir <- withr::local_tempdir()
  paths <- write_gbif_fixture(dir)

  direct <- read_gbif(paths$gz, verbose = FALSE)
  rownames(direct) <- NULL
  streamed <- stream_gbif(paths$txt, chunk_rows = 100L)

  expect_equal(names(streamed), names(direct))
  expect_equal(streamed, direct)
})


test_that("chunking does not change the streamed GBIF read", {
  skip_if_not_installed("withr")
  skip_if_not_installed("data.table")
  dir <- withr::local_tempdir()
  paths <- write_gbif_fixture(dir)

  # 3 rows per chunk splits the dropped ranks across block boundaries.
  expect_equal(stream_gbif(paths$txt, chunk_rows = 3L),
               stream_gbif(paths$txt, chunk_rows = 100L))
})


test_that("the higher classification survives the ranks being dropped", {
  skip_if_not_installed("withr")
  skip_if_not_installed("data.table")
  dir <- withr::local_tempdir()
  paths <- write_gbif_fixture(dir)

  streamed <- stream_gbif(paths$txt, chunk_rows = 2L)
  robur <- streamed[streamed$taxon_id == "7", ]

  expect_equal(robur$kingdom, "Plantae")
  expect_equal(robur$phylum, "Tracheophyta")
  expect_equal(robur$class, "Magnoliopsida")
  expect_equal(robur$order, "Fagales")
  expect_equal(robur$family, "Fagaceae")

  # The KINGDOM/PHYLUM/CLASS/ORDER rows themselves are gone.
  expect_false(any(streamed$taxon_id %in% c("1", "2", "3", "4")))
  # FAMILY and GENUS rows stay.
  expect_true(all(c("5", "6") %in% streamed$taxon_id))
})


test_that("a key naming no row resolves to NA rather than to another name", {
  skip_if_not_installed("withr")
  skip_if_not_installed("data.table")
  dir <- withr::local_tempdir()
  paths <- write_gbif_fixture(dir)

  streamed <- stream_gbif(paths$txt, chunk_rows = 100L)
  orphan <- streamed[streamed$taxon_id == "12", ]
  expect_true(is.na(orphan$family))
  expect_equal(orphan$kingdom, "Plantae")
})


test_that("the lookup covers only the ids the keys reference", {
  skip_if_not_installed("withr")
  skip_if_not_installed("data.table")
  dir <- withr::local_tempdir()
  paths <- write_gbif_fixture(dir)

  higher <- taxifydb::delim_fk_lookup(
    paths$txt, id_col = "id", value_col = "canonical_name",
    key_cols = c("kingdom_key", "phylum_key", "class_key", "order_key",
                 "family_key"),
    quote = "", na_strings = "\\N",
    col_names = taxifydb:::.gbif_col_names, verbose = FALSE
  )

  # Referenced: 1, 2, 3, 4, 5 (and 999, which no row carries).
  expect_setequal(names(higher), c("1", "2", "3", "4", "5"))
  expect_equal(unname(higher[["5"]]), "Fagaceae")
  # Species rows are never referenced, so they never enter the lookup.
  expect_false("7" %in% names(higher))
})


test_that("a block filtered down to nothing does not end the build", {
  skip_if_not_installed("withr")
  skip_if_not_installed("data.table")
  dir <- withr::local_tempdir()

  # Two real rows, then a block of only-dropped rows, then two more. Reading a
  # zero-row block as end-of-feed would silently lose the tail.
  emitted <- list(
    data.frame(taxon_id = "1", canonical_name = "Aa aa", taxon_rank = "SPECIES",
               taxonomic_status = "ACCEPTED",
               accepted_name_usage_id = NA_character_, family = "Fa",
               genus = "Aa", specific_epithet = "aa", authorship = NA_character_,
               infraspecific_epithet = NA_character_,
               stringsAsFactors = FALSE),
    data.frame(taxon_id = character(0), canonical_name = character(0),
               taxon_rank = character(0), taxonomic_status = character(0),
               accepted_name_usage_id = character(0), family = character(0),
               genus = character(0), specific_epithet = character(0),
               authorship = character(0), infraspecific_epithet = character(0),
               stringsAsFactors = FALSE),
    data.frame(taxon_id = "2", canonical_name = "Bb bb", taxon_rank = "SPECIES",
               taxonomic_status = "ACCEPTED",
               accepted_name_usage_id = NA_character_, family = "Fb",
               genus = "Bb", specific_epithet = "bb", authorship = NA_character_,
               infraspecific_epithet = NA_character_,
               stringsAsFactors = FALSE)
  )
  i <- 0L
  feed <- function() {
    i <<- i + 1L
    if (i > length(emitted)) return(NULL)
    emitted[[i]]
  }

  path <- file.path(dir, "gapped.vtr")
  build_vtr_streamed(feed, path, "test", "1.0", "http://example",
                     verbose = FALSE)
  got <- vectra::collect(vectra::tbl(path))
  expect_equal(nrow(got), 2L)
  expect_setequal(got$taxon_id, c("1", "2"))
})
