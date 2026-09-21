# A grouped enrichment carrying an authorship column keys each (name, region)
# row on the name's own concept wherever the source states it (#58). taxify's
# homonym guard keeps only the rows whose authorship matches the caller's
# accepted authorship, so a re-keyed concept's row left at such a key drops a
# range the source does give the name.

own_concept_lookups <- function() {
  fake <- function(key_ci, accepted_name) {
    data.frame(key_ci = key_ci, accepted_name = accepted_name,
               kingdom = NA_character_, n_species = 1L,
               stringsAsFactors = FALSE)
  }
  dir <- tempfile("lookups")
  dir.create(dir, recursive = TRUE)
  spec <- list(
    wcvp = fake(
      c("eucalyptus globulus", "eucalyptus bicostata",
        "eucalyptus pseudoglobulus", "sequoiadendron giganteum",
        "sequoia wellingtonia", "quercus robur", "quercus broteroana",
        "erigeron pulchellus"),
      c("Eucalyptus globulus", "Eucalyptus bicostata",
        "Eucalyptus pseudoglobulus", "Sequoiadendron giganteum",
        "Sequoia wellingtonia", "Quercus robur", "Quercus broteroana",
        "Erigeron pulchellus")),
    col = fake(
      c("eucalyptus globulus", "eucalyptus bicostata",
        "eucalyptus pseudoglobulus", "sequoiadendron giganteum",
        "sequoia wellingtonia", "quercus robur", "quercus broteroana",
        "erigeron pulchellus"),
      c("Eucalyptus globulus", "Eucalyptus globulus", "Eucalyptus globulus",
        "Sequoiadendron giganteum", "Sequoiadendron giganteum",
        "Quercus robur", "Quercus robur", "Erigeron pulchellus"))
  )
  paths <- character()
  for (nm in names(spec)) {
    p <- file.path(dir, sprintf("%s_name_lookup.vtr", nm))
    vectra::write_vtr(spec[[nm]], p)
    vectra::create_index(p, "key_ci")
    vectra::create_index(p, "accepted_name")
    paths <- c(paths, stats::setNames(p, nm))
  }
  paths
}

# One row per WCVP concept x region. The re-keyed concepts carry one more
# populated column than the name's own concept, so the trait-richest reducer
# prefers them at any key they share.
wcvp_rows <- function() {
  row <- function(name, code, status, authors, extra = NA_character_) {
    data.frame(canonical_name = name, tdwg_code = code, native_status = status,
               taxon_authors = authors, nomenclatural_remarks = extra,
               stringsAsFactors = FALSE)
  }
  rbind(
    row("Eucalyptus globulus", c("TAS", "VIC", "CAL"),
        c("native", "native", "introduced"), "Labill."),
    row("Eucalyptus bicostata", c("VIC", "NSW"), "native",
        "Maiden, Blakely & Simmonds", "nom. cons."),
    row("Eucalyptus pseudoglobulus", "TAS", "native", "(Boland) D.Nicolle",
        "nom. cons."),
    row("Sequoiadendron giganteum", c("CAL", "GER"),
        c("native", "introduced"), "(Lindl.) J.Buchholz"),
    row("Sequoia wellingtonia", "CAL", "native", "(D.Don) Poit.",
        "nom. illeg."),
    row("Quercus robur", c("GER", "POR", "SPA"), "native", "L."),
    row("Quercus broteroana", c("POR", "SPA"), "native", "O.Schwarz",
        "nom. cons."),
    row("Erigeron pulchellus", c("NCA", "SCA"), "native", "Michx."),
    row("Erigeron pulchellus", c("AUT", "SWI"), "native",
        "Hoppe & Hornsch. ex Bluff & Fingerh.")
  )
}

resolve_wcvp_rows <- function(df = wcvp_rows()) {
  paths <- own_concept_lookups()
  local_mocked_bindings(.find_lookup_paths = function(backends) paths,
                        .package = "taxifydb", .env = parent.frame())
  resolve_enrichment_names(df, group_cols = "tdwg_code",
                           backends = names(paths), verbose = FALSE)
}

authors_at <- function(out, name, code) {
  out$taxon_authors[out$canonical_name == name & out$tdwg_code == code]
}

test_that("Eucalyptus globulus keeps its own authorship in TAS and VIC", {
  out <- resolve_wcvp_rows()
  expect_equal(authors_at(out, "Eucalyptus globulus", "TAS"), "Labill.")
  expect_equal(authors_at(out, "Eucalyptus globulus", "VIC"), "Labill.")
  expect_equal(authors_at(out, "Eucalyptus globulus", "CAL"), "Labill.")
})

test_that("Sequoiadendron giganteum keeps its own authorship in CAL", {
  out <- resolve_wcvp_rows()
  expect_equal(authors_at(out, "Sequoiadendron giganteum", "CAL"),
               "(Lindl.) J.Buchholz")
})

test_that("Quercus robur keeps its own authorship in POR, SPA and GER", {
  out <- resolve_wcvp_rows()
  for (code in c("POR", "SPA", "GER")) {
    expect_equal(authors_at(out, "Quercus robur", code), "L.")
  }
})

test_that("a re-keyed concept still fills a region the own concept lacks", {
  out <- resolve_wcvp_rows()
  expect_equal(authors_at(out, "Eucalyptus globulus", "NSW"),
               "Maiden, Blakely & Simmonds")
})

test_that("a concept keeps its own rows under its own name", {
  out <- resolve_wcvp_rows()
  expect_equal(authors_at(out, "Eucalyptus bicostata", "VIC"),
               "Maiden, Blakely & Simmonds")
  expect_equal(authors_at(out, "Quercus broteroana", "SPA"), "O.Schwarz")
})

test_that("a homonym in the source keeps both concepts apart", {
  out <- resolve_wcvp_rows()
  expect_equal(authors_at(out, "Erigeron pulchellus", "NCA"), "Michx.")
  expect_equal(authors_at(out, "Erigeron pulchellus", "AUT"),
               "Hoppe & Hornsch. ex Bluff & Fingerh.")
})

test_that("without an authorship column the reducer chooses over every row", {
  df <- wcvp_rows()
  df$primary_author <- df$taxon_authors
  df$taxon_authors <- NULL
  out <- resolve_wcvp_rows(df)
  at <- out$canonical_name == "Eucalyptus globulus" & out$tdwg_code == "VIC"
  expect_equal(out$primary_author[at], "Maiden, Blakely & Simmonds")
})
