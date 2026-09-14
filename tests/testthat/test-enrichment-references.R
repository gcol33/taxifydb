# Per-value provenance: a `<col>_source` cell names the references behind the
# value an enrichment reports, and the reference table those ids resolve against
# travels with the enrichment. The value columns themselves must come out of a
# build exactly as they would without provenance.

test_that(".value_refs keeps the records that state a categorical value", {
  got <- .value_refs(
    group    = c("A", "A", "A", "B", "B"),
    value    = c("wind", "wind", "animal", "water", NA),
    ref      = c("r2", "r1", "r3", "r4", "r5"),
    reported = c("wind", "water"),
    keys     = c("A", "B"),
    type     = "cat")
  expect_equal(got, c("r1|r2", "r4"))
})

test_that(".value_refs keeps every record a median aggregates", {
  got <- .value_refs(
    group    = c("A", "A", "A", "B", "C"),
    value    = c("1", "3", "x", "2", NA),
    ref      = c("r1", "r2", "r3", "r1", "r9"),
    reported = c(2, 2, NA),
    keys     = c("A", "B", "C"),
    type     = "num")
  expect_equal(got, c("r1|r2", "r1", NA_character_))
})

test_that(".value_refs under min keeps only the records naming the minimum", {
  got <- .value_refs(c("A", "A", "A"), c("10", "20", "10"), c("r1", "r2", "r3"),
                     reported = 10, keys = "A", type = "num", reduce = "min")
  expect_equal(got, "r1|r3")
})

test_that(".value_refs under join keeps every record with a value", {
  got <- .value_refs(c("A", "A", "A"), c("x", "y", ""), c("r1", "r2", "r3"),
                     reported = "x; y", keys = "A", type = "cat",
                     reduce = "join")
  expect_equal(got, "r1|r2")
})

test_that("a pivot with references reports the same values as one without", {
  long <- data.frame(
    name  = c("Acacia a", "Acacia a", "Acacia a", "Acacia a", "Banksia b"),
    trait = c("disp", "disp", "disp", "height", "height"),
    value = c("wind", " wind", "ant", "2", "5"),
    ref   = c("D2", "D1", "D3", "D1", "D4"),
    stringsAsFactors = FALSE)
  spec <- list(dispersal = list(trait = "disp", type = "cat"),
               height_m  = list(trait = "height", type = "num"))
  with_refs <- .pivot_species_traits(long, spec)
  plain     <- .pivot_species_traits(long[c("name", "trait", "value")], spec)

  expect_identical(with_refs[names(plain)], plain)
  expect_setequal(setdiff(names(with_refs), names(plain)),
                  c("dispersal_source", "height_m_source"))
  expect_equal(with_refs$dispersal_source, c("D1|D2", NA))
  expect_equal(with_refs$height_m_source, c("D1", "D4"))
})

test_that(".reducer_ignoring picks the rows the plain reducer picks", {
  expanded <- data.frame(
    canonical_name = c("X y", "X y", "Z w"),
    a = c(1, NA, 3), a_min = c(NA, 1, NA), a_max = c(NA, 1, NA),
    b = c(NA, "p", "q"),
    a_source = c("r1", NA, "r2"), b_source = c(NA, "r3", "r4"),
    stringsAsFactors = FALSE)
  prov <- .reference_cols(names(expanded))
  expect_setequal(prov, c("a_source", "b_source"))

  plain <- .dedup_keep_richest(expanded[setdiff(names(expanded), prov)])
  wrapped <- .reducer_ignoring(.dedup_keep_richest, prov)(expanded, NULL)
  expect_identical(wrapped[names(plain)], plain)
  expect_equal(wrapped$b_source, c("r3", "r4"))
})

test_that(".gift_ref_cells drops the bias sign and reports the flagged ids", {
  got <- .gift_ref_cells(c("10280,-10321,10598", NA, "272"))
  expect_equal(got$cell, c("10280|10321|10598", NA, "272"))
  expect_equal(got$negative, "10321")
})

test_that(".gift_ref_cells builds each cell as .ref_join would row by row", {
  set.seed(84)
  ids <- c("272", "10255", "-10321", "10598", " 7", "", "-")
  x <- vapply(1:500, function(i) {
    if (i %% 7 == 0) return(NA_character_)
    paste(sample(ids, sample(1:4, 1), replace = TRUE), collapse = ",")
  }, character(1L))
  rowwise <- vapply(strsplit(ifelse(is.na(x), "", x), ",", fixed = TRUE),
                    function(p) .ref_join(sub("^-", "", trimws(p))),
                    character(1L))
  expect_identical(.gift_ref_cells(x)$cell, rowwise)
  expect_setequal(.gift_ref_cells(x)$negative, c("10321", ""))
})

test_that(".extract_doi finds a DOI in citation text", {
  expect_equal(
    .extract_doi(c("Nature 491 (2012). doi: [10.1038/nature11688](https://x).",
                   "Bosque 37(2). doi: 10.4067/S0717-92002016000200011.",
                   "no identifier", NA)),
    c("10.1038/nature11688", "10.4067/S0717-92002016000200011", NA, NA))
})

test_that(".strip_markdown drops link and emphasis markup", {
  expect_equal(
    .strip_markdown("A. B.  _Flora of Australia_. doi:  [10.1/x](https://doi.org/10.1/x)."),
    "A. B. Flora of Australia. doi: 10.1/x.")
  expect_equal(.strip_markdown("file_name_v2 stays"), "file_name_v2 stays")
})

test_that("the writer publishes the reference table and declares it", {
  dir <- withr::local_tempdir()
  df <- data.frame(canonical_name = c("Acacia a", "Banksia b"),
                   dispersal = c("wind", "ant"),
                   dispersal_source = c("D1|D2", "D3"),
                   stringsAsFactors = FALSE)
  refs <- data.frame(ref_id = c("D1", "D2", "D3", "unused"),
                     citation = c("One (2001)", "Two (2002)", "Three (2003)",
                                  "Four (2004)"),
                     doi = c("10.1/a", NA, NA, NA), stringsAsFactors = FALSE)
  vtr <- file.path(dir, "demo.vtr")
  suppressMessages(build_enrichment_vtr(df, vtr, name = "demo",
                                        version = "1", source_url = "https://example.org/d",
                                        references = refs))
  ref_path <- file.path(dir, "demo_references.vtr")
  expect_true(file.exists(ref_path))
  got <- as.data.frame(vectra::collect(vectra::tbl(ref_path)))
  expect_equal(got$ref_id, c("D1", "D2", "D3"))
  expect_equal(got$citation[got$ref_id == "D1"], "One (2001)")

  meta <- jsonlite::read_json(file.path(dir, "meta.json"), simplifyVector = TRUE)
  expect_equal(meta$references$file, "demo_references.vtr")
  expect_equal(meta$references$nrow, 3L)
  expect_equal(meta$references$content_id, unname(tools::md5sum(ref_path)))
  expect_true("dispersal_source" %in% meta$trait_cols)
})

test_that("the writer refuses a provenance id missing from the table", {
  dir <- withr::local_tempdir()
  df <- data.frame(canonical_name = "Acacia a", dispersal = "wind",
                   dispersal_source = "D1|D9", stringsAsFactors = FALSE)
  refs <- data.frame(ref_id = "D1", citation = "One", doi = NA)
  expect_error(
    suppressMessages(build_enrichment_vtr(df, file.path(dir, "demo.vtr"),
                                          name = "demo", version = "1",
                                          source_url = "https://example.org/d",
                                          references = refs)),
    "D9")
})

test_that("attach_references folds a reference listed once per scope", {
  df <- attach_references(data.frame(), data.frame(
    ref_id = c("10193", "10193", "272"),
    citation = c("WCSP (2014)", "WCSP (2014)", "Linhart (1980)"),
    doi = NA_character_, stringsAsFactors = FALSE))
  expect_equal(attr(df, "references")$ref_id, c("10193", "272"))
})

test_that("attach_references rejects duplicate ids and the delimiter", {
  expect_error(attach_references(data.frame(),
                                 data.frame(ref_id = c("a", "a"),
                                            citation = c("x", "y"), doi = NA)),
               "not unique")
  expect_error(attach_references(data.frame(),
                                 data.frame(ref_id = "a|b", citation = "x",
                                            doi = NA)),
               "delimiter")
})

test_that("an enrichment release uploads the reference table with its .vtr", {
  dir <- withr::local_tempdir()
  vtr <- file.path(dir, "demo.vtr")
  writeLines("x", vtr)
  writeLines("x", file.path(dir, "demo_references.vtr"))
  jsonlite::write_json(list(name = "demo",
                            references = list(file = "demo_references.vtr")),
                       file.path(dir, "meta.json"), auto_unbox = TRUE)
  expect_equal(basename(.with_reference_tables(vtr)),
               c("demo.vtr", "demo_references.vtr"))

  other <- file.path(withr::local_tempdir(), "plain.vtr")
  writeLines("x", other)
  expect_equal(.with_reference_tables(other), other)
})

test_that("the runtime manifest records the reference table, and a build without one clears it", {
  dir <- withr::local_tempdir()
  df <- data.frame(canonical_name = "Acacia a", dispersal = "wind",
                   dispersal_source = "D1", stringsAsFactors = FALSE)
  refs <- data.frame(ref_id = "D1", citation = "One", doi = NA)
  vtr <- file.path(dir, "demo.vtr")
  suppressMessages(build_enrichment_vtr(df, vtr, name = "demo", version = "1",
                                        source_url = "https://example.org/d",
                                        references = refs))
  mf <- file.path(dir, "manifest.json")
  suppressMessages(update_enrichment_manifest(mf, "demo", vtr,
                                              release_version = "2026.09",
                                              runtime = TRUE))
  got <- jsonlite::read_json(mf, simplifyVector = FALSE)$enrichments$demo
  cid <- unname(tools::md5sum(file.path(dir, "demo_references.vtr")))
  base <- "https://github.com/gcol33/taxifydb/releases/download/enrichment-2026.09"
  expect_equal(got$references$url, paste0(base, "/demo_references.vtr"))
  expect_equal(got$references$content_id, cid)
  expect_equal(got$references$content_url,
               sprintf("%s/demo_references-%s.vtr", base, cid))
  expect_equal(got$references$nrow, 1L)

  suppressMessages(build_enrichment_vtr(df[c("canonical_name", "dispersal")], vtr,
                                        name = "demo", version = "1",
                                        source_url = "https://example.org/d"))
  suppressMessages(update_enrichment_manifest(mf, "demo", vtr,
                                              release_version = "2026.09",
                                              runtime = TRUE))
  got <- jsonlite::read_json(mf, simplifyVector = FALSE)$enrichments$demo
  expect_null(got$references)
})

test_that("parse_brot resolves every record's SourceID to BROT's sources file", {
  dir <- withr::local_tempdir()
  dat <- file.path(dir, "brot.csv")
  writeLines(c(
    '"ID","TaxonID","Taxon","Trait","Data","Units","DataType","Method","SourceID"',
    '"a1",1,"Pinus halepensis","DispMode","W","[9]","categorical","compilation","Consensus2017"',
    '"a2",1,"Pinus halepensis","DispMode","W","[9]","categorical","compilation","Tutin1964"',
    '"a3",1,"Pinus halepensis","DispMode","Z","[9]","categorical","compilation","Other2000"',
    '"a4",1,"Pinus halepensis","SeedMass","22","mg","numeric","measure","Nathan1999a"',
    '"a5",1,"Pinus halepensis","SeedMass","15","mg","numeric","measure","Tutin1964"'
  ), dat)
  writeLines(c(
    '"ID","FullSource"',
    '"Consensus2017","BROT consensus (2017)."',
    '"Tutin1964","Tutin, T. G. (1964). Flora Europaea."',
    '"Other2000","Other (2000). doi: 10.1000/xyz"',
    '"Nathan1999a","Nathan, R. (1999)."'
  ), file.path(dir, "brot_sources.csv"))

  out <- parse_brot(dat)
  expect_equal(out$disp_mode, "W")
  expect_equal(out$disp_mode_source, "Consensus2017|Tutin1964")
  expect_equal(out$seed_mass_mg, 18.5)
  expect_equal(out$seed_mass_mg_source, "Nathan1999a|Tutin1964")
  refs <- attr(out, "references")
  expect_equal(refs$doi[refs$ref_id == "Other2000"], "10.1000/xyz")
})

test_that("parse_austraits cites each dataset by its primary source", {
  dir <- withr::local_tempdir()
  writeLines(c(
    "dataset_id,taxon_name,observation_id,trait_name,value,unit",
    "ABRS_1981,Acacia alata,1,dispersal_syndrome,myrmecochory,",
    "Smith_2005,Acacia alata,2,dispersal_syndrome,myrmecochory,",
    "Jones_2010,Acacia alata,3,dispersal_syndrome,anemochory,",
    "Smith_2005,Acacia alata,4,plant_height,2,m"
  ), file.path(dir, "traits.csv"))
  writeLines(c(
    "dataset_id,trait_name,source_primary_key,source_primary_citation,source_secondary_citation",
    'ABRS_1981,dispersal_syndrome,ABRS_1981,"B. Barlow. _Flora of Australia_. 1981.",',
    'Smith_2005,dispersal_syndrome,Smith_2005,"A. Smith. Seeds. doi:  [10.1234/abc](https://doi.org/10.1234/abc).",',
    'Jones_2010,dispersal_syndrome,Jones_2010,"B. Jones. Wind.",'
  ), file.path(dir, "methods.csv"))
  writeLines(c("@Online{ABRS_1981,", "  title = {{Flora}},", "}",
               "@Article{Jones_2010,", "  doi = {10.2/jones},", "}"),
             file.path(dir, "sources.bib"))

  out <- suppressWarnings(parse_austraits(dir))
  expect_equal(out$dispersal_syndrome, "myrmecochory")
  expect_equal(out$dispersal_syndrome_source, "ABRS_1981|Smith_2005")
  expect_equal(out$plant_height_m_source, "Smith_2005")
  refs <- attr(out, "references")
  expect_equal(refs$citation[refs$ref_id == "ABRS_1981"],
               "B. Barlow. Flora of Australia. 1981.")
  expect_equal(refs$doi[refs$ref_id == "Smith_2005"], "10.1234/abc")
  expect_equal(refs$doi[refs$ref_id == "Jones_2010"], "10.2/jones")
})

test_that("build_enrichment carries the parser's reference table to the writer", {
  dir <- withr::local_tempdir()
  parsed <- attach_references(
    data.frame(canonical_name = c("Acacia a", "Acacia a"),
               dispersal = c(NA, "wind"),
               dispersal_source = c(NA, "D1"), stringsAsFactors = FALSE),
    data.frame(ref_id = "D1", citation = "One", doi = NA))
  reg <- list(
    source_url = "https://example.org/d", version = "1", license = "CC0",
    download_fn = function(url, dest) dest,
    parse_fn = function(path) parsed,
    requires = character(0))
  local_mocked_bindings(.enrichment_build_registry = list(demo = reg),
                        probe_upstream_identity = function(url) list())
  suppressMessages(build_enrichment("demo", output_dir = dir,
                                    resolve_names = FALSE, verbose = FALSE))
  expect_true(file.exists(file.path(dir, "demo_references.vtr")))
  meta <- jsonlite::read_json(file.path(dir, "meta.json"), simplifyVector = TRUE)
  expect_equal(meta$references$nrow, 1L)
})
