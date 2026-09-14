# The cross-backbone closure (taxifydb#44). Fixtures are three tiny name
# lookups standing in for the backbones of the Minuartia/Sabulina case, which
# is where the forward-only expansion was found to lose an accepted name.

fake_lookups <- function(spec) {
  # A plain tempdir, not withr::local_tempdir(): the paths have to outlive this
  # helper's frame, and the session tempdir is cleaned up anyway.
  dir <- tempfile("lookups")
  dir.create(dir, recursive = TRUE)
  paths <- character()
  for (nm in names(spec)) {
    p <- file.path(dir, sprintf("%s_name_lookup.vtr", nm))
    d <- spec[[nm]]
    if (!"kingdom" %in% names(d)) d$kingdom <- rep(NA_character_, nrow(d))
    if (!"n_species" %in% names(d)) d$n_species <- rep(1L, nrow(d))
    vectra::write_vtr(d, p)
    vectra::create_index(p, "key_ci")
    vectra::create_index(p, "accepted_name")
    paths <- c(paths, stats::setNames(p, nm))
  }
  paths
}

lk <- function(key_ci, accepted_name, kingdom = NA_character_, n_species = 1L) {
  data.frame(key_ci = key_ci, accepted_name = accepted_name,
             kingdom = kingdom, n_species = as.integer(n_species),
             stringsAsFactors = FALSE)
}

# wcvp and col synonymise Minuartia hybrida onto the Sabulina concept; wfo
# keeps it accepted; gbif carries one bad row sending a name of the same
# concept to an unrelated genus.
minuartia_lookups <- function() {
  fake_lookups(list(
    wcvp = lk(c("sabulina tenuifolia", "minuartia hybrida", "sabulina hybrida"),
              c("Sabulina tenuifolia", "Sabulina tenuifolia",
                "Sabulina tenuifolia")),
    wfo  = lk(c("sabulina tenuifolia", "minuartia hybrida"),
              c("Sabulina tenuifolia", "Minuartia hybrida")),
    gbif = lk(c("sabulina tenuifolia", "sabulina hybrida"),
              c("Sabulina tenuifolia", "Saponaria officinalis"))
  ))
}

test_that("the forward image alone misses an accepted name only one backbone keeps", {
  m <- taxifydb:::.name_closure_map("Sabulina tenuifolia", minuartia_lookups(),
                                    reverse_hop = FALSE, verbose = FALSE)
  expect_equal(unique(m$accepted_name), "Sabulina tenuifolia")
})

test_that("the reverse hop recovers it", {
  # wfo accepts Minuartia hybrida; the source lists only Sabulina names, so
  # nothing in the forward image ever visits it.
  m <- taxifydb:::.name_closure_map("Sabulina tenuifolia", minuartia_lookups(),
                                    reverse_hop = TRUE, verbose = FALSE)
  expect_true("Minuartia hybrida" %in% m$accepted_name)
  expect_true("Sabulina tenuifolia" %in% m$accepted_name)
  expect_equal(unique(m$input_name), "Sabulina tenuifolia")
})

test_that("the hop does not chain through a third concept", {
  # gbif sends `sabulina hybrida` to Saponaria officinalis, a genuine error
  # against two backbones that keep the name within the concept. Taking the
  # reverse-discovered names' full forward image would key soapwort's row with
  # this concept's traits; only self-accepted names are taken.
  m <- taxifydb:::.name_closure_map("Sabulina tenuifolia", minuartia_lookups(),
                                    reverse_hop = TRUE, verbose = FALSE)
  expect_false("Saponaria officinalis" %in% m$accepted_name)
})

test_that("a source name written with its subgenus resolves on the binomial (#50)", {
  paths <- fake_lookups(list(
    a = lk(c("carabus cancellatus", "carabus granulatus"),
           c("Carabus cancellatus", "Carabus granulatus"))
  ))
  m <- taxifydb:::.name_closure_map(
    c("Carabus (Tachypus) cancellatus", "Carabus  (Carabus) granulatus"),
    paths, reverse_hop = FALSE, verbose = FALSE)
  expect_setequal(m$accepted_name, c("Carabus cancellatus", "Carabus granulatus"))
  expect_setequal(m$input_name, c("Carabus (Tachypus) cancellatus",
                                  "Carabus  (Carabus) granulatus"))
})


test_that("a synonym no backbone accepts is not added", {
  paths <- fake_lookups(list(
    a = lk(c("aus bus", "old name"), c("Aus bus", "Aus bus")),
    b = lk("aus bus", "Aus bus")
  ))
  m <- taxifydb:::.name_closure_map("Aus bus", paths, reverse_hop = TRUE,
                                    verbose = FALSE)
  expect_equal(unique(m$accepted_name), "Aus bus")
})

test_that("the hop is still gated on kingdom", {
  # The reverse-discovered name is accepted in a backbone that files it in
  # another kingdom, so it is a homonym rather than the same organism.
  paths <- fake_lookups(list(
    a = lk(c("aus bus", "cus dus"), c("Aus bus", "Aus bus"),
           c("Animalia", "Animalia")),
    b = lk("cus dus", "Cus dus", "Animalia"),
    c = lk("cus dus", "Cus dus", "Plantae"),
    d = lk("cus dus", "Cus dus", "Animalia"),
    e = lk("cus dus", "Aus bus", "Animalia")
  ))
  m <- taxifydb:::.name_closure_map("Aus bus", paths, reverse_hop = TRUE,
                                    verbose = FALSE)
  # Animalia wins the vote 4-1, so the name is kept -- as the same organism.
  expect_true("Cus dus" %in% m$accepted_name)

  flipped <- fake_lookups(list(
    a = lk(c("aus bus", "cus dus"), c("Aus bus", "Aus bus"),
           c("Animalia", "Plantae")),
    b = lk("cus dus", "Cus dus", "Plantae"),
    c = lk("cus dus", "Cus dus", "Plantae")
  ))
  m2 <- taxifydb:::.name_closure_map("Aus bus", flipped, reverse_hop = TRUE,
                                     verbose = FALSE)
  expect_true("Aus bus" %in% m2$accepted_name)
})

test_that("every input keeps its own accepted names", {
  paths <- fake_lookups(list(
    a = lk(c("aus bus", "xus yus"), c("Aus bus", "Xus yus")),
    b = lk(c("aus bus", "xus yus"), c("Aus bus", "Xus yus"))
  ))
  m <- taxifydb:::.name_closure_map(c("Aus bus", "Xus yus"), paths,
                                    reverse_hop = TRUE, verbose = FALSE)
  expect_equal(m$accepted_name[m$input_name == "Aus bus"], "Aus bus")
  expect_equal(m$accepted_name[m$input_name == "Xus yus"], "Xus yus")
})

test_that("a re-routing two backbones agree on is taken", {
  # `old name` belongs to the concept (a and b synonymise it onto Aus bus) but
  # c and d both route it on to Cus dus. Two independent backbones agreeing is
  # not the shape of a data error, so a user arriving through them is served.
  paths <- fake_lookups(list(
    a = lk(c("aus bus", "old name"), c("Aus bus", "Aus bus")),
    b = lk(c("aus bus", "old name"), c("Aus bus", "Aus bus")),
    c = lk("old name", "Cus dus"),
    d = lk("old name", "Cus dus")
  ))
  m <- taxifydb:::.name_closure_map("Aus bus", paths, reverse_hop = TRUE,
                                    verbose = FALSE)
  expect_true("Cus dus" %in% m$accepted_name)
})

test_that("a re-routing only one backbone makes is not", {
  # The Saponaria shape, reduced: a lone dissenting backbone sends a name of
  # the concept somewhere else, and following it would key an unrelated taxon.
  paths <- fake_lookups(list(
    a = lk(c("aus bus", "old name"), c("Aus bus", "Aus bus")),
    b = lk(c("aus bus", "old name"), c("Aus bus", "Aus bus")),
    c = lk("old name", "Cus dus")
  ))
  m <- taxifydb:::.name_closure_map("Aus bus", paths, reverse_hop = TRUE,
                                    verbose = FALSE)
  expect_false("Cus dus" %in% m$accepted_name)
})

test_that("the hop does not cross a key one backbone gives to two species", {
  # `acacia acicularis` is two names: R.Br.'s, a synonym of Acacia brownii, and
  # Humb. & Bonpl.'s, of Vachellia farnesiana. col records both and collapses
  # the key onto farnesiana, as does ncbi; lcvp and wcvp collapse it onto
  # brownii. Two backbones agreeing on brownii is not corroboration of one
  # concept here.
  acicularis <- function(col_n) fake_lookups(list(
    col  = lk(c("vachellia farnesiana", "acacia acicularis"),
              c("Vachellia farnesiana", "Vachellia farnesiana"),
              n_species = c(1L, col_n)),
    ncbi = lk("acacia acicularis", "Vachellia farnesiana"),
    wfo  = lk("vachellia farnesiana", "Vachellia farnesiana"),
    lcvp = lk("acacia acicularis", "Acacia brownii"),
    wcvp = lk("acacia acicularis", "Acacia brownii")
  ))
  m <- taxifydb:::.name_closure_map("Vachellia farnesiana", acicularis(2L),
                                    reverse_hop = TRUE, verbose = FALSE)
  expect_equal(unique(m$accepted_name), "Vachellia farnesiana")

  # The same edges without the homonym are a re-routing two backbones agree on.
  m1 <- taxifydb:::.name_closure_map("Vachellia farnesiana", acicularis(1L),
                                     reverse_hop = TRUE, verbose = FALSE)
  expect_true("Acacia brownii" %in% m1$accepted_name)
})

test_that("the hop does not enter through a placement most backbones contradict (#56)", {
  # colxr alone files `acer negundo f. crispum` under Acer pseudoplatanus; wfo
  # and lcvp file it under Acer negundo, gbif and wcvp under its autonym. The
  # key belongs to the negundo concept, so a sycamore row must not reach the
  # Acer negundo key through it, even though two backbones agree on the exit.
  crispum <- fake_lookups(list(
    colxr = lk(c("acer pseudoplatanus", "acer negundo f. crispum"),
               c("Acer pseudoplatanus", "Acer pseudoplatanus")),
    wfo   = lk(c("acer pseudoplatanus", "acer negundo", "acer negundo f. crispum"),
               c("Acer pseudoplatanus", "Acer negundo", "Acer negundo")),
    lcvp  = lk(c("acer pseudoplatanus", "acer negundo", "acer negundo f. crispum"),
               c("Acer pseudoplatanus", "Acer negundo", "Acer negundo")),
    gbif  = lk(c("acer negundo", "acer negundo f. crispum"),
               c("Acer negundo", "Acer negundo subsp. negundo")),
    wcvp  = lk(c("acer negundo", "acer negundo f. crispum"),
               c("Acer negundo", "Acer negundo subsp. negundo"))
  ))
  src <- c("Acer pseudoplatanus", "Acer negundo")
  m <- taxifydb:::.name_closure_map(src, crispum, reverse_hop = TRUE,
                                    verbose = FALSE)
  expect_equal(unique(m$accepted_name[m$input_name == "Acer pseudoplatanus"]),
               "Acer pseudoplatanus")
  expect_true("Acer negundo" %in% m$accepted_name[m$input_name == "Acer negundo"])

  # Through the build: the negundo key carries its own year, not the sycamore's
  # earlier one.
  local_mocked_bindings(.find_lookup_paths = function(backends) crispum)
  df <- data.frame(canonical_name = src, country_code = "NL",
                   alien_first_record = c(1699L, 1809L),
                   alien_first_record_status = "present",
                   stringsAsFactors = FALSE)
  reg <- taxifydb:::.enrichment_build_registry$alien_first_records
  out <- resolve_enrichment_names(df, group_cols = "country_code",
                                  backends = names(crispum), verbose = FALSE,
                                  reduce_fn = reg$reduce_fn)
  year <- stats::setNames(out$alien_first_record, out$canonical_name)
  expect_equal(year[["Acer negundo"]], 1809L)
  expect_equal(year[["Acer pseudoplatanus"]], 1699L)
})

test_that("the hop does not reach a species the backbones keep apart (#56)", {
  # `brassica macrorhiza` is disputed: col, colxr and gbif file it under
  # Brassica napus, wfo and lcvp under Brassica rapa. The entry wins its vote,
  # but every backbone holding both species resolves them apart, so rape must
  # not be keyed with swede's rows.
  macrorhiza <- fake_lookups(list(
    col   = lk(c("brassica napus", "brassica rapa", "brassica macrorhiza"),
               c("Brassica napus", "Brassica rapa", "Brassica napus")),
    colxr = lk(c("brassica napus", "brassica rapa", "brassica macrorhiza"),
               c("Brassica napus", "Brassica rapa", "Brassica napus")),
    gbif  = lk(c("brassica napus", "brassica rapa", "brassica macrorhiza"),
               c("Brassica napus", "Brassica rapa", "Brassica napus")),
    wfo   = lk(c("brassica napus", "brassica rapa", "brassica macrorhiza"),
               c("Brassica napus", "Brassica rapa", "Brassica rapa")),
    lcvp  = lk(c("brassica napus", "brassica rapa", "brassica macrorhiza"),
               c("Brassica napus", "Brassica rapa", "Brassica rapa"))
  ))
  src <- c("Brassica napus", "Brassica rapa")
  m <- taxifydb:::.name_closure_map(src, macrorhiza, reverse_hop = TRUE,
                                    verbose = FALSE)
  expect_equal(unique(m$accepted_name[m$input_name == "Brassica napus"]),
               "Brassica napus")

  local_mocked_bindings(.find_lookup_paths = function(backends) macrorhiza)
  df <- data.frame(canonical_name = src, country_code = "FR",
                   alien_first_record = c(1913L, 1985L),
                   alien_first_record_status = "present",
                   stringsAsFactors = FALSE)
  reg <- taxifydb:::.enrichment_build_registry$alien_first_records
  out <- resolve_enrichment_names(df, group_cols = "country_code",
                                  backends = names(macrorhiza), verbose = FALSE,
                                  reduce_fn = reg$reduce_fn)
  year <- stats::setNames(out$alien_first_record, out$canonical_name)
  expect_equal(year[["Brassica rapa"]], 1985L)
})

test_that("one backbone's synonymy does not key a species the others keep apart (#56)", {
  # lcvp alone files Amelanchier humilis under A. spicata; col, wcvp and itis
  # hold both as species. The spicata key must not carry humilis's year, while
  # humilis keeps its own.
  paths <- fake_lookups(list(
    lcvp = lk(c("amelanchier humilis", "amelanchier spicata"),
              c("Amelanchier spicata", "Amelanchier spicata")),
    col  = lk(c("amelanchier humilis", "amelanchier spicata"),
              c("Amelanchier humilis", "Amelanchier spicata")),
    wcvp = lk(c("amelanchier humilis", "amelanchier spicata"),
              c("Amelanchier humilis", "Amelanchier spicata")),
    itis = lk(c("amelanchier humilis", "amelanchier spicata"),
              c("Amelanchier humilis", "Amelanchier spicata"))
  ))
  m <- taxifydb:::.name_closure_map("Amelanchier humilis", paths,
                                    reverse_hop = TRUE, verbose = FALSE)
  expect_equal(unique(m$accepted_name), "Amelanchier humilis")

  # A resolution no backbone contradicts is kept: the old name of a moved
  # species reaches its current genus.
  moved <- fake_lookups(list(
    col  = lk(c("negundo aceroides", "acer negundo"), c("Acer negundo", "Acer negundo")),
    wfo  = lk(c("negundo aceroides", "acer negundo"), c("Acer negundo", "Acer negundo")),
    gbif = lk("acer negundo", "Acer negundo")
  ))
  m2 <- taxifydb:::.name_closure_map("Negundo aceroides", moved,
                                     reverse_hop = FALSE, verbose = FALSE)
  expect_equal(unique(m2$accepted_name), "Acer negundo")
})

test_that("a backbone carrying a respelling as its own species does not outvote the rest", {
  # gbif and ncbi accept both spellings of Cephaloziella massalongoi; wfo
  # resolves the source's `massalongi` onto the current spelling. The respelling
  # is one species, so the current spelling keeps the source's row.
  paths <- fake_lookups(list(
    wfo  = lk(c("cephaloziella massalongi", "cephaloziella massalongoi"),
              c("Cephaloziella massalongoi", "Cephaloziella massalongoi")),
    gbif = lk(c("cephaloziella massalongi", "cephaloziella massalongoi"),
              c("Cephaloziella massalongi", "Cephaloziella massalongoi")),
    ncbi = lk(c("cephaloziella massalongi", "cephaloziella massalongoi"),
              c("Cephaloziella massalongi", "Cephaloziella massalongoi"))
  ))
  m <- taxifydb:::.name_closure_map("Cephaloziella massalongi", paths,
                                    reverse_hop = FALSE, verbose = FALSE)
  expect_true("Cephaloziella massalongoi" %in% m$accepted_name)

  expect_equal(
    taxifydb:::.species_agreement(
      c("carex flava", "acer saccharum", "didymodon maschalogenus",
        "citrus aurantium", "fissidens arnoldi", "acer negundo"),
      c("carex flacca", "acer saccharinum", "didymodon maschalogena",
        "citrus × aurantium", "fissidens arnoldii", "acer rubrum")),
    c(FALSE, FALSE, NA, NA, NA, FALSE))
})

test_that("a hop onto a name no backbone places in one species is refused (#56)", {
  # `matricaria suaveolens` is L.'s name and Buchenau's in every backbone; the
  # forma autonym some backbones file under Tripleurospermum inodorum must not
  # key it with that species' rows.
  paths <- fake_lookups(list(
    gbif  = lk(c("tripleurospermum inodorum", "matricaria suaveolens f. suaveolens"),
               c("Tripleurospermum inodorum", "Tripleurospermum inodorum")),
    colxr = lk(c("tripleurospermum inodorum", "matricaria suaveolens f. suaveolens"),
               c("Tripleurospermum inodorum", "Tripleurospermum inodorum")),
    wfo   = lk(c("matricaria suaveolens f. suaveolens", "matricaria suaveolens"),
               c("Matricaria suaveolens", "Matricaria suaveolens"),
               n_species = c(1L, 2L)),
    lcvp  = lk(c("matricaria suaveolens f. suaveolens", "matricaria suaveolens"),
               c("Matricaria suaveolens", "Matricaria suaveolens"),
               n_species = c(1L, 3L))
  ))
  m <- taxifydb:::.name_closure_map("Tripleurospermum inodorum", paths,
                                    reverse_hop = TRUE, verbose = FALSE)
  expect_false("Matricaria suaveolens" %in% m$accepted_name)

  # The same name as a source's own resolution is kept.
  own <- taxifydb:::.name_closure_map("Matricaria suaveolens", paths,
                                      reverse_hop = FALSE, verbose = FALSE)
  expect_true("Matricaria suaveolens" %in% own$accepted_name)
})

test_that("a hop onto a name most backbones synonymise onto the input is kept", {
  # The Minuartia hybrida shape with its real vote: three backbones file the
  # reached name inside the input's species, one keeps the two apart.
  paths <- fake_lookups(list(
    col     = lk(c("sabulina tenuifolia", "minuartia hybrida"),
                 c("Sabulina tenuifolia", "Sabulina tenuifolia")),
    wcvp    = lk(c("sabulina tenuifolia", "minuartia hybrida"),
                 c("Sabulina tenuifolia", "Sabulina tenuifolia")),
    euromed = lk(c("sabulina tenuifolia", "minuartia hybrida"),
                 c("Sabulina tenuifolia", "Sabulina tenuifolia")),
    wfo     = lk(c("sabulina tenuifolia", "minuartia hybrida"),
                 c("Sabulina tenuifolia", "Minuartia hybrida"))
  ))
  m <- taxifydb:::.name_closure_map("Sabulina tenuifolia", paths,
                                    reverse_hop = TRUE, verbose = FALSE)
  expect_true("Minuartia hybrida" %in% m$accepted_name)
})

test_that("a placement the backbones split evenly still lets the hop through", {
  # One backbone synonymises the key onto the concept and one keeps it accepted:
  # nothing outvotes the entry, which is the Minuartia hybrida shape.
  paths <- fake_lookups(list(
    a = lk(c("aus bus", "cus dus"), c("Aus bus", "Aus bus")),
    b = lk("cus dus", "Cus dus")
  ))
  m <- taxifydb:::.name_closure_map("Aus bus", paths, reverse_hop = TRUE,
                                    verbose = FALSE)
  expect_true("Cus dus" %in% m$accepted_name)
})

test_that("a lookup without n_species is refused, not read as homonym-free", {
  p <- tempfile(fileext = ".vtr")
  vectra::write_vtr(data.frame(key_ci = "aus bus", accepted_name = "Aus bus",
                               stringsAsFactors = FALSE), p)
  expect_error(taxifydb:::.lookup_filter(p, "key_ci", "aus bus"), "n_species")
})

test_that("a lookup counts the species a key's rows point to", {
  bb <- data.frame(
    key_ci = c("acacia acicularis", "acacia acicularis", "acacia acicularis",
               "vachellia farnesiana", "vachellia farnesiana"),
    accepted_name = c("Acacia brownii", "Vachellia farnesiana",
                      "Vachellia farnesiana var. farnesiana",
                      "Vachellia farnesiana", "Vachellia farnesiana var. farnesiana"),
    taxonomic_status = c("SYNONYM", "SYNONYM", "SYNONYM", "ACCEPTED", "SYNONYM"),
    taxon_rank = "SPECIES", taxon_id = as.character(1:5),
    canonical_name = c("Acacia acicularis", "Acacia acicularis",
                       "Acacia acicularis", "Vachellia farnesiana",
                       "Vachellia farnesiana"),
    stringsAsFactors = FALSE)
  bb_path <- tempfile(fileext = ".vtr")
  vectra::write_vtr(bb, bb_path)
  out <- tempfile(fileext = ".vtr")
  taxifydb::build_name_lookup(bb_path, out, verbose = FALSE)
  l <- vectra::collect(vectra::tbl(out))
  n <- stats::setNames(l$n_species, l$key_ci)
  expect_equal(n[["acacia acicularis"]], 2L)
  # An autonym is the same species, not a second one.
  expect_equal(n[["vachellia farnesiana"]], 1L)
})

test_that("the species part keeps the hybrid sign and drops infraspecific ranks", {
  expect_equal(
    taxifydb:::.species_of(c("Quercus × pongtungensis", "× Agroelymus piettei",
                             "Abies lasiocarpa var. lasiocarpa", "Abies", NA)),
    c("quercus × pongtungensis", "× agroelymus piettei",
      "abies lasiocarpa", "abies", NA))
})

test_that("a lookup older than its backbone is stale", {
  bb <- tempfile(fileext = ".vtr")
  l  <- tempfile(fileext = ".vtr")
  vectra::write_vtr(data.frame(key_ci = "a", accepted_name = "A", n_species = 1L), l)
  vectra::write_vtr(data.frame(x = 1), bb)
  Sys.setFileTime(l, Sys.time() - 3600)
  expect_true(taxifydb:::.lookup_is_stale(l, bb))
  Sys.setFileTime(l, Sys.time() + 3600)
  expect_false(taxifydb:::.lookup_is_stale(l, bb))
})

test_that("a declared kingdom drops a synonym only a plant backbone supplies", {
  # wfo carries no kingdom column, so the consensus vote never sees its side
  # of a genus homonym. Its fixed kingdom does: an animal source's genus must
  # not pick up the plant genus wfo synonymises the same spelling onto.
  paths <- fake_lookups(list(
    col  = lk("elodes", "Elodes", "Animalia"),
    gbif = lk("elodes", "Elodes", "Animalia"),
    wfo  = lk("elodes", "Hypericum")
  ))
  voted <- taxifydb:::.name_closure_map("Elodes", paths, reverse_hop = FALSE,
                                        verbose = FALSE)
  expect_true("Hypericum" %in% voted$accepted_name)

  scoped <- taxifydb:::.name_closure_map("Elodes", paths, reverse_hop = FALSE,
                                         verbose = FALSE, kingdom = "animalia")
  expect_equal(unique(scoped$accepted_name), "Elodes")
})

test_that("a pair one in-scope backbone supplies survives an out-of-scope one", {
  # The same spelling reaches gbif as a moth genus and fungorum as a fungus.
  # For a fungal source the fungorum edge is the evidence that counts.
  paths <- fake_lookups(list(
    gbif     = lk("calyptra", "Calyptra", "Animalia"),
    col      = lk("calyptra", "Calyptra", "Animalia"),
    fungorum = lk("calyptra", "Calyptra")
  ))
  m <- taxifydb:::.name_closure_map("Calyptra", paths, reverse_hop = FALSE,
                                    verbose = FALSE, kingdom = "fungi")
  expect_equal(unique(m$accepted_name), "Calyptra")
})

test_that("a declared kingdom keeps a mapping no backbone places anywhere", {
  paths <- fake_lookups(list(
    ott = lk("aus", "Bus"),
    col = lk("aus", "Aus", "Plantae")
  ))
  m <- taxifydb:::.name_closure_map("Aus", paths, reverse_hop = FALSE,
                                    verbose = FALSE, kingdom = "animalia")
  expect_equal(unique(m$accepted_name), "Bus")
})

test_that("an unrecognised declared kingdom is an error, not an empty scope", {
  paths <- fake_lookups(list(col = lk("aus", "Aus", "Animalia")))
  expect_error(
    taxifydb:::.name_closure_map("Aus", paths, reverse_hop = FALSE,
                                 verbose = FALSE, kingdom = "Animalz"),
    "recognised kingdom")
})

test_that("genus grain keeps only genus-shaped accepted names", {
  # A backbone that files the source genus as a subgenus, or resolves it onto
  # a species, gives a key no taxify genus can match.
  paths <- fake_lookups(list(
    col  = lk("forelophilus", "Camponotus (Forelophilus)", "Animalia"),
    gbif = lk("forelophilus", "Forelophilus", "Animalia"),
    ott  = lk("achelia", "Achelia hispida", "Animalia")
  ))
  local_mocked_bindings(.find_lookup_paths = function(backends) paths)
  m <- resolve_name_map(c("Forelophilus", "Achelia"),
                        backends = names(paths), verbose = FALSE,
                        reverse_hop = FALSE, grain = "genus")
  expect_equal(m$accepted_name[m$input_name == "Forelophilus"], "Forelophilus")
  # Achelia's only mapping was a species, so it keeps its own name.
  expect_equal(m$accepted_name[m$input_name == "Achelia"], "Achelia")

  sp <- resolve_name_map(c("Forelophilus", "Achelia"),
                         backends = names(paths), verbose = FALSE,
                         reverse_hop = FALSE)
  expect_true("Camponotus (Forelophilus)" %in% sp$accepted_name)
})

test_that(".is_genus_name accepts one capitalised token only", {
  expect_equal(
    taxifydb:::.is_genus_name(c("Berosus", "× Cosmopsis", "Berosus (Berosus)",
                                "Achelia hispida", "Dero / Aulophorus",
                                "'Lithophila'", "berosus", NA)),
    c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
})

test_that("a genus-grain key is taken from the resolved name", {
  df <- data.frame(canonical_name = c("Abies", "Pinus"),
                   genus = c("Abies", "Pinaster"), trait = 1:2,
                   stringsAsFactors = FALSE)
  out <- taxifydb:::.genus_grain_key(df, "fake")
  expect_equal(out$genus, c("Abies", "Pinus"))

  df$canonical_name[2] <- "Aedes (Ochlerotatus)"
  expect_error(taxifydb:::.genus_grain_key(df, "fake"),
               "Aedes (Ochlerotatus)", fixed = TRUE)
})

test_that("a backbone with no kingdom cannot rescue a pair others place outside", {
  # The reverse hop from a plant genus reaches the fish genus Ammodytes: wfo
  # files `ammodytes` as a synonym of Astragalus, and the re-forward pass sends
  # it to Ammodytes, which col places in Animalia and ncbi places nowhere.
  paths <- fake_lookups(list(
    wfo  = lk(c("astragalus", "ammodytes"), c("Astragalus", "Astragalus")),
    col  = lk(c("astragalus", "ammodytes"), c("Astragalus", "Ammodytes"),
              c("Plantae", "Animalia")),
    ncbi = lk("ammodytes", "Ammodytes"),
    gbif = lk("ammodytes", "Ammodytes", "Animalia")
  ))
  m <- taxifydb:::.name_closure_map("Astragalus", paths, reverse_hop = TRUE,
                                    verbose = FALSE, kingdom = "plantae")
  expect_false("Ammodytes" %in% m$accepted_name)
  expect_true("Astragalus" %in% m$accepted_name)
})
