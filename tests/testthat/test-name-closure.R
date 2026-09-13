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
    vectra::write_vtr(d, p)
    vectra::create_index(p, "key_ci")
    vectra::create_index(p, "accepted_name")
    paths <- c(paths, stats::setNames(p, nm))
  }
  paths
}

lk <- function(key_ci, accepted_name, kingdom = NA_character_) {
  data.frame(key_ci = key_ci, accepted_name = accepted_name,
             kingdom = kingdom, stringsAsFactors = FALSE)
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
    d = lk("cus dus", "Cus dus", "Animalia")
  ))
  m <- taxifydb:::.name_closure_map("Aus bus", paths, reverse_hop = TRUE,
                                    verbose = FALSE)
  # Animalia wins the vote 3-1, so the name is kept -- as the same organism.
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
