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
