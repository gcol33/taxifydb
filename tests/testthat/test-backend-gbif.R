# Build-time GBIF status mapping. Internal (`@noRd`) so accessed via `:::`.

test_that("gbif_status_to_standard maps correctly", {
  expect_equal(taxifydb:::gbif_status_to_standard("ACCEPTED"), "ACCEPTED")
  expect_equal(taxifydb:::gbif_status_to_standard("DOUBTFUL"), "ACCEPTED")
  expect_equal(taxifydb:::gbif_status_to_standard("PROVISIONALLY_ACCEPTED"),
               "ACCEPTED")
  expect_equal(taxifydb:::gbif_status_to_standard("SYNONYM"), "SYNONYM")
  expect_equal(taxifydb:::gbif_status_to_standard("HOMOTYPIC_SYNONYM"),
               "SYNONYM")
  expect_equal(taxifydb:::gbif_status_to_standard("HETEROTYPIC_SYNONYM"),
               "SYNONYM")
  expect_equal(taxifydb:::gbif_status_to_standard("PROPARTE_SYNONYM"),
               "SYNONYM")
  expect_equal(taxifydb:::gbif_status_to_standard("MISAPPLIED"), "SYNONYM")
  expect_equal(taxifydb:::gbif_status_to_standard("AMBIGUOUS_SYNONYM"),
               "SYNONYM")
})

test_that("gbif_render_infraspecific reinstates the dropped rank marker (#45)", {
  render <- taxifydb:::gbif_render_infraspecific

  # The issue's examples: marker read back from scientific_name.
  expect_equal(
    render(
      c("Erica tenella tenella", "Veronica spicata kamelinii",
        "Aconitum firmum portae-ferratae", "Anacamptis morio litardierei",
        "Casearia tomentosa reducta"),
      c("Erica tenella var. tenella (L.) E.G.H.Oliv.",
        "Veronica spicata subsp. kamelinii Elenevsky",
        "Aconitum firmum var. portae-ferratae Starm.",
        "Anacamptis morio nothosubsp. litardierei (E.G.Camus) Kreutz",
        "Casearia tomentosa subsp. reducta Sleumer"),
      c("tenella", "kamelinii", "portae-ferratae", "litardierei", "reducta")
    ),
    c("Erica tenella var. tenella", "Veronica spicata subsp. kamelinii",
      "Aconitum firmum var. portae-ferratae",
      "Anacamptis morio nothosubsp. litardierei",
      "Casearia tomentosa subsp. reducta")
  )
})

test_that("gbif_render_infraspecific leaves zoological trinomials alone (#45)", {
  render <- taxifydb:::gbif_render_infraspecific
  # A zoological subspecies carries no connecting term in scientific_name, so
  # nothing is inserted and GBIF keeps agreeing with the zoological backbones.
  expect_equal(
    render("Panthera leo persica", "Panthera leo persica (Meyer, 1826)",
           "persica"),
    "Panthera leo persica"
  )
})

test_that("gbif_render_infraspecific handles edge cases (#45)", {
  render <- taxifydb:::gbif_render_infraspecific

  # Autonym: species author sits between species and marker; the epithet also
  # appears as the specific epithet, so anchoring on the marker matters.
  expect_equal(
    render("Erica tenella tenella", "Erica tenella L. var. tenella", "tenella"),
    "Erica tenella var. tenella"
  )
  # A forma whose author is "L. f." (filius): the trailing "f." must not be read
  # as the connecting marker.
  expect_equal(
    render("Poa annua minima", "Poa annua f. minima L. f.", "minima"),
    "Poa annua f. minima"
  )
  # No infraspecific epithet, empty inputs, and an already-marked canonical are
  # all passed through untouched.
  expect_equal(render("Poa annua", "Poa annua L.", NA_character_), "Poa annua")
  expect_equal(render(NA_character_, NA_character_, "x"), NA_character_)
  expect_equal(
    render("Poa annua var. minima", "Poa annua var. minima L.", "minima"),
    "Poa annua var. minima"
  )
})

test_that("gbif_resolve_higher resolves higher-rank keys to names (#24)", {
  # read_gbif() feeds kingdom_key/phylum_key/class_key/order_key through this
  # helper (as it already does for family_key) so each row carries denormalized
  # ancestor names before the KINGDOM..ORDER rows are dropped.
  df <- data.frame(
    id             = c("1", "2", "3", "4", "5"),
    canonical_name = c("Animalia", "Chordata", "Mammalia", "Carnivora",
                       "Canidae"),
    stringsAsFactors = FALSE
  )
  keys <- c("1", "3", "4", NA, "999")
  higher <- taxifydb:::gbif_higher_lookup(df$id, df$canonical_name)
  expect_equal(taxifydb:::gbif_resolve_higher(higher, keys),
               c("Animalia", "Mammalia", "Carnivora", NA, NA))
})
