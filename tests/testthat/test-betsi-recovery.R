# BETSI recovery: gen_spe codes, the two matrix-shape parsers, per-column
# provenance, and T-SITA wiring.
#
# Recovery-grade, not smoke: the fuzzy-coding invariant (each species x trait
# affinity block sums to 100) is the extraction's self-check, so these tests
# assert it on the built data AND that the parser rejects a matrix that breaks
# it, drifts from its declared modalities, or mistypes a hard column. Provenance
# is checked per column (a source can mix BETSI-derived and own columns).

test_that("gen_spe builds the 6-letter GEN_SPE code", {
  # verified against the Bonfanti 2022 legend (dev_notes/betsi.md)
  expect_equal(gen_spe("Brachystomella parvula"), "BRA_PAR")
  expect_equal(gen_spe("Allacma gallica"),        "ALL_GAL")
  expect_equal(gen_spe("Arrhopalites caecus"),    "ARR_CAE")
  expect_equal(gen_spe("Protaphorura tricampata"), "PRO_TRI")
  # a hyphen in the epithet is dropped before the first three letters are taken
  expect_equal(gen_spe("Genus a-bcd"), "GEN_ABC")
  # not a capitalised binomial -> NA
  expect_true(is.na(gen_spe("Onychiurus")))
  expect_true(is.na(gen_spe("")))
  expect_true(is.na(gen_spe(NA)))
})

test_that("the recovery enrichments are registered and vocabulary-consistent", {
  expect_setequal(list_betsi_recovery(),
                  c("betsi_earthworm_traits", "betsi_collembola_traits"))
  expect_true(all(list_betsi_recovery() %in% list_enrichments()))
  expect_error(build_betsi_recovery("not_a_recovery"), "not a BETSI-recovery")
})

test_that(".betsi_recovery_provenance tiers each column, fuzzy and hard", {
  # fuzzy: a modality column resolves to its logical trait's tier
  pe <- taxifydb:::.betsi_recovery_provenance(
    "betsi_earthworm_traits",
    c("canonical_name", "body_length_mm__50_100", "carbon_pref_mgkg__lt20"))
  expect_null(pe[["canonical_name"]])
  expect_equal(pe[["body_length_mm__50_100"]], "betsi_derived")
  expect_equal(pe[["carbon_pref_mgkg__lt20"]], "betsi_derived")

  # hard: BETSI-derived columns vs the study's own columns
  lu <- taxifydb:::.betsi_recovery_provenance(
    "betsi_collembola_traits",
    c("canonical_name", "body_length_mm", "ocelli_number",
      "stratification_scaled", "trophic_position", "life_form"))
  expect_equal(lu[["body_length_mm"]],        "betsi_derived")
  expect_equal(lu[["ocelli_number"]],         "betsi_derived")
  expect_equal(lu[["stratification_scaled"]], "source_study")
  expect_equal(lu[["trophic_position"]],      "source_study")
  expect_equal(lu[["life_form"]],             "source_study")
})

# ---- Earthworm (fuzzy) -----------------------------------------------------

test_that("betsi_earthworm_traits maps only body length and carbon to T-SITA", {
  m <- taxifydb:::.tsita_enrichment_meta("betsi_earthworm_traits")
  expect_equal(m$columns$body_length_mm__50_100$trait_label, "Body_length")
  expect_equal(m$columns$carbon_pref_mgkg__lt20$trait_label, "Carbon")
  expect_match(m$columns$body_length_mm__50_100$trait_uri, "TSITA_")
  expect_null(m$columns$epithelium__supple)
  expect_null(m$columns$typhlosolis__simple)
  expect_null(m$columns$cocoon_diameter_mm__1_2)
})

test_that("the frozen Pelosi matrix parses to per-species fuzzy vectors", {
  ex <- system.file("extdata", "betsi", package = "taxifydb")
  skip_if(!nzchar(ex) ||
          !file.exists(file.path(ex, "pelosi2014_earthworm.csv")),
          "run data-raw/betsi_recovery.R to freeze the matrix")

  df <- parse_betsi_recovery("betsi_earthworm_traits", ex)
  expect_equal(nrow(df), 11L)
  # provenance is metadata, not a data column
  expect_false("provenance_tier" %in% names(df))

  for (tr in c("body_length_mm", "body_mass_length_ratio", "cocoon_diameter_mm",
               "epithelium", "typhlosolis", "carbon_pref_mgkg",
               "vertical_distribution_cm")) {
    cols <- grep(paste0("^", tr, "__"), names(df), value = TRUE)
    expect_true(length(cols) >= 2L)
    expect_true(all(abs(rowSums(df[cols]) - 100) <= 1.5), info = tr)
  }

  ac <- df[df$canonical_name == "Allolobophora chlorotica", ]
  expect_equal(ac$body_length_mm__50_100, 100)
  expect_equal(ac$epithelium__supple,     100)
  expect_equal(ac$cocoon_diameter_mm__2_4, 100)
})

# ---- Collembola (hard-value) -----------------------------------------------

test_that("the frozen Lu matrix parses to per-species hard values", {
  ex <- system.file("extdata", "betsi", package = "taxifydb")
  skip_if(!nzchar(ex) ||
          !file.exists(file.path(ex, "lu2025_collembola.csv")),
          "run data-raw/betsi_recovery.R to freeze the matrix")

  df <- parse_betsi_recovery("betsi_collembola_traits", ex)
  expect_equal(nrow(df), 26L)
  expect_setequal(
    setdiff(names(df), "canonical_name"),
    c("antenna_body_ratio", "body_length_mm", "pigment_scaled", "ocelli_number",
      "furca", "reproduction", "stratification_scaled", "trophic_position",
      "life_form"))
  expect_type(df$body_length_mm, "double")
  expect_type(df$trophic_position, "character")

  cd <- df[df$canonical_name == "Ceratophysella denticulata", ]
  expect_equal(cd$body_length_mm, 1.7)
  expect_equal(cd$ocelli_number, 8)
  expect_equal(cd$trophic_position, "II")
  expect_equal(cd$life_form, "epedaphic")
})

test_that("betsi_collembola_traits maps its BETSI axes to T-SITA", {
  m <- taxifydb:::.tsita_enrichment_meta("betsi_collembola_traits")
  expect_equal(m$columns$body_length_mm$trait_label, "Body_length")
  expect_equal(m$columns$reproduction$trait_label, "Reproduction_type")
  expect_equal(m$columns$furca$trait_label, "Furcula_length")
  expect_equal(m$columns$ocelli_number$trait_label, "Max_number_of_visual_organs")
  # the authors' own / unmappable axes stay unmapped
  expect_null(m$columns$stratification_scaled)
  expect_null(m$columns$trophic_position)
  expect_null(m$columns$pigment_scaled)
  expect_null(m$columns$antenna_body_ratio)
})

# ---- Built .vtr metadata (both shapes) -------------------------------------

test_that("a built earthworm .vtr records per-column provenance + T-SITA", {
  skip_if_not_installed("vectra")
  ex <- system.file("extdata", "betsi", package = "taxifydb")
  skip_if(!file.exists(file.path(ex, "pelosi2014_earthworm.csv")))

  df <- parse_betsi_recovery("betsi_earthworm_traits", ex)
  prov <- taxifydb:::.betsi_recovery_provenance("betsi_earthworm_traits", names(df))
  td <- file.path(tempdir(), "betsi_ew_vtr")
  dir.create(td, showWarnings = FALSE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  vp <- file.path(td, "betsi_earthworm_traits.vtr")
  suppressMessages(build_enrichment_vtr(
    df, vp, "betsi_earthworm_traits", "test",
    source_url = "x", license = "facts", provenance = prov))

  meta <- jsonlite::read_json(file.path(td, "meta.json"))
  expect_equal(meta$tsita$columns$body_length_mm__50_100$trait_label, "Body_length")
  # every earthworm column is betsi_derived
  expect_equal(meta$provenance$body_length_mm__50_100, "betsi_derived")
  expect_equal(meta$provenance$epithelium__supple, "betsi_derived")
  expect_false("provenance_tier" %in% unlist(meta$trait_cols))
})

test_that("a built Collembola .vtr records mixed per-column provenance", {
  skip_if_not_installed("vectra")
  ex <- system.file("extdata", "betsi", package = "taxifydb")
  skip_if(!file.exists(file.path(ex, "lu2025_collembola.csv")))

  df <- parse_betsi_recovery("betsi_collembola_traits", ex)
  prov <- taxifydb:::.betsi_recovery_provenance("betsi_collembola_traits", names(df))
  td <- file.path(tempdir(), "betsi_col_vtr")
  dir.create(td, showWarnings = FALSE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  vp <- file.path(td, "betsi_collembola_traits.vtr")
  suppressMessages(build_enrichment_vtr(
    df, vp, "betsi_collembola_traits", "test",
    source_url = "x", license = "facts", provenance = prov))

  meta <- jsonlite::read_json(file.path(td, "meta.json"))
  expect_equal(meta$provenance$body_length_mm, "betsi_derived")
  expect_equal(meta$provenance$ocelli_number, "betsi_derived")
  expect_equal(meta$provenance$stratification_scaled, "source_study")
  expect_equal(meta$provenance$trophic_position, "source_study")
  expect_equal(meta$provenance$life_form, "source_study")
  expect_equal(meta$tsita$columns$ocelli_number$trait_label,
               "Max_number_of_visual_organs")
})

# ---- Recovery-grade negatives ----------------------------------------------

test_that("the fuzzy parser rejects a sum-to-100 violation", {
  ex <- system.file("extdata", "betsi", package = "taxifydb")
  f <- file.path(ex, "pelosi2014_earthworm.csv")
  skip_if(!file.exists(f))
  long <- utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  spec <- taxifydb:::.betsi_recovery_sources[["pelosi2014_earthworm"]]

  i <- which(long$species == "Allolobophora chlorotica" &
             long$trait == "body_length_mm" & long$class == "50-100")[1L]
  long$pct[i] <- 40
  expect_error(
    taxifydb:::.betsi_pivot_matrix(long, spec, "pelosi2014_earthworm"),
    "fuzzy-coding invariant")
})

test_that("the fuzzy parser rejects modality drift from the descriptor", {
  ex <- system.file("extdata", "betsi", package = "taxifydb")
  f <- file.path(ex, "pelosi2014_earthworm.csv")
  skip_if(!file.exists(f))
  long <- utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  spec <- taxifydb:::.betsi_recovery_sources[["pelosi2014_earthworm"]]

  long$class[long$trait == "body_length_mm" & long$class == "50-100"] <- "50-99"
  expect_error(
    taxifydb:::.betsi_pivot_matrix(long, spec, "pelosi2014_earthworm"),
    "modalities")
})

test_that("the hard parser rejects a column that is not in the descriptor", {
  ex <- system.file("extdata", "betsi", package = "taxifydb")
  f <- file.path(ex, "lu2025_collembola.csv")
  skip_if(!file.exists(f))
  wide <- utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  spec <- taxifydb:::.betsi_recovery_sources[["lu2025_collembola"]]

  wide$unexpected_trait <- 1
  expect_error(
    taxifydb:::.betsi_read_hard(wide, spec, "lu2025_collembola"),
    "do not match its descriptor")
})
