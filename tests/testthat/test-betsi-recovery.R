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

test_that("resolve_betsi_codes decodes GEN_SPE against a reference pool", {
  pool <- c("Brachystomella parvula", "Ceratophysella denticulata",
            "Heteromurus nitidus", "Folsomia candida", "Folsomia fimetaria")
  r <- resolve_betsi_codes(
    c("BRA_PAR", "CER_DEN", "HET_NIT", "FOL_CAN", "FOL_FIM", "DES_X", "ZZZ_ZZZ"),
    pool)
  rr <- stats::setNames(r$binomial, r$code)
  expect_equal(rr[["BRA_PAR"]], "Brachystomella parvula")
  expect_equal(rr[["CER_DEN"]], "Ceratophysella denticulata")
  # decodes to the correct spelling even though Bonfanti's legend carries the
  # upstream typo "Heretomurus"
  expect_equal(rr[["HET_NIT"]], "Heteromurus nitidus")
  expect_equal(rr[["FOL_CAN"]], "Folsomia candida")
  expect_equal(rr[["FOL_FIM"]], "Folsomia fimetaria")
  # genus-level and absent codes are left for the caller to drop, not guessed
  expect_true(is.na(rr[["DES_X"]]))
  expect_true(is.na(rr[["ZZZ_ZZZ"]]))
  expect_match(r$method[r$code == "DES_X"], "genus-level")
  expect_match(r$method[r$code == "ZZZ_ZZZ"], "no candidate")
})

test_that("resolve_betsi_codes splits a GEN_SPE collision by body length", {
  pool <- c("Genus alpha", "Genusx alphax")   # both encode GEN_ALP
  pbl  <- c("Genus alpha" = 2.0, "Genusx alphax" = 5.0)
  r <- resolve_betsi_codes("GEN_ALP", pool,
                           code_bl = c(GEN_ALP = 4.8), pool_bl = pbl)
  expect_equal(r$binomial, "Genusx alphax")
  expect_match(r$method, "body-length split")
  # with no body length the collision is left unresolved, never guessed
  r2 <- resolve_betsi_codes("GEN_ALP", pool)
  expect_true(is.na(r2$binomial))
  expect_match(r2$method, "ambiguous")
})

test_that("genus_dict disambiguates a first-three-letter genus collision", {
  # three genera share the first three letters "ISO" and the epithet "minor";
  # INRAE disambiguates them with a bespoke token, Bonfanti's plain rule cannot
  pool <- c("Isotomiella minor", "Isotoma minor", "Isotomurus minor")
  dict <- c(ISO = "Isotomiella", ISA = "Isotoma", ISU = "Isotomurus")

  # without the dictionary ISO_MIN matches all three -> ambiguous
  amb <- resolve_betsi_codes("ISO_MIN", pool)
  expect_true(is.na(amb$binomial))
  expect_match(amb$method, "ambiguous")

  # with the dictionary each token resolves to exactly its genus
  r <- resolve_betsi_codes(c("ISO_MIN", "ISA_MIN", "ISU_MIN"), pool,
                           genus_dict = dict)
  rr <- stats::setNames(r$binomial, r$code)
  expect_equal(rr[["ISO_MIN"]], "Isotomiella minor")
  expect_equal(rr[["ISA_MIN"]], "Isotoma minor")
  expect_equal(rr[["ISU_MIN"]], "Isotomurus minor")
  expect_true(all(grepl("dict", r$method)))

  # a bespoke token absent from the dictionary is not forced onto a near-match
  u <- resolve_betsi_codes("ISODES_MIN", pool, genus_dict = dict)
  expect_true(is.na(u$binomial))
  expect_match(u$method, "unmapped token")
})

test_that("inrae_genus_dict ships a verified token -> genus mapping", {
  d <- inrae_genus_dict()
  expect_type(d, "character")
  expect_true(all(nzchar(names(d))) && !anyNA(d))
  # the four-way Iso* split and a couple of other confirmed tokens
  expect_equal(unname(d[["ISO"]]),    "Isotomiella")
  expect_equal(unname(d[["ISU"]]),    "Isotomurus")
  expect_equal(unname(d[["ISODES"]]), "Isotomodes")
  expect_equal(unname(d[["MEGX"]]),   "Megalothorax")
  # tokens are upper-case abbreviations, genera are capitalised names
  expect_true(all(grepl("^[A-Z]+$", names(d))))
  expect_true(all(grepl("^[A-Z][a-z]+$", d)))
})

test_that("the recovery enrichments are registered and vocabulary-consistent", {
  expect_setequal(list_betsi_recovery(),
                  c("betsi_earthworm_traits", "betsi_collembola_traits",
                    "inrae_collembola_traits"))
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

# ---- Collembola INRAE (sparse fuzzy) ---------------------------------------

test_that("the sparse fuzzy parser keeps a wholly-absent block but rejects a partial one", {
  spec <- list(shape = "fuzzy", sparse = TRUE,
               traits = list(a = c("x", "y"), b = c("p", "q")))
  # Genus one and three carry both traits; Genus two carries only trait a, so its
  # trait-b block is wholly absent -- kept as NA under sparse, an error when not.
  long <- data.frame(
    species = c("Genus one", "Genus one", "Genus one", "Genus one",
                "Genus two", "Genus two",
                "Genus three", "Genus three", "Genus three", "Genus three"),
    trait   = c("a", "a", "b", "b", "a", "a", "a", "a", "b", "b"),
    class   = c("x", "y", "p", "q", "x", "y", "x", "y", "p", "q"),
    pct     = c(100, 0, 50, 50, 100, 0, 0, 100, 50, 50),
    stringsAsFactors = FALSE)

  w <- taxifydb:::.betsi_pivot_matrix(long, spec, "synthetic")
  expect_equal(nrow(w), 3L)
  expect_true(is.na(w$b__p[w$canonical_name == "Genus two"]))
  expect_equal(w$a__x[w$canonical_name == "Genus two"], 100)

  # drop one modality of Genus one's trait b: the class still appears (Genus
  # three has it) so it is a partial block, i.e. corruption, not missing data
  partial <- long[!(long$species == "Genus one" & long$trait == "b" &
                    long$class == "q"), ]
  expect_error(taxifydb:::.betsi_pivot_matrix(partial, spec, "synthetic"),
               "partial fuzzy block")

  # the same wholly-absent block is an incomplete-extraction error when the
  # source is a complete (non-sparse) matrix
  spec2 <- spec; spec2$sparse <- FALSE
  expect_error(taxifydb:::.betsi_pivot_matrix(long, spec2, "synthetic"),
               "incomplete")
})

test_that("the frozen INRAE matrix parses to sparse per-species fuzzy vectors", {
  ex <- system.file("extdata", "betsi", package = "taxifydb")
  skip_if(!nzchar(ex) ||
          !file.exists(file.path(ex, "inrae_collembola.csv")),
          "run data-raw/betsi_recovery.R to freeze the matrix")

  df <- parse_betsi_recovery("inrae_collembola_traits", ex)
  expect_equal(nrow(df), 135L)
  expect_setequal(
    setdiff(names(df), "canonical_name"),
    c("ocelli__1_3", "ocelli__4_7", "ocelli__8", "ocelli__absent",
      "furca__present", "furca__absent",
      "post_antennal_organ__present", "post_antennal_organ__absent",
      "pigmentation__present", "pigmentation__absent",
      "body_shape__cylindrical", "body_shape__spherical",
      "scales__present", "scales__absent",
      "reproduction__sexual", "reproduction__asexual"))

  # every present fuzzy block sums to 100; a wholly-absent block is NA, not zero
  for (tr in c("ocelli", "furca", "post_antennal_organ", "pigmentation",
               "body_shape", "scales", "reproduction")) {
    cols    <- grep(paste0("^", tr, "__"), names(df), value = TRUE)
    present <- rowSums(!is.na(df[cols])) == length(cols)
    expect_true(all(abs(rowSums(df[present, cols, drop = FALSE]) - 100) <= 1.5),
                info = tr)
  }

  # post-antennal organ is scored for a minority (genuinely sparse), pigmentation
  # for every species
  expect_lt(sum(!is.na(df$post_antennal_organ__present)), nrow(df))
  expect_equal(sum(!is.na(df$pigmentation__present)), nrow(df))

  # Allacma gallica is a globular springtail: spherical body, full 8 ocelli
  ag <- df[df$canonical_name == "Allacma gallica", ]
  expect_equal(ag$body_shape__spherical, 100)
  expect_equal(ag$ocelli__8, 100)
})

test_that("inrae_collembola_traits maps its shared BETSI axes to T-SITA, not pigmentation", {
  m <- taxifydb:::.tsita_enrichment_meta("inrae_collembola_traits")
  expect_equal(m$columns$ocelli__8$trait_label, "Max_number_of_visual_organs")
  expect_equal(m$columns$furca__present$trait_label, "Furcula_length")
  expect_equal(m$columns$post_antennal_organ__present$trait_label, "Postantennal_organ")
  expect_equal(m$columns$body_shape__spherical$trait_label, "Body_shape")
  expect_equal(m$columns$scales__present$trait_label, "Scales")
  expect_equal(m$columns$reproduction__sexual$trait_label, "Reproduction_type")
  # pigmentation has no faithful T-SITA concept, left unmapped by design
  expect_null(m$columns$pigmentation__present)
  expect_null(m$columns$pigmentation__absent)
})

test_that(".betsi_recovery_provenance tiers every INRAE column betsi_derived", {
  df   <- c("canonical_name", "ocelli__8", "furca__present",
            "pigmentation__absent", "reproduction__sexual")
  prov <- taxifydb:::.betsi_recovery_provenance("inrae_collembola_traits", df)
  expect_null(prov[["canonical_name"]])
  expect_true(all(unlist(prov) == "betsi_derived"))
  expect_equal(prov[["pigmentation__absent"]], "betsi_derived")
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
