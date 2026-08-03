# Genus register / backend coverage build pipeline (#23).
#
# Extractors and classification-resolution functions are internal (`@noRd`),
# so this file calls them unqualified -- testthat runs with the package
# attached (see tests/testthat.R), which puts internal functions in scope for
# a `test_that()` block the same way it would inside the package itself.

write_genus_fixture <- function(path, rows) {
  vectra::write_vtr(rows, path)
}

#' Run `code` with the working directory set to a fresh, empty temp dir
#'
#' `resolve_register_backbone_paths()` falls back to the relative path
#' `output/<name>/<name>.vtr` when a backbone is not named in
#' `backbone_paths`. Running from this repo's own working directory would let
#' a test accidentally pick up real local build output (e.g. `output/gbif/
#' gbif.vtr`), making the test's outcome depend on what happens to be built on
#' the machine running it. Isolate every test that exercises the fallback.
#' @noRd
with_isolated_wd <- function(code) {
  dir <- tempfile("register_test_wd_")
  dir.create(dir)
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)
  force(code)
}


# ---- assign_life_form() ----

test_that("assign_life_form() returns correct taxon_group for known families", {
  chk <- function(fam, expected_tg) {
    res <- assign_life_form(fam)
    expect_equal(res$taxon_group, expected_tg, info = sprintf("family: %s", fam))
  }

  chk("Sphagnaceae",     "moss")
  chk("Marchantiaceae",  "liverwort")
  chk("Anthocerotaceae", "hornwort")
  chk("Lycopodiaceae",   "lycophyte")
  chk("Polypodiaceae",   "fern")
  chk("Pinaceae",        "gymnosperm")
  chk("Asteraceae",      "angiosperm")
  chk("Parmeliaceae",    "lichen")
  chk("Agaricaceae",     "fungus")
  chk("Characeae",       "green_alga")
  chk("Fucaceae",        "brown_alga")
  chk("Corallinaceae",   "red_alga")
  chk("Peronosporaceae", "oomycete")
  chk("Bacillariaceae",  "diatom")
  chk("Physaraceae",     "slime_mould")
})

test_that("assign_life_form() returns correct kingdom_group for known families", {
  chk_kg <- function(fam, expected_kg) {
    res <- assign_life_form(fam)
    expect_equal(res$kingdom_group, expected_kg, info = sprintf("family: %s", fam))
  }

  chk_kg("Asteraceae",      "plantae")
  chk_kg("Characeae",       "plantae")   # green alga -> plantae
  chk_kg("Corallinaceae",   "plantae")   # red alga -> plantae
  chk_kg("Agaricaceae",     "fungi")
  chk_kg("Fucaceae",        "chromista") # brown alga -> chromista
  chk_kg("Physaraceae",     "protozoa")
})

test_that("assign_life_form() life_form uses spaces not underscores", {
  expect_equal(assign_life_form("Fucaceae")$life_form,   "brown alga")
  expect_equal(assign_life_form("Physaraceae")$life_form, "slime mould")
})

test_that("assign_life_form() uses kingdom fallback when family is NA or unknown", {
  chk <- function(fam, kg, exp_tg, exp_kg) {
    res <- assign_life_form(fam, kg)
    expect_equal(res$taxon_group,   exp_tg)
    expect_equal(res$kingdom_group, exp_kg)
  }
  chk(NA_character_, "Fungi",    "fungus", "fungi")
  chk(NA_character_, "Animalia", "animal", "animalia")
  chk(NA_character_, "Chromista", "unknown", "chromista")
})

test_that("assign_life_form() returns 'unknown' when family and kingdom both miss", {
  res <- assign_life_form("Unknowniaceae")
  expect_equal(res$taxon_group,   "unknown")
  expect_equal(res$kingdom_group, "unknown")
  expect_equal(res$life_form,     "unknown")
})

test_that("assign_life_form() is vectorized and family hit outranks kingdom", {
  family  <- c("Sphagnaceae", NA_character_, "Parmeliaceae")
  kingdom <- c("Plantae",     "Fungi",       "Fungi")
  result  <- assign_life_form(family, kingdom)

  expect_equal(result$taxon_group,   c("moss", "fungus", "lichen"))
  expect_equal(result$kingdom_group, c("plantae", "fungi", "fungi"))
})


# ---- normalize_kingdom_names() ----

test_that("normalize_kingdom_names() maps NCBI clades to standard kingdoms", {
  expect_equal(
    normalize_kingdom_names(c("Pseudomonadati", "Metazoa", "Viridiplantae")),
    c("Bacteria", "Animalia", "Plantae")
  )
})

test_that("normalize_kingdom_names() maps OTT names to standard kingdoms", {
  expect_equal(
    normalize_kingdom_names(c("Archaeplastida", "Chloroplastida")),
    c("Plantae", "Plantae")
  )
})

test_that("normalize_kingdom_names() maps viral realms ending in 'virae'", {
  expect_equal(normalize_kingdom_names("Orthornavirae"), "Viruses")
})

test_that("normalize_kingdom_names() leaves standard kingdoms and NA untouched", {
  expect_equal(normalize_kingdom_names(c("Plantae", NA_character_)),
               c("Plantae", NA_character_))
})


# ---- infer_kingdom_from_family() ----

test_that("infer_kingdom_from_family() fills kingdom by family majority vote", {
  resolved <- data.frame(
    genus   = c("Quercus", "Fagus", "Castanea"),
    kingdom = c("Plantae", NA_character_, NA_character_),
    family  = c("Fagaceae", "Fagaceae", "Fagaceae"),
    stringsAsFactors = FALSE
  )
  out <- infer_kingdom_from_family(resolved)
  expect_equal(out$kingdom, c("Plantae", "Plantae", "Plantae"))
})

test_that("infer_kingdom_from_family() leaves genera with no family-mate kingdom untouched", {
  resolved <- data.frame(
    genus   = c("Xxxonia"),
    kingdom = NA_character_,
    family  = c("Xxxonaceae"),
    stringsAsFactors = FALSE
  )
  out <- infer_kingdom_from_family(resolved)
  expect_true(is.na(out$kingdom))
})


# ---- resolve_genus_classification() ----

test_that("resolve_genus_classification() prefers WoRMS > COL > ... priority order", {
  col_genera <- data.frame(
    genus = "Quercus", kingdom = "Plantae", phylum = "Tracheophyta",
    class = "Magnoliopsida_col", order = "Fagales", family = "Fagaceae",
    stringsAsFactors = FALSE
  )
  worms_genera <- data.frame(
    genus = "Quercus", kingdom = NA_character_, phylum = NA_character_,
    class = "Magnoliopsida_worms", order = NA_character_, family = "Fagaceae",
    stringsAsFactors = FALSE
  )

  resolved <- resolve_genus_classification(
    list(worms = worms_genera, col = col_genera)
  )

  expect_equal(nrow(resolved), 1L)
  # WoRMS leads the priority order but has no `class` value here, so COL wins.
  expect_equal(resolved$class, "Magnoliopsida_worms")
  expect_equal(resolved$kingdom, "Plantae")
})

test_that("resolve_genus_classification() unions genera across backends", {
  col_genera <- data.frame(
    genus = "Quercus", kingdom = "Plantae", phylum = NA_character_,
    class = "Magnoliopsida", order = "Fagales", family = "Fagaceae",
    stringsAsFactors = FALSE
  )
  gbif_genera <- data.frame(
    genus = "Boletus", kingdom = "Fungi", phylum = NA_character_,
    class = "Agaricomycetes", order = "Boletales", family = "Boletaceae",
    stringsAsFactors = FALSE
  )

  resolved <- resolve_genus_classification(
    list(col = col_genera, gbif = gbif_genera, wfo = NULL)
  )

  expect_equal(nrow(resolved), 2L)
  expect_true(all(c("Quercus", "Boletus") %in% resolved$genus))
})

test_that("a source contradicting itself is read by its own majority", {
  # COL carries Pteropus three times: two flying-fox rows and one that files
  # the bat family under Fungi. Priority alone let whichever row sorted first
  # speak for the source, so the register read 66 species of flying fox as a
  # fungus, welding Ascomycota to the order Chiroptera in one row.
  col_genera <- data.frame(
    genus   = rep("Pteropus", 3),
    kingdom = c("Fungi", "Animalia", "Animalia"),
    phylum  = c("Ascomycota", "Chordata", "Chordata"),
    class   = c("Dothideomycetes", "Mammalia", "Mammalia"),
    order   = c(NA_character_, "Chiroptera", "Chiroptera"),
    family  = rep("Pteropodidae", 3),
    stringsAsFactors = FALSE
  )

  resolved <- resolve_genus_classification(list(col = col_genera))

  expect_equal(nrow(resolved), 1L)
  expect_equal(resolved$kingdom, "Animalia")
  # The whole row has to come from one kingdom, not just its first column.
  expect_equal(resolved$phylum, "Chordata")
  expect_equal(resolved$class, "Mammalia")
  expect_equal(resolved$order, "Chiroptera")
})

test_that("the majority tiebreak never outranks a higher-priority backbone", {
  # Support only breaks ties inside one source. WoRMS leads the priority order
  # with a single row; COL agreeing with itself twice must not overtake it.
  worms_genera <- data.frame(
    genus = "Alaria", kingdom = "Animalia", phylum = "Mollusca",
    class = "Gastropoda", order = NA_character_, family = "Aporrhaidae",
    stringsAsFactors = FALSE
  )
  col_genera <- data.frame(
    genus = rep("Alaria", 2), kingdom = rep("Chromista", 2),
    phylum = rep("Ochrophyta", 2), class = rep("Phaeophyceae", 2),
    order = rep("Laminariales", 2), family = rep("Alariaceae", 2),
    stringsAsFactors = FALSE
  )

  resolved <- resolve_genus_classification(
    list(worms = worms_genera, col = col_genera)
  )

  expect_equal(resolved$kingdom, "Animalia")
  expect_equal(resolved$class, "Gastropoda")
})

test_that("an even split inside a source leaves the incoming order alone", {
  col_genera <- data.frame(
    genus = rep("Ambiguus", 2), kingdom = c("Plantae", "Animalia"),
    phylum = c("Tracheophyta", "Chordata"),
    class = c("Magnoliopsida", "Aves"),
    order = c("Asterales", "Passeriformes"),
    family = c("Compositae", "Sturnidae"),
    stringsAsFactors = FALSE
  )

  resolved <- resolve_genus_classification(list(col = col_genera))

  expect_equal(resolved$kingdom, "Plantae")
  expect_equal(resolved$class, "Magnoliopsida")
})

test_that("resolve_genus_classification() returns empty_genus_df() schema on empty input", {
  resolved <- resolve_genus_classification(list())
  expect_equal(nrow(resolved), 0L)
  expect_equal(names(resolved), names(empty_genus_df()))
})


# ---- register_backbones() ----

test_that("register_backbones() lists the fixed 18-backend set", {
  expect_setequal(register_backbones(), c(
    "wfo", "col", "gbif", "itis", "ncbi", "ott", "worms", "euromed",
    "fishbase", "sealifebase", "reptiledb", "lcvp", "wcvp",
    "mdd", "avilist", "lpsn", "fungorum", "algaebase"
  ))
})


test_that("every registered extractor is placed in the classification priority", {
  expect_setequal(names(.register_extractors), .register_priority())
})


test_that("resolve_genus_classification() errors on an unplaced extractor", {
  reduced <- setdiff(.register_priority(), "lpsn")
  expect_error(
    with_mocked_bindings(
      resolve_genus_classification(list()),
      .register_priority = function() reduced
    ),
    "absent from .register_priority"
  )
})


# ---- genus extractors against synthetic .vtr fixtures ----

test_that("extract_col_genera() reads genus-rank rows with full classification", {
  vtr <- tempfile(fileext = ".vtr")
  on.exit(unlink(vtr), add = TRUE)
  write_genus_fixture(vtr, data.frame(
    taxon_id = c("1", "2"),
    canonical_name = c("Quercus", "Pinus"),
    taxon_rank = c("GENUS", "GENUS"),
    family  = c("Fagaceae", "Pinaceae"),
    kingdom = c("Plantae", "Plantae"),
    phylum  = c("Tracheophyta", "Tracheophyta"),
    class   = c("Magnoliopsida", "Pinopsida"),
    order   = c("Fagales", "Pinales"),
    stringsAsFactors = FALSE
  ))

  out <- extract_col_genera(vtr)
  expect_equal(nrow(out), 2L)
  expect_equal(names(out), c("genus", "kingdom", "phylum", "class", "order", "family"))
  expect_true(all(c("Quercus", "Pinus") %in% out$genus))
  expect_equal(out$kingdom[out$genus == "Quercus"], "Plantae")
})

test_that("extract_col_genera() returns empty_genus_df() schema when no genus rows", {
  vtr <- tempfile(fileext = ".vtr")
  on.exit(unlink(vtr), add = TRUE)
  write_genus_fixture(vtr, data.frame(
    taxon_id = "1", canonical_name = "Quercus robur", taxon_rank = "SPECIES",
    family = "Fagaceae", stringsAsFactors = FALSE
  ))

  out <- extract_col_genera(vtr)
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), names(empty_genus_df()))
})

test_that("extract_worms_genera() carries the full denormalized classification", {
  vtr <- tempfile(fileext = ".vtr")
  on.exit(unlink(vtr), add = TRUE)
  write_genus_fixture(vtr, data.frame(
    taxon_id = "1", canonical_name = "Abatus", taxon_rank = "GENUS",
    family = "Schizasteridae", kingdom = "Animalia",
    phylum = "Echinodermata", class = "Echinoidea", order = "Spatangoida",
    stringsAsFactors = FALSE
  ))

  out <- extract_worms_genera(vtr)
  expect_equal(nrow(out), 1L)
  expect_equal(out$kingdom, "Animalia")
  expect_equal(out$phylum, "Echinodermata")
})

test_that("extract_wfo_genera() has no kingdom/phylum/class/order (not stored in WFO)", {
  vtr <- tempfile(fileext = ".vtr")
  on.exit(unlink(vtr), add = TRUE)
  write_genus_fixture(vtr, data.frame(
    taxon_id = "1", canonical_name = "Quercus", taxon_rank = "GENUS",
    family = "Fagaceae", genus = "Quercus", stringsAsFactors = FALSE
  ))

  out <- extract_wfo_genera(vtr)
  expect_equal(nrow(out), 1L)
  expect_true(is.na(out$kingdom))
  expect_equal(out$family, "Fagaceae")
})

test_that("extract_euromed_genera() always stamps kingdom = Plantae", {
  vtr <- tempfile(fileext = ".vtr")
  on.exit(unlink(vtr), add = TRUE)
  write_genus_fixture(vtr, data.frame(
    taxon_id = "1", canonical_name = "Quercus", taxon_rank = "GENUS",
    family = "Fagaceae", genus = "Quercus", stringsAsFactors = FALSE
  ))

  out <- extract_euromed_genera(vtr)
  expect_equal(out$kingdom, "Plantae")
})

test_that("extract_fishbase_genera() derives genera from accepted species only", {
  vtr <- tempfile(fileext = ".vtr")
  on.exit(unlink(vtr), add = TRUE)
  write_genus_fixture(vtr, data.frame(
    taxon_id = c("1", "2", "3"),
    canonical_name = c("Abramis brama", "Abramis ballerus", "Old synonym"),
    taxon_rank = c("SPECIES", "SPECIES", "SPECIES"),
    taxonomic_status = c("ACCEPTED", "ACCEPTED", "SYNONYM"),
    family = c("Cyprinidae", "Cyprinidae", "Cyprinidae"),
    genus  = c("Abramis", "Abramis", "Abramis"),
    kingdom = c("Animalia", "Animalia", "Animalia"),
    stringsAsFactors = FALSE
  ))

  out <- extract_fishbase_genera(vtr)
  # One row per distinct genus, synonym rows and duplicate genus collapsed.
  expect_equal(nrow(out), 1L)
  expect_equal(out$genus, "Abramis")
  expect_equal(out$kingdom, "Animalia")
})

test_that("extract_wcvp_genera() unions genus-rank rows with species-derived genera", {
  vtr <- tempfile(fileext = ".vtr")
  on.exit(unlink(vtr), add = TRUE)
  write_genus_fixture(vtr, data.frame(
    taxon_id = c("1", "2"),
    canonical_name = c("Quercus", "Fagus sylvatica"),
    taxon_rank = c("GENUS", "SPECIES"),
    taxonomic_status = c(NA_character_, "ACCEPTED"),
    family = c("Fagaceae", "Fagaceae"),
    genus  = c(NA_character_, "Fagus"),
    order  = c("Fagales", "Fagales"),
    stringsAsFactors = FALSE
  ))

  out <- extract_wcvp_genera(vtr)
  expect_equal(nrow(out), 2L)
  expect_true(all(c("Quercus", "Fagus") %in% out$genus))
  expect_true(all(out$kingdom == "Plantae"))
})


# ---- resolve_register_backbone_paths() ----

test_that("resolve_register_backbone_paths() prefers an explicit override path", {
  vtr <- tempfile(fileext = ".vtr")
  on.exit(unlink(vtr), add = TRUE)
  write_genus_fixture(vtr, data.frame(
    taxon_id = "1", canonical_name = "Quercus", taxon_rank = "GENUS",
    family = "Fagaceae", genus = "Quercus", stringsAsFactors = FALSE
  ))

  no_manifest <- tempfile(fileext = ".json")
  paths <- with_isolated_wd(
    resolve_register_backbone_paths(
      backbone_paths = list(wfo = vtr),
      output_dir = tempfile(),
      manifest_path = no_manifest,
      verbose = FALSE
    )
  )

  expect_equal(unname(paths["wfo"]), vtr)
})

test_that("resolve_register_backbone_paths() errors on a nonexistent explicit override", {
  expect_error(
    resolve_register_backbone_paths(
      backbone_paths = list(wfo = tempfile(fileext = ".vtr")),
      output_dir = tempfile(),
      manifest_path = tempfile(fileext = ".json"),
      verbose = FALSE
    ),
    "does not exist"
  )
})

test_that("resolve_register_backbone_paths() skips a backbone absent from both local output and manifest", {
  no_manifest <- tempfile(fileext = ".json")
  paths <- with_isolated_wd(
    resolve_register_backbone_paths(
      backbone_paths = NULL,
      output_dir = tempfile(),
      manifest_path = no_manifest,
      verbose = FALSE
    )
  )
  expect_equal(length(paths), 0L)
})


# ---- build_genus_register() / build_backend_coverage() end-to-end ----

test_that("build_genus_register() and build_backend_coverage() build from explicit backbone_paths", {
  wfo_vtr <- tempfile(fileext = ".vtr")
  col_vtr <- tempfile(fileext = ".vtr")
  on.exit(unlink(c(wfo_vtr, col_vtr)), add = TRUE)

  write_genus_fixture(wfo_vtr, data.frame(
    taxon_id = c("1", "2"),
    canonical_name = c("Quercus", "Boletus"),
    taxon_rank = c("GENUS", "GENUS"),
    family = c("Fagaceae", "Boletaceae"),
    genus  = c("Quercus", "Boletus"),
    stringsAsFactors = FALSE
  ))
  write_genus_fixture(col_vtr, data.frame(
    taxon_id = "1", canonical_name = "Quercus", taxon_rank = "GENUS",
    family = "Fagaceae", kingdom = "Plantae", phylum = "Tracheophyta",
    class = "Magnoliopsida", order = "Fagales", stringsAsFactors = FALSE
  ))

  out_dir <- tempfile()
  no_manifest <- tempfile(fileext = ".json")
  bp <- list(wfo = wfo_vtr, col = col_vtr)

  with_isolated_wd({
    reg_dir <- file.path(out_dir, "register")
    reg_path <- build_genus_register(
      backbone_paths = bp, output_dir = reg_dir, version = "test.1",
      manifest_path = no_manifest, verbose = FALSE
    )
    expect_true(file.exists(reg_path))
    expect_true(file.exists(paste0(tools::file_path_sans_ext(reg_path), ".meta")))

    reg <- vectra::tbl(reg_path) |> vectra::collect()
    expect_true(all(c("Quercus", "Boletus") %in% reg$genus))
    # Quercus: WFO carries no classification, COL fills it in.
    q <- reg[reg$genus == "Quercus", ]
    expect_equal(q$kingdom, "Plantae")
    expect_equal(q$taxon_group, "angiosperm")
    # Boletus: only WFO (no classification), family maps to fungus via life-form table.
    b <- reg[reg$genus == "Boletus", ]
    expect_equal(b$taxon_group, "fungus")
    expect_equal(b$kingdom_group, "fungi")

    cov_dir <- file.path(out_dir, "coverage")
    cov_path <- build_backend_coverage(
      backbone_paths = bp, output_dir = cov_dir, version = "test.1",
      manifest_path = no_manifest, verbose = FALSE
    )
    expect_true(file.exists(cov_path))
    cov <- vectra::tbl(cov_path) |> vectra::collect()
    expect_true(all(c("genus", "backend", "version", "date_added") %in% names(cov)))
    expect_true(any(cov$genus == "Quercus" & cov$backend == "wfo"))
    expect_true(any(cov$genus == "Quercus" & cov$backend == "col"))
    expect_false(any(cov$genus == "Boletus" & cov$backend == "col"))
  })
})

test_that("build_genus_register() errors when no backbone can be resolved", {
  with_isolated_wd(
    expect_error(
      build_genus_register(
        backbone_paths = NULL, output_dir = tempfile(),
        manifest_path = tempfile(fileext = ".json"), verbose = FALSE
      ),
      "No backbone"
    )
  )
})

test_that("resolve_register_backbone_paths() accepts a named character vector with a missing name", {
  # build_register() feeds its own resolved-paths output (a named character
  # vector, not a list) straight back in as `backbone_paths` for both
  # sub-builds. `[[` on a name absent from a character vector errors (unlike
  # a list, where it returns NULL) -- this must not propagate as a crash.
  vtr <- tempfile(fileext = ".vtr")
  on.exit(unlink(vtr), add = TRUE)
  write_genus_fixture(vtr, data.frame(
    taxon_id = "1", canonical_name = "Quercus", taxon_rank = "GENUS",
    family = "Fagaceae", genus = "Quercus", stringsAsFactors = FALSE
  ))
  char_vec_paths <- c(wfo = vtr)  # named character vector, not a list

  paths <- with_isolated_wd(
    resolve_register_backbone_paths(
      backbone_paths = char_vec_paths,
      output_dir = tempfile(),
      manifest_path = tempfile(fileext = ".json"),
      verbose = FALSE
    )
  )

  expect_equal(unname(paths["wfo"]), vtr)
  expect_equal(length(paths), 1L)
})

test_that("build_register() builds both artifacts from the same backbone_paths", {
  wfo_vtr <- tempfile(fileext = ".vtr")
  on.exit(unlink(wfo_vtr), add = TRUE)
  write_genus_fixture(wfo_vtr, data.frame(
    taxon_id = "1", canonical_name = "Quercus", taxon_rank = "GENUS",
    family = "Fagaceae", genus = "Quercus", stringsAsFactors = FALSE
  ))

  with_isolated_wd({
    res <- build_register(
      backbone_paths = list(wfo = wfo_vtr), version = "test.1",
      manifest_path = tempfile(fileext = ".json"), verbose = FALSE
    )
    expect_true(file.exists(res$register))
    expect_true(file.exists(res$coverage))
  })
})
