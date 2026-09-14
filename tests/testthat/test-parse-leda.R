# parse_leda() reads each LEDA query export by its header: every output column
# comes from one named field, never from a column position or a pattern that
# can fall through to another field.

leda_file <- function(dir, name, header, rows, preamble_lines = 1L) {
  sql <- rep("SELECT a \"b\" FROM c;;;", preamble_lines)
  writeLines(c("The following table was created as response to the SQL query",
               sql, "on Mon Jul 14 15:27:18 CEST 2008 .", "", header, rows),
             file.path(dir, name), useBytes = TRUE)
}

local_leda_dir <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  leda_file(dir, "seed_mass.txt",
    "SBS name;SBS number;general method;diaspore type;single value [mg];sample size;valid;median;reference;mean SM [mg];maximum SM [mg];minimum SM [mg];number of replicates;general comment;Drying method;original reference;diaspore type code",
    c("Abies alba;697;actual measurement;germinule;79.2;10;1;;Ref A;79.2;;;;;;;3",
      "Abies alba;697;actual measurement;germinule;81;10;1;;Ref B;81;;;;;;Paper X (1999);3",
      "Acer campestre;811;other;one-seeded generative dispersule;60.3;;1;;Ref A;;80;40.6;;;;;2a"))
  leda_file(dir, "seed_shape.txt",
    "SBS number;SBS name;general method;diaspore type;length (single value) [mm];width (single value) [mm];height (single value) [mm];valid;sample size;reference;number of replicates;collection date;DIA morphology comment;general comment;original reference;country",
    c("697;Abies alba;estimation;germinule;10.5;4.2;;-2;;BIOLFLOR database;;;Same [S]; nicht heteromorph [n];;;",
      "811;Acer campestre;actual measurement;germinule;7;5;2;1;;Ref A;;;;;;"))
  leda_file(dir, "canopy_height.txt",
    "SBS name;SBS number;general method;single value [m];sample size;valid;reference;mean CH [m];maximum CH [m];minimum CH [m];number of replicates;standard deviation;standard error;collection date;original reference;country;external support structure",
    c("Abies alba;697;derivation from photos or drawings;50;;1;Ref A;;;;;;;;;;unknown"))
  leda_file(dir, "ssd.txt",
    "SBS number;SBS name;woodiness;number of replicates ;valid;SSD specific method;mean SSD [g/cm^3];maximum SSD [g/cm^3];minimum SSD [g/cm^3];median SSD [g/cm^3];sample size;balance error [mg];collection date;general comment;EUNIS habitat code and name;general method;original reference;reference",
    c("697;Abies alba;woody;1;1;;;719;240;;1;;;moisture content 12-15%;;actual measurement;;Wood density database",
      "697;Abies alba;woody;2;1;;.43;;;;2;;;;;actual measurement;;Hoch 2003",
      "811;Acer campestre;woody;1;1;;690;;;;1;;;Moisture content 15%;;actual measurement;;Woods of the world",
      "25800;Abutilon theophrasti;non-woody;1;1;;;;;;;;;;;derivation;;Barkman 1988"))
  leda_file(dir, "buoyancy.txt",
    "SBS number;general method;SBS name;diaspore type;gen. diaspore type;single value [%];sample size;fixed time step;valid;reference;dispersal type;dispersal vector;gen. dispersal type;gen. dispersal vector;original reference",
    c("811;actual measurement;Acer campestre;germinule;generative dispersule;100;50;T0 - immediately;1;Ref A;dysochor;   dormouse;zoochor;small mammal;",
      "811;actual measurement;Acer campestre;germinule;generative dispersule;20;50;T6 - 1 week;1;Ref A;dysochor;   dormouse;zoochor;small mammal;",
      "811;actual measurement;Acer campestre;germinule;generative dispersule;30;50;T6 - 1 week;1;Ref B;dysochor;   dormouse;zoochor;small mammal;"))
  leda_file(dir, "life_form.txt",
    "SBS number;SBS name;plant growth form;gen. plant growth form;record validity;reference;original reference",
    c("811;Acer campestre;Phanerophyte;Phanerophyte;1;BIOPOP;",
      "811;Acer campestre;Chamaephyte;Chamaephyte;1;FLORAWEB;",
      "811;Acer campestre;Phanerophyte;Phanerophyte;1;BIOLFLOR;",
      "25800;Abutilon theophrasti;Therophyte;Therophyte;1;BIOLFLOR;"),
    preamble_lines = 3L)
  leda_file(dir, "dispersal_type.txt",
    "SBS name;SBS number;dispersal type;dispersal vector;gen. dispersal vector;gen. dispersal type;BYC avg. float. cap. (sv) [%]",
    c("Acer campestre;811;meteorochor;wind;wind;meteorochor;", "Abies alba;697;;;;;"))
  dir
}

test_that("the reader takes the header after the query preamble and repairs '; ' fields", {
  dir <- local_leda_dir()
  s <- .read_leda_table(file.path(dir, "seed_shape.txt"))
  expect_equal(ncol(s), 16L)
  expect_equal(s$`SBS name`, c("Abies alba", "Acer campestre"))
  expect_equal(s$`length (single value) [mm]`, c("10.5", "7"))
  expect_equal(s$`width (single value) [mm]`, c("4.2", "5"))
  expect_equal(s$`DIA morphology comment`[1], "Same [S]; nicht heteromorph [n]")

  b <- .read_leda_table(file.path(dir, "buoyancy.txt"))
  expect_equal(b$`dispersal vector`[1], "   dormouse")
  expect_equal(b$`fixed time step`, c("T0 - immediately", "T6 - 1 week", "T6 - 1 week"))
})

test_that("a row whose excess fields no '; ' accounts for stops the read", {
  dir <- withr::local_tempdir()
  leda_file(dir, "x.txt", "SBS name;a;b", c("Abies alba;1;2", "Abies alba;1;2;3"))
  expect_error(.read_leda_table(file.path(dir, "x.txt")), "more field")
  writeLines(c("no preamble", "SBS name;a", "Abies alba;1"), file.path(dir, "z.txt"))
  expect_error(.read_leda_table(file.path(dir, "z.txt")), "preamble")
})

test_that("each column holds the field its name says", {
  dir <- local_leda_dir()
  out <- parse_leda(dir)
  row <- function(sp) out[out$canonical_name == sp, , drop = FALSE]

  expect_equal(row("Abies alba")$leda_seed_mass_mg, 80.1)
  expect_equal(row("Acer campestre")$leda_seed_mass_mg, 60.3)
  expect_equal(row("Abies alba")$seed_length_mm, 10.5)
  expect_equal(row("Acer campestre")$seed_length_mm, 7)
  expect_equal(row("Abies alba")$canopy_height_m, 50)
  expect_equal(row("Acer campestre")$floating_capacity_1week_pct, 25)
  expect_equal(row("Acer campestre")$raunkiaer_life_form, "Phanerophyte")
  expect_equal(row("Acer campestre")$raunkiaer_variable, 1L)
  expect_equal(row("Abutilon theophrasti")$raunkiaer_variable, 0L)
  expect_equal(row("Acer campestre")$dispersal_type, "meteorochor")
  expect_true(is.na(row("Abies alba")$dispersal_type))
  expect_true(all(is.na(out$leaf_mass_mg)))
  expect_false(any(c("clonal_growth", "buoyancy") %in% names(out)))
})

test_that("stem specific density above LEDA's 1.5 g/cm3 range is read as kg/m3", {
  dir <- local_leda_dir()
  out <- parse_leda(dir)
  abies <- out[out$canonical_name == "Abies alba", ]
  expect_equal(abies$ssd_g_cm3, stats::median(c(0.4795, 0.43)))
  expect_equal(out$ssd_g_cm3[out$canonical_name == "Acer campestre"], 0.69)
  expect_true(all(out$ssd_g_cm3 <= 1.5, na.rm = TRUE))
})

test_that("provenance reaches the files that used to be misread", {
  dir <- local_leda_dir()
  out <- parse_leda(dir)
  refs <- attr(out, "references")
  cite <- function(cell) refs$citation[match(strsplit(cell, "|", fixed = TRUE)[[1]], refs$ref_id)]
  abies <- out[out$canonical_name == "Abies alba", ]
  expect_setequal(cite(abies$seed_length_mm_source), "BIOLFLOR database")
  expect_setequal(cite(abies$leda_seed_mass_mg_source), c("Ref A", "Paper X (1999)"))
  expect_setequal(cite(abies$ssd_g_cm3_source), c("Wood density database", "Hoch 2003"))
  expect_equal(refs$via[refs$citation == "Paper X (1999)"], "Ref B")
  acer <- out[out$canonical_name == "Acer campestre", ]
  expect_setequal(cite(acer$raunkiaer_life_form_source), c("BIOPOP", "BIOLFLOR"))
  expect_setequal(cite(acer$floating_capacity_1week_pct_source), c("Ref A", "Ref B"))
  expect_false("dispersal_type_source" %in% names(out))
})

test_that("a named field missing from its file stops the parse", {
  dir <- local_leda_dir()
  leda_file(dir, "canopy_height.txt", "SBS name;SBS number;mean CH [m];reference",
            "Abies alba;697;50;Ref A")
  expect_error(parse_leda(dir), "single value \\[m\\]")
})
