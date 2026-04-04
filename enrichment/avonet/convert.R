# ---- AVONET enrichment: Tobias et al. 2022 (Figshare, CC BY 4.0) ----
#
# Source: AVONET — morphological, ecological and geographical data for all birds
# DOI: 10.6084/m9.figshare.16586228.v5
# ~11k bird species

# Figshare direct download — the Excel file with multiple sheets
.avonet_url <- "https://ndownloader.figshare.com/files/34480856"

download_avonet <- function(dest = tempdir()) {
  xlsx_path <- file.path(dest, "AVONET_BirdLife.xlsx")
  if (file.exists(xlsx_path) && file.size(xlsx_path) > 1000) return(xlsx_path)

  message("Downloading AVONET...")
  h <- curl::new_handle()
  curl::handle_setopt(h, followlocation = TRUE, maxredirs = 10)
  curl::handle_setheaders(h, "User-Agent" = "Mozilla/5.0 R/4.5 taxify-backbones")
  curl::curl_download(.avonet_url, xlsx_path, handle = h)

  if (!file.exists(xlsx_path) || file.size(xlsx_path) < 1000) {
    stop("AVONET download failed (empty or missing file).")
  }
  xlsx_path
}

build_avonet <- function(output_dir) {
  path <- download_avonet()

  if (grepl("\\.xlsx$", path)) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("readxl is required to read AVONET xlsx. Install with: install.packages('readxl')")
    }
    # AVONET xlsx has multiple sheets; we need the species-level averages
    sheets <- readxl::excel_sheets(path)
    # Look for species-level sheet
    sp_sheet <- grep("AVONET.*Birdlife|species|averages", sheets,
                     ignore.case = TRUE, value = TRUE)
    if (length(sp_sheet) == 0L) {
      # Try sheet names by position — typically sheet 2 is species averages
      message("Available sheets: ", paste(sheets, collapse = ", "))
      sp_sheet <- sheets[min(2L, length(sheets))]
    } else {
      sp_sheet <- sp_sheet[1]
    }
    message("Reading AVONET sheet: ", sp_sheet)
    df <- readxl::read_excel(path, sheet = sp_sheet)
    df <- as.data.frame(df, stringsAsFactors = FALSE)
  } else {
    df <- read.csv(path, stringsAsFactors = FALSE)
  }

  # Find species name column
  name_col <- intersect(
    names(df),
    c("Species1", "Species1_BirdLife", "Species", "Scientific",
      "ScientificName", "species_name")
  )
  if (length(name_col) == 0L) name_col <- names(df)[1]
  else name_col <- name_col[1]

  # Map AVONET columns to our schema
  find_col <- function(patterns) {
    for (p in patterns) {
      m <- grep(paste0("^", p, "$"), names(df), ignore.case = TRUE, value = TRUE)
      if (length(m) > 0L) return(m[1])
    }
    # Try partial match
    for (p in patterns) {
      m <- grep(p, names(df), ignore.case = TRUE, value = TRUE)
      if (length(m) > 0L) return(m[1])
    }
    NULL
  }

  safe_num <- function(col_name) {
    if (is.null(col_name)) return(rep(NA_real_, nrow(df)))
    suppressWarnings(as.numeric(df[[col_name]]))
  }

  safe_chr <- function(col_name) {
    if (is.null(col_name)) return(rep(NA_character_, nrow(df)))
    as.character(df[[col_name]])
  }

  out <- data.frame(
    canonical_name  = trimws(df[[name_col]]),
    beak_length     = safe_num(find_col(c("Beak.Length_Culmen", "Beak.Length",
                                          "culmen_length", "Bill.Length"))),
    beak_depth      = safe_num(find_col(c("Beak.Depth", "bill_depth",
                                          "Bill.Depth"))),
    wing_length     = safe_num(find_col(c("Wing.Length", "wing_length"))),
    tail_length     = safe_num(find_col(c("Tail.Length", "tail_length"))),
    tarsus_length   = safe_num(find_col(c("Tarsus.Length", "tarsus_length"))),
    body_mass_g     = safe_num(find_col(c("Mass", "Body.Mass", "body_mass",
                                          "BodyMass", "Mass.g"))),
    hand_wing_index = safe_num(find_col(c("Hand.Wing.Index", "Hand-Wing.Index",
                                          "HWI", "hand_wing_index"))),
    habitat         = safe_chr(find_col(c("Habitat", "Primary.Lifestyle",
                                          "habitat"))),
    trophic_level   = safe_chr(find_col(c("Trophic.Level", "trophic_level"))),
    trophic_niche   = safe_chr(find_col(c("Trophic.Niche", "trophic_niche"))),
    migration       = safe_chr(find_col(c("Migration", "migration"))),
    stringsAsFactors = FALSE
  )

  # Normalize migration values
  if (!all(is.na(out$migration))) {
    mig <- tolower(trimws(out$migration))
    out$migration <- ifelse(grepl("^1$|^sedentar|^resident", mig), "sedentary",
                    ifelse(grepl("^2$|^partial", mig), "partial",
                    ifelse(grepl("^3$|^full|^migra", mig), "full",
                    NA_character_)))
  }

  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0, ]
  out <- out[!duplicated(out$canonical_name), ]

  # Resolve source names against all 7 backends
  out <- resolve_enrichment_names(out)

  vtr_path <- file.path(output_dir, "avonet.vtr")
  build_enrichment_vtr(
    out, vtr_path,
    name       = "avonet",
    version    = "1.0",
    source_url = .avonet_url,
    source_doi = "10.6084/m9.figshare.16586228.v5",
    license    = "CC BY 4.0",
    attribution = "Tobias JA et al. (2022) AVONET: morphological, ecological and geographical data for all birds. Ecology Letters 25:581-597."
  )
}
