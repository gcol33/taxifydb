# ---- EltonTraits enrichment: Wilman et al. 2014 (Figshare, CC0) ----
#
# Source: EltonTraits 1.0 — Species-level foraging attributes of the
#         world's birds and mammals
# DOI: 10.6084/m9.figshare.c.3306933.v1
# ~9993 birds + ~5400 mammals = ~15.4k species

# Direct Figshare download links for the two data files
.elton_birds_url <- "https://ndownloader.figshare.com/files/5631081"
.elton_mammals_url <- "https://ndownloader.figshare.com/files/5631084"

download_elton <- function(dest = tempdir()) {
  birds_path <- file.path(dest, "BirdFuncDat.txt")
  mammals_path <- file.path(dest, "MamFuncDat.txt")

  if (!file.exists(birds_path)) {
    message("Downloading EltonTraits birds...")
    utils::download.file(.elton_birds_url, birds_path, mode = "wb", quiet = FALSE)
  }
  if (!file.exists(mammals_path)) {
    message("Downloading EltonTraits mammals...")
    utils::download.file(.elton_mammals_url, mammals_path, mode = "wb", quiet = FALSE)
  }
  list(birds = birds_path, mammals = mammals_path)
}

build_elton_traits <- function(output_dir) {
  files <- download_elton()

  # Read both files (tab-separated)
  birds <- read.delim(files$birds, stringsAsFactors = FALSE, quote = "")
  mammals <- read.delim(files$mammals, stringsAsFactors = FALSE, quote = "")

  # --- Birds ---
  # Name column: "Scientific" or "Scientific.Name" or first column
  bird_name_col <- intersect(
    names(birds),
    c("Scientific", "Scientific.Name", "ScientificName")
  )
  if (length(bird_name_col) == 0L) bird_name_col <- names(birds)[1]
  else bird_name_col <- bird_name_col[1]

  # Map columns (EltonTraits uses specific column names)
  bird_map <- list(
    diet_inv       = c("Diet.Inv", "Diet-Inv"),
    diet_vend      = c("Diet.Vend", "Diet-Vend"),
    diet_vect      = c("Diet.Vect", "Diet-Vect"),
    diet_vfish     = c("Diet.Vfish", "Diet-Vfish"),
    diet_vunk      = c("Diet.Vunk", "Diet-Vunk"),
    diet_scav      = c("Diet.Scav", "Diet-Scav"),
    diet_fruit     = c("Diet.Fruit", "Diet-Fruit"),
    diet_nect      = c("Diet.Nect", "Diet-Nect"),
    diet_seed      = c("Diet.Seed", "Diet-Seed"),
    diet_plantother = c("Diet.PlantO", "Diet-PlantO"),
    foraging_water      = c("ForStrat.watbelowsurf", "ForStrat-watbelowsurf"),
    foraging_ground     = c("ForStrat.ground", "ForStrat-ground"),
    foraging_understory = c("ForStrat.understory", "ForStrat-understory"),
    foraging_midhigh    = c("ForStrat.midhigh", "ForStrat-midhigh"),
    foraging_canopy     = c("ForStrat.canopy", "ForStrat-canopy"),
    foraging_aerial     = c("ForStrat.aerial", "ForStrat-aerial"),
    body_mass_g    = c("BodyMass.Value", "BodyMass-Value"),
    nocturnal      = c("Nocturnal", "Activity.Nocturnal", "Activity-Nocturnal")
  )

  resolve_col <- function(df, candidates) {
    # Find first matching column (handle dots/dashes)
    for (c in candidates) {
      # Try exact match first
      if (c %in% names(df)) return(c)
      # Try with dots replaced by dots (R converts dashes to dots in read.delim)
      c_dot <- gsub("-", ".", c, fixed = TRUE)
      if (c_dot %in% names(df)) return(c_dot)
    }
    NULL
  }

  extract_cols <- function(df, name_col, col_map) {
    out <- data.frame(
      canonical_name = trimws(df[[name_col]]),
      stringsAsFactors = FALSE
    )
    for (out_name in names(col_map)) {
      src <- resolve_col(df, col_map[[out_name]])
      if (!is.null(src)) {
        out[[out_name]] <- suppressWarnings(as.numeric(df[[src]]))
      } else {
        out[[out_name]] <- NA_real_
      }
    }
    out
  }

  birds_out <- extract_cols(birds, bird_name_col, bird_map)

  # --- Mammals ---
  mam_name_col <- intersect(
    names(mammals),
    c("Scientific", "Scientific.Name", "ScientificName")
  )
  if (length(mam_name_col) == 0L) mam_name_col <- names(mammals)[1]
  else mam_name_col <- mam_name_col[1]

  mammals_out <- extract_cols(mammals, mam_name_col, bird_map)

  # Combine
  out <- rbind(birds_out, mammals_out)
  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0, ]
  out <- out[!duplicated(out$canonical_name), ]

  # Resolve source names against all 7 backends
  out <- resolve_enrichment_names(out)

  vtr_path <- file.path(output_dir, "elton_traits.vtr")
  build_enrichment_vtr(
    out, vtr_path,
    name       = "elton_traits",
    version    = "1.0",
    source_url = .elton_birds_url,
    source_doi = "10.6084/m9.figshare.c.3306933.v1",
    license    = "CC0",
    attribution = "Wilman H et al. (2014) EltonTraits 1.0: Species-level foraging attributes of the world's birds and mammals. Ecology 95:2027."
  )
}
