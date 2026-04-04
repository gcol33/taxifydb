# ---- LEDA enrichment: Kleyer et al. 2008 (LEDA Traitbase) ----
#
# Source: LEDA Traitbase — NW European plant traits
# DOI: 10.1111/j.1365-2745.2008.01430.x
# ~8,000 species
#
# LEDA distributes trait data as separate CSV files from their website.
# Base URL: https://uol.de/en/biology/landeco/research/leda/data-files
#
# Each trait is a separate download. We merge them on species name.

.leda_base <- "https://uol.de/fileadmin/user_upload/biologie/ag/landeco/download/LEDA/"

# Trait file URLs (these are the direct download links)
.leda_files <- list(
  life_form       = paste0(.leda_base, "life_form.txt"),
  dispersal       = paste0(.leda_base, "dispersal_type.txt"),
  tv              = paste0(.leda_base, "TV.txt"),
  seed_mass       = paste0(.leda_base, "seed_mass.txt"),
  canopy_height   = paste0(.leda_base, "canopy_height.txt"),
  leaf_mass       = paste0(.leda_base, "leaf_mass.txt"),
  sla             = paste0(.leda_base, "SLA.txt"),
  clonal_growth   = paste0(.leda_base, "clonal_growth.txt"),
  buoyancy        = paste0(.leda_base, "buoyancy.txt")
)

download_leda <- function(dest = tempdir()) {
  leda_dir <- file.path(dest, "leda")
  dir.create(leda_dir, showWarnings = FALSE, recursive = TRUE)

  paths <- list()
  for (trait_name in names(.leda_files)) {
    path <- file.path(leda_dir, paste0(trait_name, ".txt"))
    if (!file.exists(path)) {
      message(sprintf("Downloading LEDA %s...", trait_name))
      tryCatch(
        curl::curl_download(.leda_files[[trait_name]], path, quiet = FALSE),
        error = function(e) {
          message(sprintf("  Warning: failed to download %s: %s",
                          trait_name, conditionMessage(e)))
        }
      )
    }
    if (file.exists(path)) paths[[trait_name]] <- path
  }

  paths
}

read_leda_trait <- function(path) {
  # LEDA files are semicolon-separated or tab-separated with varying encodings
  tryCatch({
    # Try semicolon first
    df <- read.csv(path, sep = ";", stringsAsFactors = FALSE, fileEncoding = "latin1")
    if (ncol(df) <= 1L) {
      # Try tab
      df <- read.delim(path, stringsAsFactors = FALSE, fileEncoding = "latin1")
    }
    df
  }, error = function(e) {
    tryCatch(
      read.delim(path, stringsAsFactors = FALSE),
      error = function(e2) NULL
    )
  })
}

build_leda <- function(output_dir) {
  paths <- download_leda()

  if (length(paths) == 0L) {
    stop("Could not download any LEDA trait files.")
  }

  # Read and merge all traits
  # Each file has a species name column + trait value(s)
  # We need to find the species name column in each file and aggregate

  find_name_col <- function(df) {
    candidates <- c("SBS_name", "species", "Species", "SBS.name",
                     "species_name", "name", "taxon")
    col <- intersect(names(df), candidates)
    if (length(col) > 0L) return(col[1])
    # Try partial match
    col <- grep("species|name|SBS", names(df), ignore.case = TRUE, value = TRUE)
    if (length(col) > 0L) return(col[1])
    names(df)[1]
  }

  # Start with an empty master table
  master <- NULL

  # Life form
  if ("life_form" %in% names(paths)) {
    df <- read_leda_trait(paths$life_form)
    if (!is.null(df) && nrow(df) > 0L) {
      nc <- find_name_col(df)
      lf_col <- grep("life.form|raunkiaer|lf_", names(df), ignore.case = TRUE, value = TRUE)
      if (length(lf_col) > 0L) {
        trait_df <- data.frame(
          canonical_name = trimws(df[[nc]]),
          raunkiaer_life_form = trimws(df[[lf_col[1]]]),
          stringsAsFactors = FALSE
        )
        # Mark variable if multiple forms per species
        counts <- table(trait_df$canonical_name)
        variable_spp <- names(counts[counts > 1])
        trait_df <- trait_df[!duplicated(trait_df$canonical_name), ]
        trait_df$raunkiaer_variable <- as.integer(
          trait_df$canonical_name %in% variable_spp
        )
        master <- trait_df
      }
    }
  }

  merge_trait <- function(master, trait_name, trait_col_patterns, out_col,
                          as_type = "numeric") {
    if (!trait_name %in% names(paths)) return(master)
    df <- read_leda_trait(paths[[trait_name]])
    if (is.null(df) || nrow(df) == 0L) return(master)

    nc <- find_name_col(df)
    tc <- NULL
    for (p in trait_col_patterns) {
      m <- grep(p, names(df), ignore.case = TRUE, value = TRUE)
      if (length(m) > 0L) { tc <- m[1]; break }
    }
    if (is.null(tc)) {
      # Use last column as trait value
      tc <- names(df)[ncol(df)]
    }

    vals <- if (as_type == "numeric") {
      suppressWarnings(as.numeric(df[[tc]]))
    } else if (as_type == "integer") {
      suppressWarnings(as.integer(df[[tc]]))
    } else {
      as.character(df[[tc]])
    }

    trait_df <- data.frame(
      canonical_name = trimws(df[[nc]]),
      val = vals,
      stringsAsFactors = FALSE
    )
    names(trait_df)[2] <- out_col

    # Aggregate: median for numeric, first for character
    if (as_type %in% c("numeric", "integer")) {
      trait_df <- aggregate(
        trait_df[[out_col]],
        by = list(canonical_name = trait_df$canonical_name),
        FUN = function(x) median(x, na.rm = TRUE)
      )
      names(trait_df)[2] <- out_col
    } else {
      trait_df <- trait_df[!duplicated(trait_df$canonical_name), ]
    }

    if (is.null(master)) return(trait_df)
    merge(master, trait_df, by = "canonical_name", all = TRUE)
  }

  master <- merge_trait(master, "dispersal",
                        c("dispersal.*type", "dispersal_type", "disp"),
                        "dispersal_type", "character")

  master <- merge_trait(master, "tv",
                        c("terminal.*velocity", "tv", "TV"),
                        "terminal_velocity_ms", "numeric")

  master <- merge_trait(master, "seed_mass",
                        c("seed.*mass", "sm_mean", "mass"),
                        "leda_seed_mass_mg", "numeric")

  master <- merge_trait(master, "canopy_height",
                        c("canopy.*height", "ch_mean", "height"),
                        "canopy_height_m", "numeric")

  master <- merge_trait(master, "leaf_mass",
                        c("leaf.*mass", "lm_mean", "mass"),
                        "leaf_mass_mg", "numeric")

  master <- merge_trait(master, "sla",
                        c("sla", "SLA", "specific.*leaf"),
                        "sla_mm2_mg", "numeric")

  master <- merge_trait(master, "clonal_growth",
                        c("clonal", "CGO", "cgo"),
                        "clonal_growth", "integer")

  master <- merge_trait(master, "buoyancy",
                        c("buoyancy", "buoy"),
                        "buoyancy", "character")

  if (is.null(master) || nrow(master) == 0L) {
    stop("No LEDA data could be parsed from downloaded files.")
  }

  # Ensure all expected columns exist
  expected <- c("canonical_name", "raunkiaer_life_form", "raunkiaer_variable",
                "dispersal_type", "terminal_velocity_ms", "leda_seed_mass_mg",
                "canopy_height_m", "leaf_mass_mg", "sla_mm2_mg",
                "clonal_growth", "buoyancy")
  for (col in expected) {
    if (!col %in% names(master)) master[[col]] <- NA
  }
  master <- master[, expected]

  master <- master[!is.na(master$canonical_name) &
                     nchar(master$canonical_name) > 0, ]
  master <- master[!duplicated(master$canonical_name), ]

  # Resolve source names against all 7 backends
  master <- resolve_enrichment_names(master)

  vtr_path <- file.path(output_dir, "leda.vtr")
  build_enrichment_vtr(
    master, vtr_path,
    name       = "leda",
    version    = "2008.1",
    source_url = .leda_base,
    source_doi = "10.1111/j.1365-2745.2008.01430.x",
    license    = "Free for academic use",
    attribution = "Kleyer M et al. (2008) The LEDA Traitbase: a database of life-history traits of the Northwest European flora. J Ecol 96:1266-1274."
  )
}
