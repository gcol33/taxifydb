# Build OTT backbone from an already-downloaded tarball.
# Bypasses download_ott() when files.opentreeoflife.org has connection issues.
# Initial use: 2026-05-01 to ~2026-05-11, while the upstream Let's Encrypt
# cert was expired (cert NotAfter: 2026-05-01 18:41 UTC).
# Source SHA256 recorded in output/ott/ott3.7.3.tgz.sha256.
#
# Usage: Rscript build_ott_from_local.R

setwd("C:/GillesC/Documents/dev/taxify-backbones")

source("shared/normalize.R")
source("shared/precompute.R")
source("shared/build.R")
source("backends/ott/convert.R")  # provides read_ott(), build_ott(), constants

local_tgz <- "output/ott/ott3.7.3.tgz"
sha_file  <- "output/ott/ott3.7.3.tgz.sha256"
out_dir   <- "output/ott"

stopifnot(file.exists(local_tgz))
stopifnot(file.exists(sha_file))
message("Using local tarball: ", local_tgz)
message("Recorded SHA256:    ", readLines(sha_file)[1L])

# Extract
extract_dir <- file.path(out_dir, "_extract")
unlink(extract_dir, recursive = TRUE)
dir.create(extract_dir, recursive = TRUE)
message("Extracting...")
utils::untar(local_tgz, exdir = extract_dir)

tsv_files <- list.files(extract_dir, pattern = "^taxonomy\\.tsv$",
                        recursive = TRUE, full.names = TRUE)
stopifnot(length(tsv_files) == 1L)
ott_dir <- dirname(tsv_files[1L])
message("OTT dir: ", ott_dir)

# Read + normalize
df <- read_ott(ott_dir, verbose = TRUE)

message("Precomputing keys and embedding synonyms...")
df <- precompute_backbone(df)

# Build .vtr
vtr_path <- file.path(out_dir, "ott.vtr")
build_vtr(df, vtr_path, "ott", .ott_version_default, .ott_url)

# Clean up extracted files (keep the .tgz + sha + vtr + meta)
unlink(extract_dir, recursive = TRUE)

message("\nDone. ", vtr_path)
