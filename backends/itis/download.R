# ---- ITIS: Download SQLite dump ----

.itis_url <- "https://www.itis.gov/downloads/itisSqlite.zip"

#' Download and extract the ITIS SQLite database
#'
#' @param dest Character. Destination directory.
#' @param verbose Logical.
#' @return Path to the extracted SQLite database file.
download_itis <- function(dest = tempdir(), verbose = TRUE) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)

  zip_path <- file.path(dest, "itisSqlite.zip")

  if (verbose) message("Downloading ITIS SQLite dump (~212 MB)...")
  utils::download.file(.itis_url, zip_path, mode = "wb", quiet = !verbose)

  if (verbose) message("Extracting...")
  utils::unzip(zip_path, exdir = dest)

  # Find the .sqlite file (name varies by release)
  sqlite_files <- list.files(dest, pattern = "\\.sqlite$",
                             recursive = TRUE, full.names = TRUE)
  if (length(sqlite_files) == 0L) {
    stop("No .sqlite file found in ITIS download.")
  }

  sqlite_path <- sqlite_files[1L]
  if (verbose) message(sprintf("ITIS database: %s", basename(sqlite_path)))

  # Clean up zip
  unlink(zip_path)

  sqlite_path
}
