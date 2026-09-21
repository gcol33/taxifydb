# Mammal Diversity Database (ASM): MDD.zip -> .vtr
#
# The MDD is the American Society of Mammalogists' reference mammal taxonomy,
# distributed as a zip of CSVs from the database's own website repository. Two
# of those files matter here:
#
#   MDD_v<ver>_<n>species.csv   accepted species, one row each, with the full
#                               higher classification (subclass down to subtribe)
#                               and the species authority.
#   Species_Syn_Current_v<ver>.csv
#                               every name ever applied, accepted and not, with
#                               the accepted species it currently belongs to.
#
# Three things about the source shape the parser, each verified against the
# data rather than inferred from a column name:
#
#   1. The two files spell binomials differently. `sciName` in the species file
#      uses an underscore ("Ornithorhynchus_anatinus"); `MDD_species` in the
#      synonym file uses a space. Joining them without normalising matches 0 of
#      66,616 synonym rows, and the failure is silent -- every synonym would
#      simply carry no accepted name.
#
#   2. The synonym's own name is `MDD_normalized_original_combination`, NOT
#      `MDD_genus` + `MDD_specificEpithet`. Those two hold the name's CURRENT
#      placement, so building a binomial from them makes most synonyms point at
#      themselves ("Bos grunniens" -> "Bos grunniens") and resolves nothing.
#      The normalized original combination is the historical name a user
#      actually types: "Macropus rufus" -> "Osphranter rufus".
#
#   3. The synonym file also contains the accepted species themselves, as
#      `MDD_validity == "species"`. Those rows are dropped here; the species
#      file is the source of truth for accepted concepts, and keeping both
#      would duplicate every species.
#
# `MDD_validity` also carries nomen_dubium, species_inquirenda, hybrid,
# unavailable and composite. None of these is a synonymy the source is willing
# to assert, so they are kept as matchable names pointing at their listed
# species rather than being dropped, the same call LCVP's "unresolved" gets.
#
# MDD's classification starts at subclass, so the backbone stamps the fixed
# higher ranks (Animalia / Chordata / Mammalia), as the Reptile Database
# backend does for reptiles.
#
# Licence: MIT, copyright ASM Mammal Diversity Database, declared on the
# repository that distributes MDD.zip.

.mdd_url <- paste0("https://github.com/mammaldiversity/mammaldiversity.github.io/",
                   "raw/refs/heads/master/assets/data/MDD.zip")
.mdd_source_doi <- "10.1093/jmammal/gyaa192"


#' Download and unpack the Mammal Diversity Database archive
#'
#' @param dest Character. Destination directory.
#' @param verbose Logical.
#' @return Path to the directory holding the extracted CSVs.
#' @export
download_mdd <- function(dest = tempdir(), verbose = TRUE) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  zip_path <- file.path(dest, "MDD.zip")

  if (verbose) {
    message("Downloading Mammal Diversity Database (~15 MB)...")
    message(sprintf("  URL: %s", .mdd_url))
  }
  h <- curl::new_handle(connecttimeout = 60, timeout = 600)
  curl::curl_download(.mdd_url, zip_path, handle = h, quiet = !verbose)

  exdir <- file.path(dest, "mdd_extracted")
  utils::unzip(zip_path, exdir = exdir)

  # The archive nests its CSVs one directory down, and macOS zips carry a
  # parallel __MACOSX tree of resource forks that must not be read as data.
  hits <- list.files(exdir, pattern = "^MDD_v.*species\\.csv$",
                     recursive = TRUE, full.names = TRUE)
  hits <- hits[!grepl("__MACOSX", hits, fixed = TRUE)]
  if (!length(hits)) {
    stop("MDD archive contains no MDD_v<ver>_<n>species.csv", call. = FALSE)
  }
  dirname(hits[[1L]])
}


#' MDD release version named by the downloaded archive
#'
#' The archive on MDD's default branch moves with each release; its species file
#' carries the version it holds (`MDD_v2.5_6801species.csv`).
#'
#' @param dir Character. Directory the archive was extracted into.
#' @return Character scalar, e.g. `"2.5"`.
#' @noRd
mdd_archive_version <- function(dir) {
  f <- list.files(dir, pattern = "^MDD_v.*species\\.csv$", recursive = TRUE)
  f <- basename(f[!grepl("__MACOSX", f, fixed = TRUE)])
  v <- unique(sub("^MDD_v([0-9][0-9.]*[0-9])_.*$", "\\1", f))
  if (length(v) != 1L || identical(v, f)) {
    stop("MDD: cannot read the release version from the species file name(s): ",
         paste(f, collapse = ", "), call. = FALSE)
  }
  v
}


#' Release date of an extracted MDD archive
#'
#' Each archive ships a `release.toml` whose `[metadata]` table carries
#' `release_date = "YYYY-MM-DD"`.
#'
#' @param dir Character. Directory the archive was extracted into.
#' @return Character `YYYY-MM-DD`.
#' @noRd
mdd_archive_release_date <- function(dir) {
  f <- list.files(dir, pattern = "^release\\.toml$", recursive = TRUE,
                  full.names = TRUE)
  f <- f[!grepl("__MACOSX", f, fixed = TRUE)]
  lines <- if (length(f) == 1L) readLines(f, warn = FALSE) else character(0)
  hit <- regmatches(lines, regexec(
    "^\\s*release_date\\s*=\\s*\"([^\"]*)\"", lines))
  hit <- unique(vapply(Filter(length, hit), `[`, character(1L), 2L))
  if (length(hit) != 1L) {
    stop("MDD: cannot read release_date from the archive's release.toml.",
         call. = FALSE)
  }
  source_date_from(hit)
}


#' Read and normalize the Mammal Diversity Database
#'
#' @param dir Character. Directory holding the extracted MDD CSVs.
#' @param verbose Logical.
#' @return A normalized data.frame ready for [precompute_backbone()].
#' @export
read_mdd <- function(dir, verbose = TRUE) {
  pick <- function(pattern) {
    f <- list.files(dir, pattern = pattern, full.names = TRUE)
    f <- f[!grepl("__MACOSX", f, fixed = TRUE)]
    if (!length(f)) stop(sprintf("MDD: no file matching %s", pattern), call. = FALSE)
    f[[1L]]
  }
  sp_path  <- pick("^MDD_v.*species\\.csv$")
  syn_path <- pick("^Species_Syn_Current_v.*\\.csv$")

  if (verbose) message("Reading MDD species and synonym tables...")
  sp  <- utils::read.csv(sp_path,  stringsAsFactors = FALSE, check.names = FALSE)
  syn <- utils::read.csv(syn_path, stringsAsFactors = FALSE, check.names = FALSE)
  if (verbose) {
    message(sprintf("  %s accepted species, %s name records",
                    format(nrow(sp), big.mark = ","),
                    format(nrow(syn), big.mark = ",")))
  }

  # ---- accepted species ----
  # sciName is underscore-separated; the space form is what a user types and
  # what the synonym file's MDD_species uses.
  acc_name <- gsub("_", " ", trimws(as.character(sp$sciName)), fixed = TRUE)
  acc_id   <- as.character(sp$id)

  # MDD splits the authority into author, year and a parentheses flag rather
  # than shipping a rendered string.
  auth_a <- trimws(as.character(sp$authoritySpeciesAuthor))
  auth_y <- trimws(as.character(sp$authoritySpeciesYear))
  paren  <- trimws(as.character(sp$authorityParentheses))
  auth   <- ifelse(nzchar(auth_a) & nzchar(auth_y), paste0(auth_a, ", ", auth_y),
                   ifelse(nzchar(auth_a), auth_a, NA_character_))
  auth   <- ifelse(!is.na(auth) & paren %in% c("1", "TRUE", "true"),
                   paste0("(", auth, ")"), auth)

  accepted <- data.frame(
    taxon_id               = acc_id,
    canonical_name         = acc_name,
    taxon_rank             = "species",
    taxonomic_status       = "ACCEPTED",
    accepted_name_usage_id = NA_character_,
    family                 = trimws(as.character(sp$family)),
    genus                  = trimws(as.character(sp$genus)),
    specific_epithet       = trimws(as.character(sp$specificEpithet)),
    authorship             = auth,
    infraspecific_epithet  = NA_character_,
    order                  = trimws(as.character(sp$order)),
    stringsAsFactors       = FALSE
  )

  # ---- synonyms ----
  # Drop the accepted species the synonym file repeats, then key each remaining
  # name on its historical combination.
  syn <- syn[trimws(as.character(syn$MDD_validity)) != "species", , drop = FALSE]
  syn_name <- trimws(as.character(syn$MDD_normalized_original_combination))
  syn_of   <- trimws(as.character(syn$MDD_species))

  keep <- nzchar(syn_name) & !is.na(syn_name) & nzchar(syn_of) & !is.na(syn_of)
  # A name whose original combination already equals the species it belongs to
  # is that species, not a synonym of it (3,708 of MDD's records). Emitting it
  # would give every such species a second row differing only in status, which
  # the match stage can read as an ambiguous target. The Reptile Database
  # backend drops the same shape.
  keep <- keep & syn_name != syn_of
  syn  <- syn[keep, , drop = FALSE]
  syn_name <- syn_name[keep]
  syn_of   <- syn_of[keep]

  # A synonym is only usable if the species it points at is an accepted concept.
  target <- match(syn_of, acc_name)
  syn    <- syn[!is.na(target), , drop = FALSE]
  syn_name <- syn_name[!is.na(target)]
  target   <- target[!is.na(target)]
  if (verbose) {
    message(sprintf("  %s synonyms resolve to an accepted species",
                    format(length(syn_name), big.mark = ",")))
  }

  syn_auth_a <- trimws(as.character(syn$MDD_author))
  syn_auth_y <- trimws(as.character(syn$MDD_year))
  syn_auth   <- ifelse(nzchar(syn_auth_a) & nzchar(syn_auth_y),
                       paste0(syn_auth_a, ", ", syn_auth_y),
                       ifelse(nzchar(syn_auth_a), syn_auth_a, NA_character_))

  # A trinomial original combination is an infraspecific name; its epithet is
  # the third token.
  parts     <- strsplit(syn_name, "[[:space:]]+")
  n_tok     <- lengths(parts)
  syn_genus <- vapply(parts, function(p) if (length(p) >= 1L) p[[1L]] else NA_character_, "")
  syn_epi   <- vapply(parts, function(p) if (length(p) >= 2L) p[[2L]] else NA_character_, "")
  syn_infra <- vapply(parts, function(p) if (length(p) >= 3L) p[[3L]] else NA_character_, "")

  synonyms <- data.frame(
    taxon_id               = paste0("mdd_syn_", as.character(syn$MDD_syn_ID)),
    canonical_name         = syn_name,
    taxon_rank             = ifelse(n_tok >= 3L, "subspecies", "species"),
    taxonomic_status       = "SYNONYM",
    accepted_name_usage_id = acc_id[target],
    family                 = trimws(as.character(syn$MDD_family)),
    genus                  = syn_genus,
    specific_epithet       = syn_epi,
    authorship             = syn_auth,
    infraspecific_epithet  = syn_infra,
    order                  = trimws(as.character(syn$MDD_order)),
    stringsAsFactors       = FALSE
  )

  out <- rbind(accepted, synonyms)

  # ---- fixed higher classification (mammals only) ----
  out$kingdom <- "Animalia"
  out$phylum  <- "Chordata"
  out$class   <- "Mammalia"

  if (verbose) message("Normalizing to unified schema...")
  col_map <- list(
    taxon_id               = "taxon_id",
    canonical_name         = "canonical_name",
    taxon_rank             = "taxon_rank",
    taxonomic_status       = "taxonomic_status",
    accepted_name_usage_id = "accepted_name_usage_id",
    family                 = "family",
    genus                  = "genus",
    specific_epithet       = "specific_epithet",
    authorship             = "authorship",
    infraspecific_epithet  = "infraspecific_epithet"
  )
  extra_cols <- list(kingdom = "kingdom", phylum = "phylum",
                     class = "class", order = "order")

  normalize_backbone(out, col_map, extra_cols)
}


#' Build the Mammal Diversity Database backbone .vtr from source
#'
#' @param output_dir Character. Output directory.
#' @param version Character or NULL. Defaults to the MDD version the downloaded
#'   archive names in its species file (`MDD_v2.5_...species.csv`).
#' @param verbose Logical.
#' @return Path to the .vtr file (invisibly).
#' @export
build_mdd <- function(output_dir = "output/mdd", version = NULL,
                      verbose = TRUE) {
  tmp <- tempfile("mdd_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  dir_path <- download_mdd(dest = tmp, verbose = verbose)
  if (is.null(version)) version <- mdd_archive_version(dir_path)
  source_date <- mdd_archive_release_date(dir_path)
  df <- read_mdd(dir_path, verbose = verbose)

  if (verbose) message("Precomputing keys and embedding synonyms...")
  df <- precompute_backbone(df)

  vtr_path <- file.path(output_dir, "mdd.vtr")
  build_vtr(df, vtr_path, "mdd", version, .mdd_url, source_date)

  invisible(vtr_path)
}
