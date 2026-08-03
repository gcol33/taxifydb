# LPSN (List of Prokaryotic names with Standing in Nomenclature): ColDP -> .vtr
#
# LPSN is the nomenclatural authority for prokaryotes, hosted by DSMZ - the same
# house as BacDive, which taxify already ships as an enrichment. It is the list
# that decides whether a bacterial or archaeal name is validly published, which
# is a question NCBI, GBIF and OTT do not answer at all.
#
# ---- Source route -------------------------------------------------------
#
# LPSN's own download page and API both sit behind a (free) DSMZ account, and
# its terms forbid automated download except through those routes. LPSN is
# CC BY-SA 4.0 and explicitly permits redistribution, so it is also published as
# a ColDP dataset on GBIF ChecklistBank (dataset 2015), which is an open mirror
# and the route taken here. Same house style as WoRMS, which taxify already
# pulls from ChecklistBank.
#
# The archive is ColDP (`NameUsage.tsv`), not the DwC-A that WoRMS returns, so
# there is no denormalized classification: family and above come from walking
# `parentID` with `resolve_hierarchy()`, the way ITIS, NCBI and OTT do.
#
# ---- The two status axes, deliberately kept apart -----------------------
#
# LPSN encodes two independent things and flattening them into one column would
# throw away the reason to have LPSN at all:
#
#   col:status      taxonomic  - accepted / provisionally accepted / synonym /
#                               bare name
#   col:nameStatus  nomenclatural - available / unavailable / UNACCEPTABLE
#
# A name can be taxonomically accepted and nomenclaturally unavailable at the
# same time. Only the taxonomic axis drives ACCEPTED vs SYNONYM; the
# nomenclatural axis is carried verbatim as `nom_status`, and the raw taxonomic
# label as `lpsn_status`, so a caller can see standing rather than infer it.
#
# `parentID` does double duty, as ColDP intends: on an accepted row it is the
# parent taxon, on a synonym row it is the accepted name. Verified against the
# real reassignments - Escherichia adecarboxylata -> Leclercia adecarboxylata,
# E. blattae -> Shimwellia blattae, E. vulneris -> Pseudescherichia vulneris.
#
# ---- Three things the file needs, each measured against it --------------
#
#   1. 892 rows carry an EMPTY `scientificName` with the name sitting inside
#      `authorship`, wrapped in double quotes: `" Acaryochloridales " Strunecky
#      and Mares 2023`. That is LPSN's own convention for a name without
#      standing, and the ColDP export preserves the quoting rather than the
#      name. All 892 recover from the quoted span, so they are parsed out
#      instead of dropped - matching a name and reporting that it has no
#      standing is the whole point of this backbone.
#   2. 49 of those recovered species and 15 of the genera are `Candidatus`
#      names, the formal category for uncultivated taxa. The prefix is part of
#      the name (NCBI stores it too) so `canonical_name` keeps it, while
#      `genus` and the epithets are taken after stripping it, so genus-level
#      lookups still land.
#   3. 42 rows are ChecklistBank placeholders with negative IDs and names like
#      "Gammaproteobacteria, not assigned to family". They are containers, not
#      names, and are dropped.
#
# LPSN's `kingdom` rank holds the 2024 Goeker & Oren kingdom names
# (Bacillati, Pseudomonadati, Thermoproteati, ...) which no other backbone uses.
# The rank every other backbone calls kingdom for prokaryotes is LPSN's DOMAIN
# (Bacteria / Archaea), so that is what fills the `kingdom` column; the LPSN
# kingdom name is kept alongside as `lpsn_kingdom`.
#
# Licence: CC BY-SA 4.0. Redistribution of LPSN material electronically
# requires a link back to the originating LPSN page, so every row carries its
# own record link in `lpsn_url`.

.lpsn_url <- "https://api.checklistbank.org/dataset/2015/archive"
.lpsn_source_page <- "https://lpsn.dsmz.de"
.lpsn_version_default <- "2026.07"


#' Download the LPSN ColDP archive from ChecklistBank
#'
#' @param dest Character. Destination directory.
#' @param verbose Logical.
#' @return Path to the directory holding the extracted `NameUsage.tsv`.
#' @export
download_lpsn <- function(dest = tempdir(), verbose = TRUE) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  zip_path <- file.path(dest, "lpsn_coldp.zip")

  if (verbose) {
    message("Downloading LPSN ColDP from ChecklistBank (~3 MB)...")
  }
  h <- curl::new_handle(connecttimeout = 60, low_speed_limit = 1000,
                        low_speed_time = 300)
  curl::curl_download(.lpsn_url, zip_path, handle = h, quiet = !verbose)

  if (verbose) message("Extracting...")
  entries <- utils::unzip(zip_path, list = TRUE)$Name
  target <- entries[grepl("NameUsage\\.tsv$", entries, ignore.case = TRUE)]
  if (length(target) == 0L) {
    stop("NameUsage.tsv not found in the LPSN ColDP archive.", call. = FALSE)
  }
  utils::unzip(zip_path, files = target[1L], exdir = dest, junkpaths = TRUE)

  unlink(zip_path)
  dest
}


# LPSN writes a name that has no standing as a quoted string inside the
# authorship field, leaving scientificName empty. Pull the first quoted span
# back out as the name and hand back the rest as the authorship.
.lpsn_recover_quoted <- function(auth) {
  hit  <- grepl('"[^"]+"', auth)
  nm   <- rep(NA_character_, length(auth))
  rest <- auth

  if (any(hit)) {
    nm[hit]   <- trimws(sub('^[^"]*"([^"]+)".*$', "\\1", auth[hit]))
    rest[hit] <- trimws(sub('"[^"]+"', "", auth[hit]))
  }
  rest[!nzchar(rest)] <- NA_character_
  list(name = nm, authorship = rest)
}


#' Read and normalize the LPSN checklist
#'
#' @param lpsn_dir Character. Directory holding `NameUsage.tsv`.
#' @param verbose Logical.
#' @return A normalized data.frame ready for [precompute_backbone()].
#' @export
read_lpsn <- function(lpsn_dir, verbose = TRUE) {
  f <- list.files(lpsn_dir, pattern = "NameUsage\\.tsv$",
                  ignore.case = TRUE, full.names = TRUE)
  if (length(f) == 0L) {
    stop("NameUsage.tsv not found in the LPSN directory.", call. = FALSE)
  }

  if (verbose) message("Reading LPSN name usages...")
  # Double quotes are CONTENT here (they mark names without standing), not
  # field wrappers, so quote processing stays off.
  d <- utils::read.delim(
    f[1L],
    fileEncoding   = "UTF-8",
    stringsAsFactors = FALSE,
    quote          = "",
    na.strings     = "",
    check.names    = FALSE
  )
  names(d) <- sub("^col:", "", names(d))
  if (verbose) {
    message(sprintf("  %s rows", format(nrow(d), big.mark = ",")))
  }

  id     <- trimws(as.character(d$ID))
  parent <- trimws(as.character(d$parentID))
  rank   <- tolower(trimws(as.character(d$rank)))
  name   <- trimws(as.character(d$scientificName))
  auth   <- trimws(as.character(d$authorship))
  nomst  <- trimws(as.character(d$nameStatus))
  status <- tolower(trimws(as.character(d$status)))
  link   <- trimws(as.character(d$link))

  name[is.na(name)]   <- ""
  auth[is.na(auth)]   <- ""
  parent[is.na(parent)] <- ""

  # ChecklistBank container rows ("X, not assigned to family") are not names.
  synthetic <- grepl("^-", id) | grepl(", not assigned to ", name, fixed = TRUE)
  if (any(synthetic) && verbose) {
    message(sprintf("  dropping %d ChecklistBank placeholder rows",
                    sum(synthetic)))
  }

  # Names with no standing keep their name inside the quoted authorship.
  blank <- !synthetic & !nzchar(name)
  if (any(blank)) {
    rec <- .lpsn_recover_quoted(auth[blank])
    name[blank] <- ifelse(is.na(rec$name), "", rec$name)
    auth[blank] <- ifelse(is.na(rec$authorship), "", rec$authorship)
    if (verbose) {
      message(sprintf("  recovered %s names from quoted authorship (%s without standing)",
                      format(sum(nzchar(name[blank])), big.mark = ","),
                      format(sum(blank), big.mark = ",")))
    }
  }

  keep <- !synthetic & nzchar(name)
  id <- id[keep]; parent <- parent[keep]; rank <- rank[keep]
  name <- name[keep]; auth <- auth[keep]; nomst <- nomst[keep]
  status <- status[keep]; link <- link[keep]

  auth[!nzchar(auth)] <- NA_character_
  link[!nzchar(link)] <- NA_character_
  nomst[!nzchar(nomst)] <- NA_character_

  # `Candidatus` is part of the name but not part of the genus.
  is_cand  <- grepl("^Candidatus ", name)
  bare     <- sub("^Candidatus ", "", name)
  tok      <- strsplit(bare, "[[:space:]]+")
  tok1     <- vapply(tok, function(p) p[[1L]], character(1L))
  tok2     <- vapply(tok, function(p) if (length(p) >= 2L) p[[2L]] else NA_character_,
                     character(1L))
  tok_last <- vapply(tok, function(p) p[[length(p)]], character(1L))

  is_sp    <- rank %in% c("species", "subspecies")
  genus    <- ifelse(rank == "genus", bare, ifelse(is_sp, tok1, NA_character_))
  epithet  <- ifelse(is_sp, tok2, NA_character_)
  infra    <- ifelse(rank == "subspecies", tok_last, NA_character_)

  # Taxonomic axis only. `provisionally accepted` and `bare name` are still
  # concepts in their own right; they are separated from a true synonym, whose
  # parentID points at the name that replaced it.
  tax_status <- ifelse(status == "synonym", "SYNONYM", "ACCEPTED")
  accepted_id <- ifelse(status == "synonym" & nzchar(parent),
                        parent, NA_character_)

  if (verbose) {
    message(sprintf("  %s accepted concepts / %s synonyms",
                    format(sum(tax_status == "ACCEPTED"), big.mark = ","),
                    format(sum(tax_status == "SYNONYM"), big.mark = ",")))
  }

  if (verbose) message("Walking the parent hierarchy...")
  h <- resolve_hierarchy(
    data.frame(id = id, parent_id = parent, rank = rank, name = name,
               stringsAsFactors = FALSE),
    target_ranks = c("family", "order", "class", "phylum", "kingdom", "domain")
  )

  out <- data.frame(
    taxon_id               = id,
    canonical_name         = name,
    taxon_rank             = rank,
    taxonomic_status       = tax_status,
    accepted_name_usage_id = accepted_id,
    family                 = h$resolved_family,
    genus                  = genus,
    specific_epithet       = epithet,
    authorship             = auth,
    infraspecific_epithet  = infra,
    # Bacteria / Archaea: what every other backbone calls the kingdom here.
    kingdom                = h$resolved_domain,
    lpsn_kingdom           = h$resolved_kingdom,
    phylum                 = h$resolved_phylum,
    class                  = h$resolved_class,
    order                  = h$resolved_order,
    nom_status             = nomst,
    lpsn_status            = status,
    candidatus             = ifelse(is_cand, "TRUE", NA_character_),
    lpsn_url               = link,
    stringsAsFactors       = FALSE
  )

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
  extra_cols <- list(
    kingdom      = "kingdom",
    lpsn_kingdom = "lpsn_kingdom",
    phylum       = "phylum",
    class        = "class",
    order        = "order",
    nom_status   = "nom_status",
    lpsn_status  = "lpsn_status",
    candidatus   = "candidatus",
    lpsn_url     = "lpsn_url"
  )

  normalize_backbone(out, col_map, extra_cols)
}


#' Build the LPSN backbone .vtr from source
#'
#' @param output_dir Character. Output directory.
#' @param version Character or NULL. Defaults to the bundled LPSN version.
#' @param verbose Logical.
#' @return Path to the .vtr file (invisibly).
#' @export
build_lpsn <- function(output_dir = "output/lpsn", version = NULL,
                       verbose = TRUE) {
  if (is.null(version)) version <- .lpsn_version_default

  tmp <- tempfile("lpsn_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  lpsn_dir <- download_lpsn(dest = tmp, verbose = verbose)
  df <- read_lpsn(lpsn_dir, verbose = verbose)

  if (verbose) message("Precomputing keys and embedding synonyms...")
  df <- precompute_backbone(df)

  vtr_path <- file.path(output_dir, "lpsn.vtr")
  build_vtr(df, vtr_path, "lpsn", version, .lpsn_source_page)

  invisible(vtr_path)
}
