# COL XR (Catalogue of Life Extended Release): flat DwC-A -> data.frame -> .vtr
#
# The Extended Release builds on the COL Base Release by programmatically
# integrating additional taxonomic and nomenclatural sources through
# ChecklistBank, and is the taxonomy GBIF.org serves by default. It is
# published monthly, each release under its own ChecklistBank dataset key, so
# the release to build is resolved from the ChecklistBank API rather than
# pinned to a fixed URL.
#
# The DwC-A export is a single flat TSV carrying dwc:/clb: namespace prefixes
# on the column names (stripped on read). Three things differ from the Base
# Release read by read_col():
#
#   scientificName is canonical. Authorship sits in its own column and is not
#   repeated in the name, so no authorship subtraction is needed.
#
#   The higher classification arrives denormalized on every row, so the
#   parent-tree propagation col_resolve_classification() performs for the Base
#   Release (which ships those Darwin Core columns empty) has nothing to do.
#
#   There are no epithet columns, so the specific and infraspecific epithets
#   are split off the canonical name against the genus column.
#
# Identifiers are alphanumeric (CRLT8, G7PX), not the integers the legacy GBIF
# backbone used. GBIF serves occurrence records only for those legacy keys, and
# neither the export nor ChecklistBank's usage records carry a mapping to them,
# so the build derives one from the GBIF backbone (see colxr_gbif_crosswalk())
# and stores it beside the COL XR identifier as `gbif_key`.

.colxr_api_base <- "https://api.checklistbank.org"

# Alias form of a Catalogue of Life Extended Release, e.g. "COL26.7 XR".
# ChecklistBank also serves xrelease datasets contributed by other projects,
# which this pattern leaves out.
.colxr_alias_pattern <- "^COL[0-9]+\\.[0-9]+ XR$"

# Columns needed for matching (after stripping namespace prefixes)
.colxr_match_cols <- c(
  "taxonID",
  "parentNameUsageID",
  "acceptedNameUsageID",
  "taxonomicStatus",
  "taxonRank",
  "scientificName",
  "scientificNameAuthorship",
  "kingdom",
  "phylum",
  "class",
  "order",
  "family",
  "genus"
)

# Extra columns preserved in the .vtr for runtime consumers
.colxr_extra_cols <- c(
  "superfamily",
  "subfamily",
  "tribe",
  "subtribe",
  "subgenus",
  "higherClassification",
  "taxGroup"
)


#' Resolve the latest Catalogue of Life Extended Release
#'
#' Queries ChecklistBank for the most recently issued COL XR dataset. Each
#' monthly release carries its own dataset key, so the key is looked up rather
#' than hard-coded.
#'
#' @param verbose Logical.
#' @return A list with `key`, `alias`, `version` and `issued`.
#' @export
colxr_latest_release <- function(verbose = TRUE) {
  url <- paste0(.colxr_api_base, "/dataset?origin=xrelease&limit=200")
  txt <- tryCatch(
    paste(readLines(url, warn = FALSE), collapse = ""),
    error = function(e) {
      stop("Could not reach ChecklistBank to resolve the latest COL XR: ",
           conditionMessage(e), call. = FALSE)
    }
  )
  res <- jsonlite::fromJSON(txt, simplifyDataFrame = FALSE)$result
  keep <- Filter(function(d) {
    !is.null(d$alias) && grepl(.colxr_alias_pattern, d$alias)
  }, res)
  if (length(keep) == 0L) {
    stop("No Catalogue of Life Extended Release found on ChecklistBank.",
         call. = FALSE)
  }

  issued <- vapply(keep, function(d) d$issued %||% "", character(1L))
  best <- keep[[which.max(as.Date(issued))]]

  out <- list(
    key     = as.character(best$key),
    alias   = best$alias,
    version = release_version_from_date(best$issued),
    issued  = best$issued,
    date    = source_date_from(best$issued)
  )
  if (verbose) {
    message(sprintf("Latest COL XR: %s (issued %s, dataset key %s)",
                    out$alias, out$issued, out$key))
  }
  out
}


#' Download and extract the COL XR Darwin Core Archive
#'
#' @param dest Character. Destination directory.
#' @param key Character or NULL. ChecklistBank dataset key. Resolved from
#'   [colxr_latest_release()] when `NULL`.
#' @param verbose Logical.
#' @return Path to the extracted TSV.
#' @export
download_colxr <- function(dest = tempdir(), key = NULL, verbose = TRUE) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  if (is.null(key)) key <- colxr_latest_release(verbose = verbose)$key

  url <- colxr_export_url(key)
  if (verbose) {
    message("Downloading COL XR from ChecklistBank (~140 MB)...")
    message(sprintf("  URL: %s", url))
  }
  zip_path <- download_curl_file(url, dest, "colxr_download.zip")

  entries <- utils::unzip(zip_path, list = TRUE)$Name
  target <- entries[grepl("\\.tsv$", entries)]
  if (length(target) == 0L) {
    stop("No .tsv found in the COL XR archive.", call. = FALSE)
  }
  if (verbose) message(sprintf("Extracting %s ...", basename(target[1L])))
  utils::unzip(zip_path, files = target[1L], exdir = dest, junkpaths = TRUE)

  unlink(zip_path)
  file.path(dest, basename(target[1L]))
}


#' ChecklistBank export URL for one dataset key
#'
#' @param key Character. ChecklistBank dataset key.
#' @return The export URL. It redirects to the generated archive, which
#'   [download_curl_file()] follows.
#' @noRd
colxr_export_url <- function(key) {
  sprintf("%s/dataset/%s/export.zip?format=DwCA", .colxr_api_base, key)
}


#' Read and normalize the COL XR export
#'
#' @param tsv_path Character. Path to the extracted COL XR TSV.
#' @param verbose Logical.
#' @return A normalized data.frame ready for [precompute_backbone()].
#' @export
read_colxr <- function(tsv_path, verbose = TRUE) {
  if (!file.exists(tsv_path)) {
    stop("COL XR TSV not found: ", tsv_path, call. = FALSE)
  }

  if (verbose) message("Reading COL XR export...")
  # quote = "" keeps genuine embedded double-quotes in informal names literal;
  # ChecklistBank ships the export without field-wrapping quotes.
  if (requireNamespace("data.table", quietly = TRUE)) {
    df <- as.data.frame(
      data.table::fread(tsv_path, sep = "\t", quote = "",
                        na.strings = c("", "NA"), encoding = "UTF-8",
                        colClasses = "character", showProgress = FALSE),
      stringsAsFactors = FALSE
    )
  } else {
    df <- utils::read.delim(tsv_path, quote = "", comment.char = "",
                            stringsAsFactors = FALSE, na.strings = "",
                            fileEncoding = "UTF-8", check.names = FALSE,
                            colClasses = "character")
  }
  if (verbose) message(sprintf("  %s rows", format(nrow(df), big.mark = ",")))
  normalize_colxr(df, verbose = verbose)
}


#' Normalize one block of COL XR rows to the unified schema
#'
#' Split out of [read_colxr()] so the streaming build can apply it to a chunk
#' at a time. Nothing here depends on rows outside the block.
#'
#' @param df A data.frame of raw COL XR rows, with the export's own column
#'   names.
#' @param gbif_crosswalk A [colxr_gbif_crosswalk()], or `NULL` to leave
#'   `gbif_key` unset.
#' @param verbose Logical.
#' @return A normalized data.frame.
#' @export
normalize_colxr <- function(df, gbif_crosswalk = NULL, verbose = TRUE) {
  # Strip namespace prefixes (dwc:taxonID -> taxonID, clb:taxGroup -> taxGroup)
  names(df) <- sub("^[a-z]+:", "", names(df))

  keep <- intersect(c(.colxr_match_cols, .colxr_extra_cols), names(df))
  df <- df[, keep, drop = FALSE]

  text_cols <- intersect(
    c("scientificName", "scientificNameAuthorship", "kingdom", "phylum",
      "class", "order", "family", "genus"),
    names(df)
  )
  for (col in text_cols) df[[col]] <- trimws(df[[col]])

  if (verbose) message("Splitting epithets off the canonical name...")
  ep <- split_scientific_name(df$scientificName, df$genus)
  df$specificEpithet <- ep$specific
  df$infraspecificEpithet <- ep$infraspecific

  if (verbose) message("Normalizing to unified schema...")
  col_map <- list(
    taxon_id                = "taxonID",
    canonical_name          = "scientificName",
    taxon_rank              = "taxonRank",
    taxonomic_status        = "taxonomicStatus",
    accepted_name_usage_id  = "acceptedNameUsageID",
    kingdom                 = "kingdom",
    phylum                  = "phylum",
    class                   = "class",
    order                   = "order",
    family                  = "family",
    genus                   = "genus",
    specific_epithet        = "specificEpithet",
    authorship              = "scientificNameAuthorship",
    infraspecific_epithet   = "infraspecificEpithet"
  )

  extra_cols <- list()
  for (col in c(.colxr_extra_cols, "parentNameUsageID")) {
    if (col %in% names(df)) extra_cols[[col]] <- col
  }

  out <- normalize_backbone(df, col_map, extra_cols)
  out$gbif_key <- if (is.null(gbif_crosswalk)) {
    rep(NA_character_, nrow(out))
  } else {
    colxr_gbif_lookup(gbif_crosswalk, out$canonical_name, out$authorship,
                      out$taxon_rank)
  }
  out
}


#' Authorship as compared across the two backbones
#'
#' COL XR and GBIF write the same authorship differently: a zoological name
#' carries its year and brackets in one and not the other ("(Hupe, 1857)" and
#' "Hupe"), and a botanical recombination carries its basionym author in
#' brackets in one and not the other ("(Romagn.) Noordel." and "Noordel.").
#' Two forms are compared. `full` keeps every author, brackets dropped;
#' `outer` keeps only the authors outside the brackets. Both fold case and
#' accents and drop years and punctuation.
#' @noRd
crosswalk_authorship <- function(authorship) {
  s <- tolower(fold_accents(ifelse(is.na(authorship), "", authorship)))
  squash <- function(x) gsub("[^a-z0-9]+", "", gsub("[0-9]{4}", " ", x))
  list(full  = squash(gsub("[()]", " ", s)),
       outer = squash(gsub("[(][^)]*[)]", " ", s)))
}


#' Crosswalk key: canonical name, normalized authorship and rank
#'
#' A unit separator joins the parts so none can run into its neighbour.
#' @noRd
crosswalk_key <- function(canonical_name, authorship, taxon_rank) {
  paste(canonical_name, authorship, taxon_rank, sep = "\x1f")
}


#' Group ids by key into `|`-delimited sets, ascending by id
#' @noRd
collapse_keys <- function(key, id) {
  o <- order(suppressWarnings(as.numeric(id)))
  id <- id[o]
  key <- key[o]
  first <- !duplicated(key)
  out <- stats::setNames(id[first], key[first])
  shared <- unique(key[duplicated(key)])
  if (length(shared) > 0L) {
    hit <- key %in% shared
    sets <- split(id[hit], key[hit])
    out[names(sets)] <- vapply(sets, paste, character(1L), collapse = "|")
  }
  out
}


#' Legacy GBIF keys for each COL XR usage, from the GBIF backbone
#'
#' COL XR replaced the taxonomy behind GBIF.org, but GBIF serves occurrence
#' records only for the numeric keys of its legacy backbone. This reads the
#' `gbif` backbone and groups its usages by canonical name, rank and
#' authorship (see `crosswalk_authorship()`). A name several GBIF usages share
#' (a homonym, or one name GBIF split itself) maps to the set of their keys,
#' `|`-delimited in ascending order.
#'
#' @param gbif_path Character. Path to the `gbif` `.vtr`.
#' @return A `colxr_gbif_crosswalk`: the key sets grouped by full authorship
#'   and by outer authorship, for [colxr_gbif_lookup()].
#' @export
colxr_gbif_crosswalk <- function(gbif_path) {
  if (!file.exists(gbif_path)) {
    stop("GBIF backbone not found: ", gbif_path, call. = FALSE)
  }
  g <- vectra::collect(vectra::select(
    vectra::tbl(gbif_path),
    taxon_id, canonical_name, authorship, taxon_rank
  ))
  g <- g[!is.na(g$canonical_name) & !is.na(g$taxon_id), , drop = FALSE]

  au <- crosswalk_authorship(g$authorship)
  has_outer <- nzchar(au$outer)
  structure(
    list(
      full  = collapse_keys(
        crosswalk_key(g$canonical_name, au$full, g$taxon_rank), g$taxon_id),
      outer = collapse_keys(
        crosswalk_key(g$canonical_name[has_outer], au$outer[has_outer],
                      g$taxon_rank[has_outer]),
        g$taxon_id[has_outer])
    ),
    class = "colxr_gbif_crosswalk"
  )
}


#' Look COL XR usages up in a GBIF crosswalk
#'
#' In order: the usage's full authorship against GBIF's, its outer authorship
#' against GBIF's outer authorship, and, where GBIF records no authorship for
#' the name, the name and rank alone. A GBIF usage that names a different
#' author is a different name and is never matched.
#'
#' @param crosswalk A [colxr_gbif_crosswalk()].
#' @param canonical_name,authorship,taxon_rank Character vectors of the COL XR
#'   usages.
#' @return Character vector of `|`-delimited GBIF keys, `NA` where none.
#' @export
colxr_gbif_lookup <- function(crosswalk, canonical_name, authorship,
                              taxon_rank) {
  taxon_rank <- rep_len(taxon_rank, length(canonical_name))
  au <- crosswalk_authorship(authorship)
  pick <- function(table, key) unname(table[key])

  out <- pick(crosswalk$full,
              crosswalk_key(canonical_name, au$full, taxon_rank))
  todo <- is.na(out) & nzchar(au$outer)
  out[todo] <- pick(crosswalk$outer,
                    crosswalk_key(canonical_name[todo], au$outer[todo],
                                  taxon_rank[todo]))
  todo <- is.na(out)
  out[todo] <- pick(crosswalk$full,
                    crosswalk_key(canonical_name[todo], "", taxon_rank[todo]))
  out
}


#' Resolve the GBIF backbone a COL XR build crosswalks against
#'
#' An explicit path wins, then a local `output/gbif/gbif.vtr`, then the
#' version published in `manifest.json`, the same order the register build uses.
#' @noRd
colxr_gbif_path <- function(gbif_path, output_dir, manifest_path, verbose) {
  manifest <- if (file.exists(manifest_path)) {
    jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  } else {
    list(backends = list())
  }
  path <- .resolve_one_backbone_path(
    "gbif", if (is.null(gbif_path)) list() else list(gbif = gbif_path),
    file.path(output_dir, "_gbif"), manifest, verbose
  )
  if (is.null(path)) {
    stop("COL XR carries the legacy GBIF key from the GBIF backbone, which ",
         "was found neither at `gbif_path`, nor in output/gbif, nor in the ",
         "manifest. Build or publish `gbif` first.", call. = FALSE)
  }
  path
}


#' Build the COL XR backbone .vtr
#'
#' @param output_dir Character. Output directory.
#' @param version Character or NULL. The COL XR release version, resolved from
#'   ChecklistBank when `NULL` so the stamped version is the date of the data
#'   rather than the date of the build.
#' @param gbif_path Character or NULL. The `gbif` `.vtr` the `gbif_key` column
#'   is derived from. Defaults to `output/gbif/gbif.vtr`, else the published
#'   build named in `manifest_path`.
#' @param manifest_path Character. Path to `manifest.json`.
#' @param verbose Logical.
#' @return Path to the .vtr file (invisibly).
#' @export
build_colxr <- function(output_dir = "output/colxr", version = NULL,
                        gbif_path = NULL,
                        manifest_path = "manifest/manifest.json",
                        verbose = TRUE) {
  release <- colxr_latest_release(verbose = verbose)
  if (is.null(version)) version <- release$version

  gbif_path <- colxr_gbif_path(gbif_path, output_dir, manifest_path, verbose)
  if (verbose) message("Building the GBIF key crosswalk from ", gbif_path)
  crosswalk <- colxr_gbif_crosswalk(gbif_path)

  tmp <- tempfile("colxr_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  tsv_path <- download_colxr(dest = tmp, key = release$key, verbose = verbose)

  # The export inflates to a multi-gigabyte TSV, so it is staged a block at a
  # time rather than assembled in memory. COL uses MISAPPLIED alongside
  # ACCEPTED/SYNONYM, and both flavours of synonym are treated as synonyms for
  # matching, exactly as for the Base Release. PROVISIONALLY ACCEPTED stays an
  # accepted concept.
  vtr_path <- file.path(output_dir, "colxr.vtr")
  build_vtr_streamed(
    delim_chunk_feed(tsv_path,
                     normalize = function(chunk) {
                       normalize_colxr(chunk, gbif_crosswalk = crosswalk,
                                       verbose = FALSE)
                     },
                     verbose = verbose),
    vtr_path, "colxr", version, colxr_export_url(release$key), release$date,
    synonym_pattern = "SYNONYM|MISAPPLIED",
    meta_extra = c(gbif_key_source = basename(gbif_path)),
    verbose = verbose
  )

  invisible(vtr_path)
}
