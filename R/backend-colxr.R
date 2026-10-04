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
#
# The export also carries no publication reference: ChecklistBank flattens an
# export this large to one table, in DwC-A and ColDP alike, and drops the
# reference table. COL XR keeps the Base Release's identifiers, so the build
# takes `name_published_in` and `year` from the COL build for every usage whose
# ID, name and authorship agree there (see colxr_col_publications()).

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
#' Queries ChecklistBank for the most recently issued COL XR dataset whose
#' Darwin Core export can be downloaded. Each monthly release carries its own
#' dataset key, so the key is looked up rather than hard-coded. ChecklistBank
#' generates an export only when a logged-in account asks for one, so when the
#' newest release has none, one is requested with [colxr_request_export()].
#' When that cannot run (no `GBIF_USER` / `GBIF_PWD`) or does not finish, the
#' newest release with an export is built instead.
#'
#' @param verbose Logical.
#' @param export_ready Function of a dataset key returning `TRUE` when its
#'   export can be downloaded.
#' @param request_export Function of a dataset key that requests its export
#'   and returns `TRUE` once it can be downloaded.
#' @return A list with `key`, `alias`, `version` and `issued`.
#' @export
colxr_latest_release <- function(verbose = TRUE,
                                 export_ready = colxr_export_ready,
                                 request_export = function(key) {
                                   colxr_request_export(key, verbose = verbose)
                                 }) {
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
  keep <- keep[order(as.Date(issued), decreasing = TRUE)]
  best <- NULL
  for (i in seq_along(keep)) {
    d <- keep[[i]]
    key <- as.character(d$key)
    if (isTRUE(export_ready(key))) {
      best <- d
      break
    }
    if (i == 1L) {
      if (verbose) {
        message(sprintf("%s has no export on ChecklistBank; requesting one.",
                        d$alias))
      }
      if (isTRUE(request_export(key))) {
        best <- d
        break
      }
    }
    if (verbose) {
      message(sprintf("%s has no export on ChecklistBank; skipping.",
                      d$alias))
    }
  }
  if (is.null(best)) {
    stop("No Catalogue of Life Extended Release on ChecklistBank has a ",
         "downloadable export.", call. = FALSE)
  }

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


#' Whether a ChecklistBank export can be downloaded
#'
#' A dataset's export URL serves the archive of a finished export job over the
#' whole dataset. The URL already redirects while that job is running, so the
#' job list is read instead: a finished DwCA job with synonyms and no taxon
#' filter.
#'
#' @param key Character. ChecklistBank dataset key.
#' @return `TRUE` when such a job exists.
#' @noRd
colxr_export_ready <- function(key) {
  url <- sprintf("%s/export?datasetKey=%s&format=DWCA&status=FINISHED&limit=50",
                 .colxr_api_base, key)
  res <- tryCatch(curl::curl_fetch_memory(url), error = function(e) NULL)
  if (is.null(res) || res$status_code != 200L) return(FALSE)
  jobs <- jsonlite::fromJSON(rawToChar(res$content),
                             simplifyDataFrame = FALSE)$result
  any(vapply(jobs, function(j) {
    r <- j$request
    isTRUE(r$synonyms) && is.null(r$root) && is.null(r$taxonID)
  }, logical(1L)))
}


#' Request a ChecklistBank export and wait for it
#'
#' ChecklistBank generates a dataset's export only when a logged-in GBIF
#' account asks for one, so a release can be issued and never become
#' downloadable. This starts a DwCA export job with the account in
#' `GBIF_USER` / `GBIF_PWD` and polls it until it finishes.
#'
#' @param key Character. ChecklistBank dataset key.
#' @param poll Seconds between status checks.
#' @param timeout Seconds before giving up.
#' @param verbose Logical.
#' @return `TRUE` when the export finished, `FALSE` when no account is
#'   configured or the job did not finish.
#' @export
colxr_request_export <- function(key, poll = 30, timeout = 3600,
                                 verbose = TRUE) {
  user <- Sys.getenv("GBIF_USER")
  pwd <- Sys.getenv("GBIF_PWD")
  if (!nzchar(user) || !nzchar(pwd)) {
    if (verbose) {
      message("GBIF_USER / GBIF_PWD not set; cannot request an export of ",
              "dataset ", key, ".")
    }
    return(FALSE)
  }

  h <- curl::new_handle(httpauth = 1L, userpwd = paste0(user, ":", pwd),
                        customrequest = "POST",
                        postfields = '{"format":"DwCA","synonyms":true}')
  curl::handle_setheaders(h, "Content-Type" = "application/json",
                          "Accept" = "application/json")
  res <- curl::curl_fetch_memory(
    sprintf("%s/dataset/%s/export", .colxr_api_base, key), handle = h
  )
  if (!res$status_code %in% c(200L, 201L, 202L)) {
    stop("ChecklistBank refused the export request for dataset ", key,
         " (HTTP ", res$status_code, "): ", rawToChar(res$content),
         call. = FALSE)
  }
  job <- jsonlite::fromJSON(rawToChar(res$content), simplifyDataFrame = FALSE)
  job <- if (is.list(job)) job$key else as.character(job)
  if (verbose) message("Requested DwCA export of dataset ", key, ": job ", job)

  deadline <- Sys.time() + timeout
  while (Sys.time() < deadline) {
    Sys.sleep(poll)
    st <- tryCatch(
      jsonlite::fromJSON(sprintf("%s/export/%s", .colxr_api_base, job),
                         simplifyDataFrame = FALSE),
      error = function(e) list(status = NA_character_)
    )
    if (verbose) message(format(Sys.time(), "%H:%M:%S"), " export ", st$status)
    if (identical(st$status, "finished")) return(TRUE)
    if (st$status %in% c("failed", "canceled")) {
      warning("Export job ", job, " ", st$status, ": ", st$error %||% "",
              call. = FALSE)
      return(FALSE)
    }
  }
  warning("Export job ", job, " did not finish within ", timeout, " s.",
          call. = FALSE)
  FALSE
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
#' @param publications A [colxr_col_publications()] table, or `NULL` to carry
#'   no publication columns.
#' @param verbose Logical.
#' @return A normalized data.frame.
#' @export
normalize_colxr <- function(df, gbif_crosswalk = NULL, publications = NULL,
                            verbose = TRUE) {
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
  if (!is.null(publications)) {
    out <- colxr_attach_publications(out, publications)
  }
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
  once <- !duplicated(data.frame(key, id, stringsAsFactors = FALSE))
  id <- id[once]
  key <- key[once]
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
#' authorship (see `crosswalk_authorship()`). GBIF files the records of a
#' synonym under its accepted taxon, so a synonym usage contributes its
#' accepted taxon's key. A name several GBIF usages share (a homonym, or one
#' name GBIF split itself) maps to the set of their keys, `|`-delimited in
#' ascending order.
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
    taxon_id, canonical_name, authorship, taxon_rank, is_synonym,
    accepted_taxon_id
  ))
  g <- g[!is.na(g$canonical_name) & !is.na(g$taxon_id), , drop = FALSE]
  g$key <- ifelse(g$is_synonym %in% TRUE & !is.na(g$accepted_taxon_id),
                  g$accepted_taxon_id, g$taxon_id)

  au <- crosswalk_authorship(g$authorship)
  has_outer <- nzchar(au$outer)
  structure(
    list(
      full  = collapse_keys(
        crosswalk_key(g$canonical_name, au$full, g$taxon_rank), g$key),
      outer = collapse_keys(
        crosswalk_key(g$canonical_name[has_outer], au$outer[has_outer],
                      g$taxon_rank[has_outer]),
        g$key[has_outer])
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
#' An explicit path wins, then the version published in `manifest.json`, which
#' is the backbone taxify serves and so the one the keys have to describe. A
#' local build is read only when the manifest lists none.
#' @noRd
colxr_gbif_path <- function(gbif_path, output_dir, manifest_path, verbose) {
  manifest <- if (file.exists(manifest_path)) {
    jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  } else {
    list(backends = list())
  }
  path <- .resolve_one_backbone_path(
    "gbif", if (is.null(gbif_path)) list() else list(gbif = gbif_path),
    file.path(output_dir, "_gbif"), manifest, verbose, prefer_local = FALSE
  )
  if (is.null(path)) {
    path <- .resolve_one_backbone_path("gbif", list(), output_dir, manifest,
                                       verbose)
  }
  if (is.null(path)) {
    stop("COL XR carries the legacy GBIF key from the GBIF backbone, which ",
         "was found neither at `gbif_path`, nor in the manifest, nor in ",
         "output/gbif. Build or publish `gbif` first.", call. = FALSE)
  }
  path
}


#' Publication references of the COL Base Release, keyed for COL XR
#'
#' COL XR keeps the Base Release's identifiers, so a usage's publication
#' reference can be read from the COL build by `taxon_id`. Only rows carrying a
#' reference are kept.
#'
#' @param col_path Path to a COL `.vtr` carrying `name_published_in`.
#' @return A data.frame of `taxon_id`, `canonical_name`, `authorship`,
#'   `name_published_in` and `year`.
#' @export
colxr_col_publications <- function(col_path) {
  cols <- c("taxon_id", "canonical_name", "authorship", "name_published_in",
            "year")
  schema <- names(vectra::collect(vectra::slice_head(vectra::tbl(col_path),
                                                     n = 1L)))
  if (!all(cols %in% schema)) {
    stop("The COL build at ", col_path, " carries no name_published_in; ",
         "rebuild COL first.", call. = FALSE)
  }
  pub <- vectra::tbl(col_path) |>
    vectra::select(!!!lapply(cols, as.name)) |>
    vectra::collect()
  pub[!is.na(pub$name_published_in), , drop = FALSE]
}


#' Attach COL publication references to normalized COL XR rows
#'
#' A row takes the reference of the COL usage with its `taxon_id` only when
#' the canonical name and authorship agree, so an identifier COL XR reuses for
#' a corrected spelling (99.7% agree on COL26.8 XR) never carries another
#' name's citation.
#'
#' @param out Normalized COL XR rows.
#' @param publications A [colxr_col_publications()] table.
#' @return `out` with `name_published_in` and `year`.
#' @noRd
colxr_attach_publications <- function(out, publications) {
  m <- match(out$taxon_id, publications$taxon_id)
  same <- !is.na(m) &
    out$canonical_name == publications$canonical_name[m] &
    ((is.na(out$authorship) & is.na(publications$authorship[m])) |
       (!is.na(out$authorship) & out$authorship == publications$authorship[m]))
  same[is.na(same)] <- FALSE
  out$name_published_in <- ifelse(same, publications$name_published_in[m],
                                  NA_character_)
  out$year <- ifelse(same, as.integer(publications$year[m]), NA_integer_)
  out
}


#' Path to the COL build COL XR reads its publication references from
#'
#' The published COL build named in the manifest, else a local build.
#' @noRd
colxr_col_path <- function(col_path, output_dir, manifest_path, verbose) {
  manifest <- if (file.exists(manifest_path)) {
    jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  } else {
    list(backends = list())
  }
  path <- .resolve_one_backbone_path(
    "col", if (is.null(col_path)) list() else list(col = col_path),
    file.path(output_dir, "_col"), manifest, verbose, prefer_local = FALSE
  )
  if (is.null(path)) {
    path <- .resolve_one_backbone_path("col", list(), output_dir, manifest,
                                       verbose)
  }
  if (is.null(path)) {
    stop("COL XR takes its publication references from the COL build, which ",
         "was found neither at `col_path`, nor in the manifest, nor in ",
         "output/col. Build or publish `col` first.", call. = FALSE)
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
#'   is derived from. Defaults to the published build named in
#'   `manifest_path`, else `output/gbif/gbif.vtr`.
#' @param col_path Character or NULL. The `col` `.vtr` the publication
#'   references are read from. Defaults to the published build named in
#'   `manifest_path`, else `output/col/col.vtr`.
#' @param manifest_path Character. Path to `manifest.json`.
#' @param verbose Logical.
#' @return Path to the .vtr file (invisibly).
#' @export
build_colxr <- function(output_dir = "output/colxr", version = NULL,
                        gbif_path = NULL, col_path = NULL,
                        manifest_path = "manifest/manifest.json",
                        verbose = TRUE) {
  release <- colxr_latest_release(verbose = verbose)
  if (is.null(version)) version <- release$version

  gbif_path <- colxr_gbif_path(gbif_path, output_dir, manifest_path, verbose)
  if (verbose) message("Building the GBIF key crosswalk from ", gbif_path)
  crosswalk <- colxr_gbif_crosswalk(gbif_path)

  col_path <- colxr_col_path(col_path, output_dir, manifest_path, verbose)
  if (verbose) message("Reading publication references from ", col_path)
  publications <- colxr_col_publications(col_path)

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
                                       publications = publications,
                                       verbose = FALSE)
                     },
                     verbose = verbose),
    vtr_path, "colxr", version, colxr_export_url(release$key), release$date,
    synonym_pattern = "SYNONYM|MISAPPLIED",
    meta_extra = c(gbif_key_source = unname(tools::md5sum(gbif_path)),
                   publication_source = unname(tools::md5sum(col_path))),
    verbose = verbose
  )

  invisible(vtr_path)
}
