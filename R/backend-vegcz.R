# Vegetation of the Czech Republic: Pladias unit list + Nomenclature pages ->
# syntaxon .vtr
#
# The national vegetation classification of the Czech Republic (Chytry (ed.)
# 2007, 2009, 2011, 2013, Vegetace Ceske republiky 1-4, Academia, Praha): 39
# classes, 138 alliances and 496 associations, each with a hierarchical code
# (class "TD", alliance "TDA", association "TDA01"). It places associations
# under their alliances, which the EuroVegChecklist, stopping at alliances,
# does not.
#
# Pladias (Chytry et al. 2021, Preslia 93: 1-87) publishes the unit list as a
# spreadsheet and the synonyms of each unit on its Nomenclature page. Both are
# public data under the Pladias Database Management and Access Rules (2022):
# freely available, for research, teaching, nature conservation and
# environmental assessment, commercial use subject to approval, citing the
# monograph. The syntaxon names and their synonymy are the published
# classification, attributed to Chytry (ed.) 2007-2013.
#
# Frozen inputs, assets of the vegcz-snapshot-<YYYY.MM> release: the unit list
# and `vegcz_synonyms.jsonl`, harvested by
# inst/py/crawlers/crawl_pladias_vegetation.py.

.vegcz_url <- "https://pladias.cz/en/vegetation/"
.vegcz_list_file <- "vegetation_units_of_the_czech_republic.xlsx"

.vegcz_snapshot_version <- "2026.10"
.vegcz_snapshot_release <- paste0("vegcz-snapshot-", .vegcz_snapshot_version)
# The day the Nomenclature pages were harvested.
.vegcz_snapshot_date    <- "2026-10-12"
.vegcz_snapshot_assets  <- c("vegcz_synonyms.jsonl", .vegcz_list_file)


#' Download the frozen Pladias vegetation snapshot
#'
#' @param dest Character. Destination directory.
#' @param local_dir Character or NULL. A local crawl output directory to use
#'   instead of downloading (default `~/dev/taxify-crawls/vegcz`).
#' @param verbose Logical.
#' @return Named list with `synonyms` and `units` paths.
#' @export
download_vegcz <- function(dest = tempdir(), local_dir = NULL,
                           verbose = TRUE) {
  if (is.null(local_dir)) local_dir <- path.expand("~/dev/taxify-crawls/vegcz")
  local <- file.path(local_dir, .vegcz_snapshot_assets)
  if (all(file.exists(local))) {
    if (verbose) message("Using local Pladias vegetation snapshot: ", local_dir)
    return(list(synonyms = local[[1L]], units = local[[2L]]))
  }
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  base <- sprintf("https://github.com/gcol33/taxifydb/releases/download/%s",
                  .vegcz_snapshot_release)
  out <- file.path(dest, .vegcz_snapshot_assets)
  for (i in seq_along(.vegcz_snapshot_assets)) {
    if (verbose) message("Downloading ", .vegcz_snapshot_assets[[i]], " ...")
    curl::curl_download(sprintf("%s/%s", base, .vegcz_snapshot_assets[[i]]),
                        out[[i]], quiet = !verbose)
  }
  list(synonyms = out[[1L]], units = out[[2L]])
}


#' Read the Czech vegetation classification into the syntaxon backbone schema
#'
#' One row per accepted unit (class, alliance, association) and one per name
#' its Pladias Nomenclature page lists as a synonym. A name cited in another
#' author's sense (`sensu ... non ...`, a pseudonym) is `MISAPPLIED`. Every row
#' carries the lineage of its accepted unit in `class` / `alliance` /
#' `association` (`order` is `NA`: the national classification does not use
#' orders).
#'
#' @param synonyms_path Path to `vegcz_synonyms.jsonl`.
#' @param units_path Path to the unit list `.xlsx`.
#' @param verbose Logical.
#' @return A data.frame in the shape of [read_evc()], with `vegcz_code`,
#'   `description_cz` and `description_en` for its own columns.
#' @export
read_vegcz <- function(synonyms_path, units_path, verbose = TRUE) {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop("openxlsx2 is required to read the Pladias unit list.", call. = FALSE)
  }
  raw <- openxlsx2::read_xlsx(units_path, sheet = 1L)
  need <- c("Rank EN", "Code", "Name", "Author citation", "Czech descriptor",
            "English descriptor")
  miss <- setdiff(need, names(raw))
  if (length(miss)) {
    stop(sprintf("Pladias unit list lacks columns: %s",
                 paste(miss, collapse = ", ")), call. = FALSE)
  }
  units <- data.frame(
    code           = trimws(as.character(raw[["Code"]])),
    name           = trimws(as.character(raw[["Name"]])),
    authorship     = trimws(as.character(raw[["Author citation"]])),
    rank           = toupper(trimws(as.character(raw[["Rank EN"]]))),
    description_cz = as.character(raw[["Czech descriptor"]]),
    description_en = as.character(raw[["English descriptor"]]),
    stringsAsFactors = FALSE
  )
  units$authorship[!nzchar(units$authorship)] <- NA_character_
  expect_len <- c(CLASS = 2L, ALLIANCE = 3L, ASSOCIATION = 5L)
  bad <- is.na(expect_len[units$rank]) | nchar(units$code) != expect_len[units$rank]
  if (any(bad)) {
    stop(sprintf("Pladias unit list: %d rows whose rank and code disagree: %s",
                 sum(bad), paste(utils::head(units$code[bad], 5L),
                                 collapse = ", ")), call. = FALSE)
  }
  name_of <- stats::setNames(units$name, units$code)
  units$class <- unname(name_of[substr(units$code, 1L, 2L)])
  units$alliance <- ifelse(nchar(units$code) >= 3L,
                           unname(name_of[substr(units$code, 1L, 3L)]),
                           NA_character_)
  units$association <- ifelse(nchar(units$code) == 5L, units$name,
                              NA_character_)
  if (verbose) {
    message(sprintf("Pladias unit list: %d classes, %d alliances, %d associations",
                    sum(units$rank == "CLASS"), sum(units$rank == "ALLIANCE"),
                    sum(units$rank == "ASSOCIATION")))
  }

  lines <- readLines(synonyms_path, encoding = "UTF-8", warn = FALSE)
  recs <- lapply(lines[nzchar(lines)], jsonlite::fromJSON,
                 simplifyVector = FALSE)
  page_code <- vapply(recs, `[[`, character(1L), "code")
  missing_units <- setdiff(units$code, page_code)
  if (length(missing_units)) {
    stop(sprintf(paste0(
      "Pladias synonymy snapshot lacks %d units of the unit list (%s); re-run ",
      "crawl_pladias_vegetation.py."), length(missing_units),
      paste(utils::head(missing_units, 5L), collapse = ", ")), call. = FALSE)
  }
  syn <- do.call(rbind, lapply(seq_along(recs), function(i) {
    s <- recs[[i]]$synonyms
    if (!length(s)) return(NULL)
    data.frame(
      code = page_code[[i]],
      italic = vapply(s, function(z) z$name %||% NA_character_, character(1L)),
      rest = vapply(s, function(z) z$rest %||% "", character(1L)),
      stringsAsFactors = FALSE
    )
  }))
  # The page sets the name in italics, so the name is read from there; the
  # parse reads the citation and its annotations from what follows.
  parsed <- taxify::parse_name(paste(syn$italic, syn$rest), code = "syntaxon")
  parsed_name <- taxify::parse_name(syn$italic, code = "syntaxon")
  syn <- data.frame(
    code       = syn$code,
    name       = parsed_name$name,
    authorship = parsed$authorship,
    rank       = toupper(parsed_name$rank),
    note       = parsed$name_status,
    remark     = NA_character_,
    stringsAsFactors = FALSE
  )

  syntaxon_backbone_rows(units, syn, code_col = "vegcz_code",
                         backbone = "vegcz")
}


#' Build the Czech vegetation classification syntaxon backbone .vtr
#'
#' @param output_dir Character.
#' @param version Character or NULL. Default the snapshot's.
#' @param local_dir Character or NULL. Local crawl snapshot directory (see
#'   [download_vegcz()]).
#' @param verbose Logical.
#' @return Path to the .vtr file (invisibly).
#' @export
build_vegcz <- function(output_dir = "output/vegcz", version = NULL,
                        local_dir = NULL, verbose = TRUE) {
  if (is.null(version)) version <- .vegcz_snapshot_version
  tmp <- tempfile("vegcz_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  snap <- download_vegcz(dest = tmp, local_dir = local_dir, verbose = verbose)
  df <- read_vegcz(snap$synonyms, snap$units, verbose = verbose)
  write_syntaxon_backbone(df, output_dir, "vegcz", version, .vegcz_url,
                          .vegcz_snapshot_date)
}
