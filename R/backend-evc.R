# EuroVegChecklist: FloraVeg.EU unit list + Nomenclature pages -> syntaxon .vtr
#
# The EuroVegChecklist (EVC; Mucina et al. 2016, Applied Vegetation Science 19
# Suppl. 1: 3-264, CC BY 4.0) is the hierarchical classification of European
# vegetation into phytosociological classes, orders and alliances. FloraVeg.EU
# (Chytry et al. 2024) maintains it: each change the European Vegetation
# Classification Committee approves is released as a numbered version of the
# unit list (List_of_European_vegetation_units_version_<n>.xlsx), and the
# synonymy of every unit is published on its Nomenclature page.
#
# The backbone is built from three frozen inputs, all hosted as assets of the
# evc-snapshot-<YYYY.MM> release: the unit list of the current version;
# `evc_synonyms.jsonl`, harvested from the Nomenclature pages by
# inst/py/crawlers/crawl_floraveg_evc.py (one line per accepted unit, its
# synonyms nested with their nomenclatural annotation); and
# `evc2016_synonyms.tsv`, the synonym lists the 2016 paper prints under each
# unit, extracted by inst/py/crawlers/extract_evc2016_synonyms.py. Some
# Nomenclature pages fail on the server partway through the list (an HTTP 500
# body inside the page, the same on every request); for those units alone the
# paper's synonyms of the unit restore what the page lost.
#
# A syntaxon is not a taxon: its name has no genus/epithet structure and
# follows the International Code of Phytosociological Nomenclature, so the
# backbone carries `block_key` instead of genus/epithet, and the denormalized
# lineage of each name's accepted unit (`class`, `order`, `alliance`) instead
# of family/genus. Names are split into name and author citation by
# taxify::parse_name(code = "syntaxon") and keyed by
# taxify::precompute_keys(code = "syntaxon"), the parse and keys taxify's
# matcher applies to a query, so the two sides cannot drift.
#
# Not a member of .backend_builders: enrichment name resolution iterates that
# set to build a per-backbone accepted-name lookup, and a lookup of vegetation
# units has no taxon names in it.

.evc_url <- "https://floraveg.eu/vegetation/"

# The unit list version the snapshot was taken against, and its release date.
.evc_list_version <- "4"
.evc_list_date    <- "2025-06-17"
.evc_list_file    <- "List_of_European_vegetation_units_version_4.xlsx"

.evc_snapshot_version <- "2026.10"
.evc_snapshot_release <- paste0("evc-snapshot-", .evc_snapshot_version)
# The day the Nomenclature pages were harvested.
.evc_snapshot_date    <- "2026-10-12"
.evc_snapshot_assets  <- c("evc_synonyms.jsonl", .evc_list_file,
                           "evc2016_synonyms.tsv")

# The ranks the EVC code encodes: "CM" a class, "CM01" an order, "CM01A" an
# alliance.
.evc_code_ranks <- c("^[A-Z]{2}$" = "CLASS", "^[A-Z]{2}[0-9]{2}$" = "ORDER",
                     "^[A-Z]{2}[0-9]{2}[A-Z]$" = "ALLIANCE")


#' Download the frozen EuroVegChecklist snapshot
#'
#' Fetches the unit list and the harvested synonymy from the
#' `evc-snapshot-<version>` release. A local crawl directory holding both
#' files (the machine that ran the crawl) is used directly.
#'
#' @param dest Character. Destination directory.
#' @param local_dir Character or NULL. A local crawl output directory to use
#'   instead of downloading (default `~/dev/taxify-crawls/evc`).
#' @param verbose Logical.
#' @return Named list with `synonyms`, `units` and `synonyms_2016` paths.
#' @export
download_evc <- function(dest = tempdir(), local_dir = NULL, verbose = TRUE) {
  if (is.null(local_dir)) local_dir <- path.expand("~/dev/taxify-crawls/evc")
  local <- file.path(local_dir, .evc_snapshot_assets)
  if (all(file.exists(local))) {
    if (verbose) message("Using local EuroVegChecklist snapshot: ", local_dir)
    return(list(synonyms = local[[1L]], units = local[[2L]],
                synonyms_2016 = local[[3L]]))
  }

  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  base <- sprintf("https://github.com/gcol33/taxifydb/releases/download/%s",
                  .evc_snapshot_release)
  out <- file.path(dest, .evc_snapshot_assets)
  for (i in seq_along(.evc_snapshot_assets)) {
    if (verbose) message("Downloading ", .evc_snapshot_assets[[i]], " ...")
    curl::curl_download(sprintf("%s/%s", base, .evc_snapshot_assets[[i]]),
                        out[[i]], quiet = !verbose)
  }
  list(synonyms = out[[1L]], units = out[[2L]], synonyms_2016 = out[[3L]])
}


#' Read the EuroVegChecklist unit list
#'
#' @param path Path to `List_of_European_vegetation_units_version_<n>.xlsx`.
#' @return A data.frame of accepted units: `code`, `mnemonic`, `rank`, `name`,
#'   `authorship`, `description`, `evc2016_name`, `evc2016_authorship`.
#' @noRd
read_evc_units <- function(path) {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop("openxlsx2 is required to read the EuroVegChecklist unit list.",
         call. = FALSE)
  }
  raw <- openxlsx2::read_xlsx(path, sheet = 1L)
  nm <- names(raw)
  col <- function(pattern) {
    i <- grep(pattern, nm)
    if (length(i) != 1L) {
      stop(sprintf("EVC unit list: expected one column matching '%s', got %d.",
                   pattern, length(i)), call. = FALSE)
    }
    as.character(raw[[i]])
  }
  code_raw <- trimws(col("^Code$"))
  na_text <- function(x) {
    x <- trimws(x)
    x[is.na(x) | !nzchar(x) | x == "NULL" |
        grepl("^Not included in EuroVegChecklist", x)] <- NA_character_
    x
  }
  units <- data.frame(
    code               = sub("\\s*\\(.*$", "", code_raw),
    mnemonic           = sub("^.*\\((.*)\\).*$", "\\1", code_raw),
    name               = na_text(col("^Syntaxon_name \\(EVC, version")),
    authorship         = na_text(col("^Author \\(EVC, version")),
    description        = na_text(col("^Description$")),
    evc2016_name       = na_text(col("^Syntaxon_name \\(original EVC")),
    evc2016_authorship = na_text(col("^Author \\(original EVC")),
    stringsAsFactors = FALSE
  )
  units$rank <- NA_character_
  for (p in names(.evc_code_ranks)) {
    units$rank[grepl(p, units$code)] <- .evc_code_ranks[[p]]
  }
  bad <- is.na(units$rank) | is.na(units$name)
  if (any(bad)) {
    stop(sprintf("EVC unit list: %d rows with an unreadable code or no name: %s",
                 sum(bad), paste(utils::head(code_raw[bad], 5L), collapse = ", ")),
         call. = FALSE)
  }
  if (anyDuplicated(units$code)) {
    stop("EVC unit list: duplicated codes.", call. = FALSE)
  }
  units
}


#' Read and normalize the EuroVegChecklist into the syntaxon backbone schema
#'
#' One row per accepted unit (class, order, alliance) and one per synonym. A
#' synonym is every name the unit's Nomenclature page lists, plus the name the
#' original EuroVegChecklist (Mucina et al. 2016) used where the current
#' version renamed the unit. Every row carries the lineage of its accepted unit
#' in `class` / `order` / `alliance`, so a match at any rank rolls up.
#'
#' @param synonyms_path Path to `evc_synonyms.jsonl`.
#' @param units_path Path to the unit list `.xlsx`.
#' @param synonyms_2016_path Path to `evc2016_synonyms.tsv`, or `NULL`. Read
#'   only for units whose Nomenclature page was truncated.
#' @param verbose Logical.
#' @return A data.frame: `taxon_id`, `canonical_name`, `authorship`,
#'   `taxon_rank`, `taxonomic_status`, `accepted_name_usage_id`, `evc_code`,
#'   `nomenclatural_note`, `remark`, `class`, `order`, `alliance`,
#'   `association` (always `NA`: the EVC stops at alliances), `evc_mnemonic`,
#'   `description`.
#' @export
read_evc <- function(synonyms_path, units_path, synonyms_2016_path = NULL,
                     verbose = TRUE) {
  units <- read_evc_units(units_path)
  if (verbose) {
    message(sprintf("EVC unit list: %d classes, %d orders, %d alliances",
                    sum(units$rank == "CLASS"), sum(units$rank == "ORDER"),
                    sum(units$rank == "ALLIANCE")))
  }

  # Lineage of each accepted unit, read off its code.
  name_of <- stats::setNames(units$name, units$code)
  units$class <- unname(name_of[substr(units$code, 1L, 2L)])
  units$order <- ifelse(nchar(units$code) >= 4L,
                        unname(name_of[substr(units$code, 1L, 4L)]),
                        NA_character_)
  units$alliance <- ifelse(nchar(units$code) == 5L, units$name, NA_character_)

  # ---- synonyms from the Nomenclature pages ----
  lines <- readLines(synonyms_path, encoding = "UTF-8", warn = FALSE)
  recs <- lapply(lines[nzchar(lines)], jsonlite::fromJSON,
                 simplifyVector = FALSE)
  page_code <- sub("\\s*\\(.*$", "", vapply(recs, `[[`, character(1L), "code"))
  missing_units <- setdiff(units$code, page_code)
  if (length(missing_units)) {
    stop(sprintf(paste0(
      "EVC synonymy snapshot lacks %d units of the unit list (%s); re-run ",
      "crawl_floraveg_evc.py."), length(missing_units),
      paste(utils::head(missing_units, 5L), collapse = ", ")), call. = FALSE)
  }
  truncated <- vapply(recs, function(r) isTRUE(r$truncated), logical(1L))
  if (verbose && any(truncated)) {
    message(sprintf(paste0(
      "  %d unit page(s) failed mid-render on FloraVeg.EU; their synonym ",
      "lists may miss their last entries: %s"), sum(truncated),
      paste(page_code[truncated], collapse = ", ")))
  }
  syn <- do.call(rbind, lapply(seq_along(recs), function(i) {
    s <- recs[[i]]$synonyms
    if (!length(s)) return(NULL)
    data.frame(
      text   = vapply(s, function(z) z$text %||% NA_character_, character(1L)),
      status = vapply(s, function(z) z$status %||% NA_character_, character(1L)),
      remark = vapply(s, function(z) z$remark %||% NA_character_, character(1L)),
      code   = page_code[[i]],
      stringsAsFactors = FALSE
    )
  }))

  # Truncated pages: the paper's synonyms of the unit fill the lost tail. A
  # paper row the page already lists is dropped as a duplicate.
  if (any(truncated) && !is.null(synonyms_2016_path)) {
    p16 <- utils::read.delim(synonyms_2016_path, quote = "", na.strings = "",
                             encoding = "UTF-8", stringsAsFactors = FALSE)
    cut_mn <- units$mnemonic[match(page_code[truncated], units$code)]
    p16 <- p16[p16$mnemonic %in% cut_mn, , drop = FALSE]
    if (nrow(p16)) {
      syn <- rbind(syn, data.frame(
        text = p16$text, status = p16$status, remark = NA_character_,
        code = units$code[match(p16$mnemonic, units$mnemonic)],
        stringsAsFactors = FALSE))
    }
    if (verbose) {
      message(sprintf(paste0(
        "  %d synonym(s) of the 2016 paper considered for the %d truncated ",
        "units"), nrow(p16), sum(truncated)))
    }
  }
  parsed <- taxify::parse_name(syn$text, code = "syntaxon")
  syn$name <- parsed$name
  syn$authorship <- parsed$authorship
  syn$rank <- toupper(parsed$rank)
  # The page's own annotation ("syntax.syn.", "2b", "phantom") first, then any
  # the parse found after the citation ("(Regionalverband)").
  syn$note <- ifelse(is.na(parsed$name_status), syn$status,
                     ifelse(is.na(syn$status), parsed$name_status,
                            paste(syn$status, parsed$name_status, sep = "; ")))
  syn <- syn[, c("code", "name", "authorship", "rank", "note", "remark")]

  # ---- names the original EVC (2016) used for units since renamed ----
  renamed <- !is.na(units$evc2016_name) &
    (units$evc2016_name != units$name |
       !identical_na(units$evc2016_authorship, units$authorship))
  syn <- rbind(syn, data.frame(
    code = units$code[renamed], name = units$evc2016_name[renamed],
    authorship = units$evc2016_authorship[renamed],
    rank = toupper(taxify::parse_name(units$evc2016_name[renamed],
                                      code = "syntaxon")$rank),
    note = "EuroVegChecklist 2016 name", remark = NA_character_,
    stringsAsFactors = FALSE))

  df <- syntaxon_backbone_rows(
    units[, c("code", "name", "authorship", "rank", "class", "order",
              "alliance", "mnemonic", "description")],
    syn, code_col = "evc_code", backbone = "evc")
  names(df)[names(df) == "mnemonic"] <- "evc_mnemonic"
  # The mnemonic names the unit a synonym belongs to as much as its code does.
  df$evc_mnemonic <- units$mnemonic[match(df$evc_code, units$code)]
  attr(df, "truncated_units") <- page_code[truncated]
  df
}


# Two character vectors equal element-wise, NA equal to NA.
identical_na <- function(a, b) {
  (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
}


#' Build the EuroVegChecklist syntaxon backbone .vtr
#'
#' @param output_dir Character.
#' @param version Character or NULL. The EuroVegChecklist version the unit
#'   list carries (default the snapshot's).
#' @param local_dir Character or NULL. Local crawl snapshot directory (see
#'   [download_evc()]).
#' @param verbose Logical.
#' @return Path to the .vtr file (invisibly).
#' @export
build_evc <- function(output_dir = "output/evc", version = NULL,
                      local_dir = NULL, verbose = TRUE) {
  if (is.null(version)) version <- .evc_list_version

  tmp <- tempfile("evc_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  snap <- download_evc(dest = tmp, local_dir = local_dir, verbose = verbose)
  df <- read_evc(snap$synonyms, snap$units, snap$synonyms_2016,
                 verbose = verbose)
  truncated <- attr(df, "truncated_units")

  write_syntaxon_backbone(
    df, output_dir, "evc", version, .evc_url, .evc_snapshot_date,
    extra = c(unit_list_version = .evc_list_version,
              unit_list_date = .evc_list_date,
              truncated_units = paste(truncated, collapse = " ")))
}
