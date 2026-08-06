# Euro+Med PlantBase: CDM snapshot -> normalized data.frame -> .vtr
#
# Euro+Med is the taxonomic reference for European and Mediterranean vascular
# plants, bryophytes and some fungi/algae. The maintained checklist lives on
# the EDIT CyberTaxonomy CDM server behind europlusmed.org; it exposes no
# working bulk export, so `inst/py/crawlers/crawl_euromed.py` harvests the full
# classification through the per-taxon portal API and freezes it as an NDJSON
# snapshot (one accepted taxon per line, its synonyms nested) plus a nodes.tsv
# of genus/suprageneric treeIndex paths. read_euromed() builds the unified
# backbone from that snapshot, the same frozen-snapshot pattern used by the
# ecoflora / floraweb enrichments. License: CC-BY-SA (applies to derived .vtr).
#
# The former source (germansl.infinitenature.org EuroMed.zip) was frozen at
# Euro+Med 2020 v1.2 and could not refresh (issue #7).

.euromed_url <- "https://europlusmed.org"          # provenance URL for build_vtr()
.euromed_version_default <- "2026.07"

# Snapshot assets (produced by crawl_euromed.py, hosted as release assets).
.euromed_snapshot_release <- "euromed-snapshot-2026.07"
.euromed_snapshot_assets <- c("euromed.jsonl", "nodes.tsv")


#' Download the frozen Euro+Med CDM snapshot
#'
#' Fetches the crawler's NDJSON snapshot and node-path table from the release.
#' If a local crawl directory already holds both files (the machine that ran
#' the crawl), they are used directly.
#'
#' @param dest Character. Destination directory.
#' @param local_dir Character or NULL. A local crawl output directory to use
#'   instead of downloading (default `~/dev/taxify-crawls/euromed`).
#' @param verbose Logical.
#' @return Named list with `jsonl` and `nodes` paths.
#' @export
download_euromed <- function(dest = tempdir(), local_dir = NULL,
                             verbose = TRUE) {
  if (is.null(local_dir)) {
    local_dir <- path.expand("~/dev/taxify-crawls/euromed")
  }
  local <- file.path(local_dir, .euromed_snapshot_assets)
  if (all(file.exists(local))) {
    if (verbose) message("Using local Euro+Med crawl snapshot: ", local_dir)
    return(list(jsonl = local[[1L]], nodes = local[[2L]]))
  }

  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  base <- sprintf("https://github.com/gcol33/taxifydb/releases/download/%s",
                  .euromed_snapshot_release)
  out <- file.path(dest, .euromed_snapshot_assets)
  for (i in seq_along(.euromed_snapshot_assets)) {
    url <- sprintf("%s/%s", base, .euromed_snapshot_assets[[i]])
    if (verbose) message("Downloading ", .euromed_snapshot_assets[[i]], " ...")
    curl::curl_download(url, out[[i]], quiet = !verbose)
  }
  list(jsonl = out[[1L]], nodes = out[[2L]])
}


# Authorship = the full name string with the canonical stripped off. For
# infraspecific autonyms the species author sits mid-name so the canonical is
# not a prefix; fall back to whatever follows the last infraspecific marker
# (empty for a true autonym, which formally has no author).
.euromed_infra_markers <- c("subsp.", "var.", "f.", "nothosubsp.", "subvar.",
                            "convar.", "proles", "race", "grex", "subf.",
                            "nothovar.", "nothof.")

.euromed_authorship <- function(fullname, canonical) {
  marker_re <- paste0("\\b(",
                      paste(gsub("\\.", "\\\\.", .euromed_infra_markers),
                            collapse = "|"),
                      ")\\s+\\S+")
  vapply(seq_along(fullname), function(i) {
    fn <- fullname[[i]]; cn <- canonical[[i]]
    if (is.na(fn) || !nzchar(fn)) return(NA_character_)
    if (!is.na(cn) && nzchar(cn)) {
      auth <- trimws(sub(cn, "", fn, fixed = TRUE))
      if (nzchar(auth) && auth != fn) return(auth)
    }
    m <- gregexpr(marker_re, fn, perl = TRUE)[[1L]]
    if (m[1L] == -1L) return(NA_character_)
    last_end <- m[length(m)] + attr(m, "match.length")[length(m)] - 1L
    a <- trimws(substring(fn, last_end + 1L))
    if (nzchar(a)) a else NA_character_
  }, character(1L))
}

# Epithets parsed from the canonical name (which carries the infraspecific
# markers, e.g. "Euphorbia helioscopia subsp. helioscopioides").
.euromed_epithets <- function(canonical) {
  words <- strsplit(canonical, "\\s+")
  rank_markers <- .euromed_infra_markers
  specific <- vapply(words, function(w) {
    if (length(w) >= 2L) w[[2L]] else NA_character_
  }, character(1L))
  infra <- vapply(words, function(w) {
    if (length(w) < 3L) return(NA_character_)
    marker_pos <- which(w %in% rank_markers)
    if (length(marker_pos) > 0L && marker_pos[[1L]] < length(w)) {
      w[[marker_pos[[1L]] + 1L]]
    } else {
      w[[3L]]
    }
  }, character(1L))
  list(specific = specific, infraspecific = infra)
}

# genus name -> family name, from the genus/suprageneric treeIndex paths. Each
# node's own id is the last "#NNN#" segment of its treeIndex; a genus inherits
# the family whose own id appears among its treeIndex ancestors.
.euromed_family_map <- function(nodes_path) {
  if (!file.exists(nodes_path)) return(stats::setNames(character(0), character(0)))
  nd <- utils::read.delim(nodes_path, header = FALSE, sep = "\t", quote = "",
                          stringsAsFactors = FALSE,
                          col.names = c("uuid", "rank", "name", "treeIndex"))
  segs <- strsplit(gsub("^#|#$", "", nd$treeIndex), "#", fixed = TRUE)
  nd$own <- vapply(segs, function(s) if (length(s)) s[[length(s)]] else NA_character_,
                   character(1L))
  rank_u <- toupper(nd$rank)

  fam <- nd[rank_u == "FAMILY" & !is.na(nd$own), , drop = FALSE]
  fam_by_id <- stats::setNames(fam$name, fam$own)
  fam_ids <- names(fam_by_id)

  gen <- nd[rank_u == "GENUS", , drop = FALSE]
  gen_fam <- vapply(seq_len(nrow(gen)), function(i) {
    anc <- segs[[which(nd$uuid == gen$uuid[[i]])[[1L]]]]
    hit <- anc[anc %in% fam_ids]
    if (length(hit)) fam_by_id[[hit[[length(hit)]]]] else NA_character_
  }, character(1L))
  m <- stats::setNames(gen_fam, gen$name)
  m[!is.na(names(m)) & nzchar(names(m))]
}


#' Read and normalize the Euro+Med CDM snapshot
#'
#' @param jsonl_path Character. Path to `euromed.jsonl` (one accepted taxon per
#'   line, synonyms nested).
#' @param nodes_path Character. Path to `nodes.tsv` (genus/suprageneric
#'   treeIndex paths for family resolution).
#' @param verbose Logical.
#' @return A normalized data.frame in the unified backbone schema.
#' @export
read_euromed <- function(jsonl_path, nodes_path, verbose = TRUE) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite is required to read the Euro+Med snapshot.", call. = FALSE)
  }
  if (verbose) message("Reading Euro+Med CDM snapshot...")
  con <- file(jsonl_path, "r")
  recs <- jsonlite::stream_in(con, verbose = FALSE)
  if (verbose) message(sprintf("  %s accepted taxa", format(nrow(recs),
                                                            big.mark = ",")))

  # ---- accepted rows ----
  acc <- data.frame(
    taxon_id               = recs$uuid,
    canonical_name         = trimws(recs$name),
    fullname               = recs$fullname,
    taxon_rank             = recs$rank,
    taxonomic_status       = "ACCEPTED",
    accepted_name_usage_id = NA_character_,
    genus                  = recs$genus,
    stringsAsFactors       = FALSE
  )

  # ---- synonym rows: unnest the nested per-taxon synonym frames ----
  # Column-wise unlist (rbind over tens of thousands of tiny frames is
  # quadratic); order is preserved so the accepted-id repeat stays aligned.
  syn_col <- recs$synonyms
  n_syn <- vapply(syn_col, function(s) if (is.data.frame(s)) nrow(s) else 0L,
                  integer(1L))
  has <- which(n_syn > 0L)
  if (length(has) > 0L) {
    pull <- function(field) {
      unlist(lapply(syn_col[has], function(s) as.character(s[[field]])),
             use.names = FALSE)
    }
    syn_name <- trimws(pull("name"))
    syn <- data.frame(
      taxon_id               = pull("uuid"),
      canonical_name         = syn_name,
      fullname               = pull("fullname"),
      taxon_rank             = pull("rank"),
      taxonomic_status       = "SYNONYM",
      accepted_name_usage_id = rep(recs$uuid[has], n_syn[has]),
      genus                  = split_scientific_name(syn_name)$genus,
      stringsAsFactors       = FALSE
    )
    all_rows <- rbind(acc, syn)
  } else {
    all_rows <- acc
  }
  if (verbose) {
    message(sprintf("  %s total rows (accepted + synonyms)",
                    format(nrow(all_rows), big.mark = ",")))
  }

  # ---- authorship, epithets, family ----
  all_rows$authorship <- .euromed_authorship(all_rows$fullname,
                                             all_rows$canonical_name)
  ep <- .euromed_epithets(all_rows$canonical_name)
  all_rows$specific_epithet <- ep$specific
  all_rows$infraspecific_epithet <- ep$infraspecific

  # blank genus (higher taxa) -> NA; family via genus map
  all_rows$genus[!nzchar(trimws(ifelse(is.na(all_rows$genus), "",
                                       all_rows$genus)))] <- NA_character_
  fam_map <- .euromed_family_map(nodes_path)
  all_rows$family <- unname(fam_map[all_rows$genus])
  # a family-rank taxon is its own family
  is_fam <- toupper(trimws(all_rows$taxon_rank)) == "FAMILY"
  all_rows$family[is_fam] <- all_rows$canonical_name[is_fam]

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
  normalize_backbone(all_rows, col_map)
}


#' Build the Euro+Med backbone .vtr
#'
#' @param output_dir Character.
#' @param version Character or NULL.
#' @param local_dir Character or NULL. Local crawl snapshot directory (see
#'   [download_euromed()]).
#' @param verbose Logical.
#' @return Path to the .vtr file (invisibly).
#' @export
build_euromed <- function(output_dir = "output/euromed", version = NULL,
                          local_dir = NULL, verbose = TRUE) {
  if (is.null(version)) version <- .euromed_version_default

  tmp <- tempfile("euromed_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  snap <- download_euromed(dest = tmp, local_dir = local_dir, verbose = verbose)
  df <- read_euromed(snap$jsonl, snap$nodes, verbose = verbose)

  if (verbose) message("Precomputing keys and embedding synonyms...")
  df <- precompute_backbone(df)

  vtr_path <- file.path(output_dir, "euromed.vtr")
  build_vtr(df, vtr_path, "euromed", version, .euromed_url)

  invisible(vtr_path)
}
