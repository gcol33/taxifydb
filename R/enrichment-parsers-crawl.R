# Parsers for the network-crawled enrichment snapshots (issue #3, scale/time).
#
#   parse_italic    ITALIC (Italian lichens) taxon pages -> per-species traits
#   parse_bacdive   BacDive open /fetch strains -> per-species microbial traits
#   parse_globi     GloBI interaction edges -> per-species interaction degree
#
# The snapshots are produced by inst/py/crawlers/crawl_{italic,bacdive,globi}.py
# on a network machine and hosted as release assets; these parsers read the
# frozen snapshot, exactly like the ecoflora/floraweb scrape snapshots.


# ---- ITALIC: Italian lichens ----------------------------------------------

#' Parse the ITALIC lichen taxon-page snapshot
#'
#' Reads the crawled NDJSON (one taxon per line) and emits one row per species
#' with the four structured lichen traits scraped from each taxon page. Names
#' carry authorship and are resolved against the backbones by the build
#' pipeline.
#'
#' @param path Character. Path to `italic.jsonl`.
#' @return data.frame with `canonical_name` + lichen trait columns.
#' @export
parse_italic <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite is required to parse the ITALIC snapshot.", call. = FALSE)
  }
  recs <- lapply(readLines(path, warn = FALSE), function(l) {
    if (!nzchar(trimws(l))) return(NULL)
    jsonlite::fromJSON(l)
  })
  recs <- Filter(Negate(is.null), recs)

  get <- function(r, key) {
    v <- r[[key]]
    if (is.null(v) || !nzchar(trimws(as.character(v)[1L]))) NA_character_
    else trimws(as.character(v)[1L])
  }
  out <- data.frame(
    canonical_name        = vapply(recs, get, character(1L), "name"),
    growth_form           = vapply(recs, get, character(1L), "Growth form"),
    substrata             = vapply(recs, get, character(1L), "Substrata"),
    photobiont            = vapply(recs, get, character(1L), "Photobiont"),
    reproductive_strategy = vapply(recs, get, character(1L),
                                   "Reproductive strategy"),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$canonical_name) & nzchar(out$canonical_name), ,
             drop = FALSE]
  .trait_finalize(out)
}


# ---- BacDive: bacteria / archaea strain phenotypes -------------------------

#' First dict from a BacDive field that may be a single object or a list
#' @noRd
.bd_list <- function(x) {
  if (is.null(x)) return(list())
  if (is.data.frame(x)) return(lapply(seq_len(nrow(x)), function(i) as.list(x[i, ])))
  if (is.list(x) && !is.null(names(x))) return(list(x))
  if (is.list(x)) return(x)
  list()
}

#' Numeric midpoint of a BacDive value like "1.3-1.6 µm" / "22-30" / "6.1"
#' @noRd
.bd_num_mid <- function(s) {
  if (is.null(s) || is.na(s)) return(NA_real_)
  s <- gsub("[^0-9.\\-]", " ", as.character(s))
  nums <- suppressWarnings(as.numeric(strsplit(trimws(s), "\\s*-\\s*|\\s+")[[1L]]))
  nums <- nums[is.finite(nums)]
  if (!length(nums)) NA_real_ else mean(nums)
}

#' Parse the BacDive strain snapshot into per-species microbial traits
#'
#' Reads the gzipped JSONL of full strain records, extracts phenotypic and
#' growth-condition traits per strain, and aggregates to one row per species
#' (categorical by mode, numeric by median). Temperature and pH prefer the
#' optimum measurement, falling back to the growth measurement; ranges are
#' reduced to their midpoint.
#'
#' @param path Character. Path to `bacdive.jsonl.gz`.
#' @return data.frame with `canonical_name` + microbial trait columns.
#' @export
parse_bacdive <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite is required to parse the BacDive snapshot.", call. = FALSE)
  }
  con <- gzfile(path, "rt")
  on.exit(close(con))
  lines <- readLines(con, warn = FALSE)

  N <- length(lines)
  nm_l <- vector("list", N); tr_l <- vector("list", N); vl_l <- vector("list", N)

  for (i in seq_len(N)) {
    l <- lines[[i]]
    if (!nzchar(trimws(l))) next
    r <- tryCatch(jsonlite::fromJSON(l), error = function(e) NULL)
    if (is.null(r)) next
    tax <- r[["Name and taxonomic classification"]]
    sp  <- if (is.list(tax)) tax[["species"]] else NULL
    if (is.null(sp) || !nzchar(trimws(as.character(sp)[1L]))) next
    sp <- trimws(as.character(sp)[1L])

    rt <- character(0); rv <- character(0)
    push <- function(trait, value) {
      if (is.null(value) || length(value) != 1L || is.na(value) ||
          !nzchar(trimws(as.character(value)))) return(invisible())
      rt[[length(rt) + 1L]] <<- trait
      rv[[length(rv) + 1L]] <<- as.character(value)
    }

    morph <- r[["Morphology"]]
    for (cm in .bd_list(if (is.list(morph)) morph[["cell morphology"]] else NULL)) {
      push("gram_stain", cm[["gram stain"]] %||% NA)
      shape <- cm[["cell shape"]] %||% NA
      if (!is.na(shape)) shape <- sub("-shaped$", "", shape)
      push("cell_shape", shape)
      motil <- cm[["motility"]] %||% NA
      if (!is.na(motil)) motil <- c(yes = "motile", no = "non-motile")[motil] %||% motil
      push("motility", motil)
      cl <- .bd_num_mid(cm[["cell length"]] %||% NA)
      if (is.finite(cl)) push("cell_length_um", cl)
      cw <- .bd_num_mid(cm[["cell width"]] %||% NA)
      if (is.finite(cw)) push("cell_width_um", cw)
    }

    phys <- r[["Physiology and metabolism"]]
    for (ox in .bd_list(if (is.list(phys)) phys[["oxygen tolerance"]] else NULL)) {
      push("oxygen_metabolism", ox[["oxygen tolerance"]] %||% NA)
    }

    cg <- r[["Culture and growth conditions"]]
    for (t in .bd_list(if (is.list(cg)) cg[["culture temp"]] else NULL)) {
      ty <- t[["type"]] %||% NA; v <- .bd_num_mid(t[["temperature"]] %||% NA)
      if (is.finite(v) && !is.na(ty)) {
        if (ty == "optimum") push("opt_temp", v)
        else if (ty == "growth") push("growth_temp", v)
      }
    }
    for (p in .bd_list(if (is.list(cg)) cg[["culture pH"]] else NULL)) {
      ty <- p[["type"]] %||% NA; v <- .bd_num_mid(p[["pH"]] %||% NA)
      if (is.finite(v) && !is.na(ty)) {
        if (ty == "optimum") push("opt_ph", v)
        else if (ty == "growth") push("growth_ph", v)
      }
    }

    if (length(rt)) {
      nm_l[[i]] <- rep.int(sp, length(rt))
      tr_l[[i]] <- unlist(rt); vl_l[[i]] <- unlist(rv)
    }
  }

  long <- data.frame(name = unlist(nm_l), trait = unlist(tr_l),
                     value = unlist(vl_l), stringsAsFactors = FALSE)
  spec <- list(
    gram_stain        = list(trait = "gram_stain",        type = "cat"),
    cell_shape        = list(trait = "cell_shape",        type = "cat"),
    motility          = list(trait = "motility",          type = "cat"),
    oxygen_metabolism = list(trait = "oxygen_metabolism", type = "cat"),
    cell_length_um    = list(trait = "cell_length_um",    type = "num"),
    cell_width_um     = list(trait = "cell_width_um",     type = "num"),
    opt_temp          = list(trait = "opt_temp",          type = "num"),
    growth_temp       = list(trait = "growth_temp",       type = "num"),
    opt_ph            = list(trait = "opt_ph",            type = "num"),
    growth_ph         = list(trait = "growth_ph",         type = "num")
  )
  w <- .pivot_species_traits(long, spec, keep_all = FALSE)

  # Prefer the optimum measurement, fall back to growth, then drop helpers.
  w$optimal_growth_temp_c <- ifelse(is.finite(w$opt_temp), w$opt_temp,
                                    w$growth_temp)
  w$optimal_growth_ph     <- ifelse(is.finite(w$opt_ph), w$opt_ph, w$growth_ph)
  w[c("opt_temp", "growth_temp", "opt_ph", "growth_ph")] <- NULL
  .trait_finalize(w)
}


# ---- GloBI: biotic interaction degree --------------------------------------

#' Parse the GloBI distinct-edge snapshot into per-species interaction degree
#'
#' Reads the crawled distinct (source, type, target) edges, resolves both
#' endpoints to their cross-backbone accepted names, and counts, per accepted
#' species, the number of distinct interaction partners (undirected), distinct
#' interaction types, and total interaction records. Degree is a distinct-count,
#' so it is aggregated at the accepted grain (both endpoints resolved) rather
#' than per raw name; the registry entry therefore sets `resolve_names = FALSE`.
#'
#' @param path Character. Path to `globi_edges.tsv.gz`.
#' @return data.frame with `canonical_name`, `interaction_degree`,
#'   `n_interaction_types`, `n_interaction_records`.
#' @export
parse_globi <- function(path) {
  e <- if (requireNamespace("data.table", quietly = TRUE)) {
    as.data.frame(
      data.table::fread(cmd = NULL, file = path, sep = "\t", quote = "",
                        header = TRUE, colClasses = "character",
                        showProgress = FALSE),
      stringsAsFactors = FALSE)
  } else {
    utils::read.delim(gzfile(path), quote = "", stringsAsFactors = FALSE,
                      colClasses = "character")
  }
  names(e)[seq_len(3L)] <- c("source_name", "interaction_type", "target_name")

  allnames <- unique(c(e$source_name, e$target_name))
  allnames <- allnames[nzchar(trimws(allnames))]
  map <- resolve_name_map(allnames, verbose = FALSE)
  # one accepted name per input (first); self-map already provided for misses
  map <- map[!duplicated(map$input_name), , drop = FALSE]
  lut <- stats::setNames(map$accepted_name, map$input_name)

  sa <- lut[e$source_name]; ta <- lut[e$target_name]
  keep <- !is.na(sa) & !is.na(ta) & sa != ta
  sa <- sa[keep]; ta <- ta[keep]; ty <- e$interaction_type[keep]

  # undirected: each endpoint sees the other as a partner
  a   <- c(sa, ta)
  b   <- c(ta, sa)
  ty2 <- c(ty, ty)

  # Aggregate at the accepted grain with base rowsum (fast C, no data.table
  # non-standard evaluation, which is unavailable from a Suggests-only namespace).
  n_records <- rowsum(rep.int(1L, length(a)), a, reorder = FALSE)
  acc_rec   <- rownames(n_records)

  # distinct partners per a: sort by (a, b), count group-and-partner changes
  o  <- order(a, b)
  a1 <- a[o]; b1 <- b[o]; n <- length(a1)
  newAB <- c(TRUE, (a1[-1L] != a1[-n]) | (b1[-1L] != b1[-n]))
  deg   <- rowsum(as.integer(newAB), a1, reorder = FALSE)

  # distinct non-empty interaction types per a
  ok  <- nzchar(ty2)
  aT  <- a[ok]; tT <- ty2[ok]
  o2  <- order(aT, tT); a2 <- aT[o2]; t2 <- tT[o2]; m <- length(a2)
  ntyp <- if (m == 0L) {
    stats::setNames(integer(0), character(0))
  } else {
    newAT <- c(TRUE, (a2[-1L] != a2[-m]) | (t2[-1L] != t2[-m]))
    drop(rowsum(as.integer(newAT), a2, reorder = FALSE)[, 1L])
  }

  out <- data.frame(
    canonical_name        = acc_rec,
    interaction_degree    = as.integer(deg[acc_rec, 1L]),
    n_interaction_types   = as.integer(ntyp[acc_rec]),
    n_interaction_records = as.integer(n_records[, 1L]),
    stringsAsFactors = FALSE
  )
  out$n_interaction_types[is.na(out$n_interaction_types)] <- 0L
  out <- out[out$interaction_degree > 0L, , drop = FALSE]
  # Keep species grain: drop higher taxa, bare genera and unresolved junk
  # ("Substrate Undetermined", "Arachnida", ...) that self-mapped through
  # resolution. A species-level query never matches those anyway.
  out <- out[.is_binomial(out$canonical_name), , drop = FALSE]
  out[order(out$canonical_name), , drop = FALSE]
}
