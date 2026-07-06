# Cross-backbone name resolution for enrichment builds.
#
# Every enrichment .vtr must be joinable regardless of which backbone produced
# the user's taxify() result. This function resolves source names against all
# 7 backends and expands the data.frame so each source row maps to every
# distinct accepted name found across backends.

#' Resolve enrichment names against all taxify backends
#'
#' Takes an enrichment data.frame with `canonical_name` + trait columns and
#' expands it so that each source name maps to all unique `accepted_name`
#' values across the requested backends.
#'
#' By default tries the fast hash-join path against per-backend
#' `name_lookup.vtr` files in the user's taxify data directory (built by
#' [build_all_name_lookups()]); falls back to per-name-per-backend
#' [taxify::taxify()] if no lookup files are found.
#'
#' @param df A data.frame with at least a `canonical_name` column.
#' @param group_cols Character vector of grouping columns. Deduplication uses
#'   `canonical_name` + `group_cols` as the key. Default `NULL`.
#' @param backends Character vector of backend names. Default: all 7.
#' @param verbose Logical.
#' @param use_lookup Logical. Try the hash-join fast path first. Default `TRUE`.
#' @return The expanded data.frame.
#' @export
resolve_enrichment_names <- function(df,
                                     group_cols = NULL,
                                     backends = c("wfo", "col", "gbif",
                                                  "itis", "ncbi", "ott",
                                                  "worms"),
                                     verbose = TRUE,
                                     use_lookup = TRUE) {
  if (!"canonical_name" %in% names(df)) {
    stop("df must have a 'canonical_name' column")
  }

  # Aggregate source rows are kept out of cross-backbone expansion: a backbone
  # without the aggregate taxon would resolve them to the bare binomial, leaking
  # the aggregate's traits onto the species key. Their key is instead folded to
  # the canonical "<binomial> aggr." form and they rejoin at the end.
  is_agg <- taxify::is_aggregate_name(df$canonical_name)
  is_agg[is.na(is_agg)] <- FALSE
  agg_df <- df[is_agg, , drop = FALSE]
  rest   <- df[!is_agg, , drop = FALSE]

  resolved <- if (nrow(rest) > 0L) {
    .resolve_species_names(rest, group_cols, backends, verbose, use_lookup)
  } else {
    rest
  }

  if (nrow(agg_df) > 0L) {
    agg_df$canonical_name <-
      taxify::normalize_aggregate_name(agg_df$canonical_name)
    combined <- rbind(resolved, agg_df[names(resolved)])
    return(.dedup_keep_richest(combined, group_cols))
  }
  resolved
}


#' Resolve non-aggregate enrichment names against all taxify backends
#'
#' Internal worker for [resolve_enrichment_names()]; expands each name to all
#' unique accepted names across the requested backends. See the wrapper for the
#' aggregate-handling contract.
#' @noRd
.resolve_species_names <- function(df,
                                   group_cols = NULL,
                                   backends = c("wfo", "col", "gbif",
                                                "itis", "ncbi", "ott",
                                                "worms"),
                                   verbose = TRUE,
                                   use_lookup = TRUE) {
  if (!"canonical_name" %in% names(df)) {
    stop("df must have a 'canonical_name' column")
  }

  map <- resolve_name_map(df$canonical_name, backends = backends,
                          verbose = verbose, use_lookup = use_lookup)
  if (nrow(map) == 0L) {
    warning("No names resolved against any backend. Returning original df.")
    return(df)
  }

  expanded <- merge(df, map, by.x = "canonical_name", by.y = "input_name",
                    all.x = TRUE)
  has_resolved <- !is.na(expanded$accepted_name)
  expanded$canonical_name[has_resolved] <- expanded$accepted_name[has_resolved]
  expanded$accepted_name <- NULL

  expanded <- .dedup_keep_richest(expanded, group_cols)

  if (verbose) {
    message(sprintf(
      "  Final enrichment: %s rows (was %s)",
      format(nrow(expanded), big.mark = ","),
      format(nrow(df), big.mark = ",")
    ))
  }

  expanded
}


#' Map names to their cross-backbone accepted names
#'
#' Resolves a vector of source names to every distinct accepted name they map
#' to across the requested taxify backends, returning the long
#' `(input_name, accepted_name)` mapping used by [resolve_enrichment_names()].
#' Names that resolve nowhere are self-mapped (`accepted_name == input_name`).
#'
#' Exposed so that rollup enrichments (host breadth, interaction degree) can
#' aggregate at the accepted-name grain -- unioning records across synonyms
#' before counting -- instead of pre-aggregating per source name and losing
#' counts when several synonyms later collapse onto one accepted name.
#'
#' @param names Character vector of source names.
#' @param backends Character vector of backend names. Default: all 7.
#' @param verbose Logical.
#' @param use_lookup Logical. Try the hash-join fast path first. Default `TRUE`.
#' @return data.frame with columns `input_name`, `accepted_name`.
#' @export
resolve_name_map <- function(names,
                             backends = c("wfo", "col", "gbif", "itis",
                                          "ncbi", "ott", "worms"),
                             verbose = TRUE, use_lookup = TRUE) {
  unique_names <- unique(names[!is.na(names) & nzchar(names)])
  empty <- data.frame(input_name = character(), accepted_name = character(),
                      stringsAsFactors = FALSE)
  if (length(unique_names) == 0L) return(empty)

  map <- NULL
  if (use_lookup) {
    lookup_paths <- .find_lookup_paths(backends)
    if (length(lookup_paths) > 0L) {
      map <- .name_map_via_lookup(unique_names, lookup_paths, verbose)
    } else if (verbose) {
      message("  No name_lookup.vtr files found; falling back to ",
              "per-backend taxify(). Run build_all_name_lookups() to enable ",
              "the fast path.")
    }
  }
  if (is.null(map)) map <- .name_map_via_taxify(unique_names, backends, verbose)
  if (is.null(map)) return(empty)

  resolved_set <- unique(map$input_name)
  unresolved <- setdiff(unique_names, resolved_set)
  if (length(unresolved) > 0L) {
    map <- rbind(map, data.frame(input_name = unresolved,
                                 accepted_name = unresolved,
                                 stringsAsFactors = FALSE))
  }
  map <- unique(map)

  if (verbose) {
    n_src <- length(unique_names)
    n_acc <- length(unique(map$accepted_name))
    message(sprintf("  %s source names -> %s unique accepted names (%.2fx)",
                    format(n_src, big.mark = ","),
                    format(n_acc, big.mark = ","),
                    n_acc / max(n_src, 1L)))
  }
  map
}


#' Build the accepted-name map via per-backend taxify() (slow path)
#' @noRd
.name_map_via_taxify <- function(unique_names, backends, verbose) {
  if (!requireNamespace("taxify", quietly = TRUE)) {
    stop("taxify package required for cross-backbone name resolution.")
  }
  all_mappings <- vector("list", length(backends))
  for (i in seq_along(backends)) {
    b <- backends[i]
    if (verbose) message(sprintf("    [%d/%d] %s...", i, length(backends), b))
    # Exact-only, matching the hash-join fast path: enrichment resolution maps
    # source names to accepted names, and a name that does not exactly match a
    # backbone must resolve to nothing, not be fuzzy-guessed onto a near
    # neighbour (which would attach the source's traits to the wrong taxon).
    # Fuzzy here also scans every unmatched name against the full backbone,
    # turning an authored-name source like ITALIC into an hours-long build.
    res <- tryCatch(
      taxify::taxify(unique_names, backend = b, fuzzy = FALSE, verbose = FALSE),
      error = function(e) {
        warning(sprintf("Backend '%s' failed: %s", b, conditionMessage(e)),
                call. = FALSE)
        NULL
      }
    )
    if (!is.null(res)) {
      matched <- res[!is.na(res$accepted_name),
                     c("input_name", "accepted_name"), drop = FALSE]
      if (nrow(matched) > 0L) all_mappings[[i]] <- unique(matched)
    }
  }
  mapping <- do.call(rbind, all_mappings)
  if (is.null(mapping) || nrow(mapping) == 0L) return(NULL)
  unique(mapping)
}


# ---- Dedup helper ----------------------------------------------------------

#' Collapse to one row per accepted name (plus group columns), keeping richest
#'
#' When several source taxa resolve to the same accepted name (subspecies and
#' synonyms collapsing onto a species), keep the best-populated source record
#' rather than an arbitrary first one, so trait-rich data is not discarded.
#' @noRd
.dedup_keep_richest <- function(expanded, group_cols = NULL) {
  key <- if (!is.null(group_cols) && length(group_cols) > 0L) {
    do.call(paste, c(expanded[c("canonical_name", group_cols)],
                     list(sep = "\x1f")))
  } else {
    expanded$canonical_name
  }
  trait_cols <- setdiff(names(expanded), c("canonical_name", group_cols))
  n_traits <- if (length(trait_cols)) {
    rowSums(!is.na(expanded[, trait_cols, drop = FALSE]))
  } else {
    rep(0L, nrow(expanded))
  }
  ord <- order(-n_traits)
  expanded <- expanded[ord, , drop = FALSE]
  expanded <- expanded[!duplicated(key[ord]), , drop = FALSE]
  rownames(expanded) <- NULL
  expanded
}


# ---- Fast-path internals ---------------------------------------------------

#' Find pre-built name_lookup.vtr files in the user's taxify data dir
#' @noRd
.find_lookup_paths <- function(backends) {
  data_root <- if (requireNamespace("taxify", quietly = TRUE)) {
    tryCatch(taxify::taxify_data_dir(), error = function(e) NULL)
  } else {
    NULL
  }
  if (is.null(data_root)) {
    data_root <- file.path(Sys.getenv("APPDATA"), "R", "data", "R", "taxify")
  }

  paths <- character()
  for (b in backends) {
    p <- file.path(data_root, b, "latest", sprintf("%s_name_lookup.vtr", b))
    if (file.exists(p)) paths <- c(paths, stats::setNames(p, b))
  }
  paths
}


#' Lowercase + collapse internal whitespace
#' @noRd
.to_key_ci <- function(x) {
  x <- as.character(x)
  x <- tolower(x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}


#' Build the accepted-name map via per-backbone lookup .vtr (fast path)
#' @noRd
.name_map_via_lookup <- function(unique_names, lookup_paths, verbose) {
  query_keys <- .to_key_ci(unique_names)
  query_df <- data.frame(
    canonical_name = unique_names,
    key_ci         = query_keys,
    stringsAsFactors = FALSE
  )

  if (verbose) {
    message(sprintf(
      "  [fast-path] resolving %s names against %d lookup tables",
      format(length(unique_names), big.mark = ","), length(lookup_paths)
    ))
  }

  all_mappings <- vector("list", length(lookup_paths))

  for (i in seq_along(lookup_paths)) {
    nm <- names(lookup_paths)[i]
    p  <- lookup_paths[i]
    t0 <- proc.time()
    matched <- tryCatch({
      vectra::tbl(p) |>
        vectra::filter(key_ci %in% query_keys) |>
        vectra::select("key_ci", "accepted_name") |>
        vectra::collect()
    }, error = function(e) {
      warning(sprintf("Lookup [%s] failed: %s", nm, conditionMessage(e)),
              call. = FALSE)
      data.frame(key_ci = character(), accepted_name = character(),
                 stringsAsFactors = FALSE)
    })
    elapsed <- (proc.time() - t0)["elapsed"]
    if (verbose) {
      message(sprintf("    [%d/%d] %-6s %s matches in %.1fs",
                      i, length(lookup_paths), nm,
                      format(nrow(matched), big.mark = ","),
                      elapsed))
    }
    all_mappings[[i]] <- matched
  }

  raw <- do.call(rbind, all_mappings)
  raw <- raw[!is.na(raw$accepted_name) & nzchar(raw$accepted_name), ]
  raw <- unique(raw)

  mapping <- merge(query_df, raw, by = "key_ci")
  mapping <- mapping[, c("canonical_name", "accepted_name")]
  names(mapping) <- c("input_name", "accepted_name")
  unique(mapping)
}
