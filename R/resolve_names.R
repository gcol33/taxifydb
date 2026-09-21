# Cross-backbone name resolution for enrichment builds.
#
# Every enrichment .vtr must be joinable regardless of which backbone produced
# the user's taxify() result. This function resolves source names against all
# taxify backbones (list_backends()) and expands the data.frame so each source
# row maps to every distinct accepted name found across backbones.

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
#' @param backends Character vector of backend names. Default: every taxify
#'   backbone ([list_backends()]).
#' @param verbose Logical.
#' @param use_lookup Logical. Try the hash-join fast path first. Default `TRUE`.
#' @param reverse_hop Logical. Add the reverse hop; see [resolve_name_map()].
#' @param strict Logical. Error, rather than warn, when a requested backbone
#'   has no `name_lookup.vtr`. Set for production asset builds.
#' @param reduce_fn Function `(df, group_cols)` collapsing the expanded table to
#'   one row per accepted name (plus `group_cols`). Defaults to keeping the
#'   trait-richest row ([.dedup_keep_richest()]); an enrichment whose value is a
#'   reduction over records (e.g. an earliest first-record year) supplies its
#'   own so synonyms collapse by that rule, not by row richness.
#' @param grain `"species"` (default) or `"genus"`; see [resolve_name_map()].
#' @param kingdom Character vector or `NULL`; the kingdoms the source covers.
#'   See [resolve_name_map()].
#' @return The expanded data.frame, carrying a `resolved_backbones` attribute
#'   naming the backbones the expansion actually reached.
#' @export
resolve_enrichment_names <- function(df,
                                     group_cols = NULL,
                                     backends = list_backends(),
                                     verbose = TRUE,
                                     use_lookup = TRUE,
                                     reverse_hop = TRUE,
                                     strict = FALSE,
                                     reduce_fn = NULL,
                                     grain = c("species", "genus"),
                                     kingdom = NULL) {
  grain <- match.arg(grain)
  if (!"canonical_name" %in% names(df)) {
    stop("df must have a 'canonical_name' column")
  }
  reducer <- reduce_fn %||% .dedup_keep_richest

  # Aggregate source rows are kept out of cross-backbone expansion: a backbone
  # without the aggregate taxon would resolve them to the bare binomial, leaking
  # the aggregate's traits onto the species key. Their key is instead folded to
  # the canonical "<binomial> aggr." form and they rejoin at the end.
  is_agg <- taxify::is_aggregate_name(df$canonical_name)
  is_agg[is.na(is_agg)] <- FALSE
  agg_df <- df[is_agg, , drop = FALSE]
  rest   <- df[!is_agg, , drop = FALSE]

  resolved <- if (nrow(rest) > 0L) {
    .resolve_species_names(rest, group_cols, backends, verbose, use_lookup,
                           reverse_hop = reverse_hop, strict = strict,
                           reduce_fn = reducer, grain = grain,
                           kingdom = kingdom)
  } else {
    rest
  }
  reached <- attr(resolved, "resolved_backbones", exact = TRUE)

  out <- if (nrow(agg_df) > 0L) {
    agg_df$canonical_name <-
      taxify::normalize_aggregate_name(agg_df$canonical_name)
    combined <- rbind(resolved, agg_df[names(resolved)])
    reducer(combined, group_cols)
  } else {
    resolved
  }
  # Re-attached last: merge()/rbind() inside the workers drop attributes, and
  # an asset built against a partial backbone set has to say so in its sidecar.
  attr(out, "resolved_backbones") <- reached
  out
}


#' Resolve non-aggregate enrichment names against all taxify backends
#'
#' Internal worker for [resolve_enrichment_names()]; expands each name to all
#' unique accepted names across the requested backends. See the wrapper for the
#' aggregate-handling contract.
#' @noRd
.resolve_species_names <- function(df,
                                   group_cols = NULL,
                                   backends = list_backends(),
                                   verbose = TRUE,
                                   use_lookup = TRUE,
                                   reverse_hop = TRUE,
                                   strict = FALSE,
                                   reduce_fn = .dedup_keep_richest,
                                   grain = "species",
                                   kingdom = NULL) {
  if (!"canonical_name" %in% names(df)) {
    stop("df must have a 'canonical_name' column")
  }

  map <- resolve_name_map(df$canonical_name, backends = backends,
                          verbose = verbose, use_lookup = use_lookup,
                          reverse_hop = reverse_hop, strict = strict,
                          grain = grain, kingdom = kingdom)
  reached <- attr(map, "resolved_backbones", exact = TRUE)
  if (nrow(map) == 0L) {
    warning("No names resolved against any backend. Returning original df.")
    attr(df, "resolved_backbones") <- reached
    return(df)
  }

  expanded <- merge(df, map, by.x = "canonical_name", by.y = "input_name",
                    all.x = TRUE)
  has_resolved <- !is.na(expanded$accepted_name)
  rekeyed <- has_resolved & expanded$accepted_name != expanded$canonical_name
  expanded$canonical_name[has_resolved] <- expanded$accepted_name[has_resolved]
  expanded$accepted_name <- NULL

  expanded <- .keep_own_concept(expanded, rekeyed, group_cols)
  expanded <- reduce_fn(expanded, group_cols)

  if (verbose) {
    message(sprintf(
      "  Final enrichment: %s rows (was %s)",
      format(nrow(expanded), big.mark = ","),
      format(nrow(df), big.mark = ",")
    ))
  }

  attr(expanded, "resolved_backbones") <- reached
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
#' @param backends Character vector of backend names. Default: every taxify
#'   backbone ([list_backends()]).
#' @param verbose Logical.
#' @param use_lookup Logical. Try the hash-join fast path first. Default `TRUE`.
#' @param reverse_hop Logical. Add the reverse hop that reaches an accepted
#'   name only one backbone keeps. `FALSE` is the forward image alone, the
#'   behaviour before this was fixed.
#' @param strict Logical. Error, rather than warn, when a requested backbone
#'   has no `name_lookup.vtr`. Set for production asset builds.
#' @param grain `"species"` (default) or `"genus"`. A genus-grain source is
#'   joined on the genus of a taxify result, so only accepted names that are
#'   themselves a genus name are kept: a backbone that files a source genus as
#'   a subgenus (`Camponotus (Forelophilus)`) or resolves it onto a species
#'   gives a key no genus can match, and one that would carry genus-level
#'   traits onto a single species. A source name left with no genus-shaped
#'   accepted name maps to itself, as an unresolved name does.
#' @param kingdom Character vector or `NULL`. The kingdoms the source covers
#'   (any spelling [taxify::normalize_kingdom_group()] folds). When given, a
#'   mapping is kept only if some backbone that supplies it places the
#'   accepted name inside that set or records no kingdom for it; the
#'   single-kingdom backbones that carry no `kingdom` column count as their
#'   fixed kingdom ([taxify::backbone_fixed_kingdom()]). This replaces the
#'   consensus vote, which cannot protect a genus-grain source: genus names
#'   are homonyms across the zoological and botanical codes far more often
#'   than binomials are. Applies to the lookup fast path only.
#' @return data.frame with columns `input_name`, `accepted_name`.
#' @export
resolve_name_map <- function(names,
                             backends = list_backends(),
                             verbose = TRUE, use_lookup = TRUE,
                             reverse_hop = TRUE,
                             strict = FALSE,
                             grain = c("species", "genus"),
                             kingdom = NULL) {
  grain <- match.arg(grain)
  unique_names <- unique(names[!is.na(names) & nzchar(names)])
  empty <- data.frame(input_name = character(), accepted_name = character(),
                      stringsAsFactors = FALSE)
  if (length(unique_names) == 0L) {
    attr(empty, "resolved_backbones") <- character()
    return(empty)
  }

  map <- NULL
  reached <- character()
  if (use_lookup) {
    lookup_paths <- .find_lookup_paths(backends)
    missing_lu <- setdiff(backends, names(lookup_paths))
    if (length(missing_lu) > 0L) {
      msg <- sprintf(
        paste0("Resolving against %d backbone(s) but %d lack a name_lookup.vtr ",
               "(%s); the accepted-name union will be narrower than requested. ",
               "Run build_all_name_lookups() for the full set before a ",
               "production enrichment build."),
        length(backends), length(missing_lu),
        paste(missing_lu, collapse = ", "))
      # An asset built against a partial backbone set is indistinguishable
      # afterwards from a complete one, so a production build stops here rather
      # than shipping a silently narrow union.
      if (isTRUE(strict)) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
    }
    if (length(lookup_paths) > 0L) {
      reached <- names(lookup_paths)
      map <- .name_map_via_lookup(unique_names, lookup_paths, verbose,
                                  reverse_hop = reverse_hop, kingdom = kingdom)
    } else if (verbose) {
      message("  No name_lookup.vtr files found; falling back to ",
              "per-backend taxify(). Run build_all_name_lookups() to enable ",
              "the fast path.")
    }
  }
  if (is.null(map)) {
    reached <- backends
    map <- .name_map_via_taxify(unique_names, backends, verbose)
  }
  if (!is.null(map) && identical(grain, "genus")) {
    map <- map[.is_genus_name(map$accepted_name), , drop = FALSE]
  }
  if (is.null(map)) {
    attr(empty, "resolved_backbones") <- reached
    return(empty)
  }

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
  attr(map, "resolved_backbones") <- reached
  map
}


#' Drop accepted names a backbone reached through a cross-kingdom homonym
#'
#' The union across backbones is the point of this step: two backbones may file
#' the same organism under different genera, and the enrichment has to join
#' whichever the user's `taxify()` returned. But a binomial can also be occupied
#' twice in different kingdoms, and then the two accepted names are not the same
#' organism at all -- unioning them writes one organism's traits under the
#' other's name.
#'
#' Lineage is what separates the two, and only at the kingdom rank, which is the
#' one every backbone that has it agrees on: *Lasiurus cinereus* and *Aeorestes
#' cinereus* are one bat reassigned between genera (Animalia both), whereas
#' *Coronella austriaca* is a snake (Animalia) in six backbones and a fossil
#' foraminiferan (Chromista) in WoRMS, which files the binomial under
#' *Coronipora austriaca*.
#'
#' A source name's kingdom is taken to be the one most backbones give it, and an
#' accepted name is dropped only on a positive contradiction. A backbone that
#' records no kingdom -- the vascular-plant backbones do not -- never
#' contradicts, so its mapping is always kept.
#' @noRd
.drop_cross_kingdom_names <- function(raw, verbose = TRUE) {
  if (!nrow(raw) || !"kingdom" %in% names(raw)) return(raw)
  k <- trimws(as.character(raw$kingdom))
  k[!nzchar(k) | k == "NA"] <- NA_character_

  known <- !is.na(k)
  if (!any(known)) return(raw[, setdiff(names(raw), "kingdom"), drop = FALSE])

  # Consensus kingdom per source key: the one the most backbones report.
  tab <- table(raw$key_ci[known], k[known])
  consensus <- colnames(tab)[max.col(tab, ties.method = "first")]
  names(consensus) <- rownames(tab)

  want <- consensus[raw$key_ci]
  drop <- known & !is.na(want) & k != want
  if (any(drop) && isTRUE(verbose)) {
    message(sprintf(
      "    [kingdom gate] dropped %s cross-kingdom homonym mapping(s)",
      format(sum(drop), big.mark = ",")))
  }
  raw[!drop, setdiff(names(raw), "kingdom"), drop = FALSE]
}


#' Drop mappings no backbone places inside the source's declared kingdoms
#'
#' A genus name is a homonym across the zoological and botanical codes far more
#' often than a binomial is, and the vascular-plant backbones carry no kingdom
#' column, so the consensus vote in `.drop_cross_kingdom_names()` never sees
#' their side of the collision: a benthic-invertebrate genus picked up plant
#' synonyms that way, and a protist genus flowering-plant ones. A source that
#' declares its kingdoms is checked against them instead.
#'
#' Each edge's kingdom is the lookup's own, else the backbone's fixed kingdom.
#' A pair survives when at least one edge supplying it places it in scope, or
#' when no edge places it anywhere. The same spelling can reach one backbone as
#' the source's organism and another as its homonym, and the in-scope edge is
#' the evidence that counts. An edge with no kingdom is no evidence either way,
#' so it cannot rescue a pair the others place outside the scope: the reverse
#' hop reaches the fish genus *Ammodytes* from a plant source through WFO's
#' synonym of *Astragalus*, and four backbones file it in Animalia while NCBI
#' and OTT record no kingdom for it.
#'
#' @param edges Edge frame with `key_ci`, `accepted_name`, `kingdom`,
#'   `backbone`.
#' @param kingdom Character vector of declared kingdoms.
#' @param verbose Logical.
#' @return The surviving `key_ci`, `accepted_name` pairs.
#' @noRd
.drop_out_of_scope_names <- function(edges, kingdom, verbose = TRUE) {
  cols <- c("key_ci", "accepted_name")
  if (!nrow(edges)) return(edges[, cols, drop = FALSE])
  scope <- .kingdom_scope(kingdom)
  k <- .edge_kingdom(edges)
  pk <- .pair_key(edges)
  keep <- pk %in% pk[!is.na(k) & k %in% scope] | !pk %in% pk[!is.na(k)]
  if (any(!keep) && isTRUE(verbose)) {
    message(sprintf(
      "    [kingdom scope] dropped %s mapping(s) outside %s",
      format(length(unique(pk[!keep])), big.mark = ","),
      paste(scope, collapse = "/")))
  }
  unique(edges[keep, cols, drop = FALSE])
}


#' Normalize a declared kingdom set, erroring when nothing is recognised
#' @noRd
.kingdom_scope <- function(kingdom) {
  scope <- unique(taxify::normalize_kingdom_group(kingdom))
  scope <- scope[!is.na(scope)]
  if (!length(scope)) {
    stop(sprintf("kingdom: none of %s is a recognised kingdom.",
                 paste(kingdom, collapse = ", ")), call. = FALSE)
  }
  scope
}


#' Each edge's kingdom: the lookup's own, else its backbone's fixed kingdom
#' @noRd
.edge_kingdom <- function(edges) {
  k <- taxify::normalize_kingdom_group(edges$kingdom)
  unknown <- is.na(k)
  k[unknown] <- taxify::backbone_fixed_kingdom(edges$backbone[unknown])
  k
}


#' Keep the edges that place a name inside the source's kingdom
#'
#' Edge-level counterpart of the two kingdom gates, for counting backbones
#' rather than keeping pairs: a backbone that reaches a spelling as a homonym in
#' another kingdom supplies a pair, but is no evidence about where the source's
#' organism sits. Without a declared scope an edge contradicting its key's
#' consensus kingdom is dropped; with one, an edge placed outside the scope.
#'
#' @param edges Edge frame with `key_ci`, `kingdom`, `backbone`.
#' @param kingdom Character vector or `NULL`.
#' @return `edges` without the out-of-kingdom rows, and without `kingdom`.
#' @noRd
.in_kingdom_edges <- function(edges, kingdom = NULL) {
  if (is.null(kingdom)) return(.drop_cross_kingdom_names(edges, verbose = FALSE))
  k <- .edge_kingdom(edges)
  edges[is.na(k) | k %in% .kingdom_scope(kingdom),
        setdiff(names(edges), "kingdom"), drop = FALSE]
}


#' Build the accepted-name map via per-backend taxify() (slow path)
#'
#' Note this path is not kingdom-gated: it is the fallback used when no
#' `name_lookup.vtr` files exist, and it already warns that the union will be
#' narrower than a production build.
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


# ---- Dedup helpers ---------------------------------------------------------

#' Keep a name's own concept wherever the source states it
#'
#' An enrichment carrying an authorship column says, row by row, which concept
#' the row describes, and taxify's grouped join keeps only the rows whose
#' authorship matches the concept the caller resolved to; the column is the one
#' [taxify::enrichment_authorship_col()] picks. Expansion re-keys a source
#' concept under every accepted name some backbone gives it, so a species
#' another backbone sinks, or an infraspecific taxon, lands on the same
#' (name, group) key as the name's own concept, and the reducer could keep it
#' there. Its authorship then disagrees with the caller's, and the runtime
#' drops a row the source states for that name: WCVP's *Eucalyptus bicostata*
#' Maiden, Blakely & Simmonds filled Victoria under *E. globulus* Labill. that
#' way.
#'
#' At every key where the source states the name's own concept, the rows
#' re-keyed onto it are dropped before reduction. A re-keyed row still fills a
#' key the own concept does not reach, under its own authorship, so the runtime
#' guard decides it as it decides any other concept. An enrichment without an
#' authorship column gives the runtime nothing to tell concepts apart by, and
#' its reducer keeps choosing over every row.
#'
#' @param expanded Expanded enrichment frame, `canonical_name` already set to
#'   the accepted name each row is keyed under.
#' @param rekeyed Logical along `expanded`: the row's source name differs from
#'   the name it is now keyed under.
#' @param group_cols Grouping columns, or `NULL`.
#' @return `expanded` without the displaced rows.
#' @noRd
.keep_own_concept <- function(expanded, rekeyed, group_cols = NULL) {
  if (is.null(taxify::enrichment_authorship_col(names(expanded))) ||
      !any(rekeyed)) {
    return(expanded)
  }
  key <- do.call(paste, c(expanded[c("canonical_name", group_cols)],
                          list(sep = "\x1f")))
  displaced <- rekeyed & key %in% key[!rekeyed]
  out <- expanded[!displaced, , drop = FALSE]
  rownames(out) <- NULL
  out
}


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


#' Match key of a name: subgenus dropped, whitespace collapsed, lowercased
#'
#' Backbone keys carry no subgenus (#50), so a source writing
#' `Carabus (Tachypus) cancellatus` has to be keyed on the binomial or it
#' resolves against nothing and ships unexpanded.
#' @noRd
.to_key_ci <- function(x) {
  x <- trimws(gsub("\\s+", " ", as.character(x)))
  tolower(drop_infrageneric(x))
}


#' The name a source row is keyed on, parsed the way taxify parses a query
#'
#' A source that writes authorship, sensu-lato notes or an unpunctuated rank
#' marker into its name column (`Atriplex nuttallii S Watson`,
#' `Achillea millefolium L sl`, `Agrostemma githago var githago`) matches no
#' backbone key verbatim, so its rows reach nothing, although [taxify::taxify()]
#' resolves the same string. Keying on [taxify::parse_name()]'s `canonical`
#' makes a source row reach exactly the taxon the runtime matches the string
#' to, and lets two spellings of one taxon meet before a per-taxon reduction.
#'
#' A key names one definite taxon or none. The parse keeps the leading name of
#' anything it cannot read whole, and that name is a different taxon from the
#' one the source recorded:
#'
#' * aggregates and species groups (`Xanthium orientale agg`,
#'   `Taraxacum officinale group`) take the aggregate route
#'   [resolve_enrichment_names()] folds to `<binomial> agg.`, on the parsed
#'   binomial so an author in the name does not split the key;
#' * a record naming two taxa (`Cakile edentula - Cakile maritima`,
#'   `Lepidium didymum/squamatum`) would credit the pair to its first member;
#' * an open-nomenclature name (`cf.`, `aff.`, `sect.`, `sensu`, `auct.`, a
#'   phrase name such as `Cardamine sp Jandakot`) does not assert the binomial
#'   it parses to;
#' * a name whose epithet the parse loses (`Amaranthus x ralletii Contre (A
#'   bouchonii x`, cut off in the source) would be keyed on its genus.
#'
#' These keep their verbatim spelling, which no backbone key matches. A name
#' open at species rank only (`Amaranthus sp`, `Amsinckia group`) is a
#' genus-level record and is keyed on the genus. An infraspecific name written
#' without its rank connector (`Amaranthus hybridus bouchonii`) keeps all three
#' words: it is never keyed on its binomial, whose species it may not belong to.
#'
#' @param x Character vector of source names.
#' @return data.frame with `canonical_name` (the key) and `qualifier` (the
#'   open-nomenclature qualifier `parse_name()` reports, else `NA`), one row
#'   per element of `x`.
#' @noRd
.source_name_key <- function(x) {
  x <- trimws(.to_utf8(x))
  p <- taxify::parse_name(ifelse(is.na(x), "", x))
  key <- p$canonical
  qual <- p$qualifier
  genus_rank <- p$rank %in% "genus"

  agg <- taxify::is_aggregate_name(x)
  agg <- (!is.na(agg) & agg) | qual %in% "group"
  species_agg <- agg & p$rank %in% "species"
  key[species_agg] <- paste(p$canonical[species_agg], "agg.")

  genus_record <- genus_rank & qual %in% c("sp.", "group") & !grepl("/", x)
  several <- grepl("\\s-\\s|/", x)
  open <- !is.na(qual) & !qual %in% c("agg.", "group") & !genus_record
  truncated <- genus_rank & !genus_record

  keep_verbatim <- (agg & !species_agg & !genus_record) | several | open |
    truncated | is.na(key) | !nzchar(key)
  key[keep_verbatim] <- x[keep_verbatim]
  data.frame(canonical_name = key, qualifier = qual,
             stringsAsFactors = FALSE)
}


#' Build the accepted-name map via per-backbone lookup .vtr (fast path)
#'
#' Delegates to the cross-backbone closure in `name_closure.R`, which adds to
#' the forward image the accepted names a backbone keeps while other backbones
#' synonymise them onto that image.
#' @noRd
.name_map_via_lookup <- function(unique_names, lookup_paths, verbose,
                                 reverse_hop = TRUE, kingdom = NULL) {
  if (verbose) {
    message(sprintf(
      "  [fast-path] resolving %s names against %d lookup tables%s",
      format(length(unique_names), big.mark = ","), length(lookup_paths),
      if (isTRUE(reverse_hop)) " (+ reverse hop)" else ""
    ))
  }
  map <- .name_closure_map(unique_names, lookup_paths,
                           reverse_hop = reverse_hop, verbose = verbose,
                           kingdom = kingdom)
  if (is.null(map) || nrow(map) == 0L) return(NULL)
  map
}


#' Is a name a genus name?
#'
#' One capitalised token, optionally carrying the nothogenus sign. A subgenus
#' rendering (`Aedes (Ochlerotatus)`), a binomial, a lumped `Dero / Aulophorus`
#' or a quoted informal name is not, so none of them can match the genus of a
#' taxify result.
#'
#' @param x Character vector.
#' @return Logical vector, `FALSE` for `NA`.
#' @noRd
.is_genus_name <- function(x) {
  x <- as.character(x)
  !is.na(x) & grepl("^(\u00d7 ?)?[A-Z][^[:space:]()/,'\"]*$", x)
}


#' Key a genus-grain enrichment on its resolved genus names
#'
#' The runtime joins a genus-grain asset on its `genus` column, so that column
#' has to carry the names the cross-backbone expansion produced. A parser's own
#' `genus` column holds the source spelling and would leave every expansion
#' unreachable, so it is replaced rather than kept. Stops when a key is not a
#' genus name, since such a row can never be joined.
#'
#' @param df Resolved enrichment data.frame with `canonical_name`.
#' @param name Enrichment identifier, for the error message.
#' @return `df` with `genus` set from `canonical_name`.
#' @noRd
.genus_grain_key <- function(df, name) {
  bad <- unique(df$canonical_name[!.is_genus_name(df$canonical_name)])
  if (length(bad)) {
    stop(sprintf(paste0(
      "Enrichment '%s' is genus-grain, but %d key(s) are not a genus name and ",
      "could never be joined: %s"),
      name, length(bad), paste(utils::head(bad, 10L), collapse = ", ")),
      call. = FALSE)
  }
  df$genus <- df$canonical_name
  df
}
