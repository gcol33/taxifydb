# Cross-backbone name closure for enrichment builds.
#
# The forward image of a source name -- what each backbone's name lookup
# resolves it to -- is not the set of accepted names the runtime has to join
# on. A backbone that keeps a name no other backbone accepts is only reachable
# by entering the concept through a name the source never lists, and the
# forward image never visits it.
#
# WCVP's distribution asset is the worked case. Its source names are all
# `Sabulina ...`; WFO resolves those to `Sabulina tenuifolia`, so the built
# `.vtr` carried no `Minuartia hybrida` key -- yet WFO accepts that name, and a
# WFO user's add_wcvp() joined on a key that did not exist. The link is a
# reverse edge: Euro+Med, COL and WCVP all map `minuartia hybrida` INTO the
# forward image, and WFO accepts that same name as itself.
#
# So the closure adds one reverse hop. It keeps two things from that hop, and
# nothing else: a name some backbone keeps ACCEPTED while others synonymise it
# onto the forward image, and a re-routing to a further name that at least two
# backbones agree on.
#
# What it will not do is take the reverse-discovered names' full forward image.
# One backbone's bad synonym record then chains two concepts together: from
# `Sabulina tenuifolia`, GBIF alone maps `sabulina hybrida -> Saponaria
# officinalis` where six backbones keep that name within the concept, and the
# unbounded image pulled soapwort into a set of 48 accepted names that would
# each be keyed with the others' traits. Measured on the Zanne woodiness
# source, over 200 sampled names against every installed backbone, the
# unbounded image leaves 16 of 1,487 backbone-name pairs unmatched against the
# forward image's 39; the two arms kept here reach 19, so what corroboration
# refuses is three pairs, and what it buys is that the canary above stays out.
#
# The entry into the hop is checked as well as the exit. A reverse edge is one
# backbone filing a name under the concept, and a single misfiled synonym opens
# the hop onto another species: COL XR alone files `acer negundo f. crispum`
# under Acer pseudoplatanus, where WFO, LCVP, GBIF and WCVP file it in Acer
# negundo, and the corroborated exit then keyed Acer negundo with sycamore's
# rows. A hop is taken only when no more backbones place its key in another
# species than in the input's own, and only when no more backbones resolve the
# input and the reached name to two species than to one: a key disputed
# between two species every backbone keeps apart (`brassica macrorhiza`,
# swede or rape) says nothing about whether they are one. The forward image is
# held to the same vote: one backbone synonymising the source name onto a
# species the others keep apart (LCVP's Amelanchier humilis -> A. spicata) does
# not key that species.

#' Read a name lookup filtered on one column
#'
#' @param p Path to a `{backend}_name_lookup.vtr`.
#' @param column Either `"key_ci"` (forward) or `"accepted_name"` (reverse).
#' @param values Character vector to filter on.
#' @return data.frame with `key_ci`, `accepted_name`, `n_species`, `kingdom`.
#' @noRd
.lookup_filter <- function(p, column, values) {
  # head() before collect(): collecting first materializes the whole lookup --
  # hundreds of MB per backbone, on every pass -- to read a column name.
  schema <- names(vectra::collect(utils::head(vectra::tbl(p), 1L)))
  if (!"n_species" %in% schema) {
    stop(sprintf(paste0(
      "%s has no n_species column, so homonym keys cannot be told apart. ",
      "Rebuild it with build_all_name_lookups()."), basename(p)), call. = FALSE)
  }
  have_k <- "kingdom" %in% schema
  sel <- c("key_ci", "accepted_name", "n_species", if (have_k) "kingdom")
  out <- if (identical(column, "key_ci")) {
    vectra::tbl(p) |>
      vectra::filter(key_ci %in% values) |>
      vectra::select(!!!lapply(sel, as.name)) |>
      vectra::collect()
  } else {
    vectra::tbl(p) |>
      vectra::filter(accepted_name %in% values) |>
      vectra::select(!!!lapply(sel, as.name)) |>
      vectra::collect()
  }
  # rep() rather than a scalar: a lookup that matched nothing is a 0-row frame,
  # and assigning a length-1 value to it errors.
  if (!have_k) out$kingdom <- rep(NA_character_, nrow(out))
  out
}


#' An empty edge frame
#' @noRd
.empty_edges <- function() {
  data.frame(key_ci = character(), accepted_name = character(),
             n_species = integer(), kingdom = character(),
             backbone = character(), stringsAsFactors = FALSE)
}


#' Run one direction of one pass across every lookup
#' @noRd
.closure_pass <- function(lookup_paths, column, values, verbose, label) {
  if (!length(values)) return(.empty_edges())
  out <- vector("list", length(lookup_paths))
  for (i in seq_along(lookup_paths)) {
    nm <- names(lookup_paths)[i]
    p  <- lookup_paths[i]
    t0 <- proc.time()
    out[[i]] <- tryCatch(
      .lookup_filter(p, column, values),
      error = function(e) {
        warning(sprintf("Lookup [%s] failed: %s", nm, conditionMessage(e)),
                call. = FALSE)
        .empty_edges()
      }
    )
    out[[i]]$backbone <- rep(nm, nrow(out[[i]]))
    if (isTRUE(verbose)) {
      message(sprintf("    %-12s %-12s %s rows in %.1fs", label, nm,
                      format(nrow(out[[i]]), big.mark = ","),
                      (proc.time() - t0)["elapsed"]))
    }
  }
  res <- do.call(rbind, out)
  if (is.null(res)) return(.empty_edges())
  res[!is.na(res$key_ci) & nzchar(res$key_ci) &
      !is.na(res$accepted_name) & nzchar(res$accepted_name), , drop = FALSE]
}


#' Pair identity of an edge frame, for set membership
#' @noRd
.pair_key <- function(d) paste(d$key_ci, d$accepted_name, sep = "\x1f")


#' The two things the reverse hop may take from the re-forward pass
#'
#' A name a backbone keeps accepted (its accepted name IS that name, compared
#' on `key_ci` so the backbones' spellings agree), and a re-routing to some
#' further name that two or more backbones agree on. A single backbone sending
#' a name of the concept somewhere else is the shape of a data error, and is
#' the one case the closure declines to follow.
#'
#' @param f2 The re-forward edges, carrying a `backbone` column.
#' @return data.frame of `key_ci`, `accepted_name`.
#' @noRd
.hop_targets <- function(f2) {
  cols <- c("key_ci", "accepted_name")
  self <- f2[.to_key_ci(f2$accepted_name) == f2$key_ci, cols, drop = FALSE]
  pk <- .pair_key(f2)
  n_bb <- tapply(f2$backbone, pk, function(x) length(unique(x)))
  corroborated <- f2[n_bb[pk] >= 2L, cols, drop = FALSE]
  unique(rbind(self, corroborated))
}


#' Refuse to hop through a key some backbone gives to two species
#'
#' A lookup keeps one accepted name per key, so for a homonym it keeps one of
#' the species that spelling was published for, by tiebreak. The source's
#' concept can reach such a key through one of those species and leave it
#' through the other: `Vachellia farnesiana` reaches `acacia acicularis`
#' (Humb. & Bonpl.), which LCVP collapses onto *Acacia brownii* (R.Br.), and
#' *A. brownii* would be keyed with the farnesiana row's traits. Which species
#' the collapse picks moves with the tiebreak, so a build keyed through
#' homonyms also changes from one lookup rebuild to the next.
#'
#' One backbone recording the key under two species is enough: the backbones
#' that record a single target for it have merely dropped the other name.
#'
#' @param f2 Re-forward edges carrying `n_species`.
#' @param verbose Logical.
#' @return `f2` without the edges of homonym keys.
#' @noRd
.drop_homonym_bridges <- function(f2, verbose = TRUE) {
  homonym <- unique(f2$key_ci[!is.na(f2$n_species) & f2$n_species > 1L])
  if (length(homonym) && isTRUE(verbose)) {
    message(sprintf("    [homonym] %s key(s) not followed by the reverse hop",
                    format(length(homonym), big.mark = ",")))
  }
  f2[!f2$key_ci %in% homonym, , drop = FALSE]
}


#' Do two species parts name one species?
#'
#' `TRUE` when the species parts ([.species_of()]) are equal, `FALSE` when they
#' name two species, and `NA` when they are two spellings of one epithet in one
#' genus ([.epithet_stem()]): a gender ending (`maschalogenus` /
#' `maschalogena`), a Latin orthographic variant (`massalongi` / `massalongoi`,
#' `arnoldi` / `arnoldii`, `haldanianum` / `haldaneanum`) or a hybrid sign
#' (`aurantium` / `× aurantium`). Backbones carry such respellings as separate
#' accepted names, and counted as two species they outvote the backbones that
#' resolve the source onto its current spelling. Only these rule-based
#' respellings count: an edit distance would also pair congeners such as
#' `saccharum` / `saccharinum`.
#'
#' @param a,b Parallel character vectors of species parts.
#' @return Logical vector, `NA` for a respelling.
#' @noRd
.species_agreement <- function(a, b) {
  out <- a == b
  cand <- which(!out & sub(" .*$", "", a) == sub(" .*$", "", b))
  if (length(cand)) {
    near <- .epithet_stem(sub("^[^ ]+ ?", "", a[cand])) ==
      .epithet_stem(sub("^[^ ]+ ?", "", b[cand]))
    out[cand[near %in% TRUE]] <- NA
  }
  out
}


#' An epithet with its spelling variation folded away
#'
#' Drops a hybrid sign, folds the orthographic alternations taxify's matcher
#' folds (`ae`/`oe` and `y` to `i`, `ph`, `rh`, `th`) plus a doubled `i` and
#' the patronymic `-oi` / `-eanus` forms, then strips the gender termination
#' (`-us`/`-a`/`-um`, `-is`/`-e`, `-er`/`-ra`/`-rum`, `-os`/`-on`). An empty
#' epithet stays empty.
#'
#' @param x Character vector of epithets, lowercased.
#' @return Character vector of stems.
#' @noRd
.epithet_stem <- function(x) {
  x <- trimws(gsub("×", "", x, fixed = TRUE))
  x <- gsub("ae|oe", "i", x)
  x <- chartr("y", "i", x)
  x <- gsub("ph", "f", x, fixed = TRUE)
  x <- gsub("rh", "r", x, fixed = TRUE)
  x <- gsub("th", "t", x, fixed = TRUE)
  x <- gsub("i+", "i", x)
  x <- sub("oi$", "i", x)
  x <- gsub("ean", "ian", x, fixed = TRUE)
  x <- sub("(er|ra|rum)$", "r", x)
  sub("(us|um|a|is|e|os|on)$", "", x)
}


#' Refuse a hop whose entry most backbones place in another species
#'
#' A hop enters the input's concept through a key some backbone files under a
#' name of the input's forward image. When more backbones file that key under a
#' different species, the entry is the misfiled record, and the hop would key
#' the input's row onto the species the key really belongs to: *Acer
#' pseudoplatanus* reached *Acer negundo* through COL XR's lone
#' `acer negundo f. crispum -> Acer pseudoplatanus`, and *Pinus strobus* reached
#' *Tsuga canadensis* through its `tsuga canadensis f. fastigiata`.
#'
#' Placements are compared on the species part ([.species_agreement()]), so a
#' backbone filing the key under the autonym of the input's species votes with
#' it, one filing it under a respelling of that species abstains, and each
#' backbone counts once per side. An even split is not a contradiction: one
#' backbone synonymising the key onto the concept while one keeps it accepted is
#' exactly the name the hop exists to reach.
#'
#' @param bridged data.frame of `input_name`, `key_ci`, `kept_name`.
#' @param f2 Re-forward edges carrying `backbone`: every placement of each key
#'   inside the source's kingdom ([.in_kingdom_edges()]).
#' @param direct The forward image, `input_name`, `accepted_name`.
#' @param verbose Logical.
#' @return `bridged` without the rows whose entry is outvoted.
#' @noRd
.drop_outvoted_entries <- function(bridged, f2, direct, verbose = TRUE) {
  if (!nrow(bridged)) return(bridged)
  pairs <- unique(bridged[, c("input_name", "key_ci"), drop = FALSE])
  votes <- merge(pairs, f2[, c("key_ci", "accepted_name", "backbone"),
                           drop = FALSE], by = "key_ci")
  votes$sp <- .species_of(votes$accepted_name)
  own <- unique(data.frame(input_name = direct$input_name,
                           sp_own = .species_of(direct$accepted_name),
                           stringsAsFactors = FALSE))
  placed <- merge(unique(votes[, c("input_name", "key_ci", "backbone", "sp")]),
                  own, by = "input_name")
  placed$agree <- .species_agreement(placed$sp, placed$sp_own)
  id <- paste(placed$input_name, placed$key_ci, placed$backbone, placed$sp,
              sep = "\x1f")
  any_true <- tapply(placed$agree %in% TRUE, id, any)
  any_near <- tapply(is.na(placed$agree), id, any)
  side <- ifelse(any_true, TRUE, ifelse(any_near, NA, FALSE))
  first <- !duplicated(id)
  votes <- unique(data.frame(input_name = placed$input_name[first],
                             key_ci = placed$key_ci[first],
                             backbone = placed$backbone[first],
                             inside = unname(side[id[first]]),
                             stringsAsFactors = FALSE))
  votes <- votes[!is.na(votes$inside), , drop = FALSE]
  if (!nrow(votes)) return(bridged)
  pk <- paste(votes$input_name, votes$key_ci, sep = "\x1f")
  count <- function(side) {
    n <- table(factor(pk[votes$inside == side], levels = unique(pk)))
    stats::setNames(as.integer(n), names(n))
  }
  n_in  <- count(TRUE)
  n_out <- count(FALSE)
  outvoted <- names(n_out)[n_out > n_in[names(n_out)]]

  drop <- paste(bridged$input_name, bridged$key_ci, sep = "\x1f") %in% outvoted
  if (any(drop) && isTRUE(verbose)) {
    message(sprintf(
      "    [entry vote] %s hop(s) through a key most backbones place in another species",
      format(length(outvoted), big.mark = ",")))
  }
  bridged[!drop, , drop = FALSE]
}


#' Refuse a mapping onto a species the backbones keep apart from the input
#'
#' A mapping from a source name to an accepted name can rest on one backbone's
#' treatment that the others contradict, and then it keys one species with
#' another's rows. Two routes produce such a mapping.
#'
#' The reverse hop reaches a name through a key the backbones disagree about,
#' and a disagreement over a third name is not evidence that the input and the
#' reached name are one species. `brassica macrorhiza` is filed under *Brassica
#' napus* by four backbones and under *Brassica rapa* by WFO and LCVP, so the
#' hop keyed rape with swede's rows, though every backbone holding both names
#' resolves them to different species. The *Minuartia hybrida* case is the
#' opposite: COL, WCVP and Euro+Med synonymise the reached name itself onto
#' *Sabulina tenuifolia*, and only WFO keeps the two apart.
#'
#' The forward image takes every backbone's resolution of the source name, so
#' one backbone's synonymy is enough to key its accepted name: LCVP alone files
#' *Amelanchier humilis* under *Amelanchier spicata*, which COL, WCVP, ITIS,
#' NCBI and OTT keep apart, and the spicata key carried humilis's rows.
#'
#' So each backbone that resolves both the input's key and the target's key
#' casts one vote: for the mapping when the two land in one species, against it
#' when they land in two ([.species_agreement()]). A backbone holding only one
#' of the names, recording a key under two species, or landing them on two
#' spellings of one epithet, casts none. The
#' mapping is refused when the votes against outnumber the votes for. A source
#' name always keeps its own resolutions where no backbone contradicts them,
#' and a user of the outvoted backbone is still served at join time, through
#' the runtime's cross-backbone recovery.
#'
#' A reached name, unlike a resolution of the source name itself, needs a key
#' that says which species it is: a hop onto a name every backbone records
#' under two or more species is refused. *Matricaria suaveolens* is L.'s name
#' (*M. chamomilla*) and Buchenau's (*M. discoidea*) in every backbone holding
#' it, and a hop through `matricaria suaveolens f. suaveolens` keyed it with
#' *Tripleurospermum inodorum*'s rows.
#'
#' @param pairs data.frame with `input_name` and the column named by `target`.
#' @param target Name of the column holding the mapped accepted name.
#' @param input_edges Forward edges of the input keys: `input_name`,
#'   `accepted_name`, `backbone`, inside the source's kingdom.
#' @param lookup_paths Named character vector of lookup `.vtr` paths.
#' @param kingdom Character vector or `NULL`; see [.in_kingdom_edges()].
#' @param reached Logical. The targets were reached by the hop rather than by
#'   resolving the source name, so a target no backbone places in a single
#'   species is refused.
#' @param verbose Logical.
#' @return `pairs` without the rows mapping onto a species kept apart.
#' @noRd
.drop_split_targets <- function(pairs, target, input_edges, lookup_paths,
                                kingdom = NULL, reached = TRUE,
                                verbose = TRUE) {
  if (!nrow(pairs)) return(pairs)
  pair_id <- function(d) paste(d$input_name, d[[target]], sep = "\x1f")
  cand <- unique(data.frame(input_name = pairs$input_name,
                            target = pairs[[target]], stringsAsFactors = FALSE))
  cand$key_ci <- .to_key_ci(cand$target)

  tgt <- .closure_pass(lookup_paths, "key_ci", unique(cand$key_ci), FALSE,
                       "[target]")
  tgt <- .in_kingdom_edges(tgt, kingdom)
  placed <- unique(tgt$key_ci[is.na(tgt$n_species) | tgt$n_species <= 1L])
  unplaced <- if (isTRUE(reached)) {
    paste(cand$input_name, cand$target, sep = "\x1f")[
      cand$key_ci %in% tgt$key_ci & !cand$key_ci %in% placed]
  } else character()
  tgt <- tgt[is.na(tgt$n_species) | tgt$n_species <= 1L, , drop = FALSE]
  tgt <- unique(data.frame(key_ci = tgt$key_ci, backbone = tgt$backbone,
                           sp_tgt = .species_of(tgt$accepted_name),
                           stringsAsFactors = FALSE))
  src <- unique(data.frame(input_name = input_edges$input_name,
                           backbone = input_edges$backbone,
                           sp_in = .species_of(input_edges$accepted_name),
                           stringsAsFactors = FALSE))

  votes <- merge(merge(cand, tgt, by = "key_ci"), src,
                 by = c("input_name", "backbone"))
  votes$agree <- .species_agreement(votes$sp_in, votes$sp_tgt)
  votes <- votes[!is.na(votes$agree), , drop = FALSE]
  split <- character()
  if (nrow(votes)) {
    votes$pair <- paste(votes$input_name, votes$target, sep = "\x1f")
    same <- tapply(votes$agree,
                   paste(votes$pair, votes$backbone, sep = "\x1f"), any)
    pair_of <- sub("\x1f[^\x1f]*$", "", names(same))
    n_for     <- tapply(same, pair_of, sum)
    n_against <- tapply(!same, pair_of, sum)
    split <- names(n_against)[n_against > n_for]
  }

  drop <- pair_id(pairs) %in% c(split, unplaced)
  if (any(drop) && isTRUE(verbose)) {
    message(sprintf(
      "    [species vote] %s %s onto a species the backbones keep apart from the input%s",
      format(length(split), big.mark = ","),
      if (isTRUE(reached)) "hop(s)" else "resolution(s)",
      if (length(unplaced)) sprintf("; %s hop(s) onto a name no backbone places in one species",
                                    format(length(unplaced), big.mark = ","))
      else ""))
  }
  pairs[!drop, , drop = FALSE]
}


#' Map source names to the accepted names every backbone can return for them
#'
#' One forward pass (the pre-existing behaviour) plus, when `reverse_hop` is
#' `TRUE`, the two arms of [.hop_targets()]. See the file header for why the
#' hop stops there.
#'
#' @param unique_names Character vector of distinct source names.
#' @param lookup_paths Named character vector of lookup `.vtr` paths.
#' @param reverse_hop Logical. Add the reverse hop. `FALSE` is the forward
#'   image alone.
#' @param verbose Logical.
#' @param kingdom Character vector or `NULL`. The source's declared kingdoms;
#'   when given, `.drop_out_of_scope_names()` replaces the consensus vote.
#' @return data.frame with `input_name`, `accepted_name`, or `NULL` when
#'   nothing resolved.
#' @noRd
.name_closure_map <- function(unique_names, lookup_paths, reverse_hop = TRUE,
                              verbose = TRUE, kingdom = NULL) {
  query_keys <- .to_key_ci(unique_names)
  fwd <- .closure_pass(lookup_paths, "key_ci", unique(query_keys), verbose,
                       "[forward]")
  if (!nrow(fwd)) return(NULL)

  rev <- .empty_edges()
  f2  <- .empty_edges()
  if (isTRUE(reverse_hop)) {
    rev <- .closure_pass(lookup_paths, "accepted_name",
                         unique(fwd$accepted_name), verbose, "[reverse]")
    revk <- setdiff(unique(rev$key_ci), query_keys)
    if (length(revk)) {
      f2 <- .closure_pass(lookup_paths, "key_ci", revk, verbose, "[re-forward]")
      f2 <- .drop_homonym_bridges(f2, verbose)
    }
  }

  # One kingdom gate over all the evidence. Without a declared scope it is a
  # vote, on deduplicated triples so a pair seen in two passes does not count
  # its backbone twice; with one, each pair is checked against the scope.
  survived <- if (is.null(kingdom)) {
    all_edges <- unique(rbind(fwd, rev, f2)[, c("key_ci", "accepted_name",
                                                "kingdom"), drop = FALSE])
    .pair_key(.drop_cross_kingdom_names(all_edges, verbose))
  } else {
    .pair_key(.drop_out_of_scope_names(rbind(fwd, rev, f2), kingdom, verbose))
  }
  keep <- function(d, cols = c("key_ci", "accepted_name")) {
    unique(d[.pair_key(d) %in% survived, cols, drop = FALSE])
  }
  inp <- data.frame(input_name = unique_names, key_ci = query_keys,
                    stringsAsFactors = FALSE)
  input_edges <- merge(
    inp, .in_kingdom_edges(fwd[.pair_key(fwd) %in% survived, , drop = FALSE],
                           kingdom),
    by = "key_ci")
  fwd <- keep(fwd)
  if (!nrow(fwd)) return(NULL)

  direct <- merge(inp, fwd, by = "key_ci")[, c("input_name", "accepted_name"),
                                           drop = FALSE]
  direct <- .drop_split_targets(direct, "accepted_name", input_edges,
                                lookup_paths, kingdom, reached = FALSE,
                                verbose = verbose)

  extra <- NULL
  if (nrow(f2)) {
    placements <- .in_kingdom_edges(f2[.pair_key(f2) %in% survived, ,
                                       drop = FALSE], kingdom)
    f2 <- keep(f2, c("key_ci", "accepted_name", "backbone"))
    # What the hop is allowed to take from the re-forward pass.
    kept <- .hop_targets(f2)
    if (nrow(kept)) {
      self <- kept
      names(self)[names(self) == "accepted_name"] <- "kept_name"
      # revkey -> the forward-image name it synonymises onto -> the input
      bridge <- merge(keep(rev), self, by = "key_ci")
      if (nrow(bridge)) {
        bridged <- merge(direct,
                         unique(bridge[, c("key_ci", "accepted_name",
                                           "kept_name"), drop = FALSE]),
                         by = "accepted_name")
        bridged <- .drop_outvoted_entries(bridged, placements, direct, verbose)
        bridged <- .drop_split_targets(bridged, "kept_name", input_edges,
                                       lookup_paths, kingdom, reached = TRUE,
                                       verbose = verbose)
        extra <- data.frame(input_name = bridged$input_name,
                            accepted_name = bridged$kept_name,
                            stringsAsFactors = FALSE)
      }
    }
  }

  out <- unique(rbind(direct, extra))
  if (verbose && !is.null(extra)) {
    gained <- setdiff(out$accepted_name, direct$accepted_name)
    message(sprintf(
      "    [reverse hop] %s accepted name(s) a backbone keeps that the forward image missed",
      format(length(gained), big.mark = ",")))
  }
  rownames(out) <- NULL
  out
}
