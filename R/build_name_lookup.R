# Build per-backbone name-equivalence lookup tables.
#
# A "name lookup" maps any input name (case-insensitive, including synonyms)
# to the single accepted name that taxify() resolves it to in that backbone:
# a hash-joinable (key_ci, accepted_name) table used by
# resolve_enrichment_names() in place of the per-name-per-backend taxify()
# loop.
#
# A name without authorship can match several backbone rows (homonym synonyms
# published by different authors) pointing to DIFFERENT accepted taxa -- e.g.
# `Pinus resinosa` resolves to Pinus resinosa but also appears as a synonym of
# Pinus sylvestris and Pinus ponderosa. Keeping every (key_ci, accepted_name)
# pair would fan one source name onto unrelated neighbours, contaminating any
# enrichment joined through the lookup. So each key is collapsed to the single
# best accepted name using the same priority taxify() applies
# (taxify::score_candidates): ACCEPTED > SYNONYM, SPECIES > higher ranks,
# nomenclaturally Valid, epithet-preserving accepted target, then lowest
# taxon_id.

#' Build a name-lookup .vtr from a backbone .vtr
#'
#' @param bb_path Character. Backbone .vtr path.
#' @param out_path Character. Lookup .vtr destination.
#' @param verbose Logical.
#' @return `out_path` (invisibly).
#' @export
build_name_lookup <- function(bb_path, out_path, verbose = TRUE) {
  if (!file.exists(bb_path)) {
    stop(sprintf("backbone .vtr not found: %s", bb_path), call. = FALSE)
  }

  if (verbose) message(sprintf("[lookup] reading %s", basename(bb_path)))

  schema <- names(vectra::tbl(bb_path) |> utils::head(1L) |> vectra::collect())
  required <- c("key_ci", "accepted_name", "taxonomic_status", "taxon_rank",
                "taxon_id", "canonical_name")
  missing <- setdiff(required, schema)
  if (length(missing) > 0L) {
    stop(sprintf("backbone .vtr %s missing columns: %s",
                 basename(bb_path), paste(missing, collapse = ", ")),
         call. = FALSE)
  }
  # kingdom rides along so resolve_enrichment_names() can tell a genus
  # reassignment from a homonym collision. Not every backbone carries one
  # (the vascular-plant backbones have no kingdom column), and an absent
  # kingdom must never contradict anything, so it stays NA.
  sel <- c(required, intersect(c("nomenclaturalStatus", "kingdom"), schema))

  bb <- vectra::tbl(bb_path) |>
    vectra::select(!!!lapply(sel, as.name)) |>
    vectra::collect()

  before <- nrow(bb)

  bb <- bb[!is.na(bb$key_ci) & nzchar(bb$key_ci) &
           !is.na(bb$accepted_name) & nzchar(bb$accepted_name), ]

  # Collapse each key to taxify()'s single best accepted name, reusing the
  # runtime scoring so the lookup is identical to taxify()'s own resolution.
  bb$taxonomicStatus  <- bb$taxonomic_status
  bb$taxonRank        <- bb$taxon_rank
  bb$matched_name_std <- bb$canonical_name
  s <- taxify::score_candidates(bb)
  ord <- order(bb$key_ci, s$status_score, s$rank_score, s$valid_score,
               s$epithet_score, bb$taxon_id)
  bb <- bb[ord, , drop = FALSE]
  keep_cols <- c("key_ci", "accepted_name",
                 if ("kingdom" %in% names(bb)) "kingdom")
  bb <- bb[!duplicated(bb$key_ci), keep_cols, drop = FALSE]
  if (!"kingdom" %in% names(bb)) bb$kingdom <- NA_character_
  rownames(bb) <- NULL

  if (verbose) {
    message(sprintf(
      "[lookup] %s -> %s keys (was %s backbone rows)",
      basename(bb_path), format(nrow(bb), big.mark = ","),
      format(before, big.mark = ",")
    ))
  }

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  vectra::write_vtr(bb, out_path, batch_size = 50000L)
  # key_ci drives the forward direction of the name closure, accepted_name the
  # reverse one; without the second index the reverse pass is a full scan of
  # every lookup.
  vectra::create_index(out_path, "key_ci")
  vectra::create_index(out_path, "accepted_name")

  size_mb <- file.size(out_path) / 1048576
  if (verbose) {
    message(sprintf("[lookup] wrote %s (%.1f MB)", out_path, size_mb))
  }

  invisible(out_path)
}


#' Build name-lookup tables for all installed taxify backbones
#'
#' Locates each backbone in the user's taxify data dir and writes a
#' `{backend}_name_lookup.vtr` alongside it.
#'
#' @param backends Character vector. Default: every taxify backbone
#'   ([list_backends()]).
#' @param overwrite Logical. Rebuild even if the lookup .vtr already exists.
#' @return Character vector of paths to the built lookups.
#' @export
build_all_name_lookups <- function(backends = list_backends(),
                                   overwrite = FALSE) {
  data_root <- if (requireNamespace("taxify", quietly = TRUE)) {
    tryCatch(taxify::taxify_data_dir(), error = function(e) NULL)
  } else {
    NULL
  }
  if (is.null(data_root)) {
    data_root <- file.path(Sys.getenv("APPDATA"), "R", "data", "R", "taxify")
  }

  paths <- character()
  for (bb in backends) {
    bb_dir <- file.path(data_root, bb, "latest")
    bb_vtr <- file.path(bb_dir, sprintf("%s.vtr", bb))
    out_vtr <- file.path(bb_dir, sprintf("%s_name_lookup.vtr", bb))

    if (!file.exists(bb_vtr)) {
      message(sprintf("[lookup] SKIP %s (backbone not installed)", bb))
      next
    }

    if (file.exists(out_vtr) && !overwrite) {
      # A lookup built before the closure carries only the key_ci index. The
      # index is a sidecar, so the reverse direction is added in place rather
      # than by rebuilding a multi-hundred-MB table.
      if (!vectra::has_index(out_vtr, "accepted_name")) {
        message(sprintf("[lookup] %s: adding accepted_name index", bb))
        vectra::create_index(out_vtr, "accepted_name")
      }
      message(sprintf("[lookup] SKIP %s (lookup exists; use overwrite=TRUE)",
                      bb))
      paths <- c(paths, out_vtr)
      next
    }

    t0 <- proc.time()
    build_name_lookup(bb_vtr, out_vtr)
    elapsed <- (proc.time() - t0)["elapsed"]
    message(sprintf("[lookup] %s done in %.1fs\n", bb, elapsed))
    paths <- c(paths, out_vtr)
  }

  paths
}
