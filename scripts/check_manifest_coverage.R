# Verify that every backbone and enrichment released on gcol33/taxifydb has a
# matching, current entry in taxify's runtime manifest
# (gcol33/taxify inst/manifest.json).
#
# Backbones: compared against their per-backend release tags. Shape-A manifest
# sync (build-heavy/build-light open a PR to taxify after each release) should
# keep these aligned; this is the safety net that makes a missed or unmerged
# sync visible instead of silently degrading users to build-from-source.
#
# Assets: a tag comparison only sees a version that moved. An asset rebuilt and
# re-uploaded under an unchanged tag leaves every version string agreeing while
# the manifest describes bytes that are no longer served. GitHub reports each
# release asset's size and a sha256 digest, so the manifest's own full_size and
# full_sha256 are compared against the served asset directly -- no download, one
# API call already made for the tag scan. `extras` sidecars are checked the same
# way, since the runtime fetches those from the manifest too.
#
# Enrichments: all share one rolling `enrichment-<version>` tag, so there is no
# per-enrichment release tag to compare against. Instead the two committed
# manifests are compared directly -- taxifydb's build-side
# `manifest/manifest.json` (from the checkout) against taxify's runtime manifest.
# A publish that updates one but not the other (the failure behind #20, where the
# enrichment publish path skipped the taxifydb copy) surfaces here rather than
# lying dormant as a stale second source of truth.
#
# Uses only base R + jsonlite + curl.
#
#   Rscript scripts/check_manifest_coverage.R
#
# Writes coverage_results.json and prints "drift_count=<n>".

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L || is.na(a[1L])) b else a

REPO           <- Sys.getenv("TAXIFYDB_REPO", "gcol33/taxifydb")
TAXIFY_MANIFEST <- Sys.getenv(
  "TAXIFY_MANIFEST_URL",
  "https://raw.githubusercontent.com/gcol33/taxify/main/inst/manifest.json"
)

# Backbones taxify can match against (must stay in sync with
# taxify's .backbone_registry()).
BACKENDS <- c("wfo", "col", "gbif", "itis", "ncbi", "ott", "worms",
              "fungorum", "algaebase", "euromed", "fishbase", "sealifebase",
              "reptiledb", "lcvp", "wcvp", "mdd", "avilist", "lpsn")

# Released .vtr entries that are not matchable backbones: the cross-backbone
# genus index and its coverage table, and the boundary geometry the region
# constraint reads. They are published, versioned and downloaded exactly like a
# backbone, so they drift the same way and are checked alongside.
SUPPORT_ASSETS <- c("genus_register", "backend_coverage", "wgsrpd", "meow")

TAG_CHECKED <- c(BACKENDS, SUPPORT_ASSETS)

gh_json <- function(url, auth = TRUE) {
  h <- curl::new_handle()
  curl::handle_setheaders(h, `User-Agent` = "taxifydb-manifest-coverage")
  token <- Sys.getenv("GH_TOKEN", Sys.getenv("GITHUB_TOKEN"))
  if (auth && nzchar(token)) {
    curl::handle_setheaders(h, Authorization = paste("Bearer", token),
                            Accept = "application/vnd.github+json")
  }
  con <- curl::curl(url, handle = h)
  on.exit(close(con))
  jsonlite::fromJSON(paste(readLines(con, warn = FALSE), collapse = "\n"),
                     simplifyVector = TRUE)
}

# Latest release tag per backend (releases come newest-first; one page covers
# years of twice-yearly builds).
rel  <- gh_json(sprintf("https://api.github.com/repos/%s/releases?per_page=100",
                        REPO))
tags <- rel$tag_name %||% character(0L)

# A backbone release tag is "<backend>-<numeric version>" and nothing else.
# The prefix alone is not enough: companion data releases share it
# (euromed-snapshot-2026.07 holds the raw CDM harvest that euromed-2026.07 is
# built from, not a .vtr), and reading one as a version reports a backbone
# release that has no .vtr behind it. Order with numeric_version rather than
# lexicographically so 3.10.1 sorts above 3.7.3.
latest_release <- vapply(TAG_CHECKED, function(be) {
  hit <- grep(sprintf("^%s-[0-9][0-9.]*$", be), tags, value = TRUE)
  if (!length(hit)) return(NA_character_)
  v <- sub(sprintf("^%s-", be), "", hit)
  v[order(numeric_version(v), decreasing = TRUE)][1L]
}, character(1L))

# Every released asset, keyed "<tag>/<asset name>", with the size and sha256
# digest GitHub reports for it.
asset_index <- local({
  idx <- new.env(parent = emptyenv())
  for (i in seq_len(nrow(rel))) {
    a <- rel$assets[[i]]
    if (!is.data.frame(a) || !nrow(a)) next
    dg <- if ("digest" %in% names(a)) a$digest else rep(NA_character_, nrow(a))
    for (k in seq_len(nrow(a))) {
      assign(paste(rel$tag_name[i], a$name[k], sep = "/"),
             list(size = a$size[k], digest = dg[k]), envir = idx)
    }
  }
  idx
})

# A manifest download URL ends "<tag>/<asset name>", which is the release asset
# the entry points at.
asset_key <- function(url) {
  if (is.null(url) || is.na(url) || !nzchar(url)) return(NA_character_)
  p <- strsplit(url, "/", fixed = TRUE)[[1L]]
  if (length(p) < 2L) return(NA_character_)
  paste(p[length(p) - 1L], p[length(p)], sep = "/")
}

# Compare a manifest entry's recorded size/sha256 against the served asset.
# Returns list(status, detail); status "ok" when the manifest describes what is
# actually downloadable.
check_asset <- function(url, size, sha256) {
  key <- asset_key(url)
  if (is.na(key)) return(list(status = "ok", detail = ""))
  if (!exists(key, envir = asset_index, inherits = FALSE)) {
    return(list(status = "asset_missing", detail = key))
  }
  a <- get(key, envir = asset_index, inherits = FALSE)
  live_sha <- sub("^sha256:", "", a$digest %||% NA_character_)
  bad <- character(0L)
  if (!is.null(size) && !is.na(size) &&
      !identical(as.numeric(a$size), as.numeric(size))) {
    bad <- c(bad, sprintf("size %s -> %s", size, a$size))
  }
  if (!is.null(sha256) && !is.na(sha256) && !is.na(live_sha) &&
      !identical(as.character(sha256), live_sha)) {
    bad <- c(bad, sprintf("sha256 %s.. -> %s..",
                          substr(sha256, 1L, 8L), substr(live_sha, 1L, 8L)))
  }
  if (length(bad)) list(status = "asset_drift", detail = paste(bad, collapse = "; "))
  else list(status = "ok", detail = "")
}

# taxify runtime manifest (public repo, no auth needed).
man <- gh_json(TAXIFY_MANIFEST, auth = FALSE)
be_entries <- man$backends

res_be <- do.call(rbind, lapply(TAG_CHECKED, function(be) {
  e     <- be_entries[[be]]
  rel_v <- latest_release[[be]]
  man_v <- e$latest %||% NA_character_
  detail <- ""
  status <- if (is.na(rel_v))                  "no_release"
            else if (is.na(man_v))             "missing_in_manifest"
            else if (!identical(rel_v, man_v)) "stale_in_manifest"
            else                               "ok"
  # Only worth asking what the asset holds once the version itself agrees.
  if (identical(status, "ok")) {
    a <- check_asset(e$full_url %||% NA_character_,
                     e$full_size %||% NA, e$full_sha256 %||% NA_character_)
    status <- a$status
    detail <- a$detail
  }
  data.frame(kind = if (be %in% BACKENDS) "backbone" else "asset",
             name = be, release = rel_v %||% NA_character_,
             manifest = man_v %||% NA_character_, status = status,
             detail = detail, stringsAsFactors = FALSE)
}))

# `extras` sidecars carry their own url/size/sha256 and may sit under a tag of
# their own, so each is resolved against the release it names.
res_extras <- do.call(rbind, lapply(TAG_CHECKED, function(be) {
  x <- be_entries[[be]]$extras
  if (is.null(x) || !length(x)) return(NULL)
  if (!is.data.frame(x)) x <- do.call(rbind, lapply(x, as.data.frame,
                                                    stringsAsFactors = FALSE))
  do.call(rbind, lapply(seq_len(nrow(x)), function(k) {
    a <- check_asset(x$url[k], x$size[k], x$sha256[k])
    data.frame(kind = "extras", name = sprintf("%s/%s", be, x$name[k]),
               release = asset_key(x$url[k]), manifest = NA_character_,
               status = a$status, detail = a$detail, stringsAsFactors = FALSE)
  }))
}))

# Enrichments: compare the two committed manifests directly. taxifydb's
# build-side copy lives in the checkout; taxify's runtime copy is the same one
# fetched above. content_id (an md5 of the built .vtr) is the strongest signal:
# two entries describing the same released asset must carry the same id, so any
# difference in latest or content_id means one manifest was published without the
# other. The comparison is symmetric -- it fires whichever side was skipped.
DB_MANIFEST <- Sys.getenv("TAXIFYDB_MANIFEST", "manifest/manifest.json")
res_enr <- NULL
if (file.exists(DB_MANIFEST)) {
  db <- jsonlite::read_json(DB_MANIFEST, simplifyVector = FALSE)
  db_enr <- db$enrichments %||% list()
  tm_enr <- man$enrichments
  enr_names <- sort(union(names(db_enr), names(tm_enr)))
  cmp <- function(a, b) identical(as.character(a %||% NA), as.character(b %||% NA))
  res_enr <- do.call(rbind, lapply(enr_names, function(nm) {
    d <- db_enr[[nm]]; t <- tm_enr[[nm]]
    status <- if (is.null(t))                          "missing_in_manifest"
              else if (is.null(d))                     "missing_in_taxifydb"
              else if (!cmp(d$latest, t$latest) ||
                       !cmp(d$content_id, t$content_id) ||
                       !cmp(d$nrow, t$nrow))            "stale_in_manifest"
              else                                     "ok"
    data.frame(kind = "enrichment", name = nm,
               release  = as.character(d$latest %||% NA_character_),
               manifest = as.character(t$latest %||% NA_character_),
               status = status, detail = "", stringsAsFactors = FALSE)
  }))
} else {
  message(sprintf("Note: %s not found; skipping enrichment coverage.",
                  DB_MANIFEST))
}

res <- rbind(res_be, res_extras, res_enr)

drift_states <- c("missing_in_manifest", "missing_in_taxifydb", "stale_in_manifest",
                  "asset_drift", "asset_missing")
drift <- res[res$status %in% drift_states, ]
jsonlite::write_json(res, "coverage_results.json", pretty = TRUE,
                     auto_unbox = TRUE)

print(res, row.names = FALSE)
cat(sprintf("drift_count=%d\n", nrow(drift)))
