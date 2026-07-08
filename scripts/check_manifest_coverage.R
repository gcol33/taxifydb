# Verify that every backbone released on gcol33/taxifydb has a matching, current
# entry in taxify's runtime manifest (gcol33/taxify inst/manifest.json).
#
# Shape-A manifest sync (build-heavy/build-light open a PR to taxify after each
# release) should keep these aligned; this is the safety net that makes a missed
# or unmerged sync visible instead of silently degrading users to
# build-from-source. Uses only base R + jsonlite + curl.
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

# Backbones taxify can match against (must stay in sync with resolve_backend()).
BACKENDS <- c("wfo", "col", "gbif", "itis", "ncbi", "ott", "worms",
              "fungorum", "algaebase", "euromed", "fishbase", "sealifebase",
              "reptiledb", "lcvp", "wcvp")

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

latest_release <- vapply(BACKENDS, function(be) {
  hit <- grep(sprintf("^%s-", be), tags, value = TRUE)
  if (!length(hit)) return(NA_character_)
  sort(sub(sprintf("^%s-", be), "", hit), decreasing = TRUE)[1L]  # YYYY.MM sorts
}, character(1L))

# taxify runtime manifest (public repo, no auth needed).
man <- gh_json(TAXIFY_MANIFEST, auth = FALSE)
be_entries <- man$backends

res <- do.call(rbind, lapply(BACKENDS, function(be) {
  rel_v <- latest_release[[be]]
  man_v <- be_entries[[be]]$latest %||% NA_character_
  status <- if (is.na(rel_v))              "no_release"
            else if (is.na(man_v))         "missing_in_manifest"
            else if (!identical(rel_v, man_v)) "stale_in_manifest"
            else                            "ok"
  data.frame(backend = be, release = rel_v %||% NA_character_,
             manifest = man_v %||% NA_character_, status = status,
             stringsAsFactors = FALSE)
}))

drift <- res[res$status %in% c("missing_in_manifest", "stale_in_manifest"), ]
jsonlite::write_json(res, "coverage_results.json", pretty = TRUE,
                     auto_unbox = TRUE)

print(res, row.names = FALSE)
cat(sprintf("drift_count=%d\n", nrow(drift)))
