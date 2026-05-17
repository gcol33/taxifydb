paths <- c(
  "manifest/manifest.json",
  "../taxify/inst/manifest.json"
)
for (p in paths) {
  cat("\n=== ", p, " ===\n", sep = "")
  j <- jsonlite::read_json(p, simplifyVector = FALSE)
  for (b in c("wfo", "col", "gbif")) {
    e <- j$backends[[b]]
    cat(sprintf("%s: latest=%s nrow=%s\n  url=%s\n",
                b, e$latest, e$nrow %||% NA, e$full_url))
    if (!is.null(e$citation)) {
      cat(sprintf("  citation: %s (%s)\n",
                  e$citation$authors, e$citation$year))
    }
    if (!is.null(e$extras) && length(e$extras) > 0) {
      for (ex in e$extras) {
        cat(sprintf("  extra: %s (%s bytes)\n", ex$name, ex$size))
      }
    }
  }
}
