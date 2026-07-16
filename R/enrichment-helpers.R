# Generic download helpers for enrichment build pipeline.
#
# These are shared by many parse functions in the enrichment registry. Each
# helper handles caching (skip re-download if the file is already present
# and non-empty), follows redirects, and sets a User-Agent header.


#' Download a file via curl
#'
#' Downloads `url` into `dest_dir/filename` if the destination does not
#' already exist (or is empty). Follows redirects, sets a User-Agent header,
#' and optionally a Referer.
#'
#' @param url Character. URL to download.
#' @param dest_dir Character. Directory to save into. Created if missing.
#' @param filename Character. Output filename.
#' @param referer Character or `NULL`. Optional Referer header.
#' @param user_agent Character or `NULL`. Override the default User-Agent.
#' @param max_tries Integer. Download attempts before giving up. Large live
#'   exports (e.g. the World Spider Trait database) intermittently drop the TLS
#'   connection mid-transfer; each retry uses exponential backoff.
#' @return Path to the downloaded file.
#' @export
download_curl_file <- function(url, dest_dir, filename, referer = NULL,
                               user_agent = NULL, max_tries = 4L) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(dest_dir, filename)
  if (file.exists(dest) && file.size(dest) > 100L) return(dest)

  ua <- user_agent %||% "Mozilla/5.0 (compatible; taxifydb/0.1)"
  tmp <- paste0(dest, ".part")

  last_err <- NULL
  for (try in seq_len(max_tries)) {
    h <- curl::new_handle()
    curl::handle_setopt(h, followlocation = TRUE, maxredirs = 10L,
                        tcp_keepalive = 1L, connecttimeout = 60L,
                        timeout = 0L, low_speed_limit = 1L,
                        low_speed_time = 120L)
    headers <- list("User-Agent" = ua)
    if (!is.null(referer)) headers[["Referer"]] <- referer
    do.call(curl::handle_setheaders, c(list(h), headers))

    ok <- tryCatch({
      curl::curl_download(url, tmp, handle = h)
      TRUE
    }, error = function(e) {
      last_err <<- conditionMessage(e)
      FALSE
    })

    if (ok && file.exists(tmp) && file.size(tmp) > 100L) {
      file.rename(tmp, dest)
      return(dest)
    }
    if (file.exists(tmp)) unlink(tmp)
    if (try < max_tries) Sys.sleep(2L * try)
  }

  stop(sprintf("Download failed after %d tries: %s%s", max_tries, url,
               if (!is.null(last_err)) paste0(" (", last_err, ")") else ""),
       call. = FALSE)
}


#' Download a ZIP and extract, returning the path to a matching file
#'
#' Downloads `url` to `dest_dir/source.zip` (if not cached), extracts into
#' `dest_dir/extracted/`, and returns the first file matching `pattern`. If
#' `pattern` is `NULL`, returns the extraction directory itself.
#'
#' @param url Character. URL to a ZIP archive.
#' @param dest_dir Character. Directory for download and extraction.
#' @param pattern Character or `NULL`. Regex to match the target file inside
#'   the ZIP. If `NULL`, returns the extraction directory itself.
#' @return Path to the matched file, or the extraction directory if `pattern`
#'   is `NULL`.
#' @export
download_and_unzip <- function(url, dest_dir, pattern = NULL) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  zip_path <- file.path(dest_dir, "source.zip")

  if (!file.exists(zip_path) || file.size(zip_path) < 100L) {
    h <- curl::new_handle()
    curl::handle_setopt(h, followlocation = TRUE, maxredirs = 10L)
    curl::handle_setheaders(h, "User-Agent" = "R/4.5 taxifydb")
    curl::curl_download(url, zip_path, handle = h)
  }

  extract_dir <- file.path(dest_dir, "extracted")
  if (!dir.exists(extract_dir)) {
    dir.create(extract_dir, recursive = TRUE)
    utils::unzip(zip_path, exdir = extract_dir)
  }

  if (is.null(pattern)) return(extract_dir)

  files <- list.files(extract_dir, pattern = pattern, full.names = TRUE,
                      recursive = TRUE, ignore.case = TRUE)
  if (length(files) == 0L) {
    stop(sprintf(
      "No file matching '%s' found in ZIP.\nContents: %s",
      pattern,
      paste(list.files(extract_dir, recursive = TRUE), collapse = ", ")
    ), call. = FALSE)
  }
  files[1L]
}


#' Fetch paginated GBIF API results
#'
#' Pages through a GBIF API endpoint that uses `offset` + `limit` and an
#' `endOfRecords` flag. Returns a combined data.frame of `$results` payloads.
#'
#' @param base_url Character. GBIF API endpoint.
#' @param params Named list. Query parameters (excluding `offset`/`limit`).
#' @param limit Integer. Page size.
#' @param max_pages Integer. Maximum pages to fetch.
#' @return A data.frame of combined results. Empty data.frame if the endpoint
#'   returned no rows.
#' @export
download_gbif_api_pages <- function(base_url, params, limit = 1000L,
                                    max_pages = 100L) {
  all_rows <- vector("list", max_pages)
  offset <- 0L
  page <- 1L

  repeat {
    if (page > max_pages) break

    query <- c(params, list(limit = limit, offset = offset))
    query_str <- paste(
      vapply(names(query), function(k) {
        paste0(k, "=", utils::URLencode(as.character(query[[k]]),
                                        reserved = TRUE))
      }, character(1L)),
      collapse = "&"
    )
    url <- paste0(base_url, "?", query_str)

    resp <- curl::curl_fetch_memory(url)
    if (resp$status_code != 200L) break

    data <- jsonlite::fromJSON(rawToChar(resp$content))
    results <- data$results
    if (is.null(results) || nrow(results) == 0L) break

    all_rows[[page]] <- results
    page <- page + 1L

    if (isTRUE(data$endOfRecords)) break
    offset <- offset + limit
  }

  all_rows <- all_rows[!vapply(all_rows, is.null, logical(1L))]
  if (length(all_rows) == 0L) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  flat <- lapply(all_rows, function(df) {
    for (col in names(df)) {
      if (is.data.frame(df[[col]]) || is.list(df[[col]])) df[[col]] <- NULL
    }
    row.names(df) <- NULL
    df
  })

  if (requireNamespace("data.table", quietly = TRUE)) {
    as.data.frame(data.table::rbindlist(flat, fill = TRUE),
                  stringsAsFactors = FALSE)
  } else {
    # Base R fallback: align columns, fill missing with NA, rbind
    all_cols <- unique(unlist(lapply(flat, names)))
    flat2 <- lapply(flat, function(df) {
      miss <- setdiff(all_cols, names(df))
      for (col in miss) df[[col]] <- NA
      df[, all_cols, drop = FALSE]
    })
    do.call(rbind, flat2)
  }
}


#' Solve an Anubis proof-of-work challenge and read the body
#'
#' Dryad serves file downloads behind an Anubis "Proof-of-Work" bot challenge:
#' a plain GET of `file_stream` returns an HTML page carrying a JSON challenge
#' (`randomData` + `difficulty`). The fast algorithm requires a nonce such that
#' `sha256(randomData + nonce)` begins with `difficulty` hex zeros. Submitting
#' the solution to the `pass-challenge` endpoint returns the file body. The
#' difficulty is small (4), so the loop completes in well under a second.
#'
#' @param stream_url Character. The `/downloads/file_stream/<id>` URL.
#' @param user_agent Character. A browser-like User-Agent (the challenge is
#'   served only to non-browser agents).
#' @return Raw vector of the file body.
#' @noRd
.anubis_fetch <- function(stream_url, user_agent, max_tries = 6L) {
  h <- curl::new_handle()
  curl::handle_setopt(h, followlocation = TRUE, maxredirs = 10L,
                      cookiefile = "", cookiejar = "")
  curl::handle_setheaders(h, "User-Agent" = user_agent)

  is_file <- function(resp) {
    resp$status_code == 200L &&
      length(resp$content) > 100L &&
      !grepl("text/html", resp$type %||% "")
  }

  for (try in seq_len(max_tries)) {
    resp <- curl::curl_fetch_memory(stream_url, handle = h)
    if (is_file(resp)) return(resp$content)

    html <- rawToChar(resp$content)
    p <- regexpr('id="anubis_challenge"', html, fixed = TRUE)
    if (p < 0L) {
      # 202 / empty interstitial: the cookie may now be primed; back off.
      Sys.sleep(2L * try)
      next
    }

    rest <- substring(html, p + attr(p, "match.length"))
    rest <- substring(rest, regexpr(">", rest, fixed = TRUE) + 1L)
    json <- substring(rest, 1L, regexpr("</script>", rest, fixed = TRUE) - 1L)
    ch <- jsonlite::fromJSON(json)

    rd     <- ch$challenge$randomData
    diff   <- ch$rules$difficulty
    id     <- ch$challenge$id
    prefix <- paste(rep("0", diff), collapse = "")

    t0 <- Sys.time()
    nonce <- 0L
    hash <- ""
    repeat {
      hash <- digest::digest(paste0(rd, nonce), algo = "sha256",
                             serialize = FALSE)
      if (startsWith(hash, prefix)) break
      nonce <- nonce + 1L
      if (nonce > 5e7L) {
        stop("Anubis solve exceeded nonce budget.", call. = FALSE)
      }
    }
    el <- max(1L, as.integer(as.numeric(difftime(Sys.time(), t0, "secs")) * 1000))

    # Anubis is the same proof-of-work software wherever it is deployed (Dryad,
    # PHAIDRA, ...); the pass-challenge endpoint lives on the same host as the
    # protected URL, so derive it from stream_url rather than hard-coding Dryad.
    host <- sub("^(https?://[^/]+)/.*$", "\\1", stream_url)
    pass <- sprintf(paste0(
      "%s/.within.website/x/cmd/anubis/api/pass-challenge",
      "?id=%s&response=%s&nonce=%d&redir=%s&elapsedTime=%d"),
      host, utils::URLencode(id, reserved = TRUE), hash, nonce,
      utils::URLencode(stream_url, reserved = TRUE), el)

    r2 <- curl::curl_fetch_memory(pass, handle = h)
    if (is_file(r2)) return(r2$content)
    # Cookie now set on the handle; re-fetch the stream directly.
    r3 <- curl::curl_fetch_memory(stream_url, handle = h)
    if (is_file(r3)) return(r3$content)
    Sys.sleep(2L * try)
  }
  stop("Dryad download did not yield a file after ", max_tries,
       " attempts (Anubis challenge throttled).", call. = FALSE)
}


#' Download a Dryad data file (handles the Anubis bot challenge)
#'
#' Resolves the newest version of a Dryad dataset DOI to a data file matching
#' `file_pattern`, then downloads it through [.anubis_fetch()]. Cached: skips the
#' download if `dest_dir/filename` already exists and is non-empty.
#'
#' @param doi Character. Dryad dataset DOI (e.g. `"10.5061/dryad.1cv08"`).
#' @param dest_dir Character. Directory to save into. Created if missing.
#' @param filename Character. Output filename.
#' @param file_pattern Character. Regex to pick the file from the version's file
#'   list (matched against the file path, case-insensitive).
#' @return Path to the downloaded file.
#' @export
download_dryad_file <- function(doi, dest_dir, filename,
                                file_pattern = "\\.csv$") {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(dest_dir, filename)
  if (file.exists(dest) && file.size(dest) > 100L) return(dest)

  ua <- paste0("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 ",
               "(KHTML, like Gecko) Chrome/120 Safari/537.36")

  ver_url <- sprintf(
    "https://datadryad.org/api/v2/datasets/doi%%3A%s/versions",
    utils::URLencode(doi, reserved = TRUE))
  ver <- jsonlite::fromJSON(rawToChar(
    curl::curl_fetch_memory(ver_url)$content), simplifyVector = FALSE)
  versions <- ver$`_embedded`$`stash:versions`
  if (!length(versions)) {
    stop(sprintf("No Dryad versions for DOI %s.", doi), call. = FALSE)
  }
  self <- versions[[length(versions)]]$`_links`$self$href

  files_url <- paste0("https://datadryad.org", self, "/files")
  fl <- jsonlite::fromJSON(rawToChar(
    curl::curl_fetch_memory(files_url)$content), simplifyVector = FALSE)
  files <- fl$`_embedded`$`stash:files`
  paths <- vapply(files, function(f) f$path %||% "", character(1L))
  hit <- which(grepl(file_pattern, paths, ignore.case = TRUE))
  if (!length(hit)) {
    stop(sprintf("No Dryad file matching '%s' (have: %s).",
                 file_pattern, paste(paths, collapse = ", ")), call. = FALSE)
  }
  dl <- files[[hit[1L]]]$`_links`$`stash:download`$href
  file_id <- sub(".*/files/(\\d+)/download.*", "\\1", dl)
  stream_url <- sprintf("https://datadryad.org/downloads/file_stream/%s",
                        file_id)

  body <- .anubis_fetch(stream_url, ua)
  writeBin(body, dest)
  if (!file.exists(dest) || file.size(dest) < 100L) {
    stop(sprintf("Dryad download produced empty file for %s.", doi),
         call. = FALSE)
  }
  dest
}


# --- Cloudflare-fronted sources ---------------------------------------------
#
# R's libcurl sends a static TLS ClientHello whose JA3/JA4 fingerprint every
# anti-bot has catalogued, so a plain download of a Cloudflare-gated source
# returns the "Just a moment" HTML challenge instead of the data. Reproducing a
# real Chrome ClientHello needs curl-impersonate (patched libcurl + BoringSSL),
# which has no R binding; the bundled inst/py/cf_fetch.py (curl_cffi) does it.
# These helpers shell out to it. Build-time only -- the runtime downloads the
# pre-built .vtr and never touches python.


#' pyenv-managed interpreters, newest version first
#'
#' PATH-based discovery cannot see these under Rscript, where Rtools' own
#' `usr/bin/python` shadows the user's python on PATH. Scans the pyenv versions
#' tree directly (Windows `pyenv-win` layout and the unix layout), honouring
#' `PYENV_ROOT`.
#' @return Character vector of python executable paths (may be empty).
#' @noRd
.pyenv_pythons <- function() {
  win  <- .Platform$OS.type == "windows"
  root <- Sys.getenv("PYENV_ROOT", unset = "")
  if (!nzchar(root)) {
    root <- if (win) file.path(path.expand("~"), ".pyenv", "pyenv-win")
            else     file.path(path.expand("~"), ".pyenv")
  }
  glob <- if (win) file.path(root, "versions", "*", "python.exe")
          else     file.path(root, "versions", "*", "bin", "python")
  pv <- Sys.glob(glob)
  if (!length(pv)) return(character(0))
  # Order by the version directory name, newest first.
  vdir  <- if (win) dirname(pv) else dirname(dirname(pv))
  ord   <- tryCatch(order(numeric_version(basename(vdir), strict = FALSE),
                          decreasing = TRUE),
                    error = function(e) seq_along(pv))
  pv[ord]
}

#' Paths registered with the Windows Python launcher (`py -0p`)
#' @return Character vector of python executable paths (may be empty).
#' @noRd
.py_launcher_pythons <- function() {
  if (.Platform$OS.type != "windows") return(character(0))
  launcher <- Sys.which("py")
  if (!nzchar(launcher)) return(character(0))
  out <- tryCatch(system2(launcher, "-0p", stdout = TRUE, stderr = FALSE),
                  error = function(e) character(0))
  m <- regmatches(out, regexpr("[A-Za-z]:\\\\.*python\\.exe", out))
  m[nzchar(m)]
}

#' Locate a Python interpreter that can import curl_cffi
#'
#' Discovery is not limited to PATH, because under Rscript Rtools' bundled
#' `usr/bin/python` (no curl_cffi) shadows the user's interpreter. Candidates,
#' in order: `TAXIFYDB_PYTHON`, `RETICULATE_PYTHON`, pyenv-managed versions
#' (newest first), interpreters registered with the Windows `py` launcher, then
#' `python3` / `python` on PATH. The first candidate that can import `curl_cffi`
#' wins. Errors (listing what was tried) if none qualifies.
#' @return Path to a usable python executable.
#' @noRd
.cf_python <- function() {
  cands <- c(Sys.getenv("TAXIFYDB_PYTHON", ""),
             Sys.getenv("RETICULATE_PYTHON", ""),
             .pyenv_pythons(),
             .py_launcher_pythons(),
             Sys.which("python3"), Sys.which("python"))
  cands <- unique(unname(cands[nzchar(cands)]))
  for (py in cands) {
    ok <- tryCatch(
      system2(py, c("-c", shQuote("import curl_cffi")),
              stdout = FALSE, stderr = FALSE) == 0L,
      error = function(e) FALSE)
    if (isTRUE(ok)) return(py)
  }
  stop("No Python with curl_cffi found. Cloudflare-gated enrichments ",
       "(hosts, usda_fungus_host, clopla) need it at build time.\n",
       "Tried:\n  ", paste(cands, collapse = "\n  "), "\n",
       "Install with:\n  pip install curl_cffi\n",
       "or point TAXIFYDB_PYTHON at a suitable interpreter.", call. = FALSE)
}


#' Path to the bundled cf_fetch.py helper
#' @noRd
.cf_fetch_script <- function() {
  p <- system.file("py", "cf_fetch.py", package = "taxifydb")
  if (!nzchar(p) || !file.exists(p)) {
    stop("Bundled cf_fetch.py not found (inst/py/cf_fetch.py).", call. = FALSE)
  }
  p
}


#' Download a Cloudflare-fronted file via the curl_cffi helper
#'
#' Downloads `url` into `dest_dir/filename` with a browser TLS impersonation
#' that passes Cloudflare's "Just a moment" challenge. Cached: skips if the
#' destination exists and is non-empty.
#'
#' @param url Character. URL to download.
#' @param dest_dir Character. Directory to save into. Created if missing.
#' @param filename Character. Output filename.
#' @return Path to the downloaded file.
#' @export
download_cf_file <- function(url, dest_dir, filename) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(dest_dir, filename)
  if (file.exists(dest) && file.size(dest) > 100L) return(dest)

  py <- .cf_python()
  status <- system2(py, c(shQuote(.cf_fetch_script()), "get",
                          shQuote(url), shQuote(dest)))
  if (status != 0L || !file.exists(dest) || file.size(dest) < 100L) {
    stop(sprintf("Cloudflare download failed: %s", url), call. = FALSE)
  }
  dest
}


#' Download a Wiley/Atypon supporting-information file
#'
#' Wiley's supplement endpoint (`/action/downloadSupplement`) returns 403 to a
#' cold request: it requires session cookies set by first loading the article
#' page and an article Referer. The bundled cf_fetch.py `wiley` mode primes the
#' session and sends the correct headers through a browser TLS impersonation,
#' clearing both the Cloudflare tier and the cookie/Referer check. Cached: skips
#' if the destination exists and is non-empty.
#'
#' @param article_url Character. The article landing-page URL (used to prime the
#'   session and as Referer), e.g. a `.../doi/10.1002/ecy.1745` URL.
#' @param doi Character. The article DOI.
#' @param sup_file Character. The supplement file name, e.g.
#'   `ecy1745-sup-0001-DataS1.zip`.
#' @param dest_dir Character. Directory to save into. Created if missing.
#' @param filename Character. Output filename.
#' @return Path to the downloaded file.
#' @export
download_wiley_supplement <- function(article_url, doi, sup_file, dest_dir,
                                      filename) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(dest_dir, filename)
  if (file.exists(dest) && file.size(dest) > 100L) return(dest)

  py <- .cf_python()
  status <- system2(py, c(shQuote(.cf_fetch_script()), "wiley",
                          shQuote(article_url), shQuote(doi),
                          shQuote(sup_file), shQuote(dest)))
  if (status != 0L || !file.exists(dest) || file.size(dest) < 100L) {
    stop(sprintf("Wiley supplement download failed: %s (%s)", sup_file, doi),
         call. = FALSE)
  }
  dest
}


#' Harvest an entire CKAN datastore resource past the offset window
#'
#' CKAN's elasticsearch-backed datastore caps `offset` at `max_result_window`
#' (10000); the helper pages the full table via the `after` (search_after)
#' cursor, through Cloudflare, writing one JSON record per line. Cached.
#'
#' @param api_base Character. CKAN action root, e.g.
#'   `https://data.nhm.ac.uk/api/3/action`.
#' @param resource_id Character. Datastore resource id.
#' @param dest_dir Character. Directory to save into. Created if missing.
#' @param filename Character. Output filename (a `.jsonl`).
#' @return Path to the downloaded JSONL file.
#' @export
harvest_ckan_datastore <- function(api_base, resource_id, dest_dir,
                                    filename) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(dest_dir, filename)
  if (file.exists(dest) && file.size(dest) > 1000L) return(dest)

  py <- .cf_python()
  status <- system2(py, c(shQuote(.cf_fetch_script()), "ckan",
                          shQuote(api_base), shQuote(resource_id),
                          shQuote(dest)))
  if (status != 0L || !file.exists(dest) || file.size(dest) < 1000L) {
    stop(sprintf("CKAN harvest failed: %s (resource %s)", api_base,
                 resource_id), call. = FALSE)
  }
  dest
}


#' Page a Supabase PostgREST table into a data.frame
#'
#' PostgREST caps a single response at 1000 rows, so a full table is read by
#' walking `limit`/`offset` until a short page arrives. The Supabase anon key is
#' sent as both `apikey` and `Authorization: Bearer` (PostgREST requires both).
#'
#' @param base Character. REST base, e.g. `https://<ref>.supabase.co/rest/v1/`.
#' @param key Character. The anon (read-only) API key.
#' @param table Character. Table name.
#' @param select Character. PostgREST `select` clause (comma-separated columns).
#' @param page Integer. Rows per request (PostgREST caps at 1000).
#' @param max_pages Integer. Safety bound on the number of requests.
#' @return A data.frame with every row of the table (or empty if none).
#' @noRd
download_supabase_table <- function(base, key, table, select,
                                    page = 1000L, max_pages = 1000L) {
  hdr <- list(apikey = key, Authorization = paste("Bearer", key))
  sel <- utils::URLencode(select, reserved = TRUE)
  out <- vector("list", max_pages)
  offset <- 0L
  i <- 1L
  repeat {
    if (i > max_pages) break
    url <- paste0(base, table, "?select=", sel,
                  "&limit=", page, "&offset=", offset)
    h <- curl::new_handle()
    curl::handle_setheaders(h, .list = hdr)
    resp <- curl::curl_fetch_memory(url, handle = h)
    if (resp$status_code >= 300L) {
      stop(sprintf("Supabase %s HTTP %d", table, resp$status_code),
           call. = FALSE)
    }
    d <- jsonlite::fromJSON(rawToChar(resp$content))
    if (is.null(d) || !is.data.frame(d) || nrow(d) == 0L) break
    out[[i]] <- d
    i <- i + 1L
    if (nrow(d) < page) break
    offset <- offset + page
  }
  out <- out[!vapply(out, is.null, logical(1L))]
  if (length(out) == 0L) return(data.frame(stringsAsFactors = FALSE))
  do.call(rbind, out)
}
