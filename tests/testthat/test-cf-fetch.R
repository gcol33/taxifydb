# Wiley's supplement endpoint answers the same request with 200 or 403 from
# one attempt to the next, so the fetcher has to retry rather than fail the
# build on the first 403.

run_cf_fetch_py <- function(code) {
  py <- tryCatch(taxifydb:::.cf_python(), error = function(e) "")
  skip_if(!nzchar(py) || !file.exists(py), "no Python interpreter for cf_fetch.py")
  script <- tempfile(fileext = ".py")
  writeLines(c(
    "import importlib.util, sys",
    sprintf("spec = importlib.util.spec_from_file_location('cf_fetch', r'%s')",
            taxifydb:::.cf_fetch_script()),
    "cf = importlib.util.module_from_spec(spec); spec.loader.exec_module(cf)",
    "cf.time.sleep = lambda s: None",
    "class Resp:",
    "    def __init__(self, status, body=b'', headers=None):",
    "        self.status_code = status; self.body = body",
    "        self.headers = headers if headers is not None else {}",
    "    def iter_content(self, chunk_size): yield self.body",
    "    def close(self): pass",
    "CHALLENGE = {'cf-mitigated': 'challenge', 'content-type': 'text/html'}",
    code), script)
  suppressWarnings(system2(py, shQuote(script), stdout = TRUE, stderr = TRUE))
}

test_that("a 403 followed by a 200 writes the file", {
  out <- tempfile()
  res <- run_cf_fetch_py(c(
    "seq = iter([Resp(403), Resp(200, b'x' * 500)])",
    sprintf("cf._fetch_with_retries(lambda: next(seq), r'%s', 'wiley')", out)))
  expect_true(file.exists(out))
  expect_equal(file.size(out), 500)
  expect_true(any(grepl("attempt 1/5 failed [(]HTTP 403[)]", res)))
})

# A bot wall answers every identical request the same way, so retrying alone
# never clears it: the fetcher has to obtain clearance and reissue.

test_that("a challenge response clears the wall and retries at once", {
  out <- tempfile()
  res <- run_cf_fetch_py(c(
    "cleared = []",
    "cf._clear_wall = lambda url: (cleared.append(url), True)[1]",
    "seq = iter([Resp(403, headers=CHALLENGE), Resp(200, b'x' * 500)])",
    sprintf("cf._fetch_with_retries(lambda: next(seq), r'%s', 'get',", out),
    "                       wall_url='https://example.org/f.xls')",
    "print('cleared:', cleared)"))
  expect_true(file.exists(out))
  expect_equal(file.size(out), 500)
  expect_true(any(grepl("cleared: ['https://example.org/f.xls']", res, fixed = TRUE)))
})

test_that("a plain non-HTML refusal is not treated as a wall", {
  res <- run_cf_fetch_py(c(
    "cleared = []",
    "cf._clear_wall = lambda url: (cleared.append(url), True)[1]",
    "seq = iter([Resp(403, headers={'content-type': 'application/json'}),",
    "            Resp(200, b'x' * 500)])",
    sprintf("cf._fetch_with_retries(lambda: next(seq), r'%s', 'get',", tempfile()),
    "                       wall_url='https://example.org/f.xls')",
    "print('cleared:', cleared)"))
  expect_true(any(grepl("cleared: []", res, fixed = TRUE)))
})

test_that("the browser opens at most once per host", {
  res <- run_cf_fetch_py(c(
    "opened = []",
    "cf._browser_clear = lambda url, wall_timeout=0: (opened.append(url),",
    "                                                 {'ua': 'x', 'cookies': [], 'ts': cf.time.time()})[1]",
    sprintf("cf.CACHE_DIR = cf.Path(r'%s')", file.path(tempdir(), "no-clearance")),
    "def req(): return Resp(403, headers=CHALLENGE)",
    "try:",
    sprintf("    cf._fetch_with_retries(req, r'%s', 'get',", tempfile()),
    "                           wall_url='https://example.org/f.xls')",
    "except SystemExit as e:",
    "    print('exit:', e.code)",
    "print('opened:', len(opened))"))
  expect_true(any(grepl("opened: 1", res, fixed = TRUE)))
  expect_true(any(grepl("needs a browser to clear", res, fixed = TRUE)))
})

test_that("persistent failure exits non-zero after every try", {
  res <- run_cf_fetch_py(c(
    "calls = []",
    "def req(): calls.append(1); return Resp(403)",
    "try:",
    sprintf("    cf._fetch_with_retries(req, r'%s', 'wiley')", tempfile()),
    "except SystemExit as e:",
    "    print('exit:', e.code, 'calls:', len(calls))"))
  expect_true(any(grepl("exit: cf_fetch wiley: HTTP 403 after 5 tries calls: 5", res, fixed = TRUE)))
})
