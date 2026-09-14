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
    "    def __init__(self, status, body=b''): self.status_code = status; self.body = body",
    "    def iter_content(self, chunk_size): yield self.body",
    "    def close(self): pass",
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
