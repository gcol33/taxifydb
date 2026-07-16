# Python discovery for the Cloudflare/curl_cffi fetch helpers.
#
# The bug this guards: under Rscript, Rtools' bundled usr/bin/python (no
# curl_cffi) shadows the user's interpreter on PATH, so PATH-only discovery
# picked the wrong python and the build errored. Discovery must also scan the
# pyenv versions tree.

test_that(".pyenv_pythons finds versions under PYENV_ROOT, newest first", {
  root <- tempfile("pyenv_")
  win  <- .Platform$OS.type == "windows"

  mk <- function(v) {
    d <- if (win) file.path(root, "versions", v)
         else     file.path(root, "versions", v, "bin")
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    file.create(file.path(d, if (win) "python.exe" else "python"))
  }
  # Deliberately out of order on disk; the helper must sort by version.
  mk("3.9.1"); mk("3.14.4"); mk("3.12.10")

  old <- Sys.getenv("PYENV_ROOT", unset = NA)
  Sys.setenv(PYENV_ROOT = root)
  on.exit({
    if (is.na(old)) Sys.unsetenv("PYENV_ROOT") else Sys.setenv(PYENV_ROOT = old)
    unlink(root, recursive = TRUE)
  }, add = TRUE)

  got <- .pyenv_pythons()
  expect_length(got, 3L)
  # Newest first: 3.14.4 > 3.12.10 > 3.9.1 (numeric, not lexical, ordering).
  expect_match(got[1], "3\\.14\\.4")
  expect_match(got[2], "3\\.12\\.10")
  expect_match(got[3], "3\\.9\\.1")
})

test_that(".pyenv_pythons returns empty when the versions tree is absent", {
  old <- Sys.getenv("PYENV_ROOT", unset = NA)
  Sys.setenv(PYENV_ROOT = tempfile("pyenv_empty_"))   # nonexistent dir
  on.exit({
    if (is.na(old)) Sys.unsetenv("PYENV_ROOT") else Sys.setenv(PYENV_ROOT = old)
  }, add = TRUE)

  expect_identical(.pyenv_pythons(), character(0))
})
