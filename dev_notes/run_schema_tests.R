# Run the unified-schema regression tests locally.
# Standalone helper for dev_notes; not part of the package.
suppressPackageStartupMessages(devtools::load_all("."))
testthat::test_file("tests/testthat/test-unified-schema.R", reporter = "summary")
