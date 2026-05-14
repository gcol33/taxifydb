cat("installed:", requireNamespace("taxify", quietly = TRUE), "\n")
cat("libpaths:\n")
cat(.libPaths(), sep = "\n")
cat("\n")
if (requireNamespace("taxify", quietly = TRUE)) {
  cat("version:", as.character(packageVersion("taxify")), "\n")
  cat("location:", find.package("taxify"), "\n")
}
cat("\ndevtools available:", requireNamespace("devtools", quietly = TRUE), "\n")
