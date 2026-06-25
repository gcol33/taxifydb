# SeaLifeBase backbone: thin wrapper over the shared rfishbase reader in
# backend-fishbase.R (server = "sealifebase"). SeaLifeBase carries Kingdom and
# Phylum columns of its own, so no defaults are needed.

.sealifebase_url <- "https://sealifebase.ropensci.org"


#' Build the SeaLifeBase backbone .vtr
#'
#' @param output_dir Character.
#' @param version Character or NULL.
#' @param verbose Logical.
#' @return Path to the .vtr file (invisibly).
#' @export
build_sealifebase <- function(output_dir = "output/sealifebase", version = NULL,
                              verbose = TRUE) {
  if (is.null(version)) version <- format(Sys.Date(), "%Y.%m")

  df <- .read_rfishbase_backbone("sealifebase", verbose = verbose)
  df <- normalize_backbone(df, .rfishbase_col_map, .rfishbase_extra_cols)

  if (verbose) message("Precomputing keys and embedding synonyms...")
  df <- precompute_backbone(df)

  vtr_path <- file.path(output_dir, "sealifebase.vtr")
  build_vtr(df, vtr_path, "sealifebase", version, .sealifebase_url)
  invisible(vtr_path)
}
