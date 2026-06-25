# FishBase / SeaLifeBase: rfishbase taxonomy -> normalized data.frame -> .vtr
#
# FishBase and SeaLifeBase share one schema and one access path (rfishbase,
# selectable by `server`). load_taxa() supplies the accepted species and their
# classification; synonyms() supplies synonym -> accepted links. The shared
# reader below is the single source of truth; build_fishbase() and
# build_sealifebase() (backend-sealifebase.R) are thin wrappers over it.

.fishbase_url <- "https://fishbase.ropensci.org"


#' Read an rfishbase taxonomy into the unified backbone schema
#'
#' Accepted species come from `load_taxa()` (where `Species` is the full
#' binomial). Synonyms come from `synonyms()` rows whose `Status` marks them as
#' a synonym, misapplied or ambiguous name; each is linked to its accepted
#' species through `SpecCode` and inherits the accepted classification.
#'
#' @param server Either "fishbase" or "sealifebase".
#' @param default_kingdom,default_phylum Used when `load_taxa()` carries no
#'   kingdom/phylum column (FishBase: all Animalia / Chordata).
#' @param verbose Logical.
#' @return A raw data.frame in the pre-normalize column layout.
#' @noRd
.read_rfishbase_backbone <- function(server,
                                     default_kingdom = NA_character_,
                                     default_phylum = NA_character_,
                                     verbose = TRUE) {
  if (!requireNamespace("rfishbase", quietly = TRUE)) {
    stop("rfishbase is required to build the ", server,
         " backbone from source.\n",
         "Install it with: install.packages(\"rfishbase\")", call. = FALSE)
  }

  if (verbose) message("Loading ", server, " taxonomy via rfishbase...")
  tx <- as.data.frame(rfishbase::load_taxa(server = server),
                      stringsAsFactors = FALSE)
  if (verbose) message(sprintf("  %s accepted species",
                               format(nrow(tx), big.mark = ",")))

  pick <- function(df, col, default = NA_character_) {
    if (col %in% names(df)) as.character(df[[col]]) else rep(default, nrow(df))
  }

  epithet <- sub("^\\S+\\s+", "", tx$Species)
  epithet[epithet == tx$Species] <- NA_character_

  acc <- data.frame(
    taxon_id               = as.character(tx$SpecCode),
    canonical_name         = as.character(tx$Species),
    taxon_rank             = "SPECIES",
    taxonomic_status       = "ACCEPTED",
    accepted_name_usage_id = as.character(tx$SpecCode),
    family                 = pick(tx, "Family"),
    genus                  = as.character(tx$Genus),
    specific_epithet       = epithet,
    authorship             = NA_character_,
    infraspecific_epithet  = NA_character_,
    kingdom                = pick(tx, "Kingdom", default_kingdom),
    phylum                 = pick(tx, "Phylum", default_phylum),
    class                  = pick(tx, "Class"),
    order                  = pick(tx, "Order"),
    stringsAsFactors = FALSE
  )

  if (verbose) message("Loading ", server, " synonyms...")
  syn <- as.data.frame(rfishbase::synonyms(server = server),
                       stringsAsFactors = FALSE)

  is_syn <- tolower(trimws(syn$Status)) %in%
    c("synonym", "ambiguous synonym", "misapplied name")
  keep <- is_syn & !is.na(syn$synonym) & syn$SpecCode %in% tx$SpecCode
  syn <- syn[keep, , drop = FALSE]

  # Binomials only (the matching engine keys on genus + epithet).
  nword <- lengths(strsplit(trimws(syn$synonym), "\\s+"))
  syn <- syn[nword >= 2L, , drop = FALSE]

  ai <- match(syn$SpecCode, tx$SpecCode)
  syndf <- data.frame(
    taxon_id               = paste0(server, "-syn-", syn$SynCode),
    canonical_name         = as.character(syn$synonym),
    taxon_rank             = "SPECIES",
    taxonomic_status       = "SYNONYM",
    accepted_name_usage_id = as.character(syn$SpecCode),
    family                 = acc$family[ai],
    genus                  = sub("\\s.*$", "", syn$synonym),
    specific_epithet       = sub("^\\S+\\s+", "", syn$synonym),
    authorship             = NA_character_,
    infraspecific_epithet  = NA_character_,
    kingdom                = acc$kingdom[ai],
    phylum                 = acc$phylum[ai],
    class                  = acc$class[ai],
    order                  = acc$order[ai],
    stringsAsFactors = FALSE
  )
  if (verbose) message(sprintf("  %s synonym names",
                               format(nrow(syndf), big.mark = ",")))

  rbind(acc, syndf)
}


# Column map: the reader already emits canonical names, so each maps to itself.
.rfishbase_col_map <- list(
  taxon_id               = "taxon_id",
  canonical_name         = "canonical_name",
  taxon_rank             = "taxon_rank",
  taxonomic_status       = "taxonomic_status",
  accepted_name_usage_id = "accepted_name_usage_id",
  family                 = "family",
  genus                  = "genus",
  specific_epithet       = "specific_epithet",
  authorship             = "authorship",
  infraspecific_epithet  = "infraspecific_epithet"
)

.rfishbase_extra_cols <- list(
  kingdom = "kingdom", phylum = "phylum", class = "class", order = "order"
)


#' Build the FishBase backbone .vtr
#'
#' @param output_dir Character.
#' @param version Character or NULL.
#' @param verbose Logical.
#' @return Path to the .vtr file (invisibly).
#' @export
build_fishbase <- function(output_dir = "output/fishbase", version = NULL,
                           verbose = TRUE) {
  if (is.null(version)) version <- format(Sys.Date(), "%Y.%m")

  df <- .read_rfishbase_backbone("fishbase",
                                 default_kingdom = "Animalia",
                                 default_phylum = "Chordata",
                                 verbose = verbose)
  df <- normalize_backbone(df, .rfishbase_col_map, .rfishbase_extra_cols)

  if (verbose) message("Precomputing keys and embedding synonyms...")
  df <- precompute_backbone(df)

  vtr_path <- file.path(output_dir, "fishbase.vtr")
  build_vtr(df, vtr_path, "fishbase", version, .fishbase_url)
  invisible(vtr_path)
}
