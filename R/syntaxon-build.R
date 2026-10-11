# Shared build steps of the syntaxon backbones (vegetation units).
#
# A syntaxon backbone is built from a list of accepted units, each placed in
# the hierarchy, and the synonyms listed under each unit. The sources differ
# (the EuroVegChecklist, a national classification), but from that point on
# the rows are assembled, identified, keyed and written the same way, here.
# Names are parsed and keyed by taxify (parse_name(code = "syntaxon"),
# precompute_keys(code = "syntaxon")), the parse and keys taxify's matcher
# applies to a query.

# The hierarchy columns every syntaxon backbone carries, broadest first: the
# accepted unit's ancestor at each rank, NA at a rank the source does not use
# (the Czech classification has no orders, the EuroVegChecklist no
# associations).
.syntaxon_hierarchy <- c("class", "order", "alliance", "association")

# A citation of another author's concept under the name ("Thero-Salicornion
# sensu Vicherek 1973 non Br.-Bl. 1933", "auct.", a pseudonym) is a
# misapplication: the name points at this unit only as that author used it.
# A bare "non" is not one: a later homonym is cited "non <earlier author>"
# and is the name of this unit all the same.
.syntaxon_misapplied <- "\\b(sensu|auct\\.|pseudonym)"


#' Assemble the rows of a syntaxon backbone
#'
#' @param units Accepted units: `code`, `name`, `authorship`, `rank`
#'   (uppercase), the hierarchy columns, and any further columns to keep
#'   (passed through on the unit's own row).
#' @param syn Synonyms: `code` (of the accepted unit), `name`, `authorship`,
#'   `rank` (may be `NA`: the unit's rank is used), `note`, `remark`.
#' @param code_col Name of the column the unit code is written to.
#' @param backbone Backbone name, for messages.
#' @return The backbone rows, before keys: `taxon_id`, `canonical_name`,
#'   `authorship`, `taxon_rank`, `taxonomic_status`
#'   (`ACCEPTED`/`SYNONYM`/`MISAPPLIED`), `accepted_name_usage_id`,
#'   `<code_col>`, `nomenclatural_note`, `remark`, the hierarchy columns and
#'   the extra unit columns (`NA` on synonym rows).
#' @noRd
syntaxon_backbone_rows <- function(units, syn, code_col, backbone) {
  for (h in .syntaxon_hierarchy) {
    if (is.null(units[[h]])) units[[h]] <- NA_character_
  }
  extra <- setdiff(names(units), c("code", "name", "authorship", "rank",
                                   .syntaxon_hierarchy))
  auth_key <- function(a) ifelse(is.na(a), "", a)

  syn <- syn[!is.na(syn$name), , drop = FALSE]
  acc_rank <- stats::setNames(units$rank, units$code)
  no_rank <- is.na(syn$rank) | !nzchar(syn$rank)
  syn$rank[no_rank] <- acc_rank[syn$code[no_rank]]
  # A synonym listed twice under one unit is one record, and a unit's own
  # name and author listed among its synonyms is the unit.
  key <- paste(syn$code, syn$name, auth_key(syn$authorship), sep = "\r")
  own <- paste(units$code, units$name, auth_key(units$authorship), sep = "\r")
  syn <- syn[!duplicated(key) & !key %in% own, , drop = FALSE]

  # Stable synonym ids: the accepted unit's code and a hash of the written
  # name, so an id survives a synonym being added to or dropped from the list.
  syn_id <- paste0(syn$code, ":", substr(vapply(
    paste(syn$name, auth_key(syn$authorship)), digest::digest, character(1L),
    algo = "md5", serialize = FALSE), 1L, 10L))
  if (anyDuplicated(syn_id)) {
    stop(sprintf("%s: synonym id collision; widen the hash prefix.", backbone),
         call. = FALSE)
  }
  misapplied <- grepl(.syntaxon_misapplied,
                      paste(auth_key(syn$authorship), auth_key(syn$note)),
                      perl = TRUE)

  accepted <- data.frame(
    taxon_id               = units$code,
    canonical_name         = units$name,
    authorship             = units$authorship,
    taxon_rank             = units$rank,
    taxonomic_status       = "ACCEPTED",
    accepted_name_usage_id = NA_character_,
    code                   = units$code,
    nomenclatural_note     = NA_character_,
    remark                 = NA_character_,
    stringsAsFactors = FALSE
  )
  synonyms <- data.frame(
    taxon_id               = syn_id,
    canonical_name         = syn$name,
    authorship             = syn$authorship,
    taxon_rank             = syn$rank,
    taxonomic_status       = ifelse(misapplied, "MISAPPLIED", "SYNONYM"),
    accepted_name_usage_id = syn$code,
    code                   = syn$code,
    nomenclatural_note     = syn$note,
    remark                 = syn$remark,
    stringsAsFactors = FALSE
  )
  df <- rbind(accepted, synonyms)
  at <- match(df$code, units$code)
  for (h in .syntaxon_hierarchy) df[[h]] <- units[[h]][at]
  for (e in extra) {
    df[[e]] <- units[[e]][at]
    df[[e]][df$taxonomic_status != "ACCEPTED"] <- NA
  }
  names(df)[names(df) == "code"] <- code_col
  rownames(df) <- NULL
  message(sprintf("  %s rows (%s accepted, %s synonyms, %s misapplied)",
                  format(nrow(df), big.mark = ","),
                  format(nrow(accepted), big.mark = ","),
                  format(sum(!misapplied), big.mark = ","),
                  format(sum(misapplied), big.mark = ",")))
  df
}


#' Key, embed and write a syntaxon backbone
#'
#' @param df Rows from [syntaxon_backbone_rows()].
#' @param output_dir,backbone,version,source_url,source_date,extra As for
#'   [build_vtr()].
#' @return Path to the `.vtr` (invisibly).
#' @noRd
write_syntaxon_backbone <- function(df, output_dir, backbone, version,
                                    source_url, source_date, extra = NULL) {
  df <- taxify::precompute_keys(df, "canonical_name", code = "syntaxon")
  df <- taxify::embed_accepted(
    df,
    id_col          = "taxon_id",
    acc_id_col      = "accepted_name_usage_id",
    name_col        = "canonical_name",
    status_col      = "taxonomic_status",
    synonym_pattern = "SYNONYM|MISAPPLIED",
    authorship_col  = "authorship"
  )
  vtr_path <- file.path(output_dir, paste0(backbone, ".vtr"))
  build_vtr(df, vtr_path, backbone, version, source_url, source_date,
            block_col = "block_key", extra = extra)
  invisible(vtr_path)
}
