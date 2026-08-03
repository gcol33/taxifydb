# Sources opened in the ninth trait wave: plant hydraulics, root-nodule
# symbiosis, prokaryote metabolic function, host-virus association breadth and
# earthworm ecological groups.
#
# Two of these redistribute derived aggregates rather than the source records,
# the treatment GloBI and InvaCost already get: VIRION's per-host counts are
# computed here from the association table, and the raw associations are not
# republished.


#' Parse the Sanchez-Martinez et al. 2020 plant hydraulics compilation
#'
#' `HydraEvol2020.csv` from the figshare deposit behind "Adaptation and
#' coordinated evolution of plant hydraulic traits" (Ecology Letters
#' 23:1599-1610): 2027 seed-plant species with the four traits the paper
#' analyses -- xylem specific conductivity (Ks), xylem resistance to embolism
#' (P50), sapwood allocation relative to leaf area (the Huber value Hv) and
#' drought exposure (the minimum midday water potential) -- alongside a
#' WorldClim/SoilGrids block describing where each species grows.
#'
#' A genealogy warning that matters for the trait registry: this is a
#' literature compilation, and so are AusTraits and BROT. Where they overlap
#' they often carry the same primary record -- 63% of the P50 values shared
#' with AusTraits and 65% of the Ks values are equal to the last digit, and 42%
#' of the P50 values shared with BROT. Agreement between them is therefore not
#' independent corroboration. The overlap is small against what this source
#' adds (74 of its 894 P50 species are in either incumbent), which is why it is
#' registered rather than rejected the way FISHMORPH's max body length was.
#'
#' The Huber value is the one column needing a conversion, and the overlap is
#' what pins it: 79 of the 208 species shared with AusTraits are equal to the
#' last digit after multiplying by 1e-4, so the source is cm2 sapwood per m2
#' leaf where AusTraits is m2/m2. The `.vtr` keeps the source unit and the
#' registry applies the factor.
#'
#' Only the hydraulic block and the mean annual temperature are kept. The rest
#' of the climate and soil columns describe a site, not the plant.
#'
#' @param path Character. Path to the downloaded `HydraEvol2020.csv`.
#' @return data.frame with `canonical_name` plus hydraulic trait columns.
#' @export
parse_hydraulics <- function(path) {
  d <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)

  need <- c("Species", "Ks", "P50", "Hv")
  miss <- setdiff(need, names(d))
  if (length(miss) > 0L) {
    stop(sprintf("Hydraulics table missing expected column(s): %s",
                 paste(miss, collapse = ", ")), call. = FALSE)
  }

  num <- function(cn) {
    if (!cn %in% names(d)) return(NA_real_)
    suppressWarnings(as.numeric(trimws(as.character(d[[cn]]))))
  }

  out <- data.frame(
    canonical_name              = .to_utf8(trimws(as.character(d$Species))),
    p50_mpa                     = num("P50"),
    sapwood_conductivity        = num("Ks"),
    leaf_conductivity           = num("Kl"),
    huber_value_cm2_m2          = num("Hv"),
    min_water_potential_mpa     = num("MinWP_md"),
    hydraulic_safety_margin_mpa = num("HSM"),
    mean_annual_temp_c          = num("MAT"),
    stringsAsFactors            = FALSE
  )

  .trait_finalize(out)
}


#' Parse NodDB, the global database of root-symbiotic nitrogen fixation
#'
#' `NodDB database v1.3b.xlsx` from the PlutoF deposit behind Tedersoo et al.
#' (2018, Journal of Vegetation Science 29:560-568). Nodulation is recorded per
#' plant genus, not per species, so this is a genus-keyed source in the same
#' shape as FungalRoot: `canonical_name` carries the genus and taxify joins it
#' on `genus`.
#'
#' The sheet puts its citation on row 1 and the header on row 2, so the header
#' is read positionally rather than by `read.csv`'s first-row assumption.
#'
#' `Consensus estimate` is the authors' verdict and is kept verbatim. The
#' canonical `nodulation_type` collapses it to the symbiont actually involved.
#' The collapse is an exact lookup rather than a regex because the vocabulary
#' contains `Rhizobia`, `likely_Rhizobia` and `unlikely_Rhizobia`, each of which
#' contains the one before it -- a pattern match in the wrong order would read
#' every negative verdict as a positive one. `unlikely_*` is the authors'
#' judgement that the genus does not nodulate and reads as `none`.
#'
#' @param path Character. Path to the downloaded `.xlsx`.
#' @return data.frame with `canonical_name` (genus) plus nodulation columns.
#' @export
parse_noddb <- function(path) {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop("Parsing NodDB requires the 'openxlsx2' package.", call. = FALSE)
  }
  raw <- openxlsx2::wb_to_df(path, sheet = 1, col_names = FALSE)
  if (nrow(raw) < 3L) {
    stop("NodDB sheet has too few rows to carry a header plus data.",
         call. = FALSE)
  }

  hdr <- trimws(as.character(unlist(raw[2L, ])))
  d   <- raw[-c(1L, 2L), , drop = FALSE]
  names(d) <- ifelse(is.na(hdr) | !nzchar(hdr),
                     paste0("V", seq_along(hdr)), hdr)

  if (!"genus" %in% names(d)) {
    stop("NodDB sheet has no 'genus' column on its second row.", call. = FALSE)
  }

  chr <- function(cn) {
    if (!cn %in% names(d)) return(NA_character_)
    v <- trimws(as.character(d[[cn]]))
    v[!nzchar(v) | v == "NA"] <- NA_character_
    v
  }
  num <- function(cn) {
    if (!cn %in% names(d)) return(NA_real_)
    suppressWarnings(as.numeric(trimws(as.character(d[[cn]]))))
  }

  genus <- chr("genus")
  keep  <- !is.na(genus)
  d     <- d[keep, , drop = FALSE]
  genus <- genus[keep]

  out <- data.frame(
    canonical_name       = .to_utf8(genus),
    genus                = .to_utf8(genus),
    nodulation_consensus = chr("Consensus estimate"),
    nodulation_type      = .noddb_symbiont(chr("Consensus estimate")),
    nodulation_clade     = chr("Nodulation_clade"),
    nodulation_family    = chr("family"),
    spp_recognized       = num("Spp recognized (The Plant List)"),
    spp_studied          = num("Spp studied"),
    positive_reports     = num("Positive reports"),
    negative_reports     = num("Negative reports"),
    stringsAsFactors     = FALSE
  )

  .trait_finalize(out)
}

#' Collapse a NodDB consensus verdict to the symbiont involved
#'
#' Exact lookup over the closed vocabulary, deliberately not a regex: three of
#' the values nest inside one another.
#' @noRd
.noddb_symbiont <- function(x) {
  map <- c(
    "Rhizobia"          = "rhizobia",
    "likely_Rhizobia"   = "rhizobia",
    "unlikely_Rhizobia" = "none",
    "Frankia"           = "frankia",
    "likely_Frankia"    = "frankia",
    "unlikely_Frankia"  = "none",
    "Nostocaceae"       = "nostocaceae",
    "likely_Nostocaceae" = "nostocaceae",
    "Present"           = "present",
    "likely_present"    = "present",
    "None"              = "none"
  )
  unname(map[trimws(as.character(x))])
}


#' Parse FAPROTAX, the functional annotation of prokaryotic taxa
#'
#' `FAPROTAX.txt` from the release zip (Louca et al. 2016, Science
#' 353:1272-1277): 92 metabolic and ecological function groups, each listing the
#' prokaryotic taxa known to perform it, compiled from IJSEM and Bergey's
#' Manual.
#'
#' The file is a grouped list rather than a table. A group begins with its name
#' plus a tab-separated attribute string; the lines under it are member taxa
#' written as taxonomic path suffixes (`*Proteobacteria*Nitrosomonas*`,
#' `*Escherichia*coli*`). A group may also inherit or exclude another group's
#' members through `add_group:` / `subtract_group:`, which are resolved here in
#' file order -- a referenced group is always defined earlier.
#'
#' Members are annotated at whatever rank the evidence supports, so the file
#' mixes species, genera and higher taxa in one list. taxify joins an enrichment
#' on a species name or a genus, so each member is reduced to whichever of those
#' it carries: the last genus-plus-epithet pair in the path if there is one, else
#' the last plain capitalised name. Strain designations (`107`, `4_3_47FAA`,
#' `2.4.3`) carry neither and are dropped. Rows keyed on a rank above genus are
#' kept as written -- they simply never match a genus, at the cost of a few
#' hundred rows.
#'
#' The result is one row per taxon with the function groups it belongs to as a
#' pipe-delimited set, the same shape the NestTrait modality columns use: a
#' prokaryote genuinely performs several of these, and collapsing to one would
#' be a choice the source does not make.
#'
#' @param path Character. Path to the unpacked release directory or to
#'   `FAPROTAX.txt` itself.
#' @return data.frame with `canonical_name`, `faprotax_functions`,
#'   `faprotax_n_functions`.
#' @export
parse_faprotax <- function(path) {
  f <- path
  if (dir.exists(path)) {
    hits <- list.files(path, pattern = "^FAPROTAX\\.txt$", full.names = TRUE,
                       recursive = TRUE)
    if (length(hits) == 0L) {
      stop("FAPROTAX: no FAPROTAX.txt found under ", path, call. = FALSE)
    }
    f <- hits[[1L]]
  }
  ln <- readLines(f, warn = FALSE)
  ln <- sub("#.*$", "", ln)
  ln <- ln[nzchar(trimws(ln))]

  members <- list()
  current <- NA_character_
  for (l in ln) {
    if (grepl("^\\s*[\"']?\\*", l)) {
      if (!is.na(current)) members[[current]] <- c(members[[current]], trimws(l))
    } else if (grepl("^\\s*(add_group|subtract_group)\\s*:", l)) {
      op    <- sub("^\\s*(add_group|subtract_group)\\s*:.*$", "\\1", l)
      other <- trimws(sub("^\\s*(add_group|subtract_group)\\s*:", "", l))
      other <- trimws(sub("[\t ].*$", "", other))
      if (!is.na(current) && !is.null(members[[other]])) {
        members[[current]] <- if (op == "add_group") {
          union(members[[current]], members[[other]])
        } else {
          setdiff(members[[current]], members[[other]])
        }
      }
    } else {
      current <- trimws(sub("[\t ].*$", "", l))
      if (is.null(members[[current]])) members[[current]] <- character(0)
    }
  }
  members <- members[vapply(members, length, integer(1L)) > 0L]
  if (length(members) == 0L) {
    stop("FAPROTAX: parsed no member taxa.", call. = FALSE)
  }

  long <- data.frame(
    name  = .faprotax_key(unlist(members, use.names = FALSE)),
    group = rep(names(members), vapply(members, length, integer(1L))),
    stringsAsFactors = FALSE
  )
  long <- long[!is.na(long$name), , drop = FALSE]
  long <- long[!duplicated(long[c("name", "group")]), , drop = FALSE]
  if (nrow(long) == 0L) {
    stop("FAPROTAX: no member taxon reduced to a species or genus key.",
         call. = FALSE)
  }

  idx <- split(seq_len(nrow(long)), long$name)
  out <- data.frame(
    canonical_name       = names(idx),
    faprotax_functions   = vapply(idx, function(i)
                             paste(sort(unique(long$group[i])), collapse = "|"),
                             character(1L), USE.NAMES = FALSE),
    faprotax_n_functions = vapply(idx, length, integer(1L), USE.NAMES = FALSE),
    stringsAsFactors     = FALSE
  )
  rownames(out) <- NULL
  .trait_finalize(out)
}

#' Reduce a FAPROTAX member path to a binomial or a single taxon name
#'
#' Scans from the end for a genus-plus-epithet pair, so a member carrying a
#' trailing strain code still yields its species. Falls back to the last plain
#' capitalised name, and returns NA for a path holding neither.
#' @noRd
.faprotax_key <- function(x) {
  s <- gsub("[\"']", "", trimws(as.character(x)))
  s <- sub("^\\*+", "", sub("\\*+$", "", s))
  parts <- strsplit(s, "*", fixed = TRUE)

  is_genus   <- function(t) grepl("^[A-Z][a-z]{2,}$", t)
  is_epithet <- function(t) grepl("^[a-z][a-z-]{2,}$", t)

  vapply(parts, function(p) {
    p <- trimws(p)
    p <- p[nzchar(p)]
    if (!length(p)) return(NA_character_)
    if (length(p) >= 2L) {
      for (i in seq(length(p), 2L)) {
        if (is_epithet(p[i]) && is_genus(p[i - 1L])) {
          return(paste(p[i - 1L], p[i]))
        }
      }
    }
    hit <- which(is_genus(p))
    if (length(hit)) p[max(hit)] else NA_character_
  }, character(1L), USE.NAMES = FALSE)
}


#' Parse VIRION into per-host virus association breadth
#'
#' `virion.csv.gz` plus `tax_table.csv.gz` from the VIRION data package (Carlson
#' et al. 2022, mBio 13:e0298521), the merged host-virus association network.
#' The association table keys hosts and viruses by hash, so the taxonomy table
#' supplies the names.
#'
#' Only per-host aggregates are written: the number of distinct viruses recorded
#' from the host, the number of distinct virus families, and the number of
#' association records behind those counts. The association records themselves
#' are not redistributed, the same treatment GloBI and InvaCost get.
#'
#' Counts are association breadth as recorded, not a biological property of the
#' host: sampling effort dominates, which is why *Homo sapiens* leads with 936
#' distinct viruses. The record count travels alongside so the effort behind a
#' number is visible.
#'
#' @param path Character. Directory holding the downloaded VIRION files.
#' @return data.frame with `canonical_name` plus virus association counts.
#' @export
parse_virion <- function(path) {
  pick <- function(pat) {
    hits <- list.files(path, pattern = pat, full.names = TRUE, recursive = TRUE)
    if (length(hits) == 0L) {
      stop(sprintf("VIRION: no file matching %s under %s", pat, path),
           call. = FALSE)
    }
    hits[[1L]]
  }
  assoc <- utils::read.csv(gzfile(pick("^virion\\.csv\\.gz$")),
                           stringsAsFactors = FALSE)
  tax   <- utils::read.csv(gzfile(pick("^tax_table\\.csv\\.gz$")),
                           stringsAsFactors = FALSE)

  need <- c("HostTaxHashID", "VirusTaxHashID")
  miss <- setdiff(need, names(assoc))
  if (length(miss) > 0L) {
    stop(sprintf("VIRION association table missing column(s): %s",
                 paste(miss, collapse = ", ")), call. = FALSE)
  }

  hi <- match(assoc$HostTaxHashID,  tax$TaxHashID)
  vi <- match(assoc$VirusTaxHashID, tax$TaxHashID)

  host   <- trimws(tax$ScientificName[hi])
  hclass <- trimws(tax$Class[hi])
  virus  <- trimws(tax$ScientificName[vi])
  vfam   <- trimws(tax$Family[vi])

  # VIRION lowercases its resolved names; the join key is a canonical binomial.
  ok <- !is.na(host) & nzchar(host) & grepl(" ", host) &
        !is.na(virus) & nzchar(virus)
  host   <- .capitalize_binomial(host[ok])
  hclass <- hclass[ok]
  virus  <- virus[ok]
  vfam   <- vfam[ok]

  if (!length(host)) {
    stop("VIRION: no association row resolved to a host binomial.",
         call. = FALSE)
  }

  idx <- split(seq_along(host), host)
  nuniq <- function(v) length(unique(v[!is.na(v) & nzchar(v)]))

  out <- data.frame(
    canonical_name      = names(idx),
    virus_richness      = vapply(idx, function(i) nuniq(virus[i]), integer(1L),
                                 USE.NAMES = FALSE),
    virus_family_count  = vapply(idx, function(i) nuniq(vfam[i]), integer(1L),
                                 USE.NAMES = FALSE),
    virus_record_count  = vapply(idx, length, integer(1L), USE.NAMES = FALSE),
    host_class          = vapply(idx, function(i) {
                            u <- hclass[i]; u <- u[!is.na(u) & nzchar(u)]
                            if (!length(u)) NA_character_
                            else names(sort(table(u), decreasing = TRUE))[[1L]]
                          }, character(1L), USE.NAMES = FALSE),
    stringsAsFactors    = FALSE
  )
  rownames(out) <- NULL
  .trait_finalize(out)
}

#' Capitalise the genus of a lowercased binomial
#' @noRd
.capitalize_binomial <- function(x) {
  s <- trimws(as.character(x))
  sub("^([a-z])", "\\U\\1", s, perl = TRUE)
}


#' Parse the sWorm global earthworm dataset into per-species ecological groups
#'
#' `SppOccData_sWorm_<date>.csv` from the sWorm data release (Phillips et al.
#' 2021, Scientific Data 8:136), the sDiv-funded compilation behind the global
#' earthworm diversity synthesis.
#'
#' The release is a community dataset -- species occurrences with abundance and
#' biomass at 10,840 sites -- and one column of it is a species trait: the
#' Bouche ecological group each earthworm was assigned from its feeding and
#' burrowing behaviour. That column is constant within a species across the
#' whole compilation (171 of 172 species carry exactly one group and none
#' carries two), so collapsing it to one row per species loses nothing.
#'
#' Abundance and wet biomass are deliberately left out. Both ship in mixed units
#' in the same column -- individuals, individuals per m2 and per m3; grams,
#' g/m2 and mg/m2 -- mixing a per-individual measure with a per-area density, so
#' neither has a unit that survives aggregation to the species. The
#' native/non-native column is left out for a different reason: it is a status at
#' a site rather than a property of the species, and a species is native in one
#' place and introduced in another.
#'
#' Earthworms are otherwise absent from the bundled trait sources.
#'
#' @param path Character. Directory holding the unpacked sWorm release.
#' @return data.frame with `canonical_name` and `ecological_group`.
#' @export
parse_sworm <- function(path) {
  hits <- list.files(path, pattern = "^SppOccData_sWorm.*\\.csv$",
                     full.names = TRUE, recursive = TRUE)
  hits <- hits[!grepl("__MACOSX", hits, fixed = TRUE)]
  if (length(hits) == 0L) {
    stop("sWorm: no SppOccData_sWorm*.csv found under ", path, call. = FALSE)
  }
  d <- utils::read.csv(hits[[1L]], stringsAsFactors = FALSE)

  need <- c("SpeciesBinomial", "Ecological_group")
  miss <- setdiff(need, names(d))
  if (length(miss) > 0L) {
    stop(sprintf("sWorm species table missing expected column(s): %s",
                 paste(miss, collapse = ", ")), call. = FALSE)
  }

  sp  <- trimws(as.character(d$SpeciesBinomial))
  grp <- trimws(as.character(d$Ecological_group))
  ok  <- nzchar(sp) & nzchar(grp) & grp != "Unknown"
  sp  <- sp[ok]; grp <- tolower(grp[ok])
  if (!length(sp)) {
    stop("sWorm: no record carries both a binomial and a known ecological group.",
         call. = FALSE)
  }

  # Majority within a species. The column is in fact constant per species in
  # the 2021 release; the vote is what keeps a later release with a
  # disagreement from resolving on row order.
  tab <- table(sp, grp)
  out <- data.frame(
    canonical_name   = rownames(tab),
    ecological_group = colnames(tab)[max.col(tab, ties.method = "first")],
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  .trait_finalize(out)
}
