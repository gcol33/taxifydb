# Parsers that map sub-national region names to ISO codes, plus conservation
# status fetched from GBIF, and WCVP distribution data.


# ---- Alien first records (Seebens et al.) ----

#' Seebens region name to ISO 3166-1 alpha-2 mapping
#'
#' Sub-national regions are mapped to their parent country.
#' Multi-country entries (e.g., "USACanada") are mapped to NA and dropped.
#' @noRd
.seebens_region_map <- c(
  "Aegean" = "GR",
  "Afghanistan" = "AF",
  "Aland Islands" = "AX",
  "Åland" = "AX",
  "Åland Islands" = "AX",
  "Alaska" = "US",
  "Albania" = "AL",
  "Algeria" = "DZ",
  "American Samoa" = "AS",
  "Amsterdam Island" = "TF",
  "Andaman and Nicobar Islands" = "IN",
  "Andorra" = "AD",
  "Angola" = "AO",
  "Anguilla" = "AI",
  "Antarctica" = "AQ",
  "Anticosti Island" = "CA",
  "Antigua and Barbuda" = "AG",
  "Antipodes Island" = "NZ",
  "Argentina" = "AR",
  "Armenia" = "AM",
  "Aruba" = "AW",
  "Ascension" = "SH",
  "Australia" = "AU",
  "Austria" = "AT",
  "Azerbaijan" = "AZ",
  "Azores" = "PT",
  "Bahamas" = "BS",
  "Bahrain" = "BH",
  "Balearic Islands" = "ES",
  "Bali" = "ID",
  "Bangladesh" = "BD",
  "Barbados" = "BB",
  "Belarus" = "BY",
  "Belgium" = "BE",
  "Belgium, France, Netherlands, Uk" = NA_character_,
  "Belize" = "BZ",
  "Benin" = "BJ",
  "Bermuda" = "BM",
  "Bhutan" = "BT",
  "Biak" = "ID",
  "Bolivia" = "BO",
  "Bonaire" = "BQ",
  "Bosnia and Herzegovina" = "BA",
  "Botswana" = "BW",
  "Brazil" = "BR",
  "British Virgin Islands" = "VG",
  "Brunei" = "BN",
  "Brunei Darussalam" = "BN",
  "Bulgaria" = "BG",
  "Burkina Faso" = "BF",
  "Burundi" = "BI",
  "Cambodia" = "KH",
  "Cameroon" = "CM",
  "Campbell" = "NZ",
  "Canada" = "CA",
  "Canary Islands" = "ES",
  "Cape Verde" = "CV",
  "Cayman Islands" = "KY",
  "Central African Republic" = "CF",
  "Chad" = "TD",
  "Chagos Archipelago" = "IO",
  "Channel Islands" = "GB",
  "Chile" = "CL",
  "China" = "CN",
  "Christmas Island" = "CX",
  "Clipperton Island" = "FR",
  "Cocos (Keeling) Islands" = "CC",
  "Cocos Islands" = "CC",
  "Colombia" = "CO",
  "Comoros" = "KM",
  "Congo, Democratic Republic of the" = "CD",
  "Congo, Republic of" = "CG",
  "Cook Islands" = "CK",
  "Corse" = "FR",
  "Corsica" = "FR",
  "Costa Rica" = "CR",
  "Cote D'Ivoire" = "CI",
  "Crete" = "GR",
  "Croatia" = "HR",
  "Crozet Islands" = "TF",
  "Crozet Islands Group" = "TF",
  "Cuba" = "CU",
  "Curacao" = "CW",
  "Cyprus" = "CY",
  "Czech Republic" = "CZ",
  "De" = NA_character_,
  "Democratic Republic of the Congo" = "CD",
  "Denmark" = "DK",
  "Djibouti" = "DJ",
  "Dominica" = "DM",
  "Dominican Republic" = "DO",
  "Easter Island" = "CL",
  "Ecuador" = "EC",
  "Egypt" = "EG",
  "El Salvador" = "SV",
  "Equatorial Guinea" = "GQ",
  "Eritrea" = "ER",
  "Estonia" = "EE",
  "Eswatini" = "SZ",
  "Ethiopia" = "ET",
  "Falkland Islands" = "FK",
  "Faroe Islands" = "FO",
  "Fernando De Noronha" = "BR",
  "Fiji" = "FJ",
  "Finland" = "FI",
  "France" = "FR",
  "France, Turkey" = NA_character_,
  "French Guiana" = "GF",
  "French Polynesia" = "PF",
  "Gabon" = "GA",
  "Galapagos" = "EC",
  "Gambia" = "GM",
  "Georgia" = "GE",
  "Germany" = "DE",
  "Germany and France" = NA_character_,
  "Germany and Spain" = NA_character_,
  "Ghana" = "GH",
  "Gibraltar" = "GI",
  "Greece" = "GR",
  "Greenland" = "GL",
  "Grenada" = "GD",
  "Guadeloupe" = "GP",
  "Guam" = "GU",
  "Guatemala" = "GT",
  "Guinea" = "GN",
  "Guinea-Bissau" = "GW",
  "Guyana" = "GY",
  "Haiti" = "HT",
  "Hawaii" = "US",
  "Hawaiian Islands" = "US",
  "Heard and Mcdonald Islands" = "HM",
  "Heard Island and McDonald Island" = "HM",
  "Honduras" = "HN",
  "Hong Kong" = "HK",
  "Hungary" = "HU",
  "Iceland" = "IS",
  "India" = "IN",
  "Indonesia" = "ID",
  "Iran" = "IR",
  "Iran, Islamic Republic of" = "IR",
  "Iraq" = "IQ",
  "Ireland" = "IE",
  "Isle of Man" = "IM",
  "Israel" = "IL",
  "Italy" = "IT",
  "Italy and Germany" = NA_character_,
  "Italy, Hungary, Spain" = NA_character_,
  "Izu Islands" = "JP",
  "Jamaica" = "JM",
  "Japan" = "JP",
  "Jersey" = "JE",
  "Jordan" = "JO",
  "Kazakhstan" = "KZ",
  "Kenya" = "KE",
  "Kerguelen Islands" = "TF",
  "Kermadec Islands" = "NZ",
  "Kiribati" = "KI",
  "Kuwait" = "KW",
  "Kyrgyzstan" = "KG",
  "Laos" = "LA",
  "Latvia" = "LV",
  "Lebanon" = "LB",
  "Lesotho" = "LS",
  "Lesser Sunda Islands" = "ID",
  "Liberia" = "LR",
  "Libya" = "LY",
  "Liechtenstein" = "LI",
  "Lithuania" = "LT",
  "Lord Howe Island" = "AU",
  "Lord Howe Islands" = "AU",
  "Luxembourg" = "LU",
  "Macao" = "MO",
  "Macedonia" = "MK",
  "Macquarie" = "AU",
  "Madagascar" = "MG",
  "Madeira" = "PT",
  "Malawi" = "MW",
  "Malaysia" = "MY",
  "Maldives" = "MV",
  "Mali" = "ML",
  "Malta" = "MT",
  "Maluku" = "ID",
  "Marshall Islands" = "MH",
  "Martinique" = "MQ",
  "Mauritania" = "MR",
  "Mauritius" = "MU",
  "Mayotte" = "YT",
  "Mexico" = "MX",
  "Micronesia" = "FM",
  "Micronesia, Federated States of" = "FM",
  "Moldova" = "MD",
  "Monaco" = "MC",
  "Mongolia" = "MN",
  "Montenegro" = "ME",
  "Montserrat" = "MS",
  "Morocco" = "MA",
  "Mozambique" = "MZ",
  "Myanmar" = "MM",
  "Namibia" = "NA",
  "Nauru" = "NR",
  "Nepal" = "NP",
  "Netherlands" = "NL",
  "New Caledonia" = "NC",
  "New Zealand" = "NZ",
  "Nicaragua" = "NI",
  "Niger" = "NE",
  "Nigeria" = "NG",
  "Niue" = "NU",
  "Norfolk Island" = "NF",
  "North Korea" = "KP",
  "North Macedonia" = "MK",
  "Northern Mariana Islands" = "MP",
  "Norway" = "NO",
  "Ogasawara Islands" = "JP",
  "Oman" = "OM",
  "Pakistan" = "PK",
  "Palau" = "PW",
  "Palestine" = "PS",
  "Palestine, State of" = "PS",
  "Panama" = "PA",
  "Papua New Guinea" = "PG",
  "Paraguay" = "PY",
  "Peru" = "PE",
  "Philippines" = "PH",
  "Pitcairn Islands" = "PN",
  "Poland" = "PL",
  "Portugal" = "PT",
  "Puerto Rico" = "PR",
  "Qatar" = "QA",
  "Republic of the Congo" = "CG",
  "Reunion" = "RE",
  "Rodriguez Island" = "MU",
  "Romania" = "RO",
  "Russia" = "RU",
  "Rwanda" = "RW",
  "Ryukyu Islands" = "JP",
  "Saint Barthelemy" = "BL",
  "Saint Helena" = "SH",
  "Saint Kitts and Nevis" = "KN",
  "Saint Lucia" = "LC",
  "Saint Martin" = "MF",
  "Saint Paul (France)" = "TF",
  "Saint Pierre and Miquelon" = "PM",
  "Saint Vincent and the Grenadines" = "VC",
  "Samoa" = "WS",
  "San Marino" = "SM",
  "Sao Tome and Principe" = "ST",
  "Sardinia" = "IT",
  "Saudi Arabia" = "SA",
  "Scattered Islands" = "TF",
  "Sea of Cortez Islands" = "MX",
  "Senegal" = "SN",
  "Serbia" = "RS",
  "Seychelles" = "SC",
  "Shetland Islands" = "GB",
  "Sicily" = "IT",
  "Sierra Leone" = "SL",
  "Singapore" = "SG",
  "Sint Maarten" = "SX",
  "Slovakia" = "SK",
  "Slovenia" = "SI",
  "Socotra Island" = "YE",
  "Solomon Islands" = "SB",
  "Somalia" = "SO",
  "South Africa" = "ZA",
  "South Georgia and the South Sandwich Islands" = "GS",
  "South Korea" = "KR",
  "South Orkney Islands" = "AQ",
  "Spain" = "ES",
  "Spain, France, Hungary" = NA_character_,
  "Sri Lanka" = "LK",
  "Sudan" = "SD",
  "Sumatra" = "ID",
  "Suriname" = "SR",
  "Svalbard and Jan Mayen" = "SJ",
  "Sweden" = "SE",
  "Switzerland" = "CH",
  "Syria" = "SY",
  "Taiwan" = "TW",
  "Tajikistan" = "TJ",
  "Tanzania" = "TZ",
  "Tasmania" = "AU",
  "Thailand" = "TH",
  "Timor Leste" = "TL",
  "Togo" = "TG",
  "Tokelau" = "TK",
  "Tonga" = "TO",
  "Trinidad and Tobago" = "TT",
  "Tristan da Cunha" = "SH",
  "Tunisia" = "TN",
  "Turkey" = "TR",
  "Turkmenistan" = "TM",
  "Turks and Caicos" = "TC",
  "Turks and Caicos Islands" = "TC",
  "Tuvalu" = "TV",
  "Uganda" = "UG",
  "Uk and Netherlands" = NA_character_,
  "Ukraine" = "UA",
  "United Arab Emirates" = "AE",
  "United Kingdom" = "GB",
  "United States" = "US",
  "United States Minor Outlying Islands" = "UM",
  "United States of America" = "US",
  "Uruguay" = "UY",
  "US Minor Outlying Islands" = "UM",
  "USACanada" = NA_character_,
  "Uzbekistan" = "UZ",
  "Vancouver Island" = "CA",
  "Vanuatu" = "VU",
  "Venezuela" = "VE",
  "Vietnam" = "VN",
  "Virgin Islands (British)" = "VG",
  "Virgin Islands (U.S.)" = "VI",
  "Virgin Islands, US" = "VI",
  "Wallis and Futuna" = "WF",
  "Western Sahara" = "EH",
  "Yemen" = "YE",
  "Zambia" = "ZM",
  "Zanzibar Island" = "TZ",
  "Zimbabwe" = "ZW"
)


#' Look up location names in the Seebens region map
#'
#' The map is keyed on the wording a release happens to use, and successive
#' releases respell the same place: v4.0 hyphenates "Timor-Leste" and
#' "Saint-Martin" where v3.1 spaced them, and accents Reunion, Curacao and
#' Sao Tome where v3.1 left them bare. Matching on a folded, punctuation-free
#' key absorbs that whole class of rewording, leaving the map to carry only
#' genuinely distinct wordings ("Hawaii" beside "Hawaiian Islands").
#'
#' @param x Character vector of location names.
#' @return ISO 3166-1 alpha-2 codes; `NA` for a name the map does not hold and
#'   for the multi-country entries it deliberately records as `NA`.
#' @noRd
.seebens_country_code <- function(x) {
  key <- .norm_region_key(.to_utf8(x))
  lookup <- .seebens_region_lookup()
  unname(lookup[match(key, names(lookup))])
}

#' Fold a location name to its lookup key: accent-free, lowercase, unpunctuated
#' @noRd
.norm_region_key <- function(x) {
  trimws(gsub("[^a-z0-9]+", " ", tolower(fold_accents(x))))
}

#' Region map re-keyed on the normalized lookup key
#'
#' Built once per session. Two wordings that fold to the same key must agree
#' on the country, or the map is ambiguous and the fold is unsafe for it.
#' @noRd
.seebens_region_lookup <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    keys <- .norm_region_key(names(.seebens_region_map))
    split_codes <- split(unname(.seebens_region_map), keys)
    clash <- vapply(split_codes,
                    function(v) length(unique(v[!is.na(v)])) > 1L, logical(1))
    if (any(clash)) {
      stop("Region names fold to one key but different countries: ",
           paste(names(split_codes)[clash], collapse = ", "), call. = FALSE)
    }
    cached <<- vapply(split_codes, function(v) {
      hit <- v[!is.na(v)]
      if (length(hit)) hit[[1L]] else NA_character_
    }, character(1))
    cached
  }
})

#' Parse the Seebens et al. global first-record database
#'
#' Reads the public dataset table, maps location names to ISO 3166-1 alpha-2
#' codes, and reduces to one row per species x country.
#'
#' Every location the source names must map. The map is keyed on wording, so a
#' release that renames a place would otherwise drop its records and return a
#' smaller table rather than an error: v4.0 alone renamed 24 locations, among
#' them "United States" to "United States of America", which is 8,050 records.
#' A name the map does not hold is therefore a hard error, and the entries that
#' resolve to no single country (a record spanning several, "USACanada") are
#' recorded in the map as `NA` so they read as known rather than missing.
#'
#' Where a species has several records for one country the earliest year wins,
#' but a record asserting the species is present is preferred to one recording
#' it as absent, uncertain or captive whatever the years are, so the retained
#' row does not date a country's occurrence from a record denying it. That
#' record's own status is published beside its year as
#' `alien_first_record_status`, since the remaining columns are reduced over
#' every record for the pair and a status reduced that way would describe some
#' record other than the one the year came from.
#'
#' @param path Character. Path to the `FirstRecords` dataset CSV.
#' @return data.frame with canonical_name + country_code + first-record cols.
#' @export
parse_alien_first_records <- function(path) {
  # Semicolon-delimited, and UTF-8 apart from one run of latin1 lines.
  df <- .read_delim_utf8(path, sep = ";", quote = "\"")

  name_col <- .first_col(df, c("taxon", "TaxonName", "scientificName"))
  loc_col  <- .first_col(df, c("location", "Region"))
  year_col <- .first_col(df, c("firstRecordEvent", "FirstRecord"))
  if (is.null(name_col) || is.null(loc_col) || is.null(year_col)) {
    stop("The first-record table has no taxon, location or year column; ",
         "it holds: ", paste(names(df), collapse = ", "), call. = FALSE)
  }

  location <- trimws(.to_utf8(df[[loc_col]]))
  df$country_code <- .seebens_country_code(location)

  # A location the map has never seen, as against one it records as spanning
  # several countries. Records carrying no location at all are sub- or
  # supra-national ("Aegean Sea", "European part of Russia") and have no
  # country to be keyed on, so they are dropped by the filter below.
  unknown <- unique(location[nzchar(location) &
                               !.norm_region_key(location) %in%
                                 names(.seebens_region_lookup())])
  if (length(unknown)) {
    stop("The first-record table names locations the region map does not ",
         "hold, so their records would be dropped silently. Add them to ",
         ".seebens_region_map: ", paste(sort(unknown), collapse = ", "),
         call. = FALSE)
  }

  status_col <- .first_col(df, c("occurrenceStatus", "PresentStatus"))
  status <- if (is.null(status_col)) rep(NA_character_, nrow(df))
            else tolower(trimws(.to_utf8(df[[status_col]])))
  # "present (not occurring in the wild)" is a captive or cultivated record.
  absent_rank <- as.integer(!(status %in% "present"))

  src_col <- .first_col(df, c("datasetName", "Source"))
  ref_col <- .first_col(df, c("bibliographicCitation", "Reference"))

  out <- data.frame(
    canonical_name               = trimws(.to_utf8(df[[name_col]])),
    country_code                 = df$country_code,
    alien_first_record           = suppressWarnings(
                                     as.integer(as.numeric(df[[year_col]]))),
    alien_first_record_status    = status,
    alien_first_record_source    = if (is.null(src_col)) NA_character_
                                   else .to_utf8(df[[src_col]]),
    alien_first_record_reference = if (is.null(ref_col)) NA_character_
                                   else .to_utf8(df[[ref_col]]),
    stringsAsFactors = FALSE
  )

  keep <- !is.na(out$canonical_name) & nzchar(out$canonical_name) &
    !is.na(out$country_code) & nchar(out$country_code) == 2L
  out <- out[keep, , drop = FALSE]
  df  <- df[keep, , drop = FALSE]
  absent_rank <- absent_rank[keep]

  ord <- order(out$canonical_name, out$country_code, absent_rank,
               out$alien_first_record, na.last = TRUE)
  out <- out[ord, , drop = FALSE]
  out <- out[!duplicated(paste(out$canonical_name, out$country_code)), ]

  # Carry every other field (raw location string, habitat, establishment
  # means, degree of establishment, ...) keyed on (species, country). The
  # verbatim year is the record as written, which for a record given as a span
  # is "-3000 - -2000"; read as a number that span is lost, and the column
  # exists to hold exactly what the point estimate above does not.
  out <- .append_all_cols(
    out, df, trimws(.to_utf8(df[[name_col]])),
    group = "country_code", group_row = df$country_code,
    cat_cols = .first_col(df, c("verbatimFirstRecordEvent",
                                "FirstRecord_orig")),
    used = c(name_col, "country_code", year_col, status_col, src_col, ref_col)
  )

  rownames(out) <- NULL
  out
}


#' Parse IUCN Red List conservation status from the GBIF Darwin Core Archive
#'
#' Reads the IUCN Red List archive published on GBIF (taxon.txt core plus the
#' Distribution extension that carries `threatStatus`) and returns one global
#' Red List category per accepted species-rank name. Reading the archive
#' directly avoids the GBIF `species/search` endpoint, whose threat facet spans
#' every checklist (so a name picks up conflicting categories from regional or
#' erroneous lists) and whose offset ceiling truncates the large categories.
#'
#' @param dir_path Character. Directory holding the extracted IUCN archive
#'   (`taxon.txt`, `distribution.txt`).
#' @return data.frame with canonical_name + conservation_status.
#' @export
parse_iucn <- function(dir_path) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required to parse the IUCN Red List archive.",
         call. = FALSE)
  }

  find_one <- function(name) {
    p <- file.path(dir_path, name)
    if (file.exists(p)) return(p)
    hits <- list.files(dir_path, pattern = paste0("^", name, "$"),
                       full.names = TRUE, recursive = TRUE)
    if (length(hits) > 0L) hits[1L] else NA_character_
  }
  tax_file  <- find_one("taxon.txt")
  dist_file <- find_one("distribution.txt")
  if (is.na(tax_file) || is.na(dist_file)) {
    stop("IUCN archive is missing taxon.txt or distribution.txt.", call. = FALSE)
  }

  # Darwin Core Archive: tab-separated, no field quoting, no header row
  # (meta.xml maps columns by position). threatStatus lives in the Distribution
  # extension (index 5), keyed to the core taxon by coreid (index 0).
  dist <- data.table::fread(
    dist_file, sep = "\t", header = FALSE, quote = "", fill = TRUE,
    select = c(1L, 5L, 6L),
    col.names = c("taxon_id", "locality", "threat_status"),
    colClasses = "character", showProgress = FALSE
  )
  dist <- dist[!is.na(dist$threat_status) & nzchar(dist$threat_status), ]
  # Every assessment in this archive is global; keep Global rows defensively.
  glob <- dist$locality == "Global"
  if (any(glob)) dist <- dist[glob, ]

  status_map <- c(
    "least concern"          = "LC",
    "near threatened"        = "NT",
    "conservation dependent" = "NT",
    "vulnerable"             = "VU",
    "endangered"             = "EN",
    "critically endangered"  = "CR",
    "extinct in the wild"    = "EW",
    "extinct"                = "EX",
    "data deficient"         = "DD"
  )
  dist$status <- status_map[tolower(trimws(dist$threat_status))]
  dist <- dist[!is.na(dist$status), ]

  # Core taxon table: id (0), genus (7), specificEpithet (8),
  # infraspecificEpithet (11), taxonomicStatus (12).
  tax <- data.table::fread(
    tax_file, sep = "\t", header = FALSE, quote = "", fill = TRUE,
    select = c(1L, 8L, 9L, 12L, 13L),
    col.names = c("taxon_id", "genus", "specific_epithet",
                  "infraspecific_epithet", "taxonomic_status"),
    colClasses = "character", showProgress = FALSE
  )
  tax <- tax[tolower(tax$taxonomic_status) == "accepted", ]
  tax <- tax[!is.na(tax$genus) & nzchar(tax$genus) &
             !is.na(tax$specific_epithet) & nzchar(tax$specific_epithet), ]

  tax$canonical_name <- gsub("\\s+", " ", trimws(paste(
    tax$genus, tax$specific_epithet,
    ifelse(is.na(tax$infraspecific_epithet), "", tax$infraspecific_epithet)
  )))

  tax$conservation_status <- dist$status[match(tax$taxon_id, dist$taxon_id)]
  tax <- tax[!is.na(tax$conservation_status), ]

  out <- data.frame(
    canonical_name      = tax$canonical_name,
    conservation_status = tax$conservation_status,
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]

  # A binomial can recur (an accepted infrataxon, or a cross-kingdom homonym).
  # Keep the most severe EXTANT assessment, accepting EW/EX only when no extant
  # assessment exists for that name, so a genuine Least Concern is never hidden
  # by an extinct relative sharing the name.
  priority <- c("CR" = 1L, "EN" = 2L, "VU" = 3L, "NT" = 4L, "LC" = 5L,
                "DD" = 6L, "EW" = 7L, "EX" = 8L)
  out$sev <- priority[out$conservation_status]
  out <- out[order(out$canonical_name, out$sev), ]
  out <- out[!duplicated(out$canonical_name), ]
  out$sev <- NULL

  rownames(out) <- NULL
  out
}


#' Parse WCVP names + distribution (from extracted ZIP directory)
#'
#' @param dir_path Character. Directory containing wcvp_names + wcvp_distribution.
#' @return data.frame with canonical_name + tdwg_code + native_status.
#' @export
parse_wcvp <- function(dir_path) {
  csvs <- list.files(dir_path, pattern = "\\.csv$|\\.txt$",
                     full.names = TRUE, recursive = TRUE)
  names_file <- grep("(?i)name", csvs, value = TRUE)
  dist_file <- grep("(?i)distribut", csvs, value = TRUE)

  if (length(names_file) == 0L || length(dist_file) == 0L) {
    stop(sprintf(
      "Could not find WCVP names/distribution files in: %s\nFiles: %s",
      dir_path, paste(basename(csvs), collapse = ", ")
    ), call. = FALSE)
  }
  names_file <- names_file[1L]
  dist_file <- dist_file[1L]

  if (requireNamespace("data.table", quietly = TRUE)) {
    names_df <- as.data.frame(data.table::fread(names_file,
                                                showProgress = FALSE))
    dist_df <- as.data.frame(data.table::fread(dist_file,
                                               showProgress = FALSE))
  } else {
    names_df <- utils::read.csv(names_file, stringsAsFactors = FALSE)
    dist_df <- utils::read.csv(dist_file, stringsAsFactors = FALSE)
  }

  find_col <- function(df, patterns) {
    for (p in patterns) {
      m <- grep(paste0("^", p, "$"), names(df), ignore.case = TRUE,
                value = TRUE)
      if (length(m) > 0L) return(m[1L])
    }
    for (p in patterns) {
      m <- grep(p, names(df), ignore.case = TRUE, value = TRUE)
      if (length(m) > 0L) return(m[1L])
    }
    NULL
  }

  id_col     <- find_col(names_df, c("plant_name_id", "kew_id", "id"))
  name_col   <- find_col(names_df, c("taxon_name", "scientific_name",
                                     "full_name", "name"))
  status_col <- find_col(names_df, c("taxon_status", "status",
                                     "taxonomic_status"))

  dist_id_col <- find_col(dist_df, c("plant_name_id", "kew_id", "id"))
  area_col    <- find_col(dist_df, c("area_code_l3", "area", "tdwg_code",
                                     "region_code"))
  intro_col   <- find_col(dist_df, c("introduced", "is_introduced"))
  extinct_col <- find_col(dist_df, c("extinct", "is_extinct"))

  if (!is.null(status_col)) {
    accepted <- names_df[tolower(names_df[[status_col]]) == "accepted", ]
  } else {
    accepted <- names_df
  }

  id_to_name <- stats::setNames(
    trimws(accepted[[name_col]]),
    as.character(accepted[[id_col]])
  )

  dist_df$canonical_name <- id_to_name[as.character(dist_df[[dist_id_col]])]
  dist_df <- dist_df[!is.na(dist_df$canonical_name), ]

  introduced <- if (!is.null(intro_col)) {
    as.integer(dist_df[[intro_col]])
  } else {
    rep(0L, nrow(dist_df))
  }
  extinct <- if (!is.null(extinct_col)) {
    as.integer(dist_df[[extinct_col]])
  } else {
    rep(0L, nrow(dist_df))
  }

  native_status <- ifelse(
    extinct == 1L, "extinct",
    ifelse(introduced == 1L, "introduced", "native")
  )

  out <- data.frame(
    canonical_name = dist_df$canonical_name,
    tdwg_code      = trimws(dist_df[[area_col]]),
    native_status  = native_status,
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]
  out <- out[!is.na(out$tdwg_code) & nchar(out$tdwg_code) > 0L, ]
  out <- out[!duplicated(paste(out$canonical_name, out$tdwg_code)), ]

  # Carry the rest of the distribution table per (species, region), then the
  # accepted name-table attributes (family, life-form / climate descriptions,
  # authorship, ...) broadcast to each of a species' regions.
  out <- .append_all_cols(
    out, dist_df, dist_df$canonical_name,
    group = "tdwg_code", group_row = trimws(dist_df[[area_col]]),
    used = c(dist_id_col, area_col, intro_col, extinct_col)
  )
  out <- .append_all_cols(
    out, accepted, trimws(accepted[[name_col]]),
    used = c(id_col, name_col, status_col)
  )
  out
}
