# Parsers that map sub-national region names to ISO codes, plus conservation
# status fetched from GBIF, and WCVP distribution data.


# ---- Alien first records (Seebens et al.) ----

#' Seebens region name to ISO 3166-1 alpha-2 mapping
#'
#' Sub-national regions are mapped to their parent country.
#' Multi-country entries (e.g., "USACanada") are mapped to NA and dropped.
#' @noRd
.seebens_region_map <- c(
  "Afghanistan" = "AF",
  "Aland Islands" = "AX",
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
  "Colombia" = "CO",
  "Comoros" = "KM",
  "Congo, Democratic Republic of the" = "CD",
  "Congo, Republic of" = "CG",
  "Cook Islands" = "CK",
  "Corse" = "FR",
  "Costa Rica" = "CR",
  "Cote D'Ivoire" = "CI",
  "Crete" = "GR",
  "Croatia" = "HR",
  "Crozet Islands Group" = "TF",
  "Cuba" = "CU",
  "Curacao" = "CW",
  "Cyprus" = "CY",
  "Czech Republic" = "CZ",
  "De" = NA_character_,
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
  "Hawaiian Islands" = "US",
  "Heard and Mcdonald Islands" = "HM",
  "Honduras" = "HN",
  "Hong Kong" = "HK",
  "Hungary" = "HU",
  "Iceland" = "IS",
  "India" = "IN",
  "Indonesia" = "ID",
  "Iran, Islamic Republic of" = "IR",
  "Iraq" = "IQ",
  "Ireland" = "IE",
  "Israel" = "IL",
  "Italy" = "IT",
  "Italy and Germany" = NA_character_,
  "Italy, Hungary, Spain" = NA_character_,
  "Izu Islands" = "JP",
  "Jamaica" = "JM",
  "Japan" = "JP",
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
  "Northern Mariana Islands" = "MP",
  "Norway" = "NO",
  "Ogasawara Islands" = "JP",
  "Oman" = "OM",
  "Pakistan" = "PK",
  "Palau" = "PW",
  "Palestine, State of" = "PS",
  "Panama" = "PA",
  "Paraguay" = "PY",
  "Peru" = "PE",
  "Philippines" = "PH",
  "Pitcairn Islands" = "PN",
  "Poland" = "PL",
  "Portugal" = "PT",
  "Puerto Rico" = "PR",
  "Qatar" = "QA",
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
  "Tuvalu" = "TV",
  "Uganda" = "UG",
  "Uk and Netherlands" = NA_character_,
  "Ukraine" = "UA",
  "United Arab Emirates" = "AE",
  "United Kingdom" = "GB",
  "United States" = "US",
  "Uruguay" = "UY",
  "US Minor Outlying Islands" = "UM",
  "USACanada" = NA_character_,
  "Uzbekistan" = "UZ",
  "Vancouver Island" = "CA",
  "Vanuatu" = "VU",
  "Venezuela" = "VE",
  "Vietnam" = "VN",
  "Virgin Islands, US" = "VI",
  "Wallis and Futuna" = "WF",
  "Western Sahara" = "EH",
  "Yemen" = "YE",
  "Zambia" = "ZM",
  "Zanzibar Island" = "TZ",
  "Zimbabwe" = "ZW"
)


#' Parse Seebens et al. Global Alien Species First Record Database
#'
#' Reads the "FirstRecords" sheet from the Seebens Excel file, maps region
#' names to ISO 3166-1 alpha-2 codes, and deduplicates per species x country
#' (keeping the earliest year).
#'
#' @param path Character. Path to the Seebens XLSX file.
#' @return data.frame with canonical_name + country_code + first-record cols.
#' @export
parse_alien_first_records <- function(path) {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop("Package 'openxlsx2' is required to parse the Seebens database.",
         call. = FALSE)
  }

  df <- openxlsx2::read_xlsx(path, sheet = "FirstRecords")

  df$country_code <- .seebens_region_map[df$Region]

  ref_col <- if ("Reference" %in% names(df)) df$Reference else rep(NA_character_, nrow(df))
  src_col <- if ("Source"    %in% names(df)) df$Source    else rep(NA_character_, nrow(df))

  out <- data.frame(
    canonical_name              = trimws(df$TaxonName),
    country_code                = df$country_code,
    alien_first_record          = as.integer(df$FirstRecord),
    alien_first_record_source   = src_col,
    alien_first_record_reference = ref_col,
    stringsAsFactors = FALSE
  )

  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]
  out <- out[!is.na(out$country_code) & nchar(out$country_code) == 2L, ]

  out <- out[order(out$canonical_name, out$country_code,
                   out$alien_first_record, na.last = TRUE), ]
  out <- out[!duplicated(paste(out$canonical_name, out$country_code)), ]

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
parse_conservation_status <- function(dir_path) {
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
  out[!duplicated(paste(out$canonical_name, out$tdwg_code)), ]
}
