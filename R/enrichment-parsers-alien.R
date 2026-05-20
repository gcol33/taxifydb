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


#' Parse conservation status from GBIF species search API
#'
#' Pulls species lists by IUCN threat category from the GBIF API and combines
#' them. Splits large categories by rank to bypass the GBIF 9999-offset cap.
#'
#' @param dummy_path Character. Ignored — the API is queried directly. Kept
#'   for interface symmetry with file-based parsers.
#' @return data.frame with canonical_name + conservation_status.
#' @export
parse_conservation_status <- function(dummy_path) {
  iucn_categories <- c(
    "LEAST_CONCERN"          = "LC",
    "NEAR_THREATENED"        = "NT",
    "VULNERABLE"             = "VU",
    "ENDANGERED"             = "EN",
    "CRITICALLY_ENDANGERED"  = "CR",
    "EXTINCT_IN_THE_WILD"    = "EW",
    "EXTINCT"                = "EX",
    "DATA_DEFICIENT"         = "DD"
  )

  base_url <- "https://api.gbif.org/v1/species/search"
  all_data <- list()

  for (category in names(iucn_categories)) {
    abbrev <- iucn_categories[[category]]
    message(sprintf("  Fetching %s (%s)...", category, abbrev))

    results <- download_gbif_api_pages(
      base_url,
      params = list(threat = category),
      limit = 1000L,
      max_pages = 100L
    )

    if (nrow(results) == 0L) next

    names_vec <- results$canonicalName
    if (is.null(names_vec)) {
      names_vec <- sub("\\s+[A-Z].*$", "", results$scientificName)
    }

    rows <- data.frame(
      canonical_name      = names_vec,
      conservation_status = abbrev,
      stringsAsFactors = FALSE
    )

    if (!is.null(results) && nrow(results) >= 9000L) {
      for (rank in c("SPECIES", "SUBSPECIES", "VARIETY")) {
        extra <- download_gbif_api_pages(
          base_url,
          params = list(threat = category, rank = rank),
          limit = 1000L,
          max_pages = 100L
        )
        if (nrow(extra) > 0L) {
          extra_names <- extra$canonicalName
          if (is.null(extra_names)) {
            extra_names <- sub("\\s+[A-Z].*$", "", extra$scientificName)
          }
          rows <- rbind(rows, data.frame(
            canonical_name      = extra_names,
            conservation_status = abbrev,
            stringsAsFactors = FALSE
          ))
        }
      }
    }

    all_data[[category]] <- rows
    message(sprintf("    %s species", format(nrow(rows), big.mark = ",")))
  }

  out <- do.call(function(...) rbind(..., make.row.names = FALSE), all_data)

  out <- out[!is.na(out$canonical_name) & nchar(out$canonical_name) > 0L, ]

  severity <- c("EX" = 1L, "EW" = 2L, "CR" = 3L, "EN" = 4L, "VU" = 5L,
                "NT" = 6L, "LC" = 7L, "DD" = 8L)
  out$sev <- severity[out$conservation_status]
  out <- out[order(out$canonical_name, out$sev), ]
  out <- out[!duplicated(out$canonical_name), ]
  out$sev <- NULL

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
