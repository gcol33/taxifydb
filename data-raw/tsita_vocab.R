# Freeze the T-SITA thesaurus into inst/extdata/tsita_vocab.csv.
#
# T-SITA (Pey et al. 2014, PLoS ONE, doi:10.1371/journal.pone.0108985, CC BY)
# is the controlled vocabulary BETSI is built on: 71 traits + 24 ecological
# preferences with definitions and stable identifiers. Its own portal
# (t-sita.cesab.org) is dead -- it now 301-redirects to the same lapsed-domain
# squatter that swallowed betsi.cesab.org -- but the thesaurus itself is live and
# maintained as SKOS at the CEFE/CNRS persistent ARK ark:/66666/th558, served by
# the Opentheso instance at opentheso.huma-num.fr. This script pulls the whole
# scheme through Opentheso's public API (no key) and flattens it to one row per
# concept, which taxifydb redistributes as the crosswalk vocabulary for its
# Collembola / soil-fauna enrichments (see gcol33/taxifydb#42).
#
# Regenerate: run from the package root. Refreshes inst/extdata/tsita_vocab.csv.

THESAURUS <- "th558"
API <- sprintf("https://opentheso.huma-num.fr/openapi/v1/thesaurus/%s?lang=en",
               THESAURUS)
OUT <- file.path("inst", "extdata", "tsita_vocab.csv")

tmp <- tempfile(fileext = ".json")
curl::curl_download(API, tmp)
j <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)

P <- list(
  id     = "http://purl.org/dc/terms/identifier",
  pref   = "http://www.w3.org/2004/02/skos/core#prefLabel",
  defn   = "http://www.w3.org/2004/02/skos/core#definition",
  scope  = "http://www.w3.org/2004/02/skos/core#scopeNote",
  broad  = "http://www.w3.org/2004/02/skos/core#broader",
  type   = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Values for a predicate; when `lang` is given, prefer that language's literals.
getv <- function(node, pred, lang = NULL) {
  x <- node[[pred]]
  if (is.null(x)) return(character(0))
  vals <- vapply(x, function(o) as.character(o$value %||% NA), character(1))
  if (!is.null(lang)) {
    langs <- vapply(x, function(o) as.character(o$lang %||% NA), character(1))
    if (any(langs == lang, na.rm = TRUE)) vals <- vals[which(langs == lang)]
  }
  vals
}
first <- function(x) if (length(x)) x[[1L]] else NA_character_

uris <- names(j)
rows <- lapply(uris, function(u) {
  n <- j[[u]]
  types <- getv(n, P$type)
  data.frame(
    uri        = u,
    id         = first(getv(n, P$id)),
    prefLabel  = first(getv(n, P$pref, "en")),
    is_concept = any(grepl("skos/core#Concept$", types)),
    broader    = first(getv(n, P$broad)),
    definition = first(getv(n, P$defn, "en")),
    scopeNote  = first(getv(n, P$scope, "en")),
    stringsAsFactors = FALSE)
})
d <- do.call(rbind, rows)

# Walk broader to the top concept (Trait / Ecological_preference) and record the
# depth below it (1 = a branch, higher = a trait or an attribute value).
label_of <- stats::setNames(d$prefLabel, d$uri)
top_of <- function(u) {
  seen <- character(0); cur <- u
  repeat {
    if (is.na(cur) || cur %in% seen) return(NA_character_)
    seen <- c(seen, cur)
    b <- d$broader[match(cur, d$uri)]
    if (is.na(b)) return(label_of[[cur]] %||% NA_character_)
    cur <- b
  }
}
depth_of <- function(u) {
  k <- 0L; cur <- u
  repeat {
    b <- d$broader[match(cur, d$uri)]
    if (is.na(b)) return(k)
    k <- k + 1L; cur <- b
    if (k > 20L) return(NA_integer_)
  }
}
d$top   <- vapply(d$uri, top_of, character(1))
d$depth <- vapply(d$uri, depth_of, integer(1))

# Opentheso renders a definition as "<text> (<source>)" and writes the literal
# "(null)" when a concept carries no source; strip that trailing marker.
conc <- d[d$is_concept & !is.na(d$prefLabel),
          c("uri", "id", "prefLabel", "top", "depth", "broader",
            "definition", "scopeNote")]
conc$definition <- trimws(sub("\\s*\\(null\\)\\s*$", "", conc$definition))
conc <- conc[order(conc$top, conc$depth, conc$prefLabel), ]

if (anyDuplicated(conc$prefLabel)) {
  stop("T-SITA prefLabels are not unique; the label->uri crosswalk assumes they ",
       "are. Duplicates: ",
       paste(unique(conc$prefLabel[duplicated(conc$prefLabel)]), collapse = ", "))
}

dir.create(dirname(OUT), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(conc, OUT, row.names = FALSE, na = "")
message(sprintf("Wrote %s: %d concepts (%s).", OUT, nrow(conc),
                paste(names(table(conc$top)), table(conc$top),
                      sep = "=", collapse = ", ")))
