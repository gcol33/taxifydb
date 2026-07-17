# MEOW (Marine Ecoregions of the World) ecoregion boundaries.
#
# Reference geometry (not a taxonomic backbone): the marine analogue of the
# WGSRPD botanical scheme. taxify uses it to map coordinates to a MEOW
# ecoregion code for the marine `region=`/`coords=` constraint, the same way it
# uses wgsrpd.vtr for terrestrial plant ranges. Stored as a long-format vertex
# table so the runtime never fetches the GeoJSON live.
#
# MEOW (Spalding et al. 2007) nests 232 ecoregions into 62 provinces and 12
# realms; the ecoregion is the finest grain and its ECO_CODE is the join key
# both here and in the marine_distribution enrichment. The GeoJSON is a frozen
# snapshot released as a taxifydb asset (the Marine Regions / TNC download is a
# form-gated shapefile with no stable programmatic URL), mirroring how the
# ecoflora / floraweb scrape snapshots are frozen.

.meow_url <- paste0(
  "https://github.com/gcol33/taxifydb/releases/download/",
  "marine-snapshots-2026.07/meow_ecos.geojson"
)


#' Download the MEOW ecoregion GeoJSON snapshot
#'
#' @param dest Character. Output path for the downloaded GeoJSON.
#' @param verbose Logical.
#' @return The path to the downloaded GeoJSON (invisibly).
#' @export
download_meow <- function(dest = tempfile(fileext = ".geojson"),
                          verbose = TRUE) {
  h <- curl::new_handle()
  curl::handle_setopt(h, followlocation = TRUE, maxredirs = 10L)
  curl::handle_setheaders(h, "User-Agent" = "R taxifydb")
  curl::curl_download(.meow_url, dest, handle = h, quiet = !verbose)
  invisible(dest)
}


#' Parse the MEOW ecoregion GeoJSON into a long-format vertex table
#'
#' One row per polygon vertex. `code` is the MEOW ecoregion code (ECO_CODE),
#' `geom` indexes the polygon within a feature (a MultiPolygon has several),
#' `ring` indexes the ring within a polygon (`1` is the outer boundary, `2+`
#' are holes), and `seq` is the vertex order within a ring. This captures the
#' full polygon topology so the runtime can rebuild native, terra, or sf
#' geometry from the same source, exactly like [read_wgsrpd()].
#'
#' The ecoregion code property is matched case-insensitively, preferring
#' `ECO_CODE`; falling back to `ECO_CODE_X`, then any `eco*code` property.
#'
#' @param path Character. Path to a MEOW ecoregion GeoJSON file.
#' @return A data.frame with columns `code`, `geom`, `ring`, `seq`, `lon`,
#'   `lat`, sorted by `code`, `geom`, `ring`, `seq`.
#' @export
read_meow <- function(path) {
  gj <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (is.null(gj$features)) {
    stop("Not a MEOW FeatureCollection: no features found.", call. = FALSE)
  }

  pick_code_prop <- function(props) {
    nm <- names(props)
    exact <- nm[toupper(nm) == "ECO_CODE"]
    if (length(exact) > 0L) return(exact[1L])
    alt <- nm[toupper(nm) == "ECO_CODE_X"]
    if (length(alt) > 0L) return(alt[1L])
    fuzzy <- nm[grepl("^eco.*code$", nm, ignore.case = TRUE)]
    if (length(fuzzy) > 0L) return(fuzzy[1L])
    NULL
  }

  chunks <- vector("list", 0L)
  ci <- 0L
  ring_to_df <- function(code, gi, ri, ring) {
    n <- length(ring)
    lon <- vapply(ring, function(pt) as.numeric(pt[[1L]]), numeric(1L))
    lat <- vapply(ring, function(pt) as.numeric(pt[[2L]]), numeric(1L))
    data.frame(code = code, geom = gi, ring = ri, seq = seq_len(n),
               lon = lon, lat = lat, stringsAsFactors = FALSE)
  }

  for (ft in gj$features) {
    props <- ft$properties
    geom <- ft$geometry
    if (is.null(props) || is.null(geom) || is.null(geom$type)) next
    code_prop <- pick_code_prop(props)
    if (is.null(code_prop)) next
    code <- props[[code_prop]]
    if (is.null(code)) next
    code <- as.character(code)
    polys <- switch(geom$type,
      Polygon      = list(geom$coordinates),
      MultiPolygon = geom$coordinates,
      next
    )
    for (gi in seq_along(polys)) {
      poly <- polys[[gi]]
      for (ri in seq_along(poly)) {
        ci <- ci + 1L
        chunks[[ci]] <- ring_to_df(code, gi, ri, poly[[ri]])
      }
    }
  }
  if (ci == 0L) {
    stop("No polygon geometry parsed from MEOW GeoJSON.", call. = FALSE)
  }

  df <- do.call(rbind, chunks)
  df <- df[order(df$code, df$geom, df$ring, df$seq), ]
  rownames(df) <- NULL
  df
}


#' Build the MEOW ecoregion boundary `.vtr`
#'
#' Downloads the MEOW ecoregion GeoJSON snapshot (or reads a local copy),
#' parses it to a long-format vertex table, and writes `meow.vtr` with an index
#' on `code` and a metadata sidecar. taxify downloads this pre-built `.vtr` and
#' rebuilds the boundary geometry offline, so the runtime never fetches the
#' GeoJSON. The marine analogue of [build_wgsrpd()].
#'
#' @param output_dir Character. Output directory. Default: `output/meow`.
#' @param version Character or NULL. Version string. Defaults to the current
#'   `YYYY.MM`.
#' @param source_path Character or NULL. A local GeoJSON to parse instead of
#'   downloading (for offline builds and testing).
#' @param verbose Logical.
#' @return Path to the built `.vtr` (invisibly).
#' @export
build_meow <- function(output_dir = NULL, version = NULL,
                       source_path = NULL, verbose = TRUE) {
  version <- version %||% format(Sys.Date(), "%Y.%m")
  output_dir <- output_dir %||% file.path("output", "meow")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  gj_path <- source_path %||% download_meow(verbose = verbose)
  df <- read_meow(gj_path)

  vtr_path <- file.path(output_dir, "meow.vtr")
  vectra::write_vtr(df, vtr_path, batch_size = 100000L)
  vectra::create_index(vtr_path, "code")

  meta_path <- paste0(tools::file_path_sans_ext(vtr_path), ".meta")
  writeLines(c(
    "backend=meow",
    paste0("version=", version),
    paste0("download_date=", format(Sys.time(), "%Y-%m-%d")),
    paste0("download_timestamp=", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
    paste0("url=", .meow_url),
    paste0("nrow=", nrow(df))
  ), meta_path)

  if (verbose) {
    message(sprintf(
      "[meow] Built %s: %d vertices across %d ecoregions, %.1f MB",
      basename(vtr_path), nrow(df), length(unique(df$code)),
      file.size(vtr_path) / 1048576
    ))
  }
  invisible(vtr_path)
}
