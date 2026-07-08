# WGSRPD Level 3 botanical region boundaries.
#
# Reference geometry (not a taxonomic backbone): the World Geographical Scheme
# for Recording Plant Distributions, Level 3. taxify uses it to map coordinates
# to TDWG Level 3 codes for the region constraint. Stored as a long-format
# vertex table so the runtime never fetches the GeoJSON live.

.wgsrpd_url <- paste0(
  "https://raw.githubusercontent.com/tdwg/wgsrpd/master/geojson/level3.geojson"
)


#' Download the WGSRPD Level 3 GeoJSON
#'
#' @param dest Character. Output path for the downloaded GeoJSON.
#' @param verbose Logical.
#' @return The path to the downloaded GeoJSON (invisibly).
#' @export
download_wgsrpd <- function(dest = tempfile(fileext = ".geojson"),
                            verbose = TRUE) {
  h <- curl::new_handle()
  curl::handle_setheaders(h, "User-Agent" = "R taxifydb")
  curl::curl_download(.wgsrpd_url, dest, handle = h, quiet = !verbose)
  invisible(dest)
}


#' Parse the WGSRPD Level 3 GeoJSON into a long-format vertex table
#'
#' One row per polygon vertex. `geom` indexes the polygon within a feature
#' (a MultiPolygon has several), `ring` indexes the ring within a polygon
#' (`1` is the outer boundary, `2+` are holes), and `seq` is the vertex order
#' within a ring. This captures the full polygon topology so the runtime can
#' rebuild native, terra, or sf geometry from the same source.
#'
#' @param path Character. Path to a WGSRPD Level 3 GeoJSON file.
#' @return A data.frame with columns `code`, `geom`, `ring`, `seq`, `lon`,
#'   `lat`, sorted by `code`, `geom`, `ring`, `seq`.
#' @export
read_wgsrpd <- function(path) {
  gj <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (is.null(gj$features)) {
    stop("Not a WGSRPD FeatureCollection: no features found.", call. = FALSE)
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
    code <- ft$properties$LEVEL3_COD
    geom <- ft$geometry
    if (is.null(code) || is.null(geom) || is.null(geom$type)) next
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
    stop("No polygon geometry parsed from WGSRPD GeoJSON.", call. = FALSE)
  }

  df <- do.call(rbind, chunks)
  df <- df[order(df$code, df$geom, df$ring, df$seq), ]
  rownames(df) <- NULL
  df
}


#' Build the WGSRPD Level 3 boundary `.vtr`
#'
#' Downloads the WGSRPD Level 3 GeoJSON (or reads a local copy), parses it to a
#' long-format vertex table, and writes `wgsrpd.vtr` with an index on `code`
#' and a metadata sidecar. taxify downloads this pre-built `.vtr` and rebuilds
#' the boundary geometry offline, so the runtime never fetches the GeoJSON.
#'
#' @param output_dir Character. Output directory. Default: `output/wgsrpd`.
#' @param version Character or NULL. Version string. Defaults to the current
#'   `YYYY.MM`.
#' @param source_path Character or NULL. A local GeoJSON to parse instead of
#'   downloading (for offline builds and testing).
#' @param verbose Logical.
#' @return Path to the built `.vtr` (invisibly).
#' @export
build_wgsrpd <- function(output_dir = NULL, version = NULL,
                         source_path = NULL, verbose = TRUE) {
  version <- version %||% format(Sys.Date(), "%Y.%m")
  output_dir <- output_dir %||% file.path("output", "wgsrpd")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  gj_path <- source_path %||% download_wgsrpd(verbose = verbose)
  df <- read_wgsrpd(gj_path)

  vtr_path <- file.path(output_dir, "wgsrpd.vtr")
  vectra::write_vtr(df, vtr_path, batch_size = 100000L)
  vectra::create_index(vtr_path, "code")

  meta_path <- paste0(tools::file_path_sans_ext(vtr_path), ".meta")
  writeLines(c(
    "backend=wgsrpd",
    paste0("version=", version),
    paste0("download_date=", format(Sys.time(), "%Y-%m-%d")),
    paste0("download_timestamp=", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
    paste0("url=", .wgsrpd_url),
    paste0("nrow=", nrow(df))
  ), meta_path)

  if (verbose) {
    message(sprintf(
      "[wgsrpd] Built %s: %d vertices across %d regions, %.1f MB",
      basename(vtr_path), nrow(df), length(unique(df$code)),
      file.size(vtr_path) / 1048576
    ))
  }
  invisible(vtr_path)
}
