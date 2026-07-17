# MEOW ecoregion GeoJSON parsing (the marine analogue of WGSRPD geometry).

meow_geojson_fixture <- function() {
  fc <- list(
    type = "FeatureCollection",
    features = list(
      list(
        type = "Feature",
        properties = list(ECO_CODE = 20164, ECOREGION = "North Sea",
                          PROVINCE = "Northern European Seas", REALM = "Temperate Northern Atlantic"),
        geometry = list(
          type = "Polygon",
          coordinates = list(list(
            list(0, 50), list(10, 50), list(10, 60), list(0, 60), list(0, 50)
          ))
        )
      ),
      list(
        type = "Feature",
        # lower-case property name to exercise case-insensitive matching
        properties = list(eco_code = 20051),
        geometry = list(
          type = "MultiPolygon",
          coordinates = list(
            list(list(list(20, 0), list(30, 0), list(30, 10), list(20, 10), list(20, 0))),
            list(list(list(40, 0), list(50, 0), list(50, 10), list(40, 10), list(40, 0)))
          )
        )
      )
    )
  )
  f <- tempfile(fileext = ".geojson")
  jsonlite::write_json(fc, f, auto_unbox = TRUE, digits = 10)
  f
}

test_that("read_meow returns a long vertex table keyed on ECO_CODE", {
  f <- meow_geojson_fixture()
  on.exit(unlink(f), add = TRUE)

  df <- read_meow(f)

  expect_setequal(names(df), c("code", "geom", "ring", "seq", "lon", "lat"))
  expect_setequal(unique(df$code), c("20164", "20051"))
  # every vertex carries numeric coordinates
  expect_true(is.numeric(df$lon) && is.numeric(df$lat))
  expect_false(anyNA(df$lon) || anyNA(df$lat))
})

test_that("read_meow indexes the two polygons of a MultiPolygon separately", {
  f <- meow_geojson_fixture()
  on.exit(unlink(f), add = TRUE)

  df <- read_meow(f)
  mp <- df[df$code == "20051", ]
  # a MultiPolygon of two squares -> geom 1 and 2, each a single outer ring
  expect_setequal(unique(mp$geom), c(1L, 2L))
  expect_equal(unique(mp$ring), 1L)
})

test_that("read_meow errors on a non-FeatureCollection", {
  f <- tempfile(fileext = ".geojson")
  on.exit(unlink(f), add = TRUE)
  jsonlite::write_json(list(type = "Nonsense"), f, auto_unbox = TRUE)
  expect_error(read_meow(f), "FeatureCollection")
})
