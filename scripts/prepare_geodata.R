library(dplyr)
library(giscoR)
library(lwgeom)
library(sf)

out_dir <- file.path("www", "geodata")
cache_dir <- file.path("data", "geodata_cache")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

target_ids <- c("NL", "BE", "CH")
europe_ids <- c(
  "AL", "AD", "AT", "BE", "BA", "BG", "HR", "CY", "CZ", "DK", "EE", "FI",
  "FR", "DE", "EL", "HU", "IS", "IE", "IT", "XK", "LV", "LI", "LT", "LU",
  "MT", "MD", "ME", "NL", "MK", "NO", "PL", "PT", "RO", "SM", "RS", "SK",
  "SI", "ES", "SE", "CH", "UA", "UK", "VA"
)

countries <- gisco_get_countries(
  year = "2024",
  epsg = "4326",
  resolution = "01",
  cache = TRUE,
  cache_dir = cache_dir,
  verbose = TRUE
) |>
  st_make_valid()

countries <- countries |>
  filter(CNTR_ID %in% europe_ids)

countries <- countries |>
  st_cast("POLYGON", warn = FALSE)

country_points <- st_coordinates(st_point_on_surface(countries))
countries <- countries[
  country_points[, "X"] >= -25 & country_points[, "X"] <= 45 &
    country_points[, "Y"] >= 34 & country_points[, "Y"] <= 72,
] |>
  group_by(CNTR_ID, NAME_ENGL) |>
  summarise(geometry = st_union(geometry), .groups = "drop") |>
  st_as_sf()

europe_outline <- countries |>
  summarise(name = "Europe outline", geometry = st_union(geometry)) |>
  st_as_sf() |>
  st_collection_extract("POLYGON") |>
  st_cast("MULTIPOLYGON")

target_countries <- countries |>
  filter(CNTR_ID %in% target_ids) |>
  transmute(
    country_id = CNTR_ID,
    country = NAME_ENGL,
    geometry
  )

belgium_nuts1 <- gisco_get_nuts(
  year = "2024",
  epsg = "4326",
  resolution = "01",
  nuts_level = "1",
  country = "BE",
  cache = TRUE,
  cache_dir = cache_dir,
  verbose = TRUE
) |>
  st_make_valid()

flanders <- belgium_nuts1 |> filter(NUTS_ID == "BE2")
wallonia <- belgium_nuts1 |> filter(NUTS_ID == "BE3")

target_regions <- bind_rows(
  target_countries |>
    filter(country_id %in% c("NL", "CH")) |>
    transmute(
      area_id = if_else(country_id == "NL", "netherlands", "switzerland"),
      area_label = country,
      area_type = "country",
      geometry
    ),
  flanders |>
    transmute(
      area_id = "flanders",
      area_label = "Flanders",
      area_type = "belgian_region",
      geometry
    ),
  wallonia |>
    transmute(
      area_id = "wallonia",
      area_label = "Wallonia",
      area_type = "belgian_region",
      geometry
    )
) |>
  st_as_sf() |>
  st_make_valid()

belgium_language_border_geometry <- st_intersection(
  st_boundary(st_union(flanders)),
  st_boundary(st_union(wallonia))
)

belgium_language_border <- st_sf(
  name = "Flanders-Wallonia border",
  geometry = st_collection_extract(belgium_language_border_geometry, "LINESTRING"),
  crs = st_crs(belgium_nuts1)
)

write_geojson <- function(x, path) {
  if (file.exists(path)) file.remove(path)
  st_write(x, path, driver = "GeoJSON", quiet = TRUE)
}

write_geojson(europe_outline, file.path(out_dir, "europe_outline.geojson"))
write_geojson(target_countries, file.path(out_dir, "target_countries.geojson"))
write_geojson(target_regions, file.path(out_dir, "target_regions.geojson"))
write_geojson(belgium_language_border, file.path(out_dir, "belgium_language_border.geojson"))

cat("Wrote geodata to", out_dir, "\n")
