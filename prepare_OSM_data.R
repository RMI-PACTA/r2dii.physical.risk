

osm_data <- osmdata::getbb("Berlin") %>%
  osmdata::opq() %>%
  osmdata::add_osm_feature(
    key = "power",
    value = "plant"
    # value = c("commercial", "industrial", "retail", "warehouse", "office")
  ) %>%
  osmdata::osmdata_sf()

osm_data <- dplyr::bind_rows(
  osm_data$osm_polygons,
  osm_data$osm_points,
  osm_data$osm_lines,
  osm_data$osm_multipolygons,
  osm_data$osm_multilines
)


osm_data <- osm_data %>%
  tidyr::pivot_longer(
    cols = !c("geometry", "osm_id"),
    values_drop_na = TRUE,
    values_to = "names_osm"
    )

osm_match_company_id <- tibble::tribble(
  ~names_osm, ~company_id,
  "Vattenfall Europe AG", 189343,
  "Vattenfall GmbH", 28428,
  "Deutsche Bahn AG", 16665,
  "innogy SE", 33952
)

osm_data <- osm_data %>%
  dplyr::inner_join(osm_match_company_id, by = "names_osm") %>%
  sf::st_as_sf() %>%
  dplyr::mutate(geometry = sf::st_centroid(geometry)) %>%
  sf::st_as_sf()

osm_data <- sf::st_join(osm_data, spData::world %>% dplyr::select(geom, iso_a2))

osm_data$latitude <- sf::st_coordinates(osm_data)[, 2]
osm_data$longitude <- sf::st_coordinates(osm_data)[, 1]

osm_data <- osm_data %>%
  sf::st_drop_geometry()

osm_ald <- osm_data %>%
  dplyr::transmute(
    asset_id = osm_id,
    asset_id_source = "OSM",
    asset_name = NA,
    asset_location = iso_a2,
    latitude,
    longitude,
    source_coordinates = "Exact Location",
    has_geo_data = TRUE,
    sector = "Power",
    technology = NA,
    asset_level_timestamp = "2021Q2",
    economic_unit = "Location",
    year = 2021,
    economic_value = 1
  )

osm_ald_original <- osm_ald
years <- 1
plus_year <- 0

for (e in c(1:years)) {
  osm_new <- osm_ald_original %>%
    dplyr::mutate(year = year + e)

  osm_ald <- rbind.data.frame(
    osm_ald,
    osm_new
  )
}

osm_owner <- osm_data %>%
  dplyr::transmute(
    company_id,
    asset_id = osm_id,
    ownership_share = 1
  )
