source("physical_risk_functions.R")

# load ald
ald <- vroom::vroom(
  fs::path(
    path_db_datastore_export,
    "credit_methodology_asset_production",
    ext = "csv"
  )
)

# kick out unrelevant columns
ald <- ald %>%
  dplyr::select(
    -c(
      # emission_factor,
      # emission_factor_unit,
      is_eu_eligible,
      is_eu_green,
      city_name,
      technology_type
    )
  )

# filter out unrelevant sectors
ald <- ald %>%
  dplyr::filter(!sector %in% c("Aviation", "Shipping"))

# create new column asset_id_source which refers to AR
ald <- ald %>%
  dplyr::mutate(asset_id_source = "AR") %>%
  dplyr::select(asset_id, asset_id_source, tidyr::everything())

# choose final latitude and longitude as well as indicate the source
ald <- ald %>%
  dplyr::mutate(
    source_coordinates = dplyr::case_when(
      !is.na(longitude) & !is.na(longitude) ~ "Exact Location",
      (is.na(longitude) | is.na(latitude)) & (!is.na(city_longitude) & !is.na(city_latitude)) ~ "City Location",
      TRUE ~ "Missing"
    ),
    longitude = dplyr::case_when(
      !is.na(longitude) & !is.na(latitude) ~ longitude,
      (is.na(longitude) | is.na(latitude)) & (!is.na(city_longitude) & !is.na(city_latitude)) ~ city_longitude,
      TRUE ~ as.double(NA)
    ),
    latitude = dplyr::case_when(
      !is.na(longitude) & !is.na(latitude) ~ latitude,
      (is.na(longitude) | is.na(latitude)) & (!is.na(city_longitude) & !is.na(city_latitude)) ~ city_latitude,
      TRUE ~ as.double(NA)
    )
  )

# create column "has_geo_data" based on whether latitude or longitude are available
ald <- ald %>%
  dplyr::mutate(
    has_geo_data = dplyr::case_when(
      is.na(latitude) | is.na(longitude) ~ FALSE,
      TRUE ~ TRUE
    )
  )

# reorder column structure
ald <- ald %>%
  dplyr::select(
    asset_id,
    asset_id_source,
    asset_name,
    asset_location,
    latitude,
    longitude,
    source_coordinates,
    has_geo_data,
    tidyr::everything()
  )

# rename production unit to economic unit
ald <- ald %>%
  dplyr::rename(economic_unit = production_unit)

# kick out unnecessary columns
ald <- ald %>%
  dplyr::select(
    -c(
      city_latitude,
      city_longitude
    )
  )

# create long format
ald <- ald %>%
  tidyr::pivot_longer(
    cols = contains("_20"),
    names_to = "year",
    values_to = "economic_value",
    values_drop_na = TRUE
    )

# create numeric year variable
ald <- ald %>%
  dplyr::mutate(year = as.double(stringr::str_remove(year, "_")))

# save data
vroom::vroom_write(
  ald,
  fs::path(
    path_db_pr_ald_prepared,
    "AR_data",
    ext = "csv"
  ),
  delim = ","
)

# subset distinct ald and prepare as geo data
distinct_ald_data <- ald %>%
  dplyr::distinct(asset_id, .keep_all = T) %>%
  dplyr::filter(has_geo_data == TRUE) %>%
  dplyr::select(asset_id, longitude, latitude)

vroom::vroom_write(
  distinct_ald_data,
  fs::path(
    path_db_pr_ald_distinct_geo_data,
    "AR_distinct_geo_data",
    ext = "csv"
  ),
  delim = ","
)

rm(ald, distinct_ald_data)
