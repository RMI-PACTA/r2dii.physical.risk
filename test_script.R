library(dplyr)
library(tidyr)
library(purrr)
library(sf)
library(r2dii.physical.risk)
library(r2dii.utils)
library(tidygeocoder)

# devtools::install_github("2DegreesInvesting/pastax.data")

# METHODOLOGY Example Workflow

## scraped data from europages

europages <- pastax.data::europages_agriculture_livestock_demo

smes <- europages

## separate with "|" and retrieve the postcode only

smes_v1 <-  smes %>%
  separate(address, into = c("address", "city","extra"), sep="\\|", fill = "right")

#if there are several "|" or un-consistencies
smes_v1$city <- if_else(is.na(smes_v1$extra), smes_v1$city, smes_v1$extra)

smes_vf <- smes_v1 %>%
  separate(city, into = c("city","postcode") ,sep = "\\s", extra = "drop") %>%
  select(-c("city", "extra"))

## change postcodes into coordinates

coordinates <- smes_vf %>%
  tidygeocoder::geocode(
    postalcode = postcode,
    country = company_country,
    method = "osm"
  )

no_coordinates <-  subset(coordinates, is.na(coordinates$lat))

company_data <- coordinates %>%
  dplyr::rename(
    longitude = long,
    latitude = lat
  )

company_data <- company_data %>%
  dplyr::mutate(
    has_geo_data = dplyr::case_when(
      is.na(latitude) | is.na(longitude) ~ FALSE,
      TRUE ~ TRUE
    )
  )

vroom::vroom_write(
  company_data,
  fs::path(
    path_db_pr_ald_prepared,
    "company_level_data",
    ext = "csv"
  ),
  delim = ","
)

## select only coordinates for joining spatially in preparing climate analytics

distinct_company_data <- company_data %>%
  select("latitude","longitude") %>%
  drop_na()

vroom::vroom_write(
  distinct_company_data,
  fs::path(
    path_db_pr_ald_distinct_geo_data,
    "company_distinct_geo_data",
    ext = "csv"
  ),
  delim = ","
)

#then join at prepare_climate_analytics_data.R

## climate data already downloaded in set_up_project_specifications.R

distinct_geo_data <- get_distinct_geo_data()


#joined_climate_company_data <- sf::st_join(distinct_geo_data, all_data_distinct_geo_data)


## joining with climate data
#Q3 :OK what is the asset_id that is found again in the climate data ? Where does it comes from ? - from AR_distinct geodata that links asset_id & coordinates.

company_climate_data <- company_data %>%
  inner_join(climate_data, by = "asset_id") %>%
  select(
    company_name,
    company_country,
    address,
    latitude,
    longitude,
    sector,
    scenario,
    model,
    period,
    hazard,
    relative_change
  )


