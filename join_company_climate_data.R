library(r2dii.physical.risk)
library(sf)
library(qs)
library(here)
library(dplyr)

source("R/load.R")

## This script will join the company data (as an sf object), to the climate data.
## It uses the sf package, and the sf::join function which will join the two data
## sets together : it will return an object that have the points (the companies coordinates)
## within the polygons (the squared regions created from climate analytics), altogether with
## all the indicators and parameters related to the risk.


## 1. IMPORT - from two functions that will get climate and company datasets

## TODO : So there is a lot of duplicates. From 4909 after distinct() - goes to 2902 obs. it is not a problem
## per say, as long as we then link the lat and long back to their respective companies.

distinct_geo_data <- get_distinct_geo_data()

#this is the geo data from climate analytics, which has only coordinates in it.
all_data_distinct_geo_data <- qs::qread(here("data", "all_data_distinct_geo_data.qs"))

#joining the climate analytics data with the geo data from the smes - this is where I have the message error

asset_scenario_data <- sf::st_join(distinct_geo_data, all_data_distinct_geo_data)

asset_scenario_data <- asset_scenario_data %>%
  filter(!is.na(geometry_id))

asset_geometry <- asset_scenario_data %>%
  select(geometry)

scenario_geometry <- asset_scenario_data %>%
  sf::st_drop_geometry() %>%
  sf::st_as_sf(coords = c("long", "lat")) %>%
  select(geometry)

sf::st_crs(scenario_geometry) <- 4326

asset_scenario_data$dist <- sf::st_distance(
  asset_geometry,
  scenario_geometry,
  by_element = TRUE
)

asset_scenario_data <- asset_scenario_data %>%
  sf::st_drop_geometry() %>%
  select(geometry_id)
#select(asset_id, geometry_id)

all_data <- qread(here("data/all_data.qs"))

asset_scenario_data <- asset_scenario_data %>%
  left_join(
    all_data %>%
      mutate(geometry_id = as.character(geometry_id)),
    by = c("geometry_id")
  )

asset_scenario_data <- asset_scenario_data %>%
  transmute(
    provider,
    hazard,
    scenario,
    period,
    is_reference_period = FALSE,
    model,
    relative_change = risk_level, # TODO: maybe change name of the variable in the beginning already
    risk_level = NA,
    reference = NA,
    absolute_change = NA,
    geometry_id
  )

# save data

qsave(asset_scenario_data, here("data", "asset_scenario_data.qs"))

asset_scenario_data %>%
  r2dii.physical.risk:::save_climate_data(
    path_db_pr_climate_data = path_db_pr_climate_data,
    use_distinct_for_assets_between_two_rasters = TRUE,
    drop_any_NAs = FALSE
  )
