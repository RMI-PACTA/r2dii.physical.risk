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

distinct_geo_data <- get_distinct_geo_data()

#this is the geo data from climate analytics, which has only coordinates in it.

# qs_files <- dir_ls(here("data", "all_data"), regexp = "[.]qs$")
# all_data <- purrr::map_df(qs_files, qs::qread)

all_data <- qread(here("data", "all_data_PR.qs"))

all_data_distinct_geo_data <- qs::qread(here("data", "all_data_distinct_geo_data_PR.qs"))

#joining the climate analytics data with the geo data from the smes

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
  select(geometry_id, company_name, id)
#select(asset_id, geometry_id)


asset_scenario_data <- asset_scenario_data %>%
  left_join(
    all_data %>%
      mutate(geometry_id = as.character(geometry_id)),
    by = c("geometry_id")
  )

asset_scenario_data <- asset_scenario_data %>%
  transmute(
    company_name,
    id,
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

# qsave(asset_scenario_data, here("data", "asset_scenario_data.qs"))

asset_scenario_data <- qread(here("data", "asset_scenario_data.qs"))

# asset_scenario_data %>%
#   r2dii.physical.risk:::save_climate_data(
#     path_db_pr_climate_data = path_db_pr_climate_data,
#     use_distinct_for_assets_between_two_rasters = TRUE,
#     drop_any_NAs = FALSE
#   )

# PR hazards of interest

## ECONOMIC DAMAGES

economic_damages <- c(
  "ec4", # 1-in-100-year expected damage from tropical cyclones
  "ec2", # annual expected damage from river floods
  "ec3", # annual expected damage from tropical cyclones
  "ec1" # labour productivity due to heat stress
)

all_data_economic_damages <- all_data |>
  filter(hazard %in% economic_damages)

PR_economic_damages <- asset_scenario %>%
  filter(hazard %in% economic_damages)

freshwater <- c(
  "maxdis", # maximum daily river discharge
  "mindis", # minimum daily river discharge
  "dis", # river discharge
  "qs" # surface runoff
)

PR_freshwater <- asset_scenario_data %>%
  filter(hazard %in% freshwater)

agriculture <- c(
  "yield_rice_co2",
  "soilmoist",
  "yield_soy_co2",
  "yield_wheat_co2"
)

PR_agriculture <- asset_scenario_data %>%
  filter(hazard %in% agriculture)


#38490 companies out of 100534 have agriculture hazards information
companies_agriculture <- PR_agriculture |>
  distinct(company_name)


