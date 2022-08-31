library(r2dii.physical.risk)
library(sf)
library(fs)
library(qs)
library(here)
library(dplyr)
library(ggplot2)

source("R/load.R")

## This script will join the company data (as an sf object), to the climate data.
## It uses the sf package, and the sf::join function which will join the two data
## sets together : it will return an object that have the points (the companies coordinates)
## within the polygons (the squared regions created from climate analytics), altogether with
## all the indicators and parameters related to the risk.

## 1. IMPORT - from two functions that will get climate and company datasets

distinct_geo_data <- get_distinct_geo_data()

qs_files <- dir_ls(here("data", "all_data"), regexp = "[.]qs$")

all_data <- purrr::map_df(qs_files, qs::qread)

qs_files_geo_data <- dir_ls(here("data", "all_data_distinct_geo_data"), regexp = "[.]qs$")

#this is the geo data from climate analytics, which has only coordinates in it.
all_data_distinct_geo_data <- purrr::map_df(qs_files_geo_data, qs::qread)

## FIXME : too big to join
# qs_files <- dir_ls(here("data", "all_data"), regexp = "[.]qs$")
# all_data <- purrr::map_df(qs_files, qs::qread)

# all_data <- qread(here("data", "all_data_PR.qs"))
# all_data_distinct_geo_data <- qs::qread(here("data", "all_data_distinct_geo_data_PR.qs"))

## 2. JOIN - join the Climate Analytics data with our company data set.

### PR - AGRICULTURE for RCPs scenarios

agriculture <- c(
  "yield_rice_co2",
  "soilmoist",
  "yield_soy_co2",
  "yield_wheat_co2"
)

ngfs_scenarios <- c(
  "ngfs_delayed_2_degrees",
  "ngfs_current_policies",
  "ngfs_net_zero"
)

all_data_agriculture <- all_data |>
  filter(hazard %in% agriculture) |>
  filter(scenario %in% rcp_scenarios)

#joining the climate analytics data with the geo data from the smes

asset_scenario_data <- get_asset_scenario_data(distinct_geo_data, all_data_distinct_geo_data, all_data)

# Save the joined data

# qsave(asset_scenario_data, here("data", "asset_scenario_data_economical_damages.qs"))

asset_scenario_data <- qread(here("data", "asset_scenario_data_economical_damages.qs"))

companies <- asset_scenario_data |>
  select(company_name, id) |>
  distinct()

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

get_asset_scenario_data(distinct_geo_data, )

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


## ANNA ECKSCHLAGER
distinct_geo_data <- distinct_geo_data |>
  sample_n(3)

asset_scenario_data <- get_asset_scenario_data(distinct_geo_data, all_data_distinct_geo_data, all_data)


anna_ngfs <- asset_scenario_data |>
  filter(scenario %in% ngfs_scenarios)

company_indicator_output <- anna_ngfs |>
  select(company_name, country.x, company_city, hazard, scenario, period, relative_change) |>
  mutate(
    hazard = case_when(
      hazard == "soilmoist" ~ "soil moisture",
      hazard == "qs" ~ "surface runoff",
      hazard == "mindis" ~ "minimum daily river discharge",
      hazard == "maxdis" ~ "maximum daily river discharge",
      hazard == "ec1" ~ "labour productivity due to heat stress",
      hazard == "ec3" ~ "annual expected damage from tropical cyclones",
      hazard == "dis" ~ "river discharge",
      TRUE ~ as.character(hazard)
    )) |>
  rename(country = country.x)


company_indicator_30 <- company_indicator_output |>
  filter(period == 2030) |>
  filter(scenario == 'ngfs_net_zero')


company_indicator_30  %>%
  ggplot(aes(x = hazard, y = relative_change, fill = relative_change)) +
  geom_col() +
  coord_flip() +
  facet_wrap(vars(scenario, period))
