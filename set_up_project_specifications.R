library(dplyr)
library(ggplot2)
library(sf)
library(here)
library(r2dii.physical.risk)

# =================================
# set paths: naming convention follows: (path_*, followed by location
# (e.g. db = dropbox/ gh = github), followed by individual folder structures)
# =================================

# ================
# set input paths
# ===============

# parent directory
path_db_pr_parent <- fs::path(
  r2dii.utils::dbox_port_00(),
  "01_ProcessedData", "08_RiskData"
  )

# raw climate risk data
path_db_pr_climate_data_raw <- fs::path(
  r2dii.utils::dbox_port_00(),
  "00_RawData",
  "15_Risk"
  )

#climate data directory
path_db_pr_climate_data <- fs::path(
  path_db_pr_parent,
  "climate_data"
  )

#for PASTAX - mask for sourcing set up spec
# path_db_pr_climate_data <- fs::path(
#   path_db_pr_parent,
#   "climate_data_sme"
# )

# CDF climate data
path_db_pr_climate_data_CDF <- fs::path(path_db_pr_climate_data, "CDF")

path_db_pr_climate_data_CDF_raw <- fs::path(
  path_db_pr_climate_data_raw,
  "Climate Data Factory"
  )

path_db_pr_climate_data_CDF_raw_geotiff <- fs::path(
  path_db_pr_climate_data_CDF_raw,
  "TCFD_Climate_Data-GeoTiff",
  "GeoTIFF"
  )

path_db_pr_climate_data_CDF_raw_geotiff_indices <- fs::path(
  path_db_pr_climate_data_CDF_raw_geotiff,
  "Indices"
  )

path_db_pr_climate_data_CDF_raw_geotiff_variables <- fs::path(
  path_db_pr_climate_data_CDF_raw_geotiff,
  "Variables"
  )

# raw climate analytics data
path_db_pr_climate_data_raw <- fs::path(
  path_db_pr_climate_data_raw,
  "ClimateAnalytics"
  )

# ALD directory
path_db_pr_ald <- fs::path(path_db_pr_parent, "asset_level_data")

# full ALD data
path_db_pr_ald_prepared <- fs::path(
  path_db_pr_ald,
  "prepared_ald"
  )

# OSM data
# path_db_pr_ald_distinct_geo_data <- fs::path(
#   path_db_pr_ald,
#   "distinct_geo_data"
#   )

path_db_pr_ald_distinct_geo_data <- fs::path(
  path_db_pr_ald,
  "company_distinct_geo_data"
  )

# data store path

path_db_datastore_export <- fs::path(
  r2dii.utils::dbox_port_00(),
  "06_DataStore",
  "DataStore_export_05172021",
  "2020Q4"
  )

# ===============
# set PACTA project paths
# ===============

# PACTA project path
pacta_project <- "IDB_Chile_Linda"

path_db_pacta_project <- fs::path(
  r2dii.utils::dbox_port2_10proj(),
  pacta_project
  )

# ===============
# set output paths
# ===============

# PACTA project output path
path_db_pacta_project_pr_output <- fs::path(
  path_db_pacta_project,
  "60_Physical_Risk"
  )

# create PACTA project output path
r2dii.physical.risk:::create_db_pr_paths(
  paths = c(path_db_pacta_project_pr_output)
)

# visualize folder structure
r2dii.physical.risk:::show_folder_structure(path_pattern = "path_")

# =================================
# load financial data
# =================================

# company_id_cb_ticker
company_id_cb_ticker <- r2dii.physical.risk:::load_company_id_cb_ticker(
  path = path_db_datastore_export
  )

# asset_level_owners
asset_level_owners <- r2dii.physical.risk:::load_asset_level_owners(
  path = path_db_datastore_export
  )

# company_ownership_tree
company_ownership_tree <- r2dii.physical.risk:::load_company_ownership_tree(
  path = path_db_datastore_export
  )

# rename to have target_company_id as company_id
company_ownership_tree <- company_ownership_tree %>%
  rename(
    subsidiary_id = company_id,
    company_id = target_company_id
  )

# join cb ticker to company tree for later bonds roll up
company_ownership_tree <- company_ownership_tree %>%
  left_join(company_id_cb_ticker, by = c("subsidiary_id" = "company_id"))

# =================================
# load ALD (all files + preparation script should follow the same name
# convention: files: XXX_data.csv; scripts: prepare_XXX_data.R)
# =================================

ald <- r2dii.physical.risk:::load_ald_data( # TODO: work with ALD timestamps
  relevant_ald = list(
    list(
      data_path = fs::path(path_db_pr_ald_prepared, "AR_data", ext = "csv"), # where does the prepared ald data lie?
      run_prepare_script_before_loading = FALSE, # do you want to run the corresponding prepare script beforehand?
      prepare_script_path = "prepare_AR_data.R", # where does the prepare script lie?
      load_data = TRUE # do you ultimately want to load the ALD of this source?
    ),

    # not working, but just as an example how this could work
    list(
      data_path = fs::path(path_db_pr_ald_prepared, "OSM_data", ext = "csv"), # where does the prepared AR data lie?
      run_prepare_script_before_loading = FALSE, # do you want to run the corresponding prepare script beforehand?
      prepare_script_path = "prepare_OSM_data.R", # where does the prepare script lie?
      load_data = FALSE # do you ultimately want to load the ALD of this source?
    )
  )
)

# currently we use only one year (5 years is rather confusing + doesnt really help for a 30 year time horizon)
ald <- ald %>%
  filter(between(year, 2020, 2020))

# =================================
# load climate data
# =================================

climate_data <- r2dii.physical.risk:::load_climate_data(
  relevant_climate_data = list(

    # # cdf data
    # list(
    #   data_path = fs::path(path_db_pr_climate_data, "CDF"), # from which data provider do you want to load climate data?
    #   run_prepare_script_before_loading = FALSE, # do you want to prepare it beforehand?
    #   prepare_script_path = "prepare_CDF_data.R", # where does the preparation script lie?
    #   load_data = TRUE, # do you ultimately want to load the climate data of this source?
    #
    #   # parameters can differ for each provider, scenrio, hazard, model and time period
    #   parameter = list(
    #     scenarios = c(
    #       "RCP85"
    #     ),
    #     hazards = c(
    #       # "cold_days_percent_wrt_10th_percentile_of_reference_period",
    #       # "heavy_precipitation_days_index_per_time_period",
    #       # "number_of_5day_heavy_precipitation_periods_per_time_period",
    #       # "number_of_cdd_periods_with_more_than_5days_per_time_period",
    #       "warm_spell_periods_per_time_period"
    #       # "dry_days_index_per_time_period"
    #     ),
    #     models = c(
    #       # "MIROC5",
    #       "GFDL-ESM2M"
    #     ),
    #     periods = c(
    #       # "1991-2020",
    #       "2021-2050"
    #       # "2051-2080"
    #       # "2071-2100"
    #     )
    #   )
    # ),

    # climate analytics data
    list(
      data_path = fs::path(here(), "ClimateAnalytics"), # from which data provider do you want to load climate data?
      run_prepare_script_before_loading = TRUE, # do you want to prepare it beforehand?
      prepare_script_path = "prepare_climate_analytics_data.R", # where does the preparation script lie?
      load_data = TRUE, # do you ultimately want to load the climate data of this source?

      # parameters can differ for each provider, scenario, hazard, model and time period (if NULL, all elements of a parameter are used)
      parameter = list(
        scenarios = c("cat", "ngfs", "rcp"),
        hazards = c(
          # "tasAdjust", # air temperature
          # "tasminAdjust", # daily minimum air temperature
          # "tasmaxAdjust", # daily maximum air temperature
          # "prAdjust", # precipitation
          # "hursAdjust", # relative humidity
          # "prsnAdjust", # snowfall
          # "hussAdjust", # specific humidity
          # "sfcWindAdjust", # wind speed
          "yield_maize_co2",
          "yield_rice_co2",
          "soilmoist",
          "yield_soy_co2",
          "yield_wheat_co2",
          "ec4", # 1-in-100-year expected damage from tropical cyclones
          "ec2", # annual expected damage from river floods
          "ec3", # annual expected damage from tropical cyclones
          "ec1", # labour productivity due to heat stress
          "lec", # land fraction annually exposed to crop failures
          "leh", # land fraction annually exposed to heat waves
          "fldfrc", # land fraction annually exposed to river floods
          "lew", # land fraction annually exposed to wild fires,
          "flddph", # river flood depth
          "maxdis", # maximum daily river discharge
          "mindis", # minimum daily river discharge
          "dis", # river discharge
          "qs" # surface runoff
        ),
        models = NULL,
        periods = c(
          "2030",
          "2050",
          "2100"
        )
      )
    )
  )
)


# select relevant columns from climate data
climate_data <- climate_data %>%
  select(
    provider,
    scenario,
    hazard,
    model,
    period,
    is_reference_period,
    risk_level,
    absolute_change,
    relative_change
    )

qsave(climate_data, here("data", "climate_data_cat.qs"))

# =================================
# load portfolio
# =================================

# load pacta total portfolio
total_portfolio_raw <- readRDS(
  fs::path(
    path_db_pacta_project,
    "30_Processed_Inputs",
    base::paste0("total_portfolio"),
    ext = "rda"
  )
)

total_portfolio <- r2dii.physical.risk:::format_portfolio_data(
  total_portfolio_raw
  )
