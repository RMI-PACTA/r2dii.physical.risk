source("physical_risk_functions.R")

# =================================
# set paths # naming convention (path_*, followed by location (e.g. db = dropbox / gh = github), followed by individual folder structures)
# =================================

# ================
# set input paths
# ===============

# parent directory
path_db_pr_parent <-                                    fs::path(r2dii.utils::dbox_port_00(), "01_ProcessedData", "08_RiskData")
## climate data directory
path_db_pr_climate_data <-                              fs::path(path_db_pr_parent, "climate_data")
### CDF climate data
path_db_pr_climate_data_CDF <-                          fs::path(path_db_pr_climate_data, "CDF")
path_db_pr_climate_data_CDF_raw <-                      fs::path(r2dii.utils::dbox_port_00(), "00_RawData", "15_Risk", "Climate Data Factory")
path_db_pr_climate_data_CDF_raw_geotiff <-              fs::path(path_db_pr_climate_data_CDF_raw, "TCFD_Climate_Data-GeoTiff", "GeoTIFF")
path_db_pr_climate_data_CDF_raw_geotiff_indices <-      fs::path(path_db_pr_climate_data_CDF_raw_geotiff, "Indices")
path_db_pr_climate_data_CDF_raw_geotiff_variables <-    fs::path(path_db_pr_climate_data_CDF_raw_geotiff, "Variables")
### WRI climate data
path_db_pr_climate_data_WRI <-                          fs::path(path_db_pr_climate_data, "WRI_data")
## ALD directory
path_db_pr_ald <-                                       fs::path(path_db_pr_parent, "asset_level_data")
### full ALD data
path_db_pr_ald_prepared <-                              fs::path(path_db_pr_ald, "prepared_ald")
### OSM data
path_db_pr_ald_distinct_geo_data <-                     fs::path(path_db_pr_ald, "distinct_geo_data")
# Analysis Inputs Path
path_db_analysis_inputs <-                              fs::path(r2dii.utils::dbox_port_00(),"07_AnalysisInputs", "2019Q4_05172021_2021")
# data store path
path_db_datastore_export <-                             fs::path(r2dii.utils::dbox_port_00(),"06_DataStore", "DataStore_export_05172021", "2020Q4")
# Github Path
path_gh_pr <-                                           fs::path(here::here(), "physical_risk")

# ===============
# set project paths
# ===============

# PACTA project path
pacta_project <-                                        "mfm_v7"
path_db_pacta_project <-                                fs::path(r2dii.utils::dbox_port2_10proj(), pacta_project)

# ===============
# set output paths
# ===============

# PACTA project output path
path_db_pacta_project_pr_output <-                      fs::path(path_db_pacta_project, "06_Physical_Risk")
path_db_pacta_project_pr_output_equity <-               fs::path(path_db_pacta_project_pr_output, "Equity")
path_db_pacta_project_pr_output_bonds <-                fs::path(path_db_pacta_project_pr_output, "Bonds")

# create PACTA project output path
create_db_pr_paths(
  paths = c(
    path_db_pacta_project_pr_output,
    path_db_pacta_project_pr_output_equity,
    path_db_pacta_project_pr_output_bonds
  )
)


# visualise folder structure
show_folder_structure(path_pattern = "path_")
#fs::dir_tree(path_db_pr_parent, type = "directory") #to many entries to be Understandable

# =================================
# load financial data
# =================================

# company_id_cb_ticker
company_id_cb_ticker <- load_company_id_cb_ticker(path = path_db_datastore_export)

# asset_level_owners
asset_level_owners <- load_asset_level_owners(path = path_db_datastore_export)

asset_level_owners <- asset_level_owners %>%
  left_join(company_id_cb_ticker, by = "company_id")

# company_ownership_tree
company_ownership_tree <- load_company_ownership_tree(path = path_db_datastore_export)

company_ownership_tree <- company_ownership_tree %>%
  rename(
    subsidiary_id = company_id,
    company_id = target_company_id
  )

company_ownership_tree <- company_ownership_tree %>%
  left_join(company_id_cb_ticker, by = c("company_id"))

# =================================
# load ALD (all files + preparation script should follow the same name convention: files: XXX_data.csv; scripts: prepare_XXX_data.R)
# =================================

ald <- load_ald_data(
  relevant_ald = list(

    list(
      data_path = fs::path(path_db_pr_ald_prepared, "AR_data", ext = "csv"),
      run_prepare_script_before_loading = FALSE,
      prepare_script_path = "physical_risk/prepare_AR_data.R",
      load_data = TRUE
    ),

    # not working, but just as an example how this could work
    list(
      data_path = fs::path(path_db_pr_ald_prepared, "OSM_data", ext = "csv"),
      run_prepare_script_before_loading = FALSE,
      prepare_script_path = "physical_risk/prepare_OSM_data.R",
      load_data = FALSE
    )
  )
)

ald <- ald %>%
  filter(between(year, 2020, 2020))

# =================================
# load climate data
# =================================

climate_data <- load_climate_data(
  relevant_climate_data = list(

    # cdf data
    list(
      data_path = fs::path(path_db_pr_climate_data, "CDF"),
      run_prepare_script_before_loading = FALSE,
      prepare_script_path = "prepare_CDF_data.R",
      load_data = TRUE,
      parameter = list(
        scenarios = c("RCP85"),
        hazards = c(
          "cold_days_percent_wrt_10th_percentile_of_reference_period",
          #"heavy_precipitation_days_index_per_time_period",
          #"number_of_5day_heavy_precipitation_periods_per_time_period",
          #"number_of_cdd_periods_with_more_than_5days_per_time_period",
          "warm_spell_periods_per_time_period"
          #"dry_days_index_per_time_period"
        ),
        models = c("MIROC5", "GFDL-ESM2M"),
        periods = c(
          "1991-2020",
          "2021-2050"
          #"2051-2080",
          #"2071-2100"
        )
      )
    ),

    # climate analytics data
    list(
      data_path = fs::path(path_db_pr_climate_data, "ClimateAnalytics"),
      run_prepare_script_before_loading = FALSE,
      prepare_script_path = "prepare_climate_analytics_data.R",
      load_data = F,
      parameter = list(
        scenarios = "rcp85",
        hazards = NULL,
        models = NULL,
        periods = c(
          "2030",
          "2050"
          #"2100"
        )
      )
    )
  )
)

climate_data <- climate_data %>%
  mutate(risk_level = if_else(is.na(risk_level), 0, round(risk_level, 0))) # questionable

climate_data <- climate_data %>% # tbd
  mutate(
    relative_change = case_when(
      relative_change > 2 ~ 2,
      relative_change < -2 ~ -2,
      TRUE ~ relative_change
    )
  )


climate_data <- climate_data %>%
  select(provider, scenario, hazard, model, period, is_reference_period ,risk_level, absolute_change, relative_change, asset_id)

# =================================
# load portfolio
# =================================

total_portfolio <- vroom::vroom(
  fs::path(
    path_db_pacta_project,
    "30_Processed_Inputs",
    base::paste0(pacta_project, "_total_portfolio"),
    ext = "csv"
  )
) %>%
  dplyr::filter(portfolio_name %in% c("IE00BF4RFH31", "IE00B4L5Y983"))


total_portfolio <- total_portfolio %>%
  group_by(portfolio_name) %>%
  mutate(portfolio_value = sum(value_usd, na.rm = TRUE)) %>%
  group_by(portfolio_name, security_mapped_sector) %>%
  mutate(
    portfolio_sector_value = sum(value_usd, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  mutate(
    portfolio_sector_share = portfolio_sector_value / portfolio_value,
    port_weight = value_usd / portfolio_value,
    ownership_weight = number_of_shares / current_shares_outstanding_all_classes
  )

total_portfolio <- total_portfolio %>%
  group_by(portfolio_name, asset_type) %>%
  mutate(portfolio_asset_type_value = sum(value_usd, na.rm = TRUE)) %>%
  group_by(portfolio_name, asset_type, security_mapped_sector) %>%
  mutate(
    portfolio_asset_type_sector_value = sum(value_usd, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  mutate(
    portfolio_asset_type_sector_share = portfolio_asset_type_sector_value / portfolio_asset_type_value,
    asset_type_port_weight = value_usd / portfolio_asset_type_value
  )

total_portfolio <- total_portfolio %>%
  select(
    portfolio_name,
    company_name,
    company_id,
    isin,
    holding_id,
    asset_type,
    security_mapped_sector,
    value_usd,
    port_weight,
    asset_type_port_weight,
    ownership_weight,
    portfolio_sector_share,
    portfolio_asset_type_sector_share
  )


cb_portfolio <- total_portfolio %>%
  filter(asset_type == "Bonds")

eq_portfolio <- total_portfolio %>%
  filter(asset_type == "Equity")


# =================================
# QA
# =================================

check_roll_up <- check_roll_up(choose_year = 2020) # check is for equity
