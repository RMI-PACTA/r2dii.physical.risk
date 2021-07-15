source("physical_risk_functions.R")

# =================================
# set project paths
# =================================
if(TRUE) {
  # parent directory
  path_db_pr_parent <-                                    fs::path(r2dii.utils::dbox_port_00(), "01_ProcessedData", "08_RiskData")
  ## climate data directory
  path_db_pr_climate_data <-                              fs::path(path_db_pr_parent, "climate_data")
  ### CDF climate data
  path_db_pr_climate_data_CDF <-                          fs::path(path_db_pr_climate_data, "CDF_data")
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
  # Github Path
  path_gh_pr <-                                           fs::path(here::here(), "physical_risk")

  # PACTA project path
  pacta_project <-                                        "mfm_v7"
  path_db_pacta_project <-                                fs::path(r2dii.utils::dbox_port2_10proj(), pacta_project)

  # data store path
  ds_dropbox_path <-                                      fs::path(r2dii.utils::dbox_port_00(),"06_DataStore/DataStore_export_05172021/2020Q4")
}

# create non existing folders
create_db_pr_paths()

# visualise folder structure
show_folder_structure(path_pattern = "path_")
fs::dir_tree(path_db_pr_parent, type = "directory")

# =================================
# load financial data
# =================================

asset_level_owners <- load_asset_level_owners(ds_dropbox_path = ds_dropbox_path)
company_ownership_tree <- load_company_ownership_tree(ds_dropbox_path = ds_dropbox_path)

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
      load_data = FALSE,
      parameter = list(
        scenarios = c("RCP85"),
        hazards = c(
          "cold_days_percent_wrt_10th_percentile_of_reference_period",
          #"heavy_precipitation_days_index_per_time_period",
          "number_of_5day_heavy_precipitation_periods_per_time_period",
          "number_of_cdd_periods_with_more_than_5days_per_time_period",
          "warm_spell_periods_per_time_period"
          #"dry_days_index_per_time_period"
        ),
        models = c("MIROC5", "GFDL-ESM2M"),
        periods = c(
          "1991-2020",
          "2021-2050",
          "2051-2080",
          "2071-2100"
        )
      )
    ),

    # climate analytics data
    list(
      data_path = fs::path(path_db_pr_climate_data, "ClimateAnalytics"),
      run_prepare_script_before_loading = FALSE,
      prepare_script_path = "prepare_climate_analytics_data.R",
      load_data = TRUE,
      parameter = list(
        scenarios = c("rcp45","rcp26"),
        hazards = c(
          "tasAdjust"
        ),
        models = c("GFDL-ESM2M,HadGEM2-ES,IPSL-CM5A-LR,MIROC5"),
        periods = c(
          "2030",
          "2050",
          "2100"
        )
      )
    )
  )
)

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
  dplyr::filter(investor_name == "BlackRock") %>%
  dplyr::filter(portfolio_name %in% c("IE00BF4RFH31", "IE00B4L5Y983"))

cb_portfolio <- vroom::vroom(
  fs::path(
    path_db_pacta_project,
    "30_Processed_Inputs",
    base::paste0(pacta_project, "_bonds_portfolio"),
    ext = "csv"
  )
) %>%
  dplyr::filter(investor_name == "BlackRock") %>%
  dplyr::filter(portfolio_name %in% c("IE00BF4RFH31", "IE00B4L5Y983"))

eq_portfolio <- vroom::vroom(
  fs::path(
    path_db_pacta_project,
    "30_Processed_Inputs",
    base::paste0(pacta_project, "_equity_portfolio"),
    ext = "csv"
  )
) %>%
  dplyr::filter(investor_name == "BlackRock") %>%
  dplyr::filter(portfolio_name %in% c("IE00BF4RFH31", "IE00B4L5Y983")) %>%
  dplyr::filter(financial_sector != "Other") %>%
  dplyr::slice_max(value_usd, n = 50)

total_portfolio <- total_portfolio %>%
  dplyr::group_by(portfolio_name) %>%
  dplyr::mutate(total_market_value = sum(value_usd, na.rm = T)) %>%
  dplyr::mutate(holding_share = value_usd/total_market_value)


results <- readr::read_rds(
  fs::path(
    path_db_pacta_project,
    "40_Results",
    "Blackrock",
    "Equity_results_portfolio",
    ext = "rda"
  )
) %>%
  dplyr::filter(investor_name == "BlackRock") %>%
  dplyr::filter(portfolio_name %in% c("IE00BF4RFH31", "IE00B4L5Y983"))


# =================================
# QA
# =================================

company_ownership_tree <- company_ownership_tree %>%
  semi_join(total_portfolio, by = c("target_company_id" = "company_id"))


asset_level_owners <- asset_level_owners %>%
  semi_join(company_ownership_tree, by = "company_id")

ald <- ald %>%
  semi_join(asset_level_owners, by = "asset_id") %>%
  filter(between(year, 2020, 2025))

climate_data <- climate_data %>%
  semi_join(asset_level_owners, by = "asset_id")


ald_test <- ald %>%
  #left_join(climate_data %>% select(scenario, hazard, model, period, risk_level, absolute_change, relative_change, asset_id), by = "asset_id") %>%
  mutate(risk_level = if_else(is.na(risk_level), 0, round(risk_level, 0)))


ald_test <- ald_test %>%
  left_join(asset_level_owners, by = "asset_id") %>%
  mutate(direct_owned_economic_value = economic_value*(ownership_share/100))


ald_test <- ald_test %>%
  left_join(company_ownership_tree %>% filter(ownership_level >= 0), by = "company_id")

ald_test <- ald_test %>%
  filter(year == 2020) %>%
  mutate(linking_stake = if_else(is.na(linking_stake), 100, linking_stake)) %>%
  mutate(final_owned_economic_value = linking_stake/100*direct_owned_economic_value)

test <- ald_test %>%
  group_by(target_company_id, year, sector) %>%
  summarise(final_owned_economic_value = sum(final_owned_economic_value, na.rm = T))




masterdata <- vroom::vroom(
  fs::path(
    ds_dropbox_path,
    "masterdata_ownership",
    ext = "csv"
  )
) %>%
  group_by(company_id, sector) %>%
  summarise(production = sum(`_2020`, na.rm = T))

test <- test %>%
  left_join(masterdata %>% transmute(sector, target_company_id = company_id, production)) %>%
  mutate(diff = round(final_owned_economic_value,0) - round(production,0)) %>%
  mutate(diff_perc = if_else(final_owned_economic_value == 0, 1, diff/final_owned_economic_value))

test %>%
  ggplot2::ggplot() +
  ggplot2::geom_histogram(ggplot2::aes(x = diff_perc), bins = 100)

