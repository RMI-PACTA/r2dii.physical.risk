format_portfolio_data <- function(data) {

  data <- data %>%
    group_by(portfolio_name) %>%
    mutate(portfolio_value = sum(value_usd, na.rm = TRUE)) %>%
    group_by(portfolio_name, security_mapped_sector) %>%
    mutate(portfolio_sector_value = sum(value_usd, na.rm = TRUE)) %>%
    group_by(portfolio_name, asset_type) %>%
    mutate(portfolio_asset_type_value = sum(value_usd, na.rm = TRUE)) %>%
    group_by(portfolio_name, asset_type, security_mapped_sector) %>%
    mutate(portfolio_asset_type_sector_value = sum(value_usd, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      portfolio_sector_share = portfolio_sector_value / portfolio_value,
      port_weight = value_usd / portfolio_value,
      ownership_weight = number_of_shares / current_shares_outstanding_all_classes,
      portfolio_asset_type_sector_share = portfolio_asset_type_sector_value / portfolio_asset_type_value,
      asset_type_port_weight = value_usd / portfolio_asset_type_value
    ) %>%
    ungroup()

  select(
    data,
    portfolio_name,
    company_name,
    company_id,
    corporate_bond_ticker,
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
}

for_loops_climate_data <- function(data, parent_path, fns) {
  climate_data <- data

  for (provider_sub in stats::na.omit(unique(climate_data$provider))) {
    cat(crayon::white(crayon::bold(paste("Processing", provider_sub, "\n"))))

    climate_data_provider_sub <- climate_data %>%
      dplyr::filter(provider == provider_sub)

    path_db_pr_climate_data_provider <- fs::path(parent_path, provider_sub)

    if (!dir.exists(path_db_pr_climate_data_provider)) {
      fs::dir_create(path_db_pr_climate_data_provider)
      cat(crayon::white(crayon::bold(paste("Just created directory for", provider_sub, "\n"))))
    }

    for (scenario_sub in unique(climate_data_provider_sub$scenario)) {
      cat(crayon::red(crayon::bold(paste("Processing", scenario_sub, "\n"))))

      climate_data_provider_sub_scenario_sub <- climate_data_provider_sub %>%
        dplyr::filter(scenario == scenario_sub)

      path_db_pr_climate_data_provider_scenario <- fs::path(path_db_pr_climate_data_provider, scenario_sub)

      if (!dir.exists(path_db_pr_climate_data_provider_scenario)) {
        fs::dir_create(path_db_pr_climate_data_provider_scenario)
        cat(crayon::red(crayon::bold(paste("Just created directory for", scenario_sub, "\n"))))
      }

      for (hazard_sub in unique(climate_data_provider_sub_scenario_sub$hazard)) {
        cat(crayon::blue(crayon::bold(paste("Processing", hazard_sub, "\n"))))

        climate_data_provider_sub_scenario_sub_hazard_sub <- climate_data_provider_sub_scenario_sub %>%
          dplyr::filter(hazard == hazard_sub)

        path_db_pr_climate_data_provider_scenario_hazards <- fs::path(path_db_pr_climate_data_provider_scenario, hazard_sub)

        if (!dir.exists(path_db_pr_climate_data_provider_scenario_hazards)) {
          fs::dir_create(path_db_pr_climate_data_provider_scenario_hazards)
          cat(crayon::blue(crayon::bold(paste("Just created directory for", hazard_sub, "in", scenario_sub, "\n"))))
        }

        for (model_sub in unique(climate_data_provider_sub_scenario_sub_hazard_sub$model)) {
          cat(crayon::cyan(crayon::bold(paste("Processing", model_sub, "of", hazard_sub, "of", scenario_sub, "\n"))))

          climate_data_provider_sub_scenario_sub_hazard_sub_model_sub <- climate_data_provider_sub_scenario_sub_hazard_sub %>%
            dplyr::filter(model == model_sub)

          path_db_pr_climate_data_provider_scenario_hazards_models <- fs::path(path_db_pr_climate_data_provider_scenario_hazards, model_sub)

          if (!dir.exists(path_db_pr_climate_data_provider_scenario_hazards_models)) {
            fs::dir_create(path_db_pr_climate_data_provider_scenario_hazards_models)
            cat(crayon::cyan(crayon::bold(paste("Just created directory for", model_sub, "in", hazard_sub, "in", scenario_sub, "\n"))))
          }

          for (period_sub in unique(climate_data_provider_sub_scenario_sub_hazard_sub_model_sub$period)) {
            cat(crayon::green(crayon::bold(paste("Processing", period_sub, "of", model_sub, "of", hazard_sub, "of", scenario_sub, "\n"))))

            climate_data_provider_sub_scenario_sub_hazard_sub_model_sub_period_sub <- climate_data_provider_sub_scenario_sub_hazard_sub_model_sub %>%
              dplyr::filter(period == period_sub)

            climate_data_provider_sub_scenario_sub_hazard_sub_model_sub_period_sub %>%
              fns(
                path_db_pr_climate_data_provider_scenario_hazards_models,
                provider_sub,
                scenario_sub,
                hazard_sub,
                model_sub,
                period_sub
                )
          }
        }
      }
    }
  }
}

save_climate_data <- function(climate_data,
                              path_db_pr_climate_data_provider,
                              use_distinct_for_assets_between_two_rasters = TRUE,
                              drop_any_NAs = TRUE) {

  # arrange final column structure and therefore verify that variables exist
  climate_data <- climate_data %>%
    dplyr::select(
      asset_id,
      provider,
      scenario,
      model,
      period,
      is_reference_period,
      hazard,
      geometry_id,
      risk_level,
      reference,
      absolute_change,
      relative_change
    )

  # verify that there are no NAs in the parameter columns. NAs can arise in the risk_level column if there are missing estimates in the data
  climate_data <- climate_data %>%
    dplyr::filter(
      !dplyr::if_any(
        c("asset_id", "geometry_id", "model", "period", "is_reference_period", "scenario", "hazard"),
        is.na
      )
    ) %>%
    assertr::verify(nrow(.) == nrow(climate_data))

  if (use_distinct_for_assets_between_two_rasters == TRUE) {
    # check assets which lie exactly between two rasters and choose the estimates for only one geometry_id by calling distinct # not ideal solution
    climate_data <- climate_data %>%
      dplyr::distinct(asset_id, model, period, hazard, scenario, .keep_all = T) %>%
      show_diff_rows(
        initial_n_rows = nrow(climate_data),
        cause = "because of non-distinct nrows grouped by asset_id, model, period, hazard, scenario"
      )
  }

  if (drop_any_NAs == TRUE) {
    # differences in the amount of NAs in risk_level and reference can happen if risk level is available only in the reference period
    climate_data <- climate_data %>%
      tidyr::drop_na() %>%
      show_diff_rows(
        initial_n_rows = nrow(climate_data),
        cause = "because of NAs in at least one variable"
      )
  }

  climate_data %>%
    for_loops_climate_data(
      parent_path = path_db_pr_climate_data_provider,
      fns = function(data, final_path) {
        data %>%
          readr::write_csv(
            fs::path(
              final_path,
              paste(scenario_sub, hazard_sub, model_sub, period_sub, "ald", sep = "_"),
              ext = "csv"
            )
          )
        Sys.sleep(0.5)
      }
    )
}
