
for (portfolio in unique(eq_portfolio$portfolio_name)) {

  eq_portfolio_sub <- eq_portfolio %>%
    filter(portfolio_name == portfolio)

  path_db_pacta_project_pr_output_equity_portfolio <- fs::path(path_db_pacta_project_pr_output_equity, portfolio)

  create_db_pr_paths(paths = path_db_pacta_project_pr_output_equity_portfolio)


  # =================================
  # subset relevant data
  # =================================

  # ========
  # subset relevant target companies
  # ========
  company_ownership_tree_sub <- company_ownership_tree %>%
    semi_join(eq_portfolio_sub, by = c("target_company_id" = "company_id"))

  # ========
  # subset relevant asset owners
  # ========
  asset_level_owners_sub <- asset_level_owners %>%
    semi_join(company_ownership_tree_sub, by = "company_id")

  # ========
  # subset relevant ALD
  # ========
  ald_sub <- ald %>%
    semi_join(asset_level_owners_sub, by = "asset_id")

  # ========
  # subset climate data for relevant ALD
  # ========
  climate_data_sub <- climate_data %>%
    semi_join(asset_level_owners_sub, by = "asset_id")




  # =================================
  # merge data
  # =================================

  # ========
  # merge ALD with Climate Data
  # ========
  analysis <- ald_sub %>%
    left_join(climate_data_sub, by = "asset_id")

  # ========
  # merge asset level owners + calculate direct owned economic value
  # ========
  analysis <- analysis %>%
    left_join(asset_level_owners, by = "asset_id")

  analysis <- analysis %>%
    mutate(direct_owned_economic_value = economic_value*(ownership_share/100))

  # ========
  # merge ownership tree
  # ========
  analysis <- analysis %>%
    left_join(company_ownership_tree %>% filter(ownership_level >= 0), by = "company_id")

  # clean linking stake + calculate company_final_owned_economic_value
  analysis <- analysis %>%
    mutate(linking_stake = if_else(is.na(linking_stake), 100, linking_stake)) %>%
    mutate(company_final_owned_economic_value = linking_stake/100*direct_owned_economic_value)

  # ========
  # merge to portfolio
  # ========
  analysis <-  eq_portfolio_sub %>%
    rename(target_company_id = company_id) %>%
    left_join(
      analysis,
      by = "target_company_id"
    )

  # =================================
  # create overview stats
  # =================================

  has_ald <- analysis %>%
    filter(!is.na(asset_id)) %>%
    distinct(portfolio_name, company_name, asset_id) %>%
    group_by(portfolio_name, company_name) %>%
    summarise(number_of_assets = n(), .groups = "keep") %>%
    mutate(has_ald = TRUE)

  has_ald_with_geo_data <- analysis %>%
    filter(!is.na(asset_id), has_geo_data == TRUE) %>%
    distinct(portfolio_name, company_name, asset_id) %>%
    group_by(portfolio_name, company_name) %>%
    summarise(number_of_assets_with_geo_data = n(), .groups = "keep") %>%
    mutate(has_geo_ald = TRUE)

  # join overview stats to eq portfolio
  eq_portfolio_sub <- eq_portfolio_sub %>%
    left_join(has_ald, by = c("portfolio_name", "company_name")) %>%
    mutate(has_ald = if_else(is.na(has_ald), FALSE, TRUE)) %>%
    assertr::verify(nrow(.) == nrow(eq_portfolio_sub))

  eq_portfolio_sub <- eq_portfolio_sub %>%
    left_join(has_ald_with_geo_data, by = c("portfolio_name", "company_name")) %>%
    mutate(has_geo_ald = if_else(is.na(has_geo_ald), FALSE, TRUE)) %>%
    assertr::verify(nrow(.) == nrow(eq_portfolio_sub))


  # plot overview stats
  eq_portfolio_sub %>%
    plot_portfolio_geo_ald_value()

  save_overview_plot(name = "portfolio_geo_ald_value", path = path_db_pacta_project_pr_output_equity_portfolio_allocation)

  # plot overview stats
  eq_portfolio_sub %>%
    plot_portfolio_geo_ald_holdings()

  save_overview_plot(name = "portfolio_geo_ald_holdings", path = path_db_pacta_project_pr_output_equity_portfolio_allocation)


  # =================================
  # add final indicators
  # =================================

  for (allocation in c("port_weight", "ownership")) {
    path_db_pacta_project_pr_output_equity_portfolio_allocation <- fs::path(path_db_pacta_project_pr_output_equity_portfolio, allocation)

    create_db_pr_paths(paths = path_db_pacta_project_pr_output_equity_portfolio_allocation)


    if (allocation == "ownership") {
      # calculate portfolio_economic_value using ownership
      analysis <- analysis %>%
        mutate(portfolio_economic_value = if_else(sector == security_mapped_sector, ownership_weight*company_final_owned_economic_value, 0))
    } else if (allocation == "port_weight") {
      # calculate portfolio_economic_value using port weight
      analysis <- analysis %>%
        mutate(portfolio_economic_value = if_else(sector == security_mapped_sector, port_weight*company_final_owned_economic_value, 0))
    }

    # calculate portfolio_economic_value_share_technology
    analysis <- analysis %>%
      group_by(portfolio_name, provider, hazard, model, period, sector, technology, year) %>%
      mutate(portfolio_economic_value_share_technology = portfolio_economic_value / sum(portfolio_economic_value, na.rm = T)) %>%
      ungroup()

    # calculate portfolio_economic_value_share_technology_company
    analysis <- analysis %>%
      group_by(portfolio_name, provider, company_name, hazard, model, period, sector, technology, year) %>%
      mutate(portfolio_economic_value_share_technology_company = portfolio_economic_value / sum(portfolio_economic_value, na.rm = T)) %>%
      ungroup()

    # calculate portfolio_economic_value_share_sector
    analysis <- analysis %>%
      group_by(portfolio_name, provider, hazard, model, period, sector, year) %>%
      mutate(portfolio_economic_value_share_sector = portfolio_economic_value / sum(portfolio_economic_value, na.rm = T)) %>%
      ungroup()

    # calculate portfolio_economic_value_share_sector_company
    analysis <- analysis %>%
      group_by(portfolio_name, provider, company_name, hazard, model, period, sector, year) %>%
      mutate(portfolio_economic_value_share_sector_company = portfolio_economic_value / sum(portfolio_economic_value, na.rm = T)) %>%
      ungroup()

    # add allocation method
    analysis <- analysis %>%
      mutate(allocation = allocation)

    # plot results
    path_db_pacta_project_pr_output_equity_portfolio_plots <- fs::path(path_db_pacta_project_pr_output_equity_portfolio_allocation, "plots")

    analysis %>%
      mutate(is_reference_period = dplyr::if_else(period == "1991-2020", TRUE, FALSE)) %>% # needs change
      filter(is_reference_period == FALSE) %>%
      filter(security_mapped_sector == sector) %>%
      for_loops_climate_data(
        parent_path = fs::path(path_db_pacta_project_pr_output_equity_portfolio_plots),
        fns = function(data, final_path) {

          ####### asset_risk_histgram
          asset_risk_histgram <- data %>%
            plot_asset_risk_histgram() +
            scale_fill_relative_risk()

          save_result_plot(name = "asset_risk_histgram", path = final_path)

          ####### company_risk_distribution
          company_risk_distribution <- data %>%
            plot_company_risk_distribution() +
            scale_fill_relative_risk()

          save_result_plot(name = "company_risk_distribution", path = final_path)

          ####### portfolio_company_risk_distribution
          portfolio_company_risk_distribution <- data %>%
            plot_portfolio_company_risk_distribution() +
            scale_fill_relative_risk()

          save_result_plot(name = "portfolio_company_risk_distribution", path = final_path)

          ####### number_of_assets
          number_of_assets <- data %>%
            plot_sector_number_of_assets() +
            scale_fill_relative_risk()

          save_result_plot(name = "number_of_assets", path = final_path)

          ####### relative_sector_production
          relative_sector_production <- data %>%
            plot_sector_relative_portfolio_economic_value() +
            scale_fill_relative_risk()

          save_result_plot(name = "relative_sector_production", path = final_path)

          ####### absolute_sector_production
          absolute_sector_production <- data %>%
            plot_sector_absolute_portfolio_economic_value() +
            scale_fill_relative_risk()

          save_result_plot(name = "absolute_sector_production", path = final_path)

        }
      )

    analysis %>%
      filter(security_mapped_sector == sector) %>%
      readr::write_csv(
        fs::path(path_db_pacta_project_pr_output_equity_portfolio_allocation, "results", ext = "csv")
      )
  }

}
