
for (asset_type in c("Equity", "Bonds")) {
  asset_type_sub <- asset_type

  asset_type_portfolio_sub <- total_portfolio %>%
    filter(asset_type == asset_type_sub)

  joining_id <- if_else(
    asset_type_sub == "Equity", "company_id", "corporate_bond_ticker"
  )

  asset_type_portfolio_sub <- asset_type_portfolio_sub %>%
    mutate(
      id = ifelse(asset_type == "Equity", company_id, corporate_bond_ticker),
      id = ifelse(is.na(id), "id_missing", id),
      id_name = ifelse(asset_type == "Equity", company_name, corporate_bond_ticker),
      id_name = ifelse(is.na(id_name), "id_name_missing", id_name)
    )

  cat(crayon::blue(crayon::bold("Processing", asset_type_sub, "\n")))

  # You can manually override the default output save path here
  # path_db_pacta_project_pr_output <- "~/Desktop/test/"

  path_db_pacta_project_pr_output_asset_type <- fs::path(
    path_db_pacta_project_pr_output,
    asset_type_sub
  )

  create_db_pr_paths(paths = path_db_pacta_project_pr_output_asset_type)



  for (portfolio in unique(asset_type_portfolio_sub$portfolio_name)) {
    asset_type_sub_portfolio_sub <- asset_type_portfolio_sub %>%
      filter(portfolio_name == portfolio)

    cat(crayon::red(crayon::bold("Processing", portfolio, "\n")))

    path_db_pacta_project_pr_output_asset_type_portfolio <- fs::path(
      path_db_pacta_project_pr_output_asset_type,
      portfolio
    )

    create_db_pr_paths(paths = path_db_pacta_project_pr_output_asset_type_portfolio)


    # =================================
    # subset relevant data
    # =================================

    # ========
    # subset relevant target companies
    # ========
    company_ownership_tree_sub <- company_ownership_tree %>%
      select(subsidiary_id, linking_stake, ownership_level, all_of(joining_id)) %>%
      semi_join(asset_type_sub_portfolio_sub, by = joining_id)

    # TODO: check bonds roll-up
    # ========
    # subset relevant asset owners
    # ========
    asset_level_owners_sub <- asset_level_owners %>%
      rename(owner_id = company_id) %>%
      semi_join(company_ownership_tree_sub, by = c("owner_id" = "subsidiary_id"))

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
      left_join(asset_level_owners_sub, by = "asset_id")

    analysis <- analysis %>%
      mutate(direct_owned_economic_value = economic_value * (ownership_share / 100))

    # ========
    # merge ownership tree
    # ========
    analysis <- analysis %>%
      left_join(
        filter(company_ownership_tree_sub, ownership_level >= 0),
        by = c("owner_id" = "subsidiary_id")
      )

    # clean linking stake + calculate company_final_owned_economic_value
    analysis <- analysis %>%
      mutate(
        linking_stake = if_else(is.na(linking_stake), 100, linking_stake)
      ) %>%
      mutate(
        company_final_owned_economic_value = linking_stake / 100 * direct_owned_economic_value
      )

    # ========
    # merge to portfolio
    # ========
    analysis <- asset_type_sub_portfolio_sub %>%
      left_join(
        analysis,
        by = joining_id
      )

    # =================================
    # create overview stats
    # =================================

    has_ald <- analysis %>%
      filter(!is.na(asset_id)) %>%
      distinct(portfolio_name, id, asset_id) %>%
      group_by(portfolio_name, id) %>%
      summarise(number_of_assets = n(), .groups = "keep") %>%
      mutate(has_ald = TRUE)

    has_ald_with_geo_data <- analysis %>%
      filter(!is.na(asset_id), has_geo_data == TRUE) %>%
      distinct(portfolio_name, id, asset_id) %>%
      group_by(portfolio_name, id) %>%
      summarise(number_of_assets_with_geo_data = n(), .groups = "keep") %>%
      mutate(has_geo_ald = TRUE)

    # join overview stats to eq portfolio
    asset_type_sub_portfolio_sub <- asset_type_sub_portfolio_sub %>%
      left_join(has_ald, by = c("portfolio_name", "id")) %>%
      mutate(has_ald = if_else(is.na(has_ald), FALSE, TRUE)) %>%
      assertr::verify(nrow(.) == nrow(asset_type_sub_portfolio_sub))

    asset_type_sub_portfolio_sub <- asset_type_sub_portfolio_sub %>%
      left_join(has_ald_with_geo_data, by = c("portfolio_name", "id")) %>%
      mutate(has_geo_ald = if_else(is.na(has_geo_ald), FALSE, TRUE)) %>%
      assertr::verify(nrow(.) == nrow(asset_type_sub_portfolio_sub))


    # plot overview stats
    asset_type_sub_portfolio_sub %>%
      plot_portfolio_geo_ald_value()

    save_overview_plot(
      name = "portfolio_geo_ald_value",
      path = path_db_pacta_project_pr_output_asset_type_portfolio
    )

    # plot overview stats
    asset_type_sub_portfolio_sub %>%
      plot_portfolio_geo_ald_holdings()

    save_overview_plot(
      name = "portfolio_geo_ald_holdings",
      path = path_db_pacta_project_pr_output_asset_type_portfolio
    )


    # =================================
    # add final indicators
    # =================================

    for (allocation in c("port_weight", "ownership")) {
      if (paste0(allocation, asset_type_sub) != "ownershipBonds") {
        path_db_pacta_project_pr_output_asset_type_portfolio_allocation <- fs::path(path_db_pacta_project_pr_output_asset_type_portfolio, allocation)

        create_db_pr_paths(paths = path_db_pacta_project_pr_output_asset_type_portfolio_allocation)

        cat(crayon::green(crayon::bold("Processing", allocation, "\n")))

        if (allocation == "ownership" & asset_type_sub == "Equity") {
          # calculate portfolio_economic_value using ownership
          analysis_final <- analysis %>%
            mutate(portfolio_economic_value = if_else(sector == security_mapped_sector, ownership_weight * company_final_owned_economic_value, 0))
        } else if (allocation == "port_weight") {
          # calculate portfolio_economic_value using port weight
          analysis_final <- analysis %>%
            mutate(portfolio_economic_value = if_else(sector == security_mapped_sector, asset_type_port_weight * company_final_owned_economic_value, 0))
        }

        # add allocation method
        analysis_final <- analysis_final %>%
          mutate(allocation = allocation)

        # plot results
        path_db_pacta_project_pr_output_asset_type_portfolio_allocation_plots <- fs::path(path_db_pacta_project_pr_output_asset_type_portfolio_allocation, "plots")

        analysis_final %>%
          filter(security_mapped_sector == sector) %>%
          filter(is_reference_period == FALSE) %>%
          for_loops_climate_data(
            parent_path = fs::path(path_db_pacta_project_pr_output_asset_type_portfolio_allocation_plots),
            fns = function(data, final_path) {

              # filter rows with belong to assets
              data <- data %>%
                rbind.data.frame(
                  analysis_final %>%
                    filter(security_mapped_sector == sector) %>%
                    filter(!sector %in% c("Cement", "Steel")) %>%
                    filter(!is.na(asset_id)) %>% # these get kicked out in the for loop
                    filter(is.na(provider))
                )

              # ensure that all assets are analysed under the given subset of paramters -> also assets with missing assets will be included
              data <- data %>%
                mutate(
                  provider = provider_sub,
                  scenario = scenario_sub,
                  hazard = hazard_sub,
                  model = model_sub,
                  period = period_sub
                )

              # calculate portfolio_economic_value_share_technology
              data <- data %>%
                group_by(portfolio_name, provider, hazard, model, period, sector, technology, year) %>%
                mutate(portfolio_economic_value_share_technology = portfolio_economic_value / sum(portfolio_economic_value, na.rm = T)) %>%
                ungroup()

              # calculate portfolio_economic_value_share_technology_company
              data <- data %>%
                group_by(portfolio_name, provider, id, hazard, model, period, sector, technology, year) %>%
                mutate(portfolio_economic_value_share_technology_company = portfolio_economic_value / sum(portfolio_economic_value, na.rm = T)) %>%
                ungroup()

              # calculate portfolio_economic_value_share_sector
              data <- data %>%
                group_by(portfolio_name, provider, hazard, model, period, sector, year) %>%
                mutate(portfolio_economic_value_share_sector = portfolio_economic_value / sum(portfolio_economic_value, na.rm = T)) %>%
                ungroup()

              # calculate portfolio_economic_value_share_sector_company
              data <- data %>%
                group_by(portfolio_name, provider, id, hazard, model, period, sector, year) %>%
                mutate(portfolio_economic_value_share_sector_company = portfolio_economic_value / sum(portfolio_economic_value, na.rm = T)) %>%
                ungroup()

              # TODO: set boundaries of relative change (can be several million % in extreme cases (e.g. snow in the sahara))
              upper_boundary <- round(quantile(data$relative_change, 0.95, na.rm = T), 2)
              lower_boundary <- round(quantile(data$relative_change, 0.05, na.rm = T), 2)

              data <- data %>%
                mutate(
                  relative_change = case_when(
                    relative_change > upper_boundary ~ upper_boundary,
                    relative_change < lower_boundary ~ lower_boundary,
                    TRUE ~ relative_change
                  )
                )

              ####### asset_risk_histgram
              asset_risk_histgram <- data %>%
                plot_asset_risk_histgram(text_size = 20) +
                scale_fill_relative_risk()

              save_result_plot(name = "asset_risk_histgram", path = final_path)

              ####### company_risk_distribution
              company_risk_distribution <- data %>%
                plot_company_risk_distribution(text_size = 20) +
                scale_fill_relative_risk()

              save_result_plot(name = "company_risk_distribution", path = final_path)

              ####### portfolio_company_risk_distribution
              portfolio_company_risk_distribution <- data %>%
                plot_portfolio_company_risk_distribution(text_size = 20) +
                scale_fill_relative_risk()

              save_result_plot(name = "portfolio_company_risk_distribution", path = final_path)

              ####### number_of_assets
              number_of_assets <- data %>%
                plot_sector_number_of_assets(text_size = 20) +
                scale_fill_relative_risk()

              save_result_plot(name = "number_of_assets", path = final_path)

              ####### relative_sector_production
              relative_sector_production <- data %>%
                plot_sector_relative_portfolio_economic_value(text_size = 20) +
                scale_fill_relative_risk()

              save_result_plot(name = "relative_sector_production", path = final_path)

              ####### absolute_sector_production
              absolute_sector_production <- data %>%
                plot_sector_absolute_portfolio_economic_value(text_size = 20) +
                scale_fill_relative_risk()

              save_result_plot(name = "absolute_sector_production", path = final_path)


              data %>%
                readr::write_csv(
                  fs::path(final_path, "results", ext = "csv")
                )
            }
          )
      }
    }
  }
}
