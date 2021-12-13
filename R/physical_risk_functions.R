create_db_pr_paths <- function(paths = NULL) {

  ###### too risky to create something unwanted (hasnt happend yet, but function should be handled with care)
  # if (is.null(paths)) {
  #   for(path in ls(envir = .GlobalEnv)[stringr::str_detect(ls(envir = .GlobalEnv), "path_db_pr")]) {
  #     current_path <- get(envir = .GlobalEnv, path)
  #
  #     if(!fs::dir_exists(current_path)) {
  #       fs::dir_create(current_path)
  #       message(paste("Just created", current_path))
  #     }
  #   }
  # }

  if (!is.null(paths)) {
    for(path in paths) {
      current_path <- path

      if(!fs::dir_exists(current_path)) {
        fs::dir_create(current_path)
        message(paste("Just created", current_path))
      }
    }
  }
}

show_folder_structure <- function(path_pattern = "path") {

  all_paths <- NA

  for(i in 1:length(ls(envir = .GlobalEnv)[stringr::str_detect(ls(envir = .GlobalEnv), path_pattern)])) {
    current_path <- get(envir = .GlobalEnv, ls(envir = .GlobalEnv)[stringr::str_detect(ls(envir = .GlobalEnv), path_pattern)][i])

    current_path <- ifelse(is.function(current_path), NA, current_path)

    all_paths <- c(all_paths, current_path)
  }

  all_paths <- all_paths[!is.na(all_paths)]

  all_paths <- lapply(strsplit(all_paths, "/"), function(all_paths) as.data.frame(t(all_paths)))
  all_paths <- plyr::rbind.fill(all_paths)
  all_paths$pathString <- apply(all_paths, 1, function(all_paths) paste(trimws(na.omit(all_paths)), collapse="/"))
  all_paths <- all_paths[order(all_paths$pathString),]

  all_paths <- data.tree::as.Node(all_paths)

  print(all_paths)
  plot(all_paths)

}

show_diff_rows <- function(data, initial_n_rows, cause = "") {
  diff <- initial_n_rows - nrow(data)
  if(diff > 0) cat(crayon::red("\n", diff, "rows have been removed", cause, "\n", "\n"))
  if(diff == 0) cat(crayon::green("\n", "Number of rows has not changed", cause, "\n", "\n"))
  if(diff < 0) cat(crayon::blue("\n", -diff, "rows have been added", cause, "\n", "\n"))

  return(data)
}

load_distinct_geo_data <- function(
  folder_distinct_geo_data = path_db_pr_ald_distinct_geo_data
) {
  # create a list of files consisting of ALD with geo data
  file_list_distinct_geo_data <- list.files(folder_distinct_geo_data)

  # load distinct_geo_data
  distinct_geo_data <- purrr::map(
    file_list_distinct_geo_data, function(x) {

      # message which files get read
      message(paste0("Processing ", x))

      # read file
      distinct_geo_data <- vroom::vroom(
        fs::path(
          path_db_pr_ald_distinct_geo_data,
          x
        )
      )

      # select relevant columns (ideally only those should be in the data actually)
      distinct_geo_data <- distinct_geo_data %>%
        dplyr::select(asset_id, longitude, latitude)


      # verify assumptions of the data -> assumptions should be ensured when creating the data, not after loading it

      ## verify that there are no NAs in the data
      distinct_geo_data %>%
        tidyr::drop_na() %>%
        assertr::verify(nrow(.) == nrow(distinct_geo_data))

      ## verify that there are no duplicates
      distinct_geo_data %>%
        dplyr::distinct() %>%
        assertr::verify(nrow(.) == nrow(distinct_geo_data))

      ## verify that longitude and latitude are doubles
      distinct_geo_data %>%
        dplyr::filter(!is.double(longitude) | !is.double(latitude)) %>%
        assertr::verify(nrow(.) == 0)

      return(distinct_geo_data)
    }
  )

  # bind rows of files with geo data
  distinct_geo_data <- distinct_geo_data %>%
    dplyr::bind_rows()

  # create sf data frame based on longitude and latitude
  distinct_geo_data <- sf::st_as_sf(distinct_geo_data, coords = c("longitude","latitude"))

  # assign crs to enable intersecting
  sf::st_crs(distinct_geo_data) <- 4326

  return(distinct_geo_data)
}

load_ald_data <- function(relevant_ald) {

  all_ald <- NULL

  for (i in 1:length(relevant_ald)) {

    if(relevant_ald[[i]]$run_prepare_script_before_loading == TRUE) source(relevant_ald[[i]]$prepare_script_path)
    if(relevant_ald[[i]]$load_data == TRUE) {
      ald <- vroom::vroom(fs::path(relevant_ald[[i]]$data_path))

      # make sure columns are available by selecting them
      ald <- ald %>%
        dplyr::select(
          asset_id,
          asset_id_source,
          asset_name,
          asset_location,
          latitude,
          longitude,
          source_coordinates,
          has_geo_data,
          sector,
          technology,
          asset_level_timestamp,
          economic_unit,
          year,
          economic_value
        )
    }

    if(is.null(all_ald) & relevant_ald[[i]]$load_data == TRUE) {
      all_ald <- ald
    } else if (!is.null(all_ald) & relevant_ald[[i]]$load_data == TRUE) {
      all_ald <- rbind.data.frame(
        all_ald,
        ald
      )
    }
  }

  return(all_ald)
}

load_climate_data <- function(relevant_climate_data) {

  all_climate_data <- NULL

  for (i in 1:length(relevant_climate_data)) {

    if(relevant_climate_data[[i]]$run_prepare_script_before_loading == TRUE) source(relevant_climate_data[[i]]$prepare_script_path)
    if(relevant_climate_data[[i]]$load_data == TRUE) {

      file_list <- data.frame(data_paths = list.files(relevant_climate_data[[i]]$data_path, recursive = T))

      if(!is.null(relevant_climate_data[[i]]$parameter$scenarios)) {
        file_list <- file_list %>%
          dplyr::filter(stringr::str_detect(data_paths, paste(relevant_climate_data[[i]]$parameter$scenarios, collapse = "|")))
      }

      if(!is.null(relevant_climate_data[[i]]$parameter$hazards)) {
        file_list <- file_list %>%
          dplyr::filter(stringr::str_detect(data_paths, paste(relevant_climate_data[[i]]$parameter$hazards, collapse = "|")))
      }

      if(!is.null(relevant_climate_data[[i]]$parameter$models)) {
        file_list <- file_list %>%
          dplyr::filter(stringr::str_detect(data_paths, paste(relevant_climate_data[[i]]$parameter$models, collapse = "|")))
      }

      if(!is.null(relevant_climate_data[[i]]$parameter$periods)) {
        file_list <- file_list %>%
          dplyr::filter(stringr::str_detect(data_paths, paste(relevant_climate_data[[i]]$parameter$periods, collapse = "|")))
      }

      climate_data <- purrr::map(
        file_list, function(x) {
          cat(crayon::green("Processing ", stringr::str_replace_all(x, "/", " => "), "\n"))
          Sys.sleep(0.5)

          data <- vroom::vroom(
            fs::path(
              relevant_climate_data[[i]]$data_path, x),

            col_types = c(
              provider = "c",
              model = "c",
              period = "c",
              scenario = "c",
              geometry_id = "c",
              asset_id = "c",
              hazard = "c",
              risk_level = "d",
              reference = "d",
              absolute_change = "d",
              relative_change = "d"
            )
          )
        }
      )

      climate_data <- dplyr::bind_rows(climate_data)


    }

    if(is.null(all_climate_data) & relevant_climate_data[[i]]$load_data == TRUE) {
      all_climate_data <- climate_data
    } else if (!is.null(all_climate_data) & relevant_climate_data[[i]]$load_data == TRUE) {
      all_climate_data <- rbind.data.frame(
        all_climate_data,
        climate_data
      )
    }
  }

  return(all_climate_data)
}

load_asset_level_owners <- function(
  path = path_db_datastore_export
) {
  # ald owners
  asset_level_owners <- vroom::vroom(
    fs::path(
      path,
      "asset_level_owners",
      ext = "csv"
    )
  )

  # make sure columns are available by selecting them
  asset_level_owners <- asset_level_owners %>%
    dplyr::select(company_id, asset_id, ownership_share)

  # make sure asset owners are unique (can be different if one asset uses different technologies)
  asset_level_owners <- asset_level_owners %>%
    dplyr::distinct() %>%
    show_diff_rows(initial_n_rows = nrow(asset_level_owners), cause = "after calling distinct()")

  return(asset_level_owners)
}

load_company_financial_data <- function(
  path = path_db_datastore_export
) {
  # company_financial_data
  company_financial_data <- vroom::vroom(
    fs::path(
      path,
      "company_financial_data",
      ext = "csv"
    )
  )

  # make sure columns are available by selecting them
  company_financial_data <- company_financial_data %>%
    dplyr::select(company_name, company_id)

  # ensure company IDs are unique
  company_financial_data <- company_financial_data %>%
    dplyr::distinct(company_id, .keep_all = T) %>%
    show_diff_rows(
      initial_n_rows = nrow(company_financial_data),
      cause = "after calling distinct(company_id)"
    )

  return(company_financial_data)

}

load_company_ownership_tree <- function(
  path = path_db_datastore_export
) {
  # ownership_tree
  ownership_tree <- vroom::vroom(
    fs::path(
      path,
      "company_ownership_bidirectional",
      ext = "csv"
    )
  )

  # make sure columns are available by selecting them
  ownership_tree <- ownership_tree %>%
    dplyr::select(target_company_id, company_id, linking_stake, ownership_level)

  # ensure that total column structure is unique
  ownership_tree <- ownership_tree %>%
    dplyr::distinct() %>%
    show_diff_rows(
      initial_n_rows = nrow(ownership_tree),
      cause = "after calling distinct()"
    )

  return(ownership_tree)

}

load_company_id_cb_ticker <- function(path = path_db_datastore_export) {
  # consolidated_financial_data
  consolidated_financial_data <- vroom::vroom(
    fs::path(
      path,
      "consolidated_financial_data",
      ext = "csv"
    )
  )

  company_id_cb_ticker <- consolidated_financial_data %>%
    dplyr::select(company_id, corporate_bond_ticker) %>%
    dplyr::filter(!is.na(corporate_bond_ticker)) %>%
    dplyr::distinct(company_id, corporate_bond_ticker)

  return(company_id_cb_ticker)
}


save_climate_data <- function(
  climate_data,
  path_db_pr_climate_data_provider,
  use_distinct_for_assets_between_two_rasters = TRUE,
  drop_any_NAs = TRUE
) {

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
        is.na)
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

for_loops_climate_data <- function(data, parent_path, fns) {

  climate_data <- data

  for(provider in na.omit(unique(climate_data$provider))) {

    provider_sub <<- provider
    cat(crayon::white(crayon::bold(paste("Processing", provider_sub, "\n"))))

    climate_data_provider_sub <- climate_data %>%
      dplyr::filter(provider == provider_sub)

    path_db_pr_climate_data_provider <- fs::path(parent_path, provider_sub)

    if(!dir.exists(path_db_pr_climate_data_provider)) {
      fs::dir_create(path_db_pr_climate_data_provider)
      cat(crayon::white(crayon::bold(paste("Just created directory for", provider_sub, "\n"))))
    }

    for(scenario in unique(climate_data_provider_sub$scenario)) {

      scenario_sub <<- scenario
      cat(crayon::red(crayon::bold(paste("Processing", scenario_sub, "\n"))))

      climate_data_provider_sub_scenario_sub <- climate_data_provider_sub %>%
        dplyr::filter(scenario == scenario_sub)

      path_db_pr_climate_data_provider_scenario <- fs::path(path_db_pr_climate_data_provider, scenario_sub)

      if(!dir.exists(path_db_pr_climate_data_provider_scenario)) {
        fs::dir_create(path_db_pr_climate_data_provider_scenario)
        cat(crayon::red(crayon::bold(paste("Just created directory for", scenario_sub, "\n"))))
      }

      for(hazard in unique(climate_data_provider_sub_scenario_sub$hazard)) {

        hazard_sub <<- hazard
        cat(crayon::blue(crayon::bold(paste("Processing", hazard_sub, "\n"))))

        climate_data_provider_sub_scenario_sub_hazard_sub <- climate_data_provider_sub_scenario_sub %>%
          dplyr::filter(hazard == hazard_sub)

        path_db_pr_climate_data_provider_scenario_hazards <- fs::path(path_db_pr_climate_data_provider_scenario, hazard_sub)

        if(!dir.exists(path_db_pr_climate_data_provider_scenario_hazards)) {
          fs::dir_create(path_db_pr_climate_data_provider_scenario_hazards)
          cat(crayon::blue(crayon::bold(paste("Just created directory for", hazard_sub, "in", scenario_sub, "\n"))))
        }

        for(model in unique(climate_data_provider_sub_scenario_sub_hazard_sub$model)) {

          model_sub <<- model
          cat(crayon::cyan(crayon::bold(paste("Processing", model_sub, "of", hazard_sub, "of", scenario_sub, "\n"))))

          climate_data_provider_sub_scenario_sub_hazard_sub_model_sub <- climate_data_provider_sub_scenario_sub_hazard_sub %>%
            dplyr::filter(model == model_sub)

          path_db_pr_climate_data_provider_scenario_hazards_models <- fs::path(path_db_pr_climate_data_provider_scenario_hazards, model_sub)

          if(!dir.exists(path_db_pr_climate_data_provider_scenario_hazards_models)) {
            fs::dir_create(path_db_pr_climate_data_provider_scenario_hazards_models)
            cat(crayon::cyan(crayon::bold(paste("Just created directory for", model_sub, "in", hazard_sub, "in", scenario_sub, "\n"))))
          }

          for(period in unique(climate_data_provider_sub_scenario_sub_hazard_sub_model_sub$period)) {

            period_sub <<- period
            cat(crayon::green(crayon::bold(paste("Processing", period_sub, "of", model_sub, "of", hazard_sub, "of", scenario_sub, "\n"))))

            climate_data_provider_sub_scenario_sub_hazard_sub_model_sub_period_sub <- climate_data_provider_sub_scenario_sub_hazard_sub_model_sub %>%
              dplyr::filter(period == period_sub)

            climate_data_provider_sub_scenario_sub_hazard_sub_model_sub_period_sub %>%
              fns(final_path = path_db_pr_climate_data_provider_scenario_hazards_models)
          }
        }
      }
    }
  }
}

scale_fill_relative_risk <- function() {
  ggplot2::scale_fill_gradientn(
    colors = rev(RColorBrewer::brewer.pal(11, "RdBu")),
    #breaks = c(-2, -1 , 0, 1, 2),
    #limits = c(-2,2),
    na.value = "grey50",
    labels = scales::percent
  )
}

plot_sector_absolute_portfolio_economic_value <- function(data, text_size = 12) {
  data %>%
    dplyr::arrange(relative_change) %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(aes(x = as.character(year), y = portfolio_economic_value, fill = relative_change)) +
    ggplot2::theme_minimal() +
    ggplot2::facet_wrap( ~ sector, scales = "free", nrow = 1) +
    ggplot2::labs(
      x = "Year",
      caption = paste(
        "Parameter:",
        provider_sub,
        scenario_sub,
        hazard_sub,
        model_sub,
        period_sub
      ),
      y = "",
      title = "Absolute Sector Production"
    ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white"),
      text = ggplot2::element_text(size = text_size)
    )
}

plot_sector_relative_portfolio_economic_value <- function(data, text_size = 12) {
  data %>%
    dplyr::arrange(relative_change) %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(aes(x = as.character(year), y = portfolio_economic_value_share_sector, fill = relative_change)) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::facet_grid( ~ sector)  +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Year",
      caption = paste(
        "Parameter:",
        provider_sub,
        scenario_sub,
        hazard_sub,
        model_sub,
        period_sub
      ),
      y = "",
      title = "Share Sector Production"
    ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white"),
      text = ggplot2::element_text(size = text_size)
    )
}

plot_sector_number_of_assets <- function(data, text_size = 12) {
  data %>%
    dplyr::distinct(portfolio_name, hazard, model, period, asset_id, year, .keep_all = T) %>% # some assets producing different technologies (automotive!!)
    dplyr::count(portfolio_name, hazard, model, period, sector, technology, year, relative_change) %>%
    dplyr::arrange(relative_change) %>%
    ggplot() +
    ggplot2::geom_col(aes(x = as.character(year), y = n, fill = relative_change)) +
    ggplot2::theme_minimal() +
    ggplot2::facet_wrap( ~ sector, scales = "free", nrow = 1) +
    ggplot2::labs(
      x = "Year",
      caption = paste(
        "Parameter:",
        provider_sub,
        scenario_sub,
        hazard_sub,
        model_sub,
        period_sub
      ),
      y = "",
      title = "Number of assets"
    ) +
    ggplot2::theme(
      plot.background = element_rect(fill = "white"),
      text = element_text(size = text_size)
    )
}

plot_portfolio_company_risk_distribution <- function(data, text_size = 12) {

  data <- data %>%
    dplyr::group_by(id_name, port_weight, relative_change) %>%
    dplyr::summarise(
      portfolio_economic_value_share_sector_company = sum(portfolio_economic_value_share_sector_company, na.rm = T), .groups = "keep"
    ) %>%
    dplyr::mutate(new_metric = port_weight*portfolio_economic_value_share_sector_company) %>%
    dplyr::arrange(relative_change)

  data %>%
    ggplot() +
    ggplot2::geom_col(aes(x = reorder(id_name, port_weight), y = new_metric, fill = relative_change)) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90)
    ) +
    ggplot2::labs(
      x = "Year",
      caption = paste(
        "Parameter:",
        provider_sub,
        scenario_sub,
        hazard_sub,
        model_sub,
        period_sub
      ),
      y = "% Portfolio Weight",
      title = "Risk distribution among the biggest companies in the portfolio"
    ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white"),
      text = ggplot2::element_text(size = text_size)
    )
}


plot_company_risk_distribution <- function(data, text_size = 12) {

  sub_set <- data %>%
    dplyr::distinct(holding_id, .keep_all = T) %>%
    dplyr::group_by(id_name) %>%
    dplyr::summarise(port_weight = sum(port_weight, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::slice_max(port_weight, n = 10) %>%
    dplyr::arrange(dplyr::desc(port_weight)) %>% # necessary as often same weight
    dplyr::slice(c(1:10)) %>% # necessary as often same weight
    dplyr::select(id_name, port_weight)

  data <- data %>%
    dplyr::select(-port_weight) %>%
    dplyr::inner_join(
      sub_set,
      by = "id_name"
    )

  data <- data %>%
    dplyr::mutate(id_name = paste(round(port_weight*100, 2), "% ", id_name)) %>%
    dplyr::group_by(id_name, port_weight, relative_change) %>%
    dplyr::summarise(
      portfolio_economic_value_share_sector_company = sum(portfolio_economic_value_share_sector_company, na.rm = T), .groups = "keep"
    ) %>%
    dplyr::arrange(relative_change)

  data %>%
    ggplot() +
    ggplot2::geom_col(aes(x = reorder(id_name, port_weight), y = portfolio_economic_value_share_sector_company, fill = relative_change)) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Year",
      caption = paste(
        "Parameter:",
        provider_sub,
        scenario_sub,
        hazard_sub,
        model_sub,
        period_sub
      ),
      y = "",
      title = "Risk distribution among the biggest companies in the portfolio"
    ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white"),
      text = ggplot2::element_text(size = text_size)
    )
}

plot_asset_risk_histgram <- function(data, text_size = 12) {
  data %>%
    dplyr::mutate(relative_change = round(relative_change, 1)) %>%
    dplyr::count(sector, relative_change) %>%
    ggplot() +
    ggplot2::geom_col(aes(x = relative_change, y = n, fill = relative_change), position = "dodge") +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Relative Change",
      caption = paste(
        "Parameter:",
        provider_sub,
        scenario_sub,
        hazard_sub,
        model_sub,
        period_sub
      ),
      y = "",
      title = "Risk distribution among the identified assets in the portfolio"
    ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white"),
      text = ggplot2::element_text(size = text_size)
    )
}

save_result_plot <- function(name, path = final_path, height = 20, width = 30) {
  ggplot2::ggsave(fs::path(path, paste(name, provider_sub, scenario_sub, hazard_sub, model_sub, period_sub), ext = "png"), height = height, width = width)

  cat(crayon::yellow(crayon::bold(paste("Saved plot", name, "for", provider_sub, "for", period_sub, "of", model_sub, "of", hazard_sub, "of", scenario_sub, "\n"))))
}

save_overview_plot <- function(name, path = final_path, height = 20, width = 30) {
  ggplot2::ggsave(fs::path(path, name, ext = "png"), height = height, width = width)

  cat(crayon::yellow(crayon::bold(paste("Saved plot", name, "\n"))))
}

plot_portfolio_geo_ald_value <- function(data) {

  data %>%
    dplyr::group_by(portfolio_name, has_geo_ald) %>%
    dplyr::summarise(
      sum_value_usd_mio = sum(value_usd, na.rm = T)/10^6, .groups = "keep"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(portfolio_name) %>%
    dplyr::mutate(
      share_value_usd = sum_value_usd_mio/sum(sum_value_usd_mio, na.rm = T),
    ) %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(aes(x = portfolio_name, y = share_value_usd, fill = has_geo_ald)) +
    ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~ . * sum(asset_type_sub_portfolio_sub %>% dplyr::filter(portfolio_name %in% c("IE00B4L5Y983")) %>% dplyr::pull(value_usd))/10^6)) +
    ggplot2::labs(
      x = "",
      y = "% Portfolio Value",
      title = "Portfolio Value of holdings associated with at least one ALD",
      fill = "Has ALD"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white")
    )
}

plot_portfolio_geo_ald_holdings <- function(data) {

  data %>%
    dplyr::group_by(portfolio_name, has_geo_ald) %>%
    dplyr::summarise(
      n = n(), .groups = "keep"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(portfolio_name) %>%
    dplyr::mutate(
      n = n/sum(n, na.rm = T)
    ) %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(aes(x = portfolio_name, y = n, fill = has_geo_ald)) +
    ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~ . * nrow(asset_type_sub_portfolio_sub))) +
    ggplot2::labs(
      x = "",
      y = "% Portfolio Holdings",
      title = "Holdings associated with at least one ALD",
      fill = "Has ALD"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white")
    )
}

# check_roll_up <- function(choose_year) {
#
#   company_ownership_tree <- company_ownership_tree %>%
#     rename(
#       target_company_id = company_id,
#       company_id = subsidiary_id,
#     ) %>%
#     semi_join(total_portfolio, by = c("target_company_id" = "company_id"))
#
#
#   asset_level_owners <- asset_level_owners %>%
#     semi_join(company_ownership_tree, by = "company_id")
#
#   ald <- ald %>%
#     semi_join(asset_level_owners, by = "asset_id")
#
#   climate_data <- climate_data %>%
#     semi_join(asset_level_owners, by = "asset_id")
#
#
#   check_roll_up <- ald %>%
#     left_join(asset_level_owners, by = "asset_id") %>%
#     mutate(direct_owned_economic_value = economic_value*(ownership_share/100))
#
#
#   check_roll_up <- check_roll_up %>%
#     left_join(company_ownership_tree %>% filter(ownership_level >= 0), by = "company_id")
#
#   check_roll_up <- check_roll_up %>%
#     filter(year == choose_year) %>%
#     mutate(linking_stake = if_else(is.na(linking_stake), 100, linking_stake)) %>%
#     mutate(final_owned_economic_value = linking_stake/100*direct_owned_economic_value)
#
#   check_roll_up <- check_roll_up %>%
#     group_by(target_company_id, year, sector, technology) %>%
#     summarise(new_roll_up_production = sum(final_owned_economic_value, na.rm = T))
#
#   masterdata <- vroom::vroom(
#     fs::path(
#       path_db_datastore_export,
#       "masterdata_ownership",
#       ext = "csv"
#     )
#   ) %>%
#     group_by(company_id, company_name, sector, technology) %>%
#     summarise(ar_roll_up_production = sum(`_2021`, na.rm = T))
#
#   results <- readr::read_rds(
#     fs::path(
#       path_db_pacta_project,
#       "40_Results",
#       "Meta Investor",
#       "Equity_results_company",
#       ext = "rda"
#     )
#   ) %>%
#     filter(year == choose_year) %>%
#     distinct(company_name, ald_sector, technology, plan_tech_prod,.keep_all = T)
#
#
#   check_roll_up <- check_roll_up %>%
#     left_join(masterdata %>% transmute(sector, technology, target_company_id = company_id, ar_roll_up_production)) %>%
#     left_join(results %>% transmute(sector = ald_sector, technology, company_name, results_production = plan_tech_prod), by = c("sector", "technology", "company_name")) %>%
#     mutate(diff_ar_results = round(results_production,0) - round(ar_roll_up_production,0)) %>%
#     mutate(diff_ar_new = round(new_roll_up_production,0) - round(ar_roll_up_production,0)) %>%
#     mutate(diff_ar_new_perc = if_else(new_roll_up_production == 0, 1, diff_ar_new/new_roll_up_production))
#
#   check_roll_up <- check_roll_up %>%
#     select(company_id, company_name, year, sector, technology,results_production, ar_roll_up_production, diff_ar_results, new_roll_up_production, diff_ar_new, diff_ar_new_perc)
#
#   plot <- check_roll_up %>%
#     ggplot2::ggplot() +
#     ggplot2::geom_histogram(ggplot2::aes(x = diff_ar_new_perc), binwidth = 0.1)
#
#   print(plot)
#
#   View(check_roll_up)
#
#   return(check_roll_up)
# }
