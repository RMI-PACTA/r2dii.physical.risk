library(dplyr)

library(ggplot2)
library(sf)

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
        filter(!is.double(longitude) | !is.double(latitude)) %>%
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

      climate_data <- bind_rows(climate_data)


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
  ds_dropbox_path = ds_dropbox_path
) {
  # ald owners
  asset_level_owners <- vroom::vroom(
    fs::path(
      ds_dropbox_path,
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
  ds_dropbox_path = ds_dropbox_path
) {
  # company_financial_data
  company_financial_data <- vroom::vroom(
    fs::path(
      ds_dropbox_path,
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
    show_diff_rows(initial_n_rows = nrow(company_financial_data), cause = "after calling distinct(company_id)")

  return(company_financial_data)

}

load_company_ownership_tree <- function(
  ds_dropbox_path = ds_dropbox_path
) {
  # ownership_tree
  ownership_tree <- vroom::vroom(
    fs::path(
      ds_dropbox_path,
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
    show_diff_rows(initial_n_rows = nrow(ownership_tree), cause = "after calling distinct()")

  return(ownership_tree)

}


save_climate_data <- function(
  climate_data,
  path_db_pr_climate_data_provider,
  use_distinct_for_assets_between_two_rasters = TRUE,
  drop_any_NAs = TRUE
) {

  # arrange final column structure and therefore verify that variables exist
  climate_data <- climate_data %>%
    dplyr::select(asset_id,  provider, scenario, model, period, is_reference_period, hazard, geometry_id, risk_level, reference, absolute_change, relative_change)

  # verify that there are no NAs in the parameter columns. NAs can arise in the risk_level column if there are missing estimates in the data
  climate_data <- climate_data %>%
    dplyr::filter(!dplyr::if_any(c("asset_id",  "geometry_id", "model", "period", "is_reference_period", "scenario", "hazard"), is.na)) %>%
    assertr::verify(nrow(.) == nrow(climate_data))

  if (use_distinct_for_assets_between_two_rasters == TRUE) {
    # check assets which lie exactly between two rasters and choose the estimates for only one geometry_id by calling distinct # not ideal solution
    climate_data <- climate_data %>%
      dplyr::distinct(asset_id, model, period, hazard, scenario, .keep_all = T) %>%
      show_diff_rows(initial_n_rows = nrow(climate_data), cause = "because of non-distinct nrows grouped by asset_id, model, period, hazard, scenario")
  }

  if (drop_any_NAs == TRUE) {
    # differences in the amount of NAs in risk_level and reference can happen if risk level is available only in the reference period
    climate_data <- climate_data %>%
      tidyr::drop_na() %>%
      show_diff_rows(initial_n_rows = nrow(climate_data), cause = "because of NAs in at least one variable")
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

  for(scenario in na.omit(unique(climate_data$scenario))) {

    scenario_sub <<- scenario
    cat(crayon::red(crayon::bold(paste("Processing", scenario_sub, "\n"))))

    climate_data_scenario_sub <- climate_data %>%
      filter(scenario == scenario_sub)

    path_db_pr_climate_data_provider_scenario <- fs::path(parent_path, scenario_sub)

    if(!dir.exists(path_db_pr_climate_data_provider_scenario)) {
      fs::dir_create(path_db_pr_climate_data_provider_scenario)
      cat(crayon::red(crayon::bold(paste("Just created directory for", scenario_sub, "\n"))))
    }

    for(hazard in unique(climate_data_scenario_sub$hazard)) {

      hazard_sub <<- hazard
      cat(crayon::blue(crayon::bold(paste("Processing", hazard_sub, "\n"))))

      climate_data_scenario_sub_hazard_sub <- climate_data_scenario_sub %>%
        filter(hazard == hazard_sub)

      path_db_pr_climate_data_provider_scenario_hazards <- fs::path(path_db_pr_climate_data_provider_scenario, hazard_sub)

      if(!dir.exists(path_db_pr_climate_data_provider_scenario_hazards)) {
        fs::dir_create(path_db_pr_climate_data_provider_scenario_hazards)
        cat(crayon::blue(crayon::bold(paste("Just created directory for", hazard_sub, "in", scenario_sub, "\n"))))
      }

      for(model in unique(climate_data_scenario_sub_hazard_sub$model)) {

        model_sub <<- model
        cat(crayon::cyan(crayon::bold(paste("Processing", model_sub, "of", hazard_sub, "of", scenario_sub, "\n"))))

        climate_data_scenario_sub_hazard_sub_model_sub <- climate_data_scenario_sub_hazard_sub %>%
          filter(model == model_sub)

        path_db_pr_climate_data_provider_scenario_hazards_models <- fs::path(path_db_pr_climate_data_provider_scenario_hazards, model_sub)

        if(!dir.exists(path_db_pr_climate_data_provider_scenario_hazards_models)) {
          fs::dir_create(path_db_pr_climate_data_provider_scenario_hazards_models)
          cat(crayon::cyan(crayon::bold(paste("Just created directory for", model_sub, "in", hazard_sub, "in", scenario_sub, "\n"))))
        }

        for(period in unique(climate_data_scenario_sub_hazard_sub_model_sub$period)) {

          period_sub <<- period
          cat(crayon::green(crayon::bold(paste("Processing", period_sub, "of", model_sub, "of", hazard_sub, "of", scenario_sub, "\n"))))

          climate_data_scenario_sub_hazard_sub_model_sub_period_sub <- climate_data_scenario_sub_hazard_sub_model_sub %>%
            filter(period == period_sub)

          climate_data_scenario_sub_hazard_sub_model_sub_period_sub %>%
            fns(final_path = path_db_pr_climate_data_provider_scenario_hazards_models)
        }
      }
    }
  }
}

scale_fill_relative_risk <- function() {
  scale_fill_gradientn(colors = rev(RColorBrewer::brewer.pal(11, "RdBu")), breaks = c(-2, -1 , 0, 1, 2), limits = c(-2,2))
}

plot_sector_absolute_portfolio_final_owned_economic_value <- function(data) {
  data %>%
    arrange(relative_change) %>%
    ggplot() +
    geom_col(aes(x = as.character(year), y = portfolio_final_owned_economic_value, fill = relative_change)) +
    theme_minimal() +
    facet_wrap(portfolio_name ~ sector, scales = "free", nrow = 1) +
    labs(
      x = "Year",
      caption = paste(
        "Parameter:",
        scenario_sub,
        hazard_sub,
        model_sub,
        period_sub
      ),
      y = "",
      title = "Absolute Sector Production"
    )
}

plot_sector_relative_portfolio_final_owned_economic_value <- function(data) {
  data %>%
    arrange(relative_change) %>%
    ggplot() +
    geom_col(aes(x = as.character(year), y = portfolio_final_owned_economic_value_share_sector, fill = relative_change)) +
    scale_y_continuous(labels = scales::percent) +
    facet_grid(portfolio_name ~ sector)  +
    theme_minimal() +
    labs(
      x = "Year",
      caption = paste(
        "Parameter:",
        scenario_sub,
        hazard_sub,
        model_sub,
        period_sub
      ),
      y = "",
      title = "Share Sector Production"
    )
}

plot_sector_number_of_assets <- function(data) {
  data %>%
    distinct(portfolio_name, hazard, model, period, asset_id, year, .keep_all = T) %>% # some assets producing different technologies (automotive!!)
    count(portfolio_name, hazard, model, period, sector, technology, year, relative_change) %>%
    arrange(relative_change) %>%
    ggplot() +
    geom_col(aes(x = as.character(year), y = n, fill = relative_change)) +
    theme_minimal() +
    facet_wrap(portfolio_name ~ sector, scales = "free", nrow = 1) +
    labs(
      x = "Year",
      caption = paste(
        "Parameter:",
        scenario_sub,
        hazard_sub,
        model_sub,
        period_sub
      ),
      y = "",
      title = "Number of assets"
    )
}

plot_portfolio_company_risk_distribution <- function(data) {

  data <- data %>%
    group_by(company_name, port_weight, relative_change) %>%
    summarise(
      portfolio_final_owned_economic_value_share_sector_company = sum(portfolio_final_owned_economic_value_share_sector_company, na.rm = T), .groups = "keep"
    ) %>%
    mutate(new_metric = port_weight*portfolio_final_owned_economic_value_share_sector_company) %>%
    arrange(relative_change)

  data %>%
    ggplot() +
    geom_col(aes(x = reorder(company_name, port_weight), y = new_metric, fill = relative_change)) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90)
    ) +
    labs(
      x = "Year",
      caption = paste(
        "Parameter:",
        scenario_sub,
        hazard_sub,
        model_sub,
        period_sub
      ),
      y = "% Portfolio Weight",
      title = "Risk distribution among the biggest companies in the portfolio"
    )
}


plot_company_risk_distribution <- function(data) {

  data <- data %>%
    semi_join(
      data %>%
        distinct(company_name, .keep_all = T) %>%
        slice_max(port_weight, n = 10),
      by = "company_name"
    )

  data <- data %>%
    mutate(company_name = paste(round(port_weight*100, 2), "% ", company_name)) %>%
    group_by(company_name, port_weight, relative_change) %>%
    summarise(
      portfolio_final_owned_economic_value_share_sector_company = sum(portfolio_final_owned_economic_value_share_sector_company, na.rm = T), .groups = "keep"
    ) %>%
    arrange(relative_change)

  data %>%
    ggplot() +
    geom_col(aes(x = reorder(company_name, port_weight), y = portfolio_final_owned_economic_value_share_sector_company, fill = relative_change)) +
    scale_y_continuous(labels = scales::percent) +
    coord_flip() +
    theme_minimal() +
    labs(
      x = "Year",
      caption = paste(
        "Parameter:",
        scenario_sub,
        hazard_sub,
        model_sub,
        period_sub
      ),
      y = "",
      title = "Risk distribution among the biggest companies in the portfolio"
    )
}

plot_asset_risk_histgram <- function(data) {
  data %>%
    mutate(relative_change = round(relative_change, 1)) %>%
    count(sector, relative_change) %>%
    ggplot() +
    geom_col(aes(x = relative_change, y = n, fill = relative_change), position = "dodge") +
    scale_x_continuous(labels = scales::percent, limits = c(-2,2)) +
    theme_minimal() +
    labs(
      x = "Relative Change",
      caption = paste(
        "Parameter:",
        scenario_sub,
        hazard_sub,
        model_sub,
        period_sub
      ),
      y = "",
      title = "Risk distribution among the identified assets in the portfolio"
    )
}


check_roll_up <- function(choose_year) {

  company_ownership_tree <- company_ownership_tree %>%
    semi_join(total_portfolio, by = c("target_company_id" = "company_id"))


  asset_level_owners <- asset_level_owners %>%
    semi_join(company_ownership_tree, by = "company_id")

  ald <- ald %>%
    semi_join(asset_level_owners, by = "asset_id")

  climate_data <- climate_data %>%
    semi_join(asset_level_owners, by = "asset_id")


  test <- ald %>%
    left_join(asset_level_owners, by = "asset_id") %>%
    mutate(direct_owned_economic_value = economic_value*(ownership_share/100))


  test <- test %>%
    left_join(company_ownership_tree %>% filter(ownership_level >= 0), by = "company_id")

  test <- test %>%
    filter(year == choose_year) %>%
    mutate(linking_stake = if_else(is.na(linking_stake), 100, linking_stake)) %>%
    mutate(final_owned_economic_value = linking_stake/100*direct_owned_economic_value)

  test <- test %>%
    group_by(target_company_id, year, sector, technology) %>%
    summarise(new_roll_up_production = sum(final_owned_economic_value, na.rm = T))

  masterdata <- vroom::vroom(
    fs::path(
      path_db_datastore_export,
      "masterdata_ownership",
      ext = "csv"
    )
  ) %>%
    group_by(company_id, company_name, sector, technology) %>%
    summarise(ar_roll_up_production = sum(`_2021`, na.rm = T))

  results <- readr::read_rds(
    fs::path(
      path_db_pacta_project,
      "40_Results",
      "Meta Investor",
      "Equity_results_company",
      ext = "rda"
    )
  ) %>%
    filter(year == choose_year) %>%
    distinct(company_name, ald_sector, technology, plan_tech_prod,.keep_all = T)


  test <- test %>%
    left_join(masterdata %>% transmute(sector, technology, target_company_id = company_id, ar_roll_up_production)) %>%
    left_join(results %>% transmute(sector = ald_sector, technology, company_name, results_production = plan_tech_prod), by = c("sector", "technology", "company_name")) %>%
    mutate(diff_ar_results = round(results_production,0) - round(ar_roll_up_production,0)) %>%
    mutate(diff_ar_new = round(new_roll_up_production,0) - round(ar_roll_up_production,0)) %>%
    mutate(diff_ar_new_perc = if_else(new_roll_up_production == 0, 1, diff_ar_new/new_roll_up_production))

  test <- test %>%
    select(company_id, company_name, year, sector, technology,results_production, ar_roll_up_production, diff_ar_results, new_roll_up_production, diff_ar_new, diff_ar_new_perc)

  plot <- test %>%
    ggplot2::ggplot() +
    ggplot2::geom_histogram(ggplot2::aes(x = diff_ar_new_perc), binwidth = 0.1)

  print(plot)

  View(test)

  return(test)
}

save_plot <- function(name, final_path = final_path) {
  ggsave(fs::path(final_path, paste(name, scenario_sub, hazard_sub, model_sub, period_sub), ext = "png"), height = 20, width = 30)

  cat(crayon::yellow(crayon::bold(paste("Saved plot", name, "for", period_sub, "of", model_sub, "of", hazard_sub, "of", scenario_sub, "\n"))))
}


