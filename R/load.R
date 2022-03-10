#for PASTAX project
get_distinct_geo_data <- function(company_data) {

  distinct_company_data <- company_data %>%
    dplyr::select(asset_id, longitude, latitude)

  # create sf data frame based on longitude and latitude, geometry are sfc_POINTs
  # Note: Return sf object to a data frame by setting geometry to NULL st_set_geometry(points_sf, NULL) to remove it and convert to df/tibble again

  distinct_company_geo_data <- sf::st_as_sf(distinct_company_data, coords = c("longitude","latitude"))

  return(distinct_company_geo_data)
}

load_distinct_geo_data <- function(folder_distinct_geo_data = path_db_pr_ald_distinct_geo_data) {
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
  distinct_geo_data <- sf::st_as_sf(distinct_geo_data, coords = c("longitude", "latitude"))

  # assign crs to enable intersecting
  sf::st_crs(distinct_geo_data) <- 4326

  return(distinct_geo_data)
}

load_ald_data <- function(relevant_ald) {
  all_ald <- NULL

  for (i in 1:length(relevant_ald)) {
    if (relevant_ald[[i]]$run_prepare_script_before_loading == TRUE) source(relevant_ald[[i]]$prepare_script_path)
    if (relevant_ald[[i]]$load_data == TRUE) {
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

    if (is.null(all_ald) & relevant_ald[[i]]$load_data == TRUE) {
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
    if (relevant_climate_data[[i]]$run_prepare_script_before_loading == TRUE) source(relevant_climate_data[[i]]$prepare_script_path)
    if (relevant_climate_data[[i]]$load_data == TRUE) {
      file_list <- data.frame(data_paths = list.files(relevant_climate_data[[i]]$data_path, recursive = T))

      if (!is.null(relevant_climate_data[[i]]$parameter$scenarios)) {
        file_list <- file_list %>%
          dplyr::filter(stringr::str_detect(data_paths, paste(relevant_climate_data[[i]]$parameter$scenarios, collapse = "|")))
      }

      if (!is.null(relevant_climate_data[[i]]$parameter$hazards)) {
        file_list <- file_list %>%
          dplyr::filter(stringr::str_detect(data_paths, paste(relevant_climate_data[[i]]$parameter$hazards, collapse = "|")))
      }

      if (!is.null(relevant_climate_data[[i]]$parameter$models)) {
        file_list <- file_list %>%
          dplyr::filter(stringr::str_detect(data_paths, paste(relevant_climate_data[[i]]$parameter$models, collapse = "|")))
      }

      if (!is.null(relevant_climate_data[[i]]$parameter$periods)) {
        file_list <- file_list %>%
          dplyr::filter(stringr::str_detect(data_paths, paste(relevant_climate_data[[i]]$parameter$periods, collapse = "|")))
      }

      climate_data <- purrr::map(
        file_list, function(x) {
          cat(crayon::green("Processing ", stringr::str_replace_all(x, "/", " => "), "\n"))
          Sys.sleep(0.5)

          data <- vroom::vroom(
            fs::path(
              relevant_climate_data[[i]]$data_path, x
            ),
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

    if (is.null(all_climate_data) & relevant_climate_data[[i]]$load_data == TRUE) {
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

load_asset_level_owners <- function(path = path_db_datastore_export) {
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

load_company_financial_data <- function(path = path_db_datastore_export) {
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

load_company_ownership_tree <- function(path = path_db_datastore_export) {
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
