devtools::load_all(".")

# =================================
# load distinct_geo_data which will subset the raw climate data
# =================================

distinct_geo_data <- load_distinct_geo_data()

# =================================
# load climate data
# =================================


# list geo tiff indices files
file_list_CDF_raw_geotiff_indices <- list.files(
  path_db_pr_climate_data_CDF_raw_geotiff_indices
  )

# for loop to load data for temperature and precipitation hazards and to create one dataframe in the long format
for (i in 1:length(c("Temperature", "Precipitation"))) {

  # chose data type -> different column structure
  type_sub <- c("Temperature", "Precipitation")[i]

  message(paste("Processing files of type", type_sub))

  # subset files based on data type
  if (type_sub == "Precipitation") {
    file_list_CDF_raw_geotiff_indices_sub <- file_list_CDF_raw_geotiff_indices[stringr::str_detect(file_list_CDF_raw_geotiff_indices, "prAdjust") | stringr::str_detect(file_list_CDF_raw_geotiff_indices, "pr-Indices")]
  }
  if (type_sub == "Temperature") {
    file_list_CDF_raw_geotiff_indices_sub <- file_list_CDF_raw_geotiff_indices[stringr::str_detect(file_list_CDF_raw_geotiff_indices, "tasAdjust") | stringr::str_detect(file_list_CDF_raw_geotiff_indices, "tas-Indices")]
  }

  # load file containing the column name index indicating the position of climate variables
  column_name_index <- readr::read_csv(
    fs::path(path_db_pr_climate_data_CDF_raw, "column_name_index", ext = "csv")
    )

  column_name_index <- column_name_index %>%
    dplyr::filter(type == type_sub)

  # load climate data
  climate_data <- purrr::map(
    file_list_CDF_raw_geotiff_indices_sub, function(x) {
      message(paste0("Processing ", x))

      # load data (specific for each file type)
      data <- stars::read_stars(
        fs::path(path_db_pr_climate_data_CDF_raw_geotiff_indices, x), proxy = F
        )

      data <- split(data, "band")

      # ALWAYS THE AIM: SAVE DATA AS SF
      data <- sf::st_as_sf(data)

      # create parameter columns
      data <- data %>%
        dplyr::mutate(
          # name the source
          provider = "CDF",
          source = x,
          # state the scenario (RCP85 for CDF)
          scenario = "RCP85",
          # identify model based on pattern in the file name
          model = dplyr::case_when(
            stringr::str_detect(x, "GFDL-ESM2M") ~ "GFDL-ESM2M",
            stringr::str_detect(x, "HadGEM2-ES") ~ "HadGEM2-ES",
            stringr::str_detect(x, "IPSL-CM5A-LR") ~ "IPSL-CM5A-LR",
            stringr::str_detect(x, "MIROC5") ~ "MIROC5",
            stringr::str_detect(x, "pr-Indices") ~ "Observed",
            stringr::str_detect(x, "tas-Indices") ~ "Observed"
          ),
          # identify period based on pattern in the file name
          period = dplyr::case_when(
            stringr::str_detect(x, "1991-2020") ~ "1991-2020",
            stringr::str_detect(x, "2021-2050") ~ "2021-2050",
            stringr::str_detect(x, "2051-2080") ~ "2051-2080",
            stringr::str_detect(x, "2071-2100") ~ "2071-2100",
          ),
          is_reference_period = dplyr::if_else(period == "1991-2020", TRUE, FALSE)
        )

      # create geometry id by using hashs
      data <- data %>%
        dplyr::mutate(geometry_id = openssl::md5(as.character(geometry)))

      # intersect data with distinct_geo_data to identify the relevant geometries and assign ald risk scores
      data <- distinct_geo_data %>%
        sf::st_join(data)
    }
  )

  # bind list together
  climate_data <- dplyr::bind_rows(climate_data)

  # assign column names variables based on index file
  variables <- data.frame(variable = names(climate_data)) %>%
    dplyr::left_join(column_name_index, by = "variable") %>%
    dplyr::mutate(
      variable_name = dplyr::if_else(is.na(variable_name), variable, variable_name)
      )

  names(climate_data) <- variables$variable_name

  # drop geometry as it is unnecessary and slows down the whole process
  climate_data <- climate_data %>%
    sf::st_drop_geometry()

  # de-select source column as it is not needed
  climate_data <- climate_data %>%
    dplyr::select(-source)

  # pivot longer as long is a better format
  climate_data <- climate_data %>%
    tidyr::pivot_longer(
      cols = !c("asset_id", "geometry_id", "model", "period", "scenario", "provider", "is_reference_period"),
      values_to = "risk_level",
      names_to = "hazard"
      )

  # calculate absolute and relative changes for each asset by creating reference value based on parameters
  climate_data <- climate_data %>%
    dplyr::group_by(asset_id, geometry_id, model, scenario, hazard) %>%
    dplyr::mutate(reference = sum(dplyr::if_else(period == "1991-2020", risk_level, 0))) %>%
    dplyr::mutate(
      absolute_change = risk_level - reference,
      relative_change = if_else(absolute_change == 0, 0, absolute_change / reference) # to avoid NaNs
    ) %>%
    dplyr::ungroup()


  # save data for sub type
  if (type_sub == "Temperature") climate_data_temperature <- climate_data
  if (type_sub == "Precipitation") climate_data_precipitation <- climate_data

  rm(climate_data, file_list_CDF_raw_geotiff_indices_sub, variables, type_sub)
}

# bind climate_data for precipitation and temperature
climate_data <- rbind.data.frame(
  climate_data_temperature,
  climate_data_precipitation
)

rm(climate_data_temperature, climate_data_precipitation, file_list_CDF_raw_geotiff_indices, column_name_index)



# ================================= # =================================
# ================================= # =================================
# QA data (it would be good if this part is the same for all different providers and data sources, but not necessary
# ================================= # =================================
# ================================= # =================================

# check assets which lie exactly between two rasters
non_distinct_assets_climate_data <- climate_data %>%
  dplyr::anti_join(
    climate_data %>%
      dplyr::distinct(asset_id, model, period, hazard, scenario, .keep_all = T),
    by = c(names(climate_data))
  )


# check NAs in climate data (possibility to kickout later)
# differences in the amount of NAs in risk_level and reference can happen if risk level is available only in the reference period
# NAs for absolute and relative changes should be same

NAs_climate_data <- climate_data %>%
  dplyr::filter(dplyr::if_any(dplyr::everything(), is.na))

# question: shall we kickout a whole group per asset_id, scenario, model, hazard if one period has missing values?
# might lead to issues later on when it is assumed that each group is complete

NAs_climate_data %>%
  dplyr::group_by(hazard) %>%
  dplyr::summarise(
    NAs_risk_level = sum(is.na(risk_level)),
    NAs_reference = sum(is.na(reference)),
    NAs_absolute_change = sum(is.na(absolute_change)),
    NAs_relative_change = sum(is.na(relative_change))
  )

# ================================= # =================================
# ================================= # =================================
# ================================= # =================================
# save data | as this part should be the same for all different providers and data sources --> always use the same function
# ================================= # =================================
# ================================= # =================================
# ================================= # =================================
for (hazard_sub in unique(climate_data$hazard)) { # using loop otherwise to computational intensive (i.e. storing 150 million rows (= 50 original data + 50 million provider + 50 million scenario))

  climate_data %>%
    filter(hazard == hazard_sub) %>%
    save_climate_data(
      path_db_pr_climate_data = path_db_pr_climate_data,
      use_distinct_for_assets_between_two_rasters = TRUE,
      drop_any_NAs = TRUE
    )
}
