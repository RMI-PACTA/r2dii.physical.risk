source("physical_risk_functions.R")

# =================================
# load distinct_geo_data which will subset the raw climate data
# =================================

distinct_geo_data <- load_distinct_geo_data()

# =================================
# load list of all countries iso codes
# =================================

all_countries <- rworldmap::countryRegions %>%
  dplyr::transmute(iso = ISO3) %>%
  dplyr::pull(iso)

# =================================
# set climate analytics parameters
# =================================

# cross parameters
api_paramter <- tidyr::crossing(
  region = c(
    "AFRICA",
    "EUROPE",
    "NORTH_AMERICA",
    "SOUTH_AMERICA",
    "ASIA",
    "OCEANIA",
    "ATA"

  ),
  indicator = c(
    "tasAdjust", # air temperature
    "tasminAdjust", # daily minimum air temperature
    "tasmaxAdjust", # daily maximum air temperature
    "prAdjust", # precipitation
    "hursAdjust", # relative humidity
     "prsnAdjust", # snowfall
    "hussAdjust", # specific humidity
    "sfcWindAdjust", # wind speed
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
  scenario = c(
    "cat_current",
    "h_cpol",
    "d_delfrag",
    "o_1p5c",
    "rcp26",
    "rcp45",
    "rcp60",
    "rcp85"
  ),
  year = c(
    2030,
    2050,
    2100
  )
)

# create links
api_paramter <- api_paramter %>%
  dplyr::mutate(
    link = paste0(
      "https://cie-api.climateanalytics.org/api/map-difference/?iso=",
      region,
      "&var=",
      indicator,
      "&season=annual&format=csv&scenarios=",
      scenario,
      "&years=",
      year
    )
  )


# define starting variables + data frame
processed_links <- 0
processed_indicators <- 0
all_duration <- NA

for (sub_indicator in unique(api_paramter$indicator)) {

  # define start time to get an estimate how much time is needed to load the data
  start_time <- Sys.time()

  # subset the links by indicator
  api_paramter_sub <- api_paramter %>%
    filter(indicator == sub_indicator)

  # create target data frame
  all_data <- data.frame(
    region = NA_character_,
    indicator = NA_character_,
    scenario = NA_character_,
    year = NA_integer_,
    link = NA_character_,
    variable_id = NA_character_,
    country = NA_character_,
    warming_level = NA_character_,
    reference = NA_character_,
    difference = NA_character_,
    gcm_gim_combinations = NA_character_,
    all_used_runs = NA_character_,
    lat = NA_character_,
    long = NA_character_,
    risk_level = NA_character_
  )

  # count processed hazards
  processed_indicators <- processed_indicators + 1

  # scrape data
  for (sub_link in api_paramter_sub$link) {

    # add an estimate of how many links have been downloaded
    processed_links <- processed_links + 1

    # message which link is currently processed
    message(paste0("Processing link Nr.", processed_links, " of ", nrow(api_paramter)))
    print(sub_link)

    # filter api_paramater to get the relevant parameters for each links
    api_paramter_sub_sub <- api_paramter_sub %>%
      dplyr::filter(link == sub_link)

    # download data
    data <- vroom::vroom(sub_link, delim = ",")

    # in case a set of parameters is not available, the downloaded data will only have one column
    # --> only proceed with data wrangling if set of parameters is available
    if(ncol(data) == 2) {

      # as names of downloaded data vary based on parameters, create agnostic variables
      names(data) <- c("v1","v2")

      # slice rows which contain parameter information
      summary <- data %>%
        dplyr::slice(c(1:7))

      # prep sumamry of parameter information
      summary <- summary %>%
        tidyr::pivot_wider(id_cols = "v2", names_from = "v1", values_from = "v2") %>%
        janitor::clean_names(case = "snake")

      # kickout rows which contain parameter information
      data <- data %>%
        slice(-c(1:7))

      # slice rows which contains the number of longitudes
      index <- data %>%
        dplyr::select(v2) %>%
        dplyr::slice(1)

      # count the number of longitudes
      index <- stringr::str_count(index$v2, ",")

      # add 1 to the number of longitudes, as there is always one comma less
      index <- index + 1

      # expand the data
      data <- data %>%
        tidyr::separate(v2,into = as.character(c(1:index)),  sep = ",")

      # use longitude as the column name
      colnames(data) <- data %>%
        dplyr::slice(1)

      # kickout row which contains the longitude
      data <- data %>%
        dplyr::slice(-c(1))

      # rename latitude
      data <- data %>%
        dplyr::rename(lat = `lat \\ lon`)

      # pivot longer
      data <- data %>%
        tidyr::pivot_longer(cols = !lat, values_to = "risk_level", names_to = "long")

      # kickout coordinates without risk data (as countries are not squares this is expected to happen)
      data <- data %>%
        dplyr::filter(risk_level != "")

      # add summary and paramter information to data
      data <- dplyr::bind_cols(
        api_paramter_sub_sub,
        summary,
        data
      )

      # bind data from previous loops to all data
      all_data <- rbind.data.frame(
        all_data,
        data
      )
    }

    # ensure that there is always a small break between downloading data
    Sys.sleep(1)

  }

  # kickput out the starting row of the target data frame
  all_data <- all_data %>%
    filter(!is.na(link))

  if(nrow(all_data > 0)) {
    # apply distinct to eliminate duplicate entries for the same centroids used at the borders of to regions / countries
    all_data <- all_data %>%
      distinct(scenario, indicator, year, long, lat, .keep_all = TRUE)

    # duplicate long lats for sf coordinates
    all_data <- all_data %>%
      mutate(
        duplicate_long = long,
        duplicate_lat = lat
      )

    # create sf coordinates / points
    all_data <- all_data %>%
      sf::st_as_sf(coords = c("duplicate_long","duplicate_lat"))

    # hash geometry
    all_data <- all_data %>%
      dplyr::mutate(geometry_id = openssl::md5(as.character(geometry)))


    # to do: verify counted grouped lat longs equals distinct lat longs


    # create sf data frame based on longitude and latitude
    all_data_distinct_geo_data <- all_data %>%
      sf::st_drop_geometry() %>%
      distinct(long, lat, geometry_id) %>%
      as_tibble()

    all_data_distinct_geo_data <- all_data_distinct_geo_data %>%
      mutate(
        long = as.numeric(long),
        lat = as.numeric(lat),
      ) %>%
      mutate(
        geometry_id = as.character(geometry_id),
        north_lat = lat + 0.5,
        south_lat = lat,
        east_lng = long + 0.5,
        west_lng = long
        # north_lat = lat + 0.5/2,
        # south_lat = lat - 0.5/2,
        # east_lng = long + 0.5/2,
        # west_lng = long - 0.5/2
      ) %>%
      as.data.frame()

    all_data_distinct_geo_data_polygons <- lapply(
      1:nrow(all_data_distinct_geo_data), function(x) {
        ## create a matrix of coordinates that also 'close' the polygon
        res <- matrix(c(all_data_distinct_geo_data[x, 'west_lng'], all_data_distinct_geo_data[x, 'north_lat'],
                        all_data_distinct_geo_data[x, 'east_lng'], all_data_distinct_geo_data[x, 'north_lat'],
                        all_data_distinct_geo_data[x, 'east_lng'], all_data_distinct_geo_data[x, 'south_lat'],
                        all_data_distinct_geo_data[x, 'west_lng'], all_data_distinct_geo_data[x, 'south_lat'],
                        all_data_distinct_geo_data[x, 'west_lng'], all_data_distinct_geo_data[x, 'north_lat'])  ## need to close the polygon
                      , ncol =2, byrow = T
        )
        ## create polygon objects
        st_polygon(list(res))

      }
    )

    all_data_distinct_geo_data <- st_sf(all_data_distinct_geo_data, st_sfc(all_data_distinct_geo_data_polygons), crs = 4326)

    asset_scenario_data <- sf::st_join(distinct_geo_data, all_data_distinct_geo_data)

    asset_scenario_data <- asset_scenario_data %>%
      filter(!is.na(geometry_id))

    asset_geometry <- asset_scenario_data %>%
      select(geometry)

    scenario_geometry <- asset_scenario_data %>%
      sf::st_drop_geometry() %>%
      sf::st_as_sf(coords = c("long","lat")) %>%
      select(geometry)

    sf::st_crs(scenario_geometry) <- 4326

    asset_scenario_data$dist <- sf::st_distance(asset_geometry, scenario_geometry, by_element=TRUE)

    asset_scenario_data <- asset_scenario_data %>%
      sf::st_drop_geometry() %>%
      select(asset_id, geometry_id)


    asset_scenario_data <- asset_scenario_data %>%
      left_join(
        all_data %>%
          mutate(geometry_id = as.character(geometry_id)),
        by = c("geometry_id")
      )

    asset_scenario_data <- asset_scenario_data %>%
      transmute(
        asset_id,
        provider = "ClimateAnalytics",
        hazard = indicator,
        scenario,
        period = year,
        model = "Ensemble",
        relative_change = risk_level,
        risk_level = NA,
        reference = NA,
        absolute_change = NA,
        geometry_id
      )


    asset_scenario_data %>%
      save_climate_data(
        path_db_pr_climate_data_provider = fs::path(path_db_pr_climate_data, "ClimateAnalytics"),
        use_distinct_for_assets_between_two_rasters = TRUE,
        drop_any_NAs = FALSE
      )

    # determine the end time
    end_time <- Sys.time()

    # calculate how long it took
    duration <- as.double(end_time) - as.double(start_time)
    all_duration <- c(all_duration, duration)

    # estimate how many minutes are left based on current processing time
    minutes_left <- round((length(unique(api_paramter$indicator)) - processed_indicators)*mean(all_duration, na.rm = TRUE)/60,0)

    # print estimated processing time which is left
    message("Roughly ", minutes_left, " minutes left")
  }

  Sys.sleep(2)
}

