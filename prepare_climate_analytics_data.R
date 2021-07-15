source("physical_risk/physical_risk_functions.R")

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
  region = all_countries,
  indicator = c(
    "tasAdjust"
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

# create link
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


# =================================
# prep webscraping data
# =================================
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

processed_links <- 0

# =================================
# scrape data
# =================================
for (sub_link in api_paramter$link) {

  # define start time to get an estimate how much time is needed to load the data
  start_time <- Sys.time()

  # add an estimate of how many links have been downloaded
  processed_links <- processed_links + 1

  # message which link is currently processed
  message(paste0("Processing link Nr.", processed_links, " of ", nrow(api_paramter)))
  print(sub_link)

  # filter api_paramater to get the relevant parameters for each links
  api_paramter_sub <- api_paramter %>%
    filter(link == sub_link)

  # download data
  data <- vroom::vroom(sub_link, delim = ",")

  # in case a set of parameters is not available, the downloaded data will only have one column
  # --> only proceed data wrangling if set of parameters is available
  if(ncol(data) == 2) {

    # as names of downloaded data vary based on parameters, create agnostic variables
    names(data) <- c("v1","v2")

    # slice rows which contain parameter information
    summary <- data %>%
      slice(c(1:7))

    # prep sumamry of parameter information
    summary <- summary %>%
      pivot_wider(id_cols = "v2", names_from = "v1", values_from = "v2") %>%
      janitor::clean_names(case = "snake")

    # kickout rows which contain parameter information
    data <- data %>%
      slice(-c(1:7))

    # slice rows which contains the number of longitudes
    index <- data %>% select(v2) %>% slice(1)

    # count the number of longitudes
    index <- stringr::str_count(index$v2, ",")

    # add 1 to the number of longitudes, as there is always one comma less
    index <- index + 1

    # expand the data
    data <- data %>%
      tidyr::separate(v2,into = as.character(c(1:index)),  sep = ",")

    # use longitude as the column name
    colnames(data) <- data %>% slice(1)

    # kickout row which contains the longitude
    data <- data %>% slice(-c(1))

    # rename latitude
    data <- data %>%
      rename(lat = `lat \\ lon`)

    # pivot longer
    data <- data %>%
      pivot_longer(cols = !lat, values_to = "risk_level", names_to = "long")

    # kickout coordinates without risk data (as countries are not squares this is expected to happen)
    data <- data %>%
      filter(risk_level != "")

    # add summary and paramter information to data
    data <- bind_cols(
      api_paramter_sub,
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
  Sys.sleep(0.2)

  # determine the end time
  end_time <- Sys.time()

  # calculate how long it took
  duration <- as.double(end_time) - as.double(start_time)

  # estimate how many minutes are left based on current processing time
  minutes_left <- round(((nrow(api_paramter)) - processed_links)*duration/60,0)

  # print estimated processing time which is left
  message("Roughly ", minutes_left, " minutes left")

}

all_data <- all_data %>%
  filter(!is.na(link))

# dupliactes for country
all_data <- all_data %>%
  distinct(scenario, indicator, year, long, lat, .keep_all = TRUE)

all_data_test <- all_data %>%
  mutate(
    duplicate_long = long,
    duplicate_lat = lat
  )


all_data_test <- all_data_test %>%
  sf::st_as_sf(coords = c("duplicate_long","duplicate_lat"))

all_data_test <- all_data_test %>%
  dplyr::mutate(geometry_id = openssl::md5(as.character(geometry)))


# to do: verify counted grouped lat longs equals distinct lat longs


# create sf data frame based on longitude and latitude
all_data_distinct_geo_data <- all_data_test %>%
  sf::st_drop_geometry() %>%
  distinct(long, lat, geometry_id) %>%
  mutate(
    duplicate_long = long,
    duplicate_lat = lat
  ) %>%
  as_tibble()

all_data_distinct_geo_data <- sf::st_as_sf(all_data_distinct_geo_data, coords = c("duplicate_long","duplicate_lat"))

# assign crs to enable intersecting
sf::st_crs(all_data_distinct_geo_data) <- 4326

test <- sf::st_join(distinct_geo_data, all_data_distinct_geo_data, sf::st_nearest_feature)


asset_geometry <- test %>%
  select(geometry)


scenario_geometry <- test %>%
  sf::st_drop_geometry() %>%
  sf::st_as_sf(coords = c("long","lat")) %>%
  select(geometry)

sf::st_crs(scenario_geometry) <- 4326


dist = sf::st_distance(distinct_geo_data, all_data_distinct_geo_data[nearest,], by_element=TRUE)

test$dist <- sf::st_distance(asset_geometry, scenario_geometry, by_element=TRUE)

test <- test %>%
  sf::st_drop_geometry() %>%
  select(asset_id, geometry_id)


test <- test %>%
  left_join(all_data_test, by = c("geometry_id"))


test <- test %>%
  transmute(
    asset_id,
    provider = "ClimateAnalytics",
    hazard = indicator,
    scenario,
    period = year,
    model = gcm_gim_combinations,
    relative_change = risk_level,
    risk_level = NA,
    reference = NA,
    absolute_change = NA,
    geometry_id
  )


test %>%
  save_climate_data(
    path_db_pr_climate_data_provider = fs::path(path_db_pr_climate_data, "ClimateAnalytics"),
    use_distinct_for_assets_between_two_rasters = TRUE,
    drop_any_NAs = FALSE
  )
