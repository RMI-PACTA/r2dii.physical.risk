library(r2dii.physical.risk)
library(here)
library(qs)
library(readr)
library(dplyr)


## This script is a first draft where a page from climate analytics is downloaded, read and saved, using
## the qs package.
## Transformation on these data will be to create sf objects, to be then join with the company data,
## in another script (function).


## 1. IMPORT - Import one file from climate analytics as a test. Here is one temperature data, tasAdjust
## in Germany.

raw_climate_data <- read_csv(here("data","tasAdjust_DEU_1.5_vs_1986-2006_annual.csv"))

## 2. TRANFORM - Transform the data into a usable format to change it into a sf object.

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
    # all_countries, # explodes number of parameters but single regions have higher resolution
    "AFRICA",
    "EUROPE",
    "NORTH_AMERICA",
    "SOUTH_AMERICA",
    "ASIA",
    "OCEANIA",
    "ATA" # antarctica
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
    "cat_current", # cat current policies
    "h_cpol", # NGFS current policies
    "d_delfrag", # NGFS delayed 2°
    "o_1p5c", # NGFS net zero
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

# =================================
# set up for loop for indicator
# =================================

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

  # create lat and long start vector (used to determine raster size later)
  all_lats <- NA_integer_
  all_longs <- NA_integer_

  # count processed hazards
  processed_indicators <- processed_indicators + 1

  # =================================
  # download all links for one indicators with for loop
  # =================================

  # scrape data
for (sub_link in api_paramter_sub$link[1]) {

    # add an estimate of how many links have been downloaded
    processed_links <- processed_links + 1

    # message which link is currently processed
    message(paste0("Processing link Nr.", processed_links, " of ", nrow(api_paramter)))
    print(sub_link)

    # filter api_parameter to get the relevant parameters for each links
    api_paramter_sub_sub <- api_paramter_sub %>%
      dplyr::filter(link == sub_link)

    # download data
    data <- vroom::vroom(sub_link, delim = ",")

    # in case a set of parameters is not available, the downloaded data will only have one column
    # --> only proceed with data wrangling if set of parameters is available
    if (ncol(data) == 2) {

      # as names of downloaded data vary based on parameters, create agnostic variables
      names(data) <- c("v1", "v2")

      # slice rows which contain parameter information
      summary <- data %>%
        dplyr::slice(c(1:7))

      # prep sumamry of parameter information
      summary <- summary %>%
        tidyr::pivot_wider(names_from = "v1", values_from = "v2") %>%
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
        tidyr::separate(v2, into = as.character(c(1:index)), sep = ",")

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

      # subset lat and long
      all_lats <- unique(c(all_lats, as.numeric(data$lat))) %>% sort()
      all_longs <- unique(c(all_longs, as.numeric(data$long))) %>% sort()


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
}

  # kickput out the starting row of the target data frame
  all_data <- all_data %>%
    filter(!is.na(link))

  diff_lats <- all_lats[-1] - all_lats[-length(all_lats)]
  median_diff_lats <- median(diff_lats, na.rm = TRUE)
  cat(crayon::red("Using", median_diff_lats, "as median lat", "\n"))

  diff_longs <- all_longs[-1] - all_longs[-length(all_longs)]
  median_diff_longs <- median(diff_longs, na.rm = TRUE)
  cat(crayon::red("Using", median_diff_longs, "as median long", "\n"))

  if (nrow(all_data > 0)) {
    all_data <- all_data %>%
      mutate(
        scenario = case_when(
          scenario == "cat_current" ~ "cat_current_policies",
          scenario == "h_cpol" ~ "ngfs_current_policies",
          scenario == "d_delfrag" ~ "ngfs_delayed_2_degrees",
          scenario == "o_1p5c" ~ "ngfs_net_zero",
          #TRUE ~ == scenario,
          TRUE ~ as.character(scenario)
        ),
        model = "Ensemble", # to many models -> just say ensemble
        provider = "ClimateAnalytics"
      ) %>%
      rename(
        hazard = indicator,
        period = year
      )

    # transform risk level from percentages to decimals (100% -> 1)
    all_data <- all_data %>%
      dplyr::mutate(risk_level = as.numeric(risk_level) / 100)

    # apply distinct to eliminate duplicate entries for the same centroids used at the borders of to regions / countries
    all_data <- all_data %>%
      distinct(scenario, hazard, period, long, lat, .keep_all = TRUE)

    # duplicate long lats for sf coordinates
    all_data <- all_data %>%
      mutate(
        duplicate_long = long,
        duplicate_lat = lat
      )

    # create sf coordinates / points
    all_data <- all_data %>%
      sf::st_as_sf(coords = c("duplicate_long", "duplicate_lat"))

    # hash geometry
    all_data <- all_data %>%
      dplyr::mutate(geometry_id = openssl::md5(as.character(geometry)))

    # create sf data frame based on longitude and latitude
    all_data_distinct_geo_data <- all_data %>%
      sf::st_drop_geometry() %>%
      distinct(long, lat, geometry_id) %>%
      as_tibble()

    # save coordinates as numeric
    all_data_distinct_geo_data <- all_data_distinct_geo_data %>%
      mutate(
        long = as.numeric(long),
        lat = as.numeric(lat),
      )

    # calculate square coordinates based on climate analytics (they use the centroid as the lower left hand corner (did double check with their website and replicated their "squares"))
    all_data_distinct_geo_data <- all_data_distinct_geo_data %>%
      mutate(
        geometry_id = as.character(geometry_id),
        north_lat = lat + median_diff_lats,
        south_lat = lat,
        east_lng = long + median_diff_longs,
        west_lng = long
        # north_lat = lat + median_diff_lats/2,
        # south_lat = lat - median_diff_lats/2,
        # east_lng = long + median_diff_longs/2,
        # west_lng = long - median_diff_longs/2
      ) %>%
      as.data.frame()

    # build polygons based on calculated "corners"
    all_data_distinct_geo_data_polygons <- lapply(
      1:nrow(all_data_distinct_geo_data), function(x) {
        ## create a matrix of coordinates that also 'close' the polygon
        res <- matrix(c(
          all_data_distinct_geo_data[x, "west_lng"], all_data_distinct_geo_data[x, "north_lat"],
          all_data_distinct_geo_data[x, "east_lng"], all_data_distinct_geo_data[x, "north_lat"],
          all_data_distinct_geo_data[x, "west_lng"], all_data_distinct_geo_data[x, "south_lat"],
          all_data_distinct_geo_data[x, "west_lng"], all_data_distinct_geo_data[x, "north_lat"]
        ) ## need to close the polygon
        ,
        ncol = 2, byrow = T
        )
        ## create polygon objects
        st_polygon(list(res))
      }
    )

    all_data_distinct_geo_data <- st_sf(all_data_distinct_geo_data, st_sfc(all_data_distinct_geo_data_polygons), crs = 4326)

    qsave(all_data_distinct_geo_data, here(path_desktop, "company_distinct_geo_data.qs"))
  }
