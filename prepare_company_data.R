library(r2dii.physical.risk)
library(dplyr)
library(tidyr)
library(tidygeocoder)
library(pastax.data)
library(here)

# devtools::install_github("2DegreesInvesting/pastax.data") install it regularely to be in sync with Mauro's update

## This script will prepare the data scraped from public data sources, like for e.g Europages or Kompass.
## It will essentially transform the postcodes into latitude and longitude coordinates, and also create a
## new data file which will have only the distinct geographical data, for e.g only with latitude and longitude
## information, with an arbitrary company_id. This at the end will directly be a function, that will load any
## file and be ultimately put into a single load file that will be sourced.
## NB: Think in term in functions and packages, multiple scripts are less portable. Function also better to write the file
## into the user's path (use package here).


## 1. IMPORT - download the data from europages, from the package pastax.data

europages <- pastax.data::europages_agriculture_livestock_demo

## 2. TIDY - separate the address into two parts, with the | separator

europages <-  europages %>%
  tidyr::separate(address, into = c("address", "city", "extra"), sep="\\|", fill = "right")

## if there are several "|" or un-consistencies

europages$city <- if_else(is.na(europages$extra), europages$city, europages$extra)

europages <- europages %>%
  tidyr::separate(city, into = c("city","postcode") ,sep = "\\s", extra = "drop") %>%
  dplyr::select(-c("city", "extra"))

## change postcodes into coordinates using Open Street Map

company_data <- europages %>%
  tidygeocoder::geocode(
    postalcode = postcode,
    country = company_country,
    method = "osm"
  )

company_data <- company_data %>%
  dplyr::rename(
    longitude = long,
    latitude = lat
  ) %>%
  dplyr::mutate(
    has_geo_data = dplyr::case_when(
      is.na(latitude) | is.na(longitude) ~ FALSE,
      TRUE ~ TRUE
    )
  )

## create new data with only the latitude and longitude with a company_id

distinct_company_data <- company_data %>%
  dplyr::filter(has_geo_data == TRUE) %>%
  dplyr::select("latitude","longitude")#, "company_id")

vroom::vroom_write(
  distinct_company_data,
  fs::path(
    # create new path in the Dropbox only for PASTAX ? - set_up_path script ?
    # path_db_pr_ald_distinct_geo_data,
    # here("company_data", "company_distinct_geo_data.csv")
    "company_distinct_geo_data",
    ext = "csv"
  ),
  delim = ","
)



