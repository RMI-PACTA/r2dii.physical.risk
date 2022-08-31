library(r2dii.physical.risk)
library(arrow)
library(dplyr)
library(tidygeocoder)
library(tiltData)
library(here)
library(qs)
library(fs)
library(vroom)
library(pak)
library(tibble)

source("R/load.R")

## re-install tiltData package regularly to be in sync with Mauro's updates

# remove.packages("tilt")
# usethis::create_github_token()
# gitcreds::gitcreds_set()
# pak::pkg_install("2DegreesInvesting/tiltData@*release")

#Version 0.0.2 - 2022-08-24
# packageVersion("tiltData")

## This script will prepare the data scraped from public data sources, for now we are using Europages.
## It will essentially transform the postcodes into latitude and longitude coordinates, and also create a
## new data file which will have only the distinct geographical data, so only latitude and longitude coordinates.
## This at the end will directly be a function, that will load any file and be ultimately put into a single load file that will be sourced.
## NB: Think in term in functions and packages, multiple scripts are less portable. Function also better to write the file
## into the user's path (use package here).


## 1. IMPORT - download the data from Europages, from the package tiltData

ep <- open_dataset("~/Downloads/ep")

german_companies <- ep |>
  filter(country == "germany") |>
  distinct(id, company_name, company_city, postcode, country)

companies <- german_companies |> collect()

## 1. TIDY - remove non-existent postcodes
#FIXME: Create an issue on Github: is it the code or Europages ?

tidy_europages <- companies %>%
  filter(!is.na(postcode))

tidy_europages <- tidy_europages %>%
  mutate(postcode = as.numeric(postcode))

## Change postcodes into coordinates using Open Street Map
## It is a slow process, can take up to 6 hours passing row by row the addresses.

#chunk the data set

unique_postcode <- tidy_europages %>%
  select(postcode, country) %>%
  distinct()

chunks <- 150
chunked <- unique_postcode %>%
  mutate(chunk = as.integer(cut(row_number(), chunks)))

out <- path(here(), "output_osm_germany")

if (!dir_exists(out)) dir_create(out)

for (i in unique(chunked$chunk)) {

  # 1. Pass this chunk's address into the geocode function that convert it into lat and long coordinates.
  this_chunk <- filter(chunked, chunk == i)

  this_result <- this_chunk %>%
    tidygeocoder::geocode(
    postalcode = postcode,
    country = country,
    method = "osm"
  )

  # 2. Save the results to a .csv file.
  vroom_write(this_result, path(out, paste0(i, ".csv")))
}

files <- dir_ls(out)

# split out into different part and then bind rows - could be faster / break problem into chunks
# write function that takes the path and splits the df into n numbers

#FIXME : the for loop does not read every files

output_path <- here("ouput_company_data_ger")

if (!dir_exists(output_path)) dir_create(output_path)

for (i in seq(length(files))) {

  this_list <- purrr::map_df(files[i:i+10], ~readr::read_tsv(.,col_types = list(postcode = col_double())))

  i <- i+10

  this_name <- paste0("company_data_",i,".qs")

  qsave(this_list, path(output_path, this_name))
}

this_list <- purrr::map_df(files, ~readr::read_tsv(.,col_types = list(postcode = col_double())))

qsave(this_list, path(output_path, "company_data_ger.qs"))

qs_files <- dir_ls(output_path, regexp = "company_data.qs")

qs_files <- dir_ls(output_path, regexp = "[.]qs$")

company_data_osm <- purrr::map_df(qs_files, qs::qread)

qsave(company_data_osm, here("data","company_data_osm_ger.qs"))

# company_data_osm <- qread("data/company_data_osm.qs")

company_data <- company_data_osm %>%
  dplyr::rename(
    longitude = long,
    latitude = lat
  ) %>%
  dplyr::mutate(
    has_geo_data = dplyr::case_when(
      is.na(latitude) | is.na(longitude) ~ FALSE,
      TRUE ~ TRUE
    ))


## create new data with only the latitude and longitude with a company_id. id is the unique identifer per company.

companies <- tidy_europages %>%
  select(company_name, country, postcode, id, company_city) %>%
  distinct()

distinct_company_data <- companies %>%
  left_join(company_data, by = c("country", "postcode")) %>%
  dplyr::select(latitude, longitude, company_name, id, has_geo_data, country, company_city) %>%
  filter(has_geo_data == TRUE)

## coverage of 100 - 9.5 % = 90.5 %
coverage <- (nrow(companies) - nrow(distinct_company_data))/nrow(companies)

## NO COORDINATES :

no_coords_companies <- companies |>
  left_join(company_data, by = c("country", "postcode")) %>%
  dplyr::select(company_name,has_geo_data, id, country, company_city) %>%
  filter(has_geo_data == FALSE) |>


coords_companies <- no_coords_companies %>%
  sample_n(1000) |>
  tidygeocoder::geocode(
    city = company_city,
    country = country,
    method = "osm"
  )


## save the data
qsave(distinct_company_data, here("data", "company_distinct_geo_data_germany.qs"))

