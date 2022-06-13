library(r2dii.physical.risk)
library(dplyr)
library(tidyr)
library(tidygeocoder)
library(tilt)
library(here)
library(qs)
library(fs)
library(vroom)
library(pak)
library(tibble)

## re-install tilt regularly to be in sync with Mauro's updates

# remove.packages("tilt")
# create_github_token()
# gitcreds::gitcreds_set()
# pak::pkg_install("2DegreesInvesting/pastax.data")


## This script will prepare the data scraped from public data sources, like for e.g Europages or Kompass.
## It will essentially transform the postcodes into latitude and longitude coordinates, and also create a
## new data file which will have only the distinct geographical data, for e.g only with latitude and longitude
## information, with an arbitrary company_id. This at the end will directly be a function, that will load any
## file and be ultimately put into a single load file that will be sourced.
## NB: Think in term in functions and packages, multiple scripts are less portable. Function also better to write the file
## into the user's path (use package here).


## 1. IMPORT - download the data from europages, from the package pastax.data

europages <- tilt::ep()

## 2. TRANSFORM- separate the address into two parts, with the | separator
## if there are several "|" or un-consistencies, I put them in the column called "extra"

sep_europages <-  europages %>%
  mutate(for_postcode = europages$address) %>%
  tidyr::separate(for_postcode, into = c("address_for_postcode", "city_1", "extra"), sep="\\|", fill = "right")

sep_europages <- sep_europages %>%
  mutate(city_1 = if_else(is.na(sep_europages$extra), sep_europages$city_1, sep_europages$extra))

sep_europages <- sep_europages %>%
  tidyr::separate(city_1, into = c("city_2","postcode") ,sep = "\\s", remove = FALSE, extra = "drop") %>%
  dplyr::select(-c("extra", "city_1", "city_2", "address_for_postcode")) %>%
  dplyr::relocate(postcode, .after = address)

## FIXME : for the NA's postcodes, there are some addresses that does not have the | separator (e.g "21910 saulon la chapelle)

nas_postcodes <- sep_europages %>%
  mutate(address_nas = sep_europages$address) %>%
  filter(is.na(postcode)) %>%
  tidyr::separate(address_nas, into = c("postcode_2","city_3") ,sep = "\\s", remove = FALSE, extra = "drop")

sep_europages_test <- sep_europages %>%
  left_join(nas_postcodes) %>%
  mutate(postcode = if_else(is.na(postcode), postcode_2, postcode)) %>%
  select(-c(postcode_2, city_3))


## 1. TIDY - remove non-existent (five of them does not have addresses) or obsolete postcodes

tidy_europages <- sep_europages_test %>%
  filter(!is.na(postcode))

# This will only retrieve postcode that are numbers, hence removing the addresses that
# does not have any postcodes.

tidy_europages <-tidy_europages %>%
  filter(grepl("[0-9]+", postcode))

## Change postcodes into coordinates using Open Street Map
## It is a very slow process, can take up to 8 hours passing row by row the addresses.

# company_data_osm <- qread("data/company_data_osm.qs")

#chunk the data set

chunks <- 5000
chunked <- tidy_europages %>%
  mutate(chunk = as.integer(cut(row_number(), chunks)))

out <- path(here(), "output")

if (!dir_exists(out)) dir_create(out)

for (i in unique(chunked$chunk)) {

  # 1. Pass this chunk's address into the geocode function that convert it into coordinates.
  this_chunk <- filter(chunked_1, chunk == i)

  this_result <- this_chunk %>%
    tidygeocoder::geocode(
    postalcode = postcode,
    country = country,
    method = "osm"
  )

  # 2. If this chunk converted nothing, move to the next chunk
  osm_nothing <- (is.na(this_result$lat) && is.na(this_result$long))
  if (osm_nothing) next()

  # 3. Else, save the result to a .csv file.
  vroom_write(this_result, path(out_2, paste0(i, ".csv")))

}

files <- dir_ls(out)

# split out into different part and then bind rows - could be faster / break problem into chunks
# write function that takes the path and splits the df into n numbers

for (i in getlength(files)) {

  this_list <- purrr::map_df(files[i:i+1000], ~readr::read_tsv(.,col_types = list(postcode = col_double())))

  i <- i+1000

  this_name <- c("company_data",i,".qs")

  qsave(this_list, here("output_company_data", this_name))
}

list_files <- purrr::map_df(files[10382:10383], ~readr::read_tsv(.,col_types = list(postcode = col_double())))

qsave(list_files, here("company_data_100.qs"))
qsave(list_files, here("company_data_101_1000.qs"))
qs_files <- dir_ls(here(), regexp = "[.]qs$")

company_data_osm <- purrr::map_df(qs_files, qs::qread)

qsave(company_data_osm, here("data","company_data_osm.qs"))

company_data_osm <- qread("data/company_data_osm.qs")

company_data <- company_data_osm %>%
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
  dplyr::select(latitude, longitude, company_name)

## save the data
qsave(distinct_company_data, here("data", "company_distinct_geo_data.qs"))
