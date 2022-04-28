library(r2dii.physical.risk)
library(dplyr)
library(tidyr)
library(tidygeocoder)
library(pastax.data)
library(here)
library(qs)
library(fs)
library(vroom)
library(pak)
library(tibble)

## re-install pastax.data regularly to be in sync with Mauro's updates

# remove.packages("pastax.data")
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

europages <- pastax.data::ep_agriculture_livestock

## 2. TRANSFORM- separate the address into two parts, with the | separator
## if there are several "|" or un-consistencies, I put them in the column called "extra"

europages <-  europages %>%
  tidyr::separate(address, into = c("address", "city", "extra"), sep="\\|", fill = "right")

europages$city <- if_else(is.na(europages$extra), europages$city, europages$extra)

europages <- europages %>%
  tidyr::separate(city, into = c("city_1","postcode") ,sep = "\\s", remove = FALSE, extra = "drop") %>%
  dplyr::select(-c("extra", "city_1"))

## 1. TIDY - remove non-existent or obsolete postcodes

tidy_europages <- europages %>%
  filter(!is.na(postcode))

# this will only retrieve postcode that are numbers, hence removing the addresses that
# does not have any postcodes.

tidy_europages_1<-tidy_europages %>%
  filter(grepl("[0-9]+", postcode))

## Change postcodes into coordinates using Open Street Map
## It is a very slow process, can take up to 8 hours passing row by row the addresses.

# company_data_osm <- qread("data/company_data_osm.qs")

#chunk the data set row by row
chunks <- nrow(tidy_europages)
chunked <- tidy_europages %>%
  mutate(chunk = as.integer(cut(row_number(), chunks)))


out <- path(here(), "output")
if (!dir_exists(out)) dir_create(out)

chunked_1 <- slice(chunked,-(1:48464))

for (i in unique(chunked_1$chunk)) {

  # 1. Pass this chunk's address into the geocode function that convert it into coordinates.
  this_chunk <- filter(chunked, chunk == i)

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
  vroom_write(this_result, path(out, paste0(i, ".csv")))

}

files <- dir_ls(out)

files_cut <- cut(files)

# split out into different part and then bind rows - could be faster / break problem into chunks
# write function that takes the path and splits the df into n numbers

list_files <- purrr::map_df(files[101:1000], ~readr::read_tsv(.,col_types = list(postcode = col_double())))

qsave(list_files, here("company_data_100.qs"))
qsave(list_files, here("company_data_101_1000.qs"))
qs_files <- dir_ls(here(), regexp = "[.]qs$")

company_data <- purrr::map_df(qs_files, qs::qread)

#FIXME : use cache

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
