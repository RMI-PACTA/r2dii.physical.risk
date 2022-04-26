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

## 2. TIDY - separate the address into two parts, with the | separator
## if there are several "|" or un-consistencies, I put them in the column called "extra"

europages <-  europages %>%
  tidyr::separate(address, into = c("address", "city", "extra"), sep="\\|", fill = "right")

europages$city <- if_else(is.na(europages$extra), europages$city, europages$extra)

europages <- europages %>%
  tidyr::separate(city, into = c("city_1","postcode") ,sep = "\\s", remove = FALSE, extra = "drop") %>%
  dplyr::select(-c("extra", "city_1"))

## FIXME Santo and Albeda does not have any postcodes - do not know how to remove the rows yet.

tidy_europages <- europages %>%
  filter(!is.na(postcode))

## change postcodes into coordinates using Open Street Map
## FIXME see how the speed up the process -  Mauro : it is very slow (4 hours to convert all the addresses. + there is a maximum
## (2500) of demands per day. Also for every chunk (230 rows), there are less that are passed (like 180-199).
## I would like to filter first before putting them into the functions...)

# company_data_osm <- qread("data/company_data_osm.qs")

chunks <- nrow(tidy_europages)
chunked <- tidy_europages %>%
  mutate(chunk = as.integer(cut(row_number(), chunks)))

## FIXME how to use here function here ?
# out <- here("osm_data")

here()

out <- path(here(), "output")
if (!dir_exists(out)) dir_create(out)

chunked_1 <- slice(chunked,-(1:8489))

for (i in unique(chunked_1$chunk)) {

  # 1. Match this chunk against the entire `ald` dataset.
  this_chunk <- filter(chunked, chunk == i)

  this_result <- this_chunk %>%
    tidygeocoder::geocode(
    postalcode = postcode,
    country = country,
    method = "osm"
  )

  # 2. If this chunk matched nothing, move to the next chunk
  osm_nothing <- nrow(this_result) == 0L
  if (osm_nothing) next()

  # 3. Else, save the result to a .csv file.
  vroom_write(this_result, path(out, paste0(i, ".csv")))

}

company_data_osm <- vroom(dir_ls(out))

company_data_osm <- tidy_europages %>%
  tidygeocoder::geocode(
    postalcode = postcode,
    country = country,
    method = "osm"
  )

#FIXME : arrow package - see dependencies
#use cache - pin package // arrow error

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

## out of 5062 companies, 4909 have long and lat coordinates

distinct_company_data <- company_data %>%
  dplyr::filter(has_geo_data == TRUE) %>%
  dplyr::select(latitude, longitude, company_name)

## save the data
qsave(distinct_company_data, here("data", "company_distinct_geo_data.qs"))
