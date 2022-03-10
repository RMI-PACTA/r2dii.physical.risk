library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(sf)
library(r2dii.physical.risk)
library(r2dii.utils)
library(tidygeocoder)
library(tmaptools)


# METHODOLOGY Example Workflow

## information on companies level - scraped from e.g public data source, based on Mauro's work
smes <- tribble(
   ~asset_name, ~address, ~country, ~phone_numer, ~entity_type, ~year_established, ~date_last_update, ~activity, ~activity_classification_name, ~activity_classification_code, ~corporate_capital, ~corporate_capital_currency, ~company_code, ~company_size_at_adress, ~company_size, ~company_id_external, ~certification_type, ~banks_import_area, ~turnover, ~net_profit, ~main_activity, ~secondary_activity, ~legal_address, ~legal_adress_country, ~sector, ~sector_classification_name, ~sector_classification_code,~asset_id,~company_id,
   "Mr Carl Carlos", "Lycee Pro Le Bocage Don Bosco | 69003 LYON 03", "France", "0033420042", "Headquarters", "2006", "Jan 13", "Plant propagation", "NAF08", "0130Z", 10000L, "EUR", "FR000666", "0-9", "20", "FR00007", "ISO9001", "West.Europe", 45000L, 20, "Trees", "Plants", "4040 Rue de Elephants Mame TER,69003 LYON 03", "France", "Plant propagation" , "NACE", "0130", "A1","C1"
)

## remove spaces from address
smes <-  smes %>%
  dplyr::mutate(
    address =  gsub("[[:space:]]", "", address)
  )

## separate with "|" and retrieve the postcode only (will work in EU because postcode always 5 digit)
smes <-  smes %>%
  dplyr::mutate(
      post_code = separate(smes, address, into = c("pre", "post")) %>%
        pull("post") %>%
        substring(0,5)
      )

## change postcodes into coordinates
coordinates <- smes %>%
  tidygeocoder::geocode(
    postalcode = post_code,
    country = country,
    method = "osm"
  )

company_data <- coordinates %>%
  dplyr::rename(
    longitude = long,
    latitude = lat
  )

company_data <- company_data %>%
  dplyr::mutate(
    has_geo_data = dplyr::case_when(
      is.na(latitude) | is.na(longitude) ~ FALSE,
      TRUE ~ TRUE
    )
  )

vroom::vroom_write(
  company_data,
  fs::path(
    path_db_pr_ald_prepared,
    "company_level_data",
    ext = "csv"
  ),
  delim = ","
)

get_distinct_geo_data <- function(company_data) {

  distinct_company_data <- company_data %>%
    dplyr::select(asset_id, longitude, latitude)

  # create sf data frame based on longitude and latitude #geometry are sfc_POINT
  # Note: Return sf object to a data frame by setting geometry to NULL st_set_geometry(points_sf, NULL) to remove it and convert to df/tibble again

  distinct_company_geo_data <- sf::st_as_sf(distinct_company_data, coords = c("longitude","latitude"))

  return(distinct_company_geo_data)
}

distinct_geo_company_data <- get_distinct_geo_data(company_data)

vroom::vroom_write(
  distinct_company_data,
  fs::path(
    path_db_pr_ald_distinct_geo_data,
    "company_distinct_geo_data",
    ext = "csv"
  ),
  delim = ","
)


## climate data
climate_data <- tibble::tribble(
  ~scenario,     ~model, ~period,    ~hazard, ~relative_change, ~asset_id
  "rcp26", "Ensemble",   2100L, "prAdjust",            0.004,      "A1"
)

## joining with climate data
#Q3 :OK what is the asset_id that is found again in the climate data ? Where does it comes from ? - from AR_distinct geodata that links asset_id & coordinates.

company_climate_data <- company_data %>%
  inner_join(climate_data, by = "asset_id") %>%
  select(
    company_name,
    value_eur,
    company_country,
    address,
    latitude,
    longitude,
    sector,
    year_established,
    scenario,
    model,
    period,
    hazard,
    relative_change
  )


#its not actually the portfolio_economic_value but its just to have the same name variables to fake plot
company_climate_data  <- company_climate_data  %>%
  group_by(sector) %>%
  mutate(
    portfolio_economic_value = value_eur/sum(value_eur, na.rm = TRUE),
    year = year_established
  ) %>%
  ungroup()


provider <- "Fake Data Provider"
scenario <- "rcp26"
hazard <- unique(out$hazard)
model <- unique(out$model)
period <- unique(out$period)


#useless blob just for fun
r2dii.physical.risk:::plot_sector_absolute_portfolio_economic_value(
  out,
  provider_sub = "Climate Data",
  scenario_sub = scenario,
  hazard_sub = hazard,
  model_sub = model,
  period_sub = period
)


## joining portfolio with ald
out <- portfolio %>%
  inner_join(company_data, by = c("asset_id", "company_id"))


## let's imagine a portfolio made of one SME, of which asset_id = company_id
portfolio <- tribble(
  ~portfolio_name,    ~company_name, ~company_adress, ~company_country, ~company_id, ~asset_id, ~corporate_bond_ticker,          ~isin, ~holding_id, ~asset_type, ~security_mapped_sector, ~value_eur,
  "pastax", "Mr Carl Carlos", "Lycee Pro Le Bocage Don Bosco | 69003 LYON 03", "France",    "C1",  "A1",                "ABC", "AB1234567890",        123L,    "Equity",                 "Agriculture",     4000L
)



