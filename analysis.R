
# =================================
# subset relevant data
# =================================

# ========
# subset relevant target companies
# ========
company_ownership_tree <- company_ownership_tree %>%
  semi_join(total_portfolio, by = c("target_company_id" = "company_id"))

# ========
# subset relevant asset owners
# ========
asset_level_owners <- asset_level_owners %>%
  semi_join(company_ownership_tree, by = "company_id")

# ========
# subset relevant ALD
# ========
ald <- ald %>%
  semi_join(asset_level_owners, by = "asset_id")

# ========
# subset climate data for relevant ALD
# ========
climate_data <- climate_data %>%
  semi_join(asset_level_owners, by = "asset_id")




# =================================
# merge data
# =================================

# ========
# merge ALD with Climate Data
# ========
analysis <- ald %>%
  left_join(climate_data, by = "asset_id")

# ========
# merge asset level owners + calculate direct owned economic value
# ========
analysis <- analysis %>%
  left_join(asset_level_owners, by = "asset_id")

analysis <- analysis %>%
  mutate(direct_owned_economic_value = economic_value*(ownership_share/100))

# ========
# merge ownership tree
# ========
analysis <- analysis %>%
  left_join(company_ownership_tree %>% filter(ownership_level >= 0), by = "company_id")

# clean linking stake + calculate company_final_owned_economic_value
analysis <- analysis %>%
  mutate(linking_stake = if_else(is.na(linking_stake), 100, linking_stake)) %>%
  mutate(company_final_owned_economic_value = linking_stake/100*direct_owned_economic_value)


analysis <-  eq_portfolio %>%
  rename(target_company_id = company_id) %>%
  left_join(
    analysis,
    by = "target_company_id"
  )



# ========
# add final indicators
# ========

# calculate port weight
analysis <- analysis %>%
  mutate(portfolio_final_owned_economic_value = if_else(sector == security_mapped_sector, ownership_weight*company_final_owned_economic_value, 0))

# calculate portfolio_final_owned_economic_value_share_technology
analysis <- analysis %>%
  group_by(portfolio_name, provider, hazard, model, period, sector, technology, year) %>%
  mutate(portfolio_final_owned_economic_value_share_technology = portfolio_final_owned_economic_value / sum(portfolio_final_owned_economic_value, na.rm = T)) %>%
  ungroup()

# calculate portfolio_final_owned_economic_value_share_technology_company
analysis <- analysis %>%
  group_by(portfolio_name, provider, company_name, hazard, model, period, sector, technology, year) %>%
  mutate(portfolio_final_owned_economic_value_share_technology_company = portfolio_final_owned_economic_value / sum(portfolio_final_owned_economic_value, na.rm = T)) %>%
  ungroup()

# calculate portfolio_final_owned_economic_value_share_sector
analysis <- analysis %>%
  group_by(portfolio_name, provider, hazard, model, period, sector, year) %>%
  mutate(portfolio_final_owned_economic_value_share_sector = portfolio_final_owned_economic_value / sum(portfolio_final_owned_economic_value, na.rm = T)) %>%
  ungroup()

# calculate portfolio_final_owned_economic_value_share_sector_company
analysis <- analysis %>%
  group_by(portfolio_name, provider, company_name, hazard, model, period, sector, year) %>%
  mutate(portfolio_final_owned_economic_value_share_sector_company = portfolio_final_owned_economic_value / sum(portfolio_final_owned_economic_value, na.rm = T)) %>%
  ungroup()


# create overview stats
has_ald <- analysis %>%
  filter(!is.na(asset_id)) %>%
  distinct(portfolio_name, company_name, asset_id) %>%
  group_by(portfolio_name, company_name) %>%
  summarise(number_of_assets = n()) %>%
  mutate(has_ald = TRUE)

has_ald_with_geo_data <- analysis %>%
  filter(!is.na(asset_id), has_geo_data == TRUE) %>%
  distinct(portfolio_name, company_name, asset_id) %>%
  group_by(portfolio_name, company_name) %>%
  summarise(number_of_assets_with_geo_data = n()) %>%
  mutate(has_geo_ald = TRUE)

# join overview stats to eq portfolio
eq_portfolio <- eq_portfolio %>%
  left_join(has_ald, by = c("portfolio_name", "company_name")) %>%
  mutate(has_ald = if_else(is.na(has_ald), FALSE, TRUE)) %>%
  left_join(has_ald_with_geo_data, by = c("portfolio_name", "company_name")) %>%
  mutate(has_geo_ald = if_else(is.na(has_geo_ald), FALSE, TRUE)) %>%
  assertr::verify(nrow(.) == nrow(eq_portfolio))


# plot overview stats
eq_portfolio %>%
  group_by(portfolio_name, has_ald) %>%
  summarise(
    sum_value_usd_mio = sum(value_usd, na.rm = T)/10^6,
  ) %>%
  ungroup() %>%
  group_by(portfolio_name) %>%
  mutate(
    share_value_usd = sum_value_usd_mio/sum(sum_value_usd_mio, na.rm = T),
  ) %>%
  ggplot() +
  geom_col(aes(x = portfolio_name, y = share_value_usd, fill = has_ald)) +
  scale_y_continuous(sec.axis = sec_axis(~ . * sum(eq_portfolio %>% filter(portfolio_name %in% c("IE00B4L5Y983")) %>% pull(value_usd))/10^6)) +
  labs(
    x = "",
    y = "% Portfolio Value",
    title = "Portfolio Value of holdings associated with at least one ALD",
    fill = "Has ALD"
  ) +
  theme_minimal()

# plot overview stats
eq_portfolio %>%
  group_by(portfolio_name, has_ald) %>%
  summarise(
    n = n()
  ) %>%
  ungroup() %>%
  group_by(portfolio_name) %>%
  mutate(
    n = n/sum(n, na.rm = T)
  ) %>%
  ggplot() +
  geom_col(aes(x = portfolio_name, y = n, fill = has_ald)) +
  scale_y_continuous(sec.axis = sec_axis(~ . * nrow(eq_portfolio %>% filter(portfolio_name %in% c("IE00B4L5Y983"))))) +
  labs(
    x = "",
    y = "% Portfolio Holdings",
    title = "Holdings associated with at least one ALD",
    fill = "Has ALD"
  ) +
  theme_minimal()


#


analysis %>%
  mutate(is_reference_period = dplyr::if_else(period == "1991-2020", TRUE, FALSE)) %>%
  filter(is_reference_period == FALSE) %>%
  for_loops_climate_data(
    parent_path = fs::path(here::here(), "test"),
    fns = function(data, final_path) {

      ####### asset_risk_histgram
      asset_risk_histgram <- data %>%
        plot_asset_risk_histgram() +
        scale_fill_relative_risk() +
        theme(
          plot.background = element_rect(fill = "white")
        )

      save_plot(name = "asset_risk_histgram", final_path = final_path)

      ####### company_risk_distribution
      company_risk_distribution <- data %>%
        plot_company_risk_distribution() +
        scale_fill_relative_risk() +
        theme(
          plot.background = element_rect(fill = "white")
        )

      save_plot(name = "company_risk_distribution", final_path = final_path)

      ####### portfolio_company_risk_distribution
      portfolio_company_risk_distribution <- data %>%
        plot_portfolio_company_risk_distribution() +
        scale_fill_relative_risk() +
        theme(
          plot.background = element_rect(fill = "white")
        )

      save_plot(name = "portfolio_company_risk_distribution", final_path = final_path)

      ####### number_of_assets
      number_of_assets <- data %>%
        plot_sector_number_of_assets() +
        scale_fill_relative_risk() +
        theme(
          plot.background = element_rect(fill = "white")
        )

      save_plot(name = "number_of_assets", final_path = final_path)

      ####### relative_sector_production
      relative_sector_production <- data %>%
        plot_sector_relative_portfolio_final_owned_economic_value() +
        scale_fill_relative_risk() +
        theme(
          plot.background = element_rect(fill = "white")
        )

      save_plot(name = "relative_sector_production", final_path = final_path)

      ####### absolute_sector_production
      absolute_sector_production <- data %>%
        plot_sector_absolute_portfolio_final_owned_economic_value() +
        scale_fill_relative_risk() +
        theme(
          plot.background = element_rect(fill = "white")
        )

      save_plot(name = "absolute_sector_production", final_path = final_path)

    }
  )

