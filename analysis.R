
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
  semi_join(asset_level_owners, by = "asset_id") %>%
  filter(between(year, 2020, 2020))

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
  left_join(climate_data %>% select(scenario, hazard, model, period, risk_level, absolute_change, relative_change, asset_id), by = "asset_id") %>%
  mutate(risk_level = if_else(is.na(risk_level), 0, round(risk_level, 0)))

# ========
# merge asset level owners + calculate direct owned economic value
# ========
analysis <- analysis %>%
  left_join(asset_level_owners, by = "asset_id") %>%
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

eq_portfolio <- eq_portfolio %>%
  left_join(
    eq_portfolio %>%
      distinct(holding_id, .keep_all = T) %>%
      group_by(portfolio_name) %>%
      mutate(
        portfolio_market_value = sum(value_usd, na.rm = T),
        port_weight = value_usd / portfolio_market_value
      ) %>%
      ungroup() %>%
      select(holding_id, port_weight),
    by = "holding_id"
  )

# calculate ownership weight
analysis <- analysis %>%
  mutate(ownership_weight = number_of_shares / current_shares_outstanding_all_classes)

analysis <- analysis %>%
  left_join(
    analysis %>%
      distinct(holding_id, .keep_all = T) %>%
      group_by(portfolio_name) %>%
      mutate(
        portfolio_market_value = sum(value_usd, na.rm = T),
        port_weight = value_usd / portfolio_market_value
      ) %>%
      ungroup() %>%
      select(holding_id, port_weight),
    by = "holding_id"
  )

# calculate port weight
analysis <- analysis %>%
  mutate(portfolio_final_owned_economic_value = if_else(stringr::str_detect(sector, financial_sector), ownership_weight*company_final_owned_economic_value, 0))

# calculate portfolio_final_owned_economic_value_share_technology
analysis <- analysis %>%
  group_by(portfolio_name, hazard, model, period, sector, technology, year) %>%
  mutate(portfolio_final_owned_economic_value_share_technology = portfolio_final_owned_economic_value/sum(portfolio_final_owned_economic_value, na.rm = T)) %>%
  ungroup()

# calculate portfolio_final_owned_economic_value_share_technology_company
analysis <- analysis %>%
  group_by(portfolio_name, company_name, hazard, model, period, sector, technology, year) %>%
  mutate(portfolio_final_owned_economic_value_share_technology_company = portfolio_final_owned_economic_value/sum(portfolio_final_owned_economic_value, na.rm = T)) %>%
  ungroup()

# calculate portfolio_final_owned_economic_value_share_sector
analysis <- analysis %>%
  group_by(portfolio_name, hazard, model, period, sector, year) %>%
  mutate(portfolio_final_owned_economic_value_share_sector = portfolio_final_owned_economic_value/sum(portfolio_final_owned_economic_value, na.rm = T)) %>%
  ungroup()

# calculate portfolio_final_owned_economic_value_share_sector_company
analysis <- analysis %>%
  group_by(portfolio_name, company_name, hazard, model, period, sector, year) %>%
  mutate(portfolio_final_owned_economic_value_share_sector_company = portfolio_final_owned_economic_value/sum(portfolio_final_owned_economic_value, na.rm = T)) %>%
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

for(hazard in unique(analysis$hazard)[!is.na(unique(analysis$hazard))]) {

  sub_hazard <- hazard

  plot <- analysis %>%
    filter(hazard == sub_hazard, model == "MIROC5", period == "2071-2100") %>%
    arrange(relative_change) %>%
    ggplot() +
    geom_col(aes(x = year, y = portfolio_final_owned_economic_value_share_technology, fill = relative_change)) +
    scale_fill_gradientn(colors = rev(RColorBrewer::brewer.pal(11, "RdBu")), breaks = c(-1, -0.5, 0, 0.5, 1), limits = c(-1,1)) +
    scale_y_continuous(labels = scales::percent) +
    facet_grid(portfolio_name ~ sector)  +
    theme_minimal() +
    theme(
      text = element_text(size = 12)
    ) +
    labs(title = sub_hazard)

  print(plot)

}

for(hazard in unique(analysis$hazard)[!is.na(unique(analysis$hazard))]) {

  sub_hazard <- hazard

  plot <- analysis %>%
    filter(hazard == sub_hazard, model == "MIROC5", period == "2071-2100") %>%
    arrange(relative_change) %>%
    ggplot() +
    geom_col(aes(x = year, y = portfolio_final_owned_economic_value_share_sector, fill = relative_change)) +
    scale_fill_gradientn(colors = rev(RColorBrewer::brewer.pal(11, "RdBu")), breaks = c(-1, -0.5, 0, 0.5, 1), limits = c(-1,1)) +
    scale_y_continuous(labels = scales::percent) +
    facet_grid(portfolio_name ~ sector)  +
    theme_minimal() +
    theme(
      text = element_text(size = 12)
    )
  print(plot)

}


for(hazard in unique(analysis$hazard)[!is.na(unique(analysis$hazard))]) {

  sub_hazard <- hazard

  plot <- analysis %>%
    filter(hazard == sub_hazard, model == "MIROC5", period == "2071-2100") %>%
    distinct(portfolio_name, hazard, model, period, asset_id, year, .keep_all = T) %>%
    count(portfolio_name, hazard, model, period, sector, technology, year, relative_change) %>%
    arrange(relative_change) %>%
    ggplot() +
    geom_col(aes(x = year, y = n, fill = relative_change)) +
    scale_fill_gradientn(colors = rev(RColorBrewer::brewer.pal(11, "RdBu")), breaks = c(-1, -0.5, 0, 0.5, 1), limits = c(-1,1)) +
    theme_minimal() +
    facet_wrap(portfolio_name ~ sector, scales = "free", nrow = 2) +
    labs(
      x = "Year",
      y = "Number of assets"
    )

  print(plot)

}


analysis %>%
  filter(!is.na(model)) %>%
  for_loops_climate_data(
    parent_path = fs::path(here::here(), "test"),
    fns = function(x, final_path) {
      plot <- x %>%
        plot_sector_absolute_portfolio_final_owned_economic_value() +
        scale_fill_relative_risk()

      ggsave(fs::path(final_path, paste(scenario_sub, hazard_sub, model_sub, period_sub), ext = "png"))

    }
  )

