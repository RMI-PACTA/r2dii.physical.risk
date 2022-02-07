scale_fill_relative_risk <- function() {
  ggplot2::scale_fill_gradientn(
    colors = rev(RColorBrewer::brewer.pal(11, "RdBu")),
    # breaks = c(-2, -1 , 0, 1, 2),
    # limits = c(-2,2),
    na.value = "grey50",
    labels = scales::percent
  )
}

plot_sector_absolute_portfolio_economic_value <- function(data,
                                                          provider_sub,
                                                          scenario_sub,
                                                          hazard_sub,
                                                          model_sub,
                                                          period_sub,
                                                          text_size = 12) {
  data %>%
    dplyr::arrange(relative_change) %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(aes(x = as.character(year), y = portfolio_economic_value, fill = relative_change)) +
    ggplot2::theme_minimal() +
    ggplot2::facet_wrap(~sector, scales = "free", nrow = 1) +
    ggplot2::labs(
      x = "Year",
      caption = paste(
        "Parameter:",
        provider_sub,
        scenario_sub,
        hazard_sub,
        model_sub,
        period_sub
      ),
      y = "",
      title = "Absolute Sector Production"
    ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white"),
      text = ggplot2::element_text(size = text_size)
    )
}

plot_sector_relative_portfolio_economic_value <- function(data,
                                                          provider_sub,
                                                          scenario_sub,
                                                          hazard_sub,
                                                          model_sub,
                                                          period_sub,
                                                          text_size = 12) {
  data %>%
    dplyr::arrange(relative_change) %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(aes(x = as.character(year), y = portfolio_economic_value_share_sector, fill = relative_change)) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::facet_grid(~sector) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Year",
      caption = paste(
        "Parameter:",
        provider_sub,
        scenario_sub,
        hazard_sub,
        model_sub,
        period_sub
      ),
      y = "",
      title = "Share Sector Production"
    ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white"),
      text = ggplot2::element_text(size = text_size)
    )
}

plot_sector_number_of_assets <- function(data,
                                         provider_sub,
                                         scenario_sub,
                                         hazard_sub,
                                         model_sub,
                                         period_sub,
                                         text_size = 12) {
  data %>%
    dplyr::distinct(portfolio_name, hazard, model, period, asset_id, year, .keep_all = T) %>% # some assets producing different technologies (automotive!!)
    dplyr::count(portfolio_name, hazard, model, period, sector, technology, year, relative_change) %>%
    dplyr::arrange(relative_change) %>%
    ggplot() +
    ggplot2::geom_col(aes(x = as.character(year), y = n, fill = relative_change)) +
    ggplot2::theme_minimal() +
    ggplot2::facet_wrap(~sector, scales = "free", nrow = 1) +
    ggplot2::labs(
      x = "Year",
      caption = paste(
        "Parameter:",
        provider_sub,
        scenario_sub,
        hazard_sub,
        model_sub,
        period_sub
      ),
      y = "",
      title = "Number of assets"
    ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white"),
      text = ggplot2::element_text(size = text_size)
    )
}

plot_portfolio_company_risk_distribution <- function(data,
                                                     provider_sub,
                                                     scenario_sub,
                                                     hazard_sub,
                                                     model_sub,
                                                     period_sub,
                                                     text_size = 12) {
  data <- data %>%
    dplyr::group_by(id_name, port_weight, relative_change) %>%
    dplyr::summarise(
      portfolio_economic_value_share_sector_company = sum(portfolio_economic_value_share_sector_company, na.rm = T), .groups = "keep"
    ) %>%
    dplyr::mutate(new_metric = port_weight * portfolio_economic_value_share_sector_company) %>%
    dplyr::arrange(relative_change)

  data %>%
    ggplot() +
    ggplot2::geom_col(aes(x = stats::reorder(id_name, port_weight), y = new_metric, fill = relative_change)) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90)
    ) +
    ggplot2::labs(
      x = "Year",
      caption = paste(
        "Parameter:",
        provider_sub,
        scenario_sub,
        hazard_sub,
        model_sub,
        period_sub
      ),
      y = "% Portfolio Weight",
      title = "Risk distribution among the biggest companies in the portfolio"
    ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white"),
      text = ggplot2::element_text(size = text_size)
    )
}


plot_company_risk_distribution <- function(data,
                                           provider_sub,
                                           scenario_sub,
                                           hazard_sub,
                                           model_sub,
                                           period_sub,
                                           text_size = 12) {
  sub_set <- data %>%
    dplyr::distinct(holding_id, .keep_all = T) %>%
    dplyr::group_by(id_name) %>%
    dplyr::summarise(port_weight = sum(port_weight, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::slice_max(port_weight, n = 10) %>%
    dplyr::arrange(dplyr::desc(port_weight)) %>% # necessary as often same weight
    dplyr::slice(c(1:10)) %>% # necessary as often same weight
    dplyr::select(id_name, port_weight)

  data <- data %>%
    dplyr::select(-port_weight) %>%
    dplyr::inner_join(
      sub_set,
      by = "id_name"
    )

  data <- data %>%
    dplyr::mutate(id_name = paste(round(port_weight * 100, 2), "% ", id_name)) %>%
    dplyr::group_by(id_name, port_weight, relative_change) %>%
    dplyr::summarise(
      portfolio_economic_value_share_sector_company = sum(portfolio_economic_value_share_sector_company, na.rm = T), .groups = "keep"
    ) %>%
    dplyr::arrange(relative_change)

  data %>%
    ggplot() +
    ggplot2::geom_col(aes(x = stats::reorder(id_name, port_weight), y = portfolio_economic_value_share_sector_company, fill = relative_change)) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Year",
      caption = paste(
        "Parameter:",
        provider_sub,
        scenario_sub,
        hazard_sub,
        model_sub,
        period_sub
      ),
      y = "",
      title = "Risk distribution among the biggest companies in the portfolio"
    ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white"),
      text = ggplot2::element_text(size = text_size)
    )
}

plot_asset_risk_histgram <- function(data,
                                     provider_sub,
                                     scenario_sub,
                                     hazard_sub,
                                     model_sub,
                                     period_sub,
                                     text_size = 12) {
  data %>%
    dplyr::mutate(relative_change = round(relative_change, 1)) %>%
    dplyr::count(sector, relative_change) %>%
    ggplot() +
    ggplot2::geom_col(aes(x = relative_change, y = n, fill = relative_change), position = "dodge") +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Relative Change",
      caption = paste(
        "Parameter:",
        provider_sub,
        scenario_sub,
        hazard_sub,
        model_sub,
        period_sub
      ),
      y = "",
      title = "Risk distribution among the identified assets in the portfolio"
    ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white"),
      text = ggplot2::element_text(size = text_size)
    )
}

plot_portfolio_geo_ald_value <- function(data) {
  data %>%
    dplyr::group_by(portfolio_name, has_geo_ald) %>%
    dplyr::summarise(
      sum_value_usd_mio = sum(value_usd, na.rm = T) / 10^6, .groups = "keep"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(portfolio_name) %>%
    dplyr::mutate(
      share_value_usd = sum_value_usd_mio / sum(sum_value_usd_mio, na.rm = T),
    ) %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(aes(x = portfolio_name, y = share_value_usd, fill = has_geo_ald)) +
    ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~ . * sum(asset_type_sub_portfolio_sub %>% dplyr::filter(portfolio_name %in% c("IE00B4L5Y983")) %>% dplyr::pull(value_usd)) / 10^6)) +
    ggplot2::labs(
      x = "",
      y = "% Portfolio Value",
      title = "Portfolio Value of holdings associated with at least one ALD",
      fill = "Has ALD"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white")
    )
}

plot_portfolio_geo_ald_holdings <- function(data) {
  data %>%
    dplyr::group_by(portfolio_name, has_geo_ald) %>%
    dplyr::summarise(
      n = dplyr::n(), .groups = "keep"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(portfolio_name) %>%
    dplyr::mutate(
      n = n / sum(n, na.rm = T)
    ) %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(aes(x = portfolio_name, y = n, fill = has_geo_ald)) +
    ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~ . * nrow(asset_type_sub_portfolio_sub))) +
    ggplot2::labs(
      x = "",
      y = "% Portfolio Holdings",
      title = "Holdings associated with at least one ALD",
      fill = "Has ALD"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white")
    )
}

save_result_plot <- function(name,
                             provider_sub,
                             scenario_sub,
                             hazard_sub,
                             model_sub,
                             period_sub,
                             path = final_path,
                             height = 20,
                             width = 30) {
  ggplot2::ggsave(fs::path(path, paste(name, provider_sub, scenario_sub, hazard_sub, model_sub, period_sub), ext = "png"), height = height, width = width)

  cat(crayon::yellow(crayon::bold(paste("Saved plot", name, "for", provider_sub, "for", period_sub, "of", model_sub, "of", hazard_sub, "of", scenario_sub, "\n"))))
}

save_overview_plot <- function(name, path = final_path, height = 20, width = 30) {
  ggplot2::ggsave(fs::path(path, name, ext = "png"), height = height, width = width)

  cat(crayon::yellow(crayon::bold(paste("Saved plot", name, "\n"))))
}
