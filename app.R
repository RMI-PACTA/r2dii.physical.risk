library(shiny)
library(leaflet)
library(spData)
library(tmap)
library(r2dii.physical.risk)

linebreaks <- function(n) {
  HTML(strrep(br(), n))
}

distinct_geo_data <- r2dii.physical.risk:::load_distinct_geo_data()

result_files <- list.files(path_db_pacta_project_pr_output, recursive = T)[stringr::str_detect(list.files(path_db_pacta_project_pr_output, recursive = T), "results")]

analysis <- vroom::vroom(fs::path(path_db_pacta_project_pr_output, result_files)) %>%
  filter(security_mapped_sector == sector)

ui <- fluidPage(
  fluidRow(
    column(
      3,
      h2("Financial Parameter"),
      selectInput(
        "portfolio",
        label = "Portfolio",
        choices = analysis %>% filter(has_geo_data == TRUE) %>% distinct(portfolio_name) %>% pull(portfolio_name),
        multiple = TRUE
      ),
      selectInput(
        "asset_type",
        label = "Asset Type",
        choices = c("Equity", "Bonds"),
        multiple = FALSE
      ),
      selectInput(
        "sector",
        label = "Sector",
        choices = analysis %>% filter(has_geo_data == TRUE) %>% distinct(sector) %>% pull(sector),
        multiple = TRUE
      ),
      selectInput(
        "company_name",
        label = "Company Name",
        choices = analysis %>% filter(has_geo_data == TRUE) %>% distinct(company_name) %>% pull(company_name),
        multiple = TRUE
      ),
      selectInput(
        "country",
        label = "Asset Location",
        choices = analysis %>% filter(has_geo_data == TRUE) %>% distinct(asset_location) %>% pull(asset_location),
        multiple = TRUE
      ),
      selectInput(
        "allocation",
        label = "Allocation",
        choices = c("Portfolio Weight", "Ownership"),
        multiple = FALSE
      ),
      sliderInput(
        "ownership_level",
        label = "Ownership Level",
        min = min(analysis %>% distinct(asset_location, .keep_all = T) %>% pull(ownership_level), na.rm = T),
        max = max(analysis %>% distinct(asset_location, .keep_all = T) %>% pull(ownership_level), na.rm = T),
        value = c(
          min(analysis %>% distinct(asset_location, .keep_all = T) %>% pull(ownership_level), na.rm = T),
          max(analysis %>% distinct(asset_location, .keep_all = T) %>% pull(ownership_level), na.rm = T)
        )
      ),
      br(),
      h2("Climate Parameter"),
      selectInput(
        "provider",
        label = "Climate Data Provider",
        choices = analysis %>% filter(has_geo_data == TRUE) %>% distinct(provider) %>% pull(provider),
        multiple = FALSE,
        selected = (analysis %>% filter(has_geo_data == TRUE) %>% distinct(provider) %>% pull(provider))[1]
      ),
      selectInput(
        "scenario",
        label = "Scenario",
        choices = analysis %>% filter(has_geo_data == TRUE) %>% distinct(scenario) %>% pull(scenario),
        multiple = FALSE,
        selected = (analysis %>% filter(has_geo_data == TRUE) %>% distinct(scenario) %>% pull(scenario))[1]
      ),
      selectInput(
        "hazard",
        label = "Hazard",
        choices = analysis %>% filter(has_geo_data == TRUE) %>% distinct(hazard) %>% pull(hazard),
        multiple = FALSE,
        selected = (analysis %>% filter(has_geo_data == TRUE) %>% distinct(hazard) %>% pull(hazard))[1]
      ),
      selectInput(
        "model",
        label = "Model",
        choices = analysis %>% filter(has_geo_data == TRUE) %>% distinct(model) %>% pull(model),
        multiple = FALSE
      ),
      selectInput(
        "period",
        label = "Period",
        choices = analysis %>% filter(has_geo_data == TRUE) %>% distinct(period) %>% pull(period),
        multiple = FALSE
      ),
      selectInput(
        "indicator",
        label = "Indicator",
        choices = c("raw_model_output", "relative_change", "absolute_change"),
        multiple = FALSE
      ),
      sliderInput(
        "indicator_range",
        label = "Indicator Range",
        min = round(min(analysis$relative_change, na.rm = T), 2),
        max = round(max(analysis$relative_change, na.rm = T), 2),
        value = c(round(min(analysis$relative_change, na.rm = T), 2), round(max(analysis$relative_change, na.rm = T), 2))
      )
    ),
    column(
      9,
      br(),
      tmapOutput(outputId = "map", height = 1000)
    )
  ),
  linebreaks(4),
  plotOutput(outputId = "asset_risk_histgram"),
  linebreaks(4),
  plotOutput(outputId = "company_risk_distribution", height = 800),
  linebreaks(4),
  plotOutput(outputId = "portfolio_company_risk_distribution", height = 800),
  linebreaks(4),
  plotOutput(outputId = "number_of_assets"),
  linebreaks(4),
  plotOutput(outputId = "relative_sector_production"),
  linebreaks(4),
  plotOutput(outputId = "absolute_sector_production")
)



server <- function(input, output, session) {
  observeEvent(input$provider, {
    sub_analysis <- analysis %>%
      filter(provider %in% input$provider)

    updateSelectInput(
      session, "scenario",
      label = "Scenario",
      choices = sub_analysis %>% filter(has_geo_data == TRUE) %>% distinct(scenario) %>% pull(scenario)
    )

    updateSelectInput(
      session, "hazard",
      label = "Hazard",
      choices = sub_analysis %>% filter(has_geo_data == TRUE) %>% distinct(hazard) %>% pull(hazard),
    )

    updateSelectInput(
      session, "model",
      label = "Model",
      choices = sub_analysis %>% filter(has_geo_data == TRUE) %>% distinct(model) %>% pull(model)
    )

    updateSelectInput(
      session, "period",
      label = "Period",
      choices = sub_analysis %>% filter(has_geo_data == TRUE) %>% distinct(period) %>% pull(period)
    )
  })

  observeEvent(input$indicator, {
    if (input$indicator == "relative_change") {
      updateSliderInput(
        session, "indicator_range",
        min = round(min(analysis$relative_change, na.rm = T), 2),
        max = round(max(analysis$relative_change, na.rm = T), 2),
        value = c(round(min(analysis$relative_change, na.rm = T), 2), round(max(analysis$relative_change, na.rm = T), 2))
      )
    }

    if (input$indicator == "absolute_change") {
      updateSliderInput(
        session, "indicator_range",
        min = round(min(analysis$absolute_change, na.rm = T), 2),
        max = round(max(analysis$absolute_change, na.rm = T), 2),
        value = c(round(min(analysis$absolute_change, na.rm = T), 2), round(max(analysis$absolute_change, na.rm = T), 2))
      )
    }

    if (input$indicator == "raw_model_output") {
      updateSliderInput(
        session, "indicator_range",
        min = round(min(analysis$risk_level, na.rm = T), 2),
        max = round(max(analysis$risk_level, na.rm = T), 2),
        value = c(round(min(analysis$risk_level, na.rm = T), 2), round(max(analysis$risk_level, na.rm = T), 2))
      )
    }
  })


  sub_analysis_financial_parameter <- reactive({
    sub_analysis_financial_parameter <- analysis

    sub_analysis_financial_parameter <- sub_analysis_financial_parameter %>% rename(raw_model_output = risk_level)

    if (isTruthy(input$portfolio)) {
      sub_analysis_financial_parameter <- sub_analysis_financial_parameter %>% filter(portfolio_name == input$portfolio)
    }
    if (isTruthy(input$asset_type)) {
      sub_analysis_financial_parameter <- sub_analysis_financial_parameter %>% filter(asset_type == input$asset_type)
    }
    if (isTruthy(input$sector)) {
      sub_analysis_financial_parameter <- sub_analysis_financial_parameter %>% filter(sector == input$sector)
    }
    if (isTruthy(input$company_name)) {
      sub_analysis_financial_parameter <- sub_analysis_financial_parameter %>% filter(company_name == input$company_name)
    }
    if (isTruthy(input$ownership_level)) {
      sub_analysis_financial_parameter <- sub_analysis_financial_parameter %>% filter(between(ownership_level, input$ownership_level[1], input$ownership_level[2]))
    }
    if (isTruthy(input$country)) {
      sub_analysis_financial_parameter <- sub_analysis_financial_parameter %>% filter(asset_location == input$country)
    }
    if (isTruthy(input$allocation)) {
      if (input$allocation == "Ownership") sub_analysis_financial_parameter <- sub_analysis_financial_parameter %>% filter(allocation == "ownership")
      if (input$allocation == "Portfolio Weight") sub_analysis_financial_parameter <- sub_analysis_financial_parameter %>% filter(allocation == "port_weight")
    }

    Encoding(sub_analysis_financial_parameter$asset_name) <- "latin1"

    return(sub_analysis_financial_parameter)
  })

  sub_analysis <- reactive({
    sub_analysis <- sub_analysis_financial_parameter()

    if (isTruthy(input$scenario)) {
      sub_analysis <- sub_analysis %>% filter(scenario == input$scenario)
    }
    if (isTruthy(input$model)) {
      sub_analysis <- sub_analysis %>% filter(model == input$model)
    }
    if (isTruthy(input$period)) {
      sub_analysis <- sub_analysis %>% filter(period == input$period)
    }
    if (isTruthy(input$hazard)) {
      sub_analysis <- sub_analysis %>% filter(hazard == input$hazard)
    }
    if (isTruthy(input$indicator_range)) {
      if (input$indicator == "relative_change") sub_analysis <- sub_analysis %>% filter(between(relative_change, input$indicator_range[1], input$indicator_range[2]))
      if (input$indicator == "raw_model_output") sub_analysis <- sub_analysis %>% filter(between(raw_model_output, input$indicator_range[1], input$indicator_range[2]))
      if (input$indicator == "absolute_change") sub_analysis <- sub_analysis %>% filter(between(absolute_change, input$indicator_range[1], input$indicator_range[2]))
    }

    return(sub_analysis)
  })



  output$map = renderTmap({

    sub_analysis_financial_parameter <- sub_analysis_financial_parameter() %>% select(asset_name, company_name, technology, sector, economic_value, economic_unit, asset_id)
    sub_analysis <- sub_analysis() %>% select(relative_change, raw_model_output, absolute_change, asset_id)

    if(isTruthy(input$portfolio)) {

      if(input$indicator == "relative_change") {
        tm_shape(
          distinct_geo_data %>%
            inner_join(sub_analysis_financial_parameter, by = "asset_id") %>%
            left_join(sub_analysis, by = "asset_id")
        ) +
          tm_dots(
            id = "asset_name",
            col = input$indicator,
            size = 0.04,
            popup.vars = c("Company" = "company_name", "Technology" = "technology", "Sector" = "sector", "Production" = "economic_value", "Unit" = "economic_unit", "relative_change"),
            palette = rev(c(RColorBrewer::brewer.pal(11, "RdBu"))),
            breaks = c(-2, -1, 0, 1, 2),
            style = "cont"
          )
      } else {
        tm_shape(
          distinct_geo_data %>%
            inner_join(sub_analysis_financial_parameter, by = "asset_id") %>%
            left_join(sub_analysis, by = "asset_id")
        ) +
          tm_dots(
            id = "asset_name",
            col = input$indicator,
            size = 0.04,
            popup.vars = c("Company" = "company_name", "Technology" = "technology", "Sector" = "sector", "Production" = "economic_value", "Unit" = "economic_unit", "relative_change"),
            palette = rev(c(RColorBrewer::brewer.pal(11, "RdBu"))),
            style = "cont"
          )
      }


    } else {
      tm_shape(distinct_geo_data  %>% slice_sample(n = 10000)) +
        tm_dots(
          id = "asset_id",
          size = 0,
          palette = rev(c(RColorBrewer::brewer.pal(11, "RdBu"))),
          #breaks = c(-1,-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1),
          style = "cont"
        ) #+
    }
  })

  output$asset_risk_histgram <- renderPlot({
    sub_analysis <- sub_analysis()

    provider_sub <- input$provider
    model_sub <- input$model
    scenario_sub <- input$scenario
    hazard_sub <- input$hazard
    period_sub <- input$period

    sub_analysis %>%
      r2dii.physical.risk:::plot_asset_risk_histgram(
        provider_sub,
        scenario_sub,
        hazard_sub,
        model_sub,
        period_sub
      ) +
      r2dii.physical.risk:::scale_fill_relative_risk()
  })

  output$company_risk_distribution <- renderPlot({
    sub_analysis <- sub_analysis()

    provider_sub <- input$provider
    model_sub <- input$model
    scenario_sub <- input$scenario
    hazard_sub <- input$hazard
    period_sub <- input$period

    sub_analysis %>%
      r2dii.physical.risk:::plot_company_risk_distribution(
        provider_sub,
        scenario_sub,
        hazard_sub,
        model_sub,
        period_sub
      ) +
      r2dii.physical.risk:::scale_fill_relative_risk()
  })

  output$portfolio_company_risk_distribution <- renderPlot({
    sub_analysis <- sub_analysis()

    provider_sub <- input$provider
    model_sub <- input$model
    scenario_sub <- input$scenario
    hazard_sub <- input$hazard
    period_sub <- input$period

    sub_analysis %>%
      r2dii.physical.risk:::plot_portfolio_company_risk_distribution(
        provider_sub,
        scenario_sub,
        hazard_sub,
        model_sub,
        period_sub
      ) +
      r2dii.physical.risk:::scale_fill_relative_risk()
  })


  output$relative_sector_production <- renderPlot({
    sub_analysis <- sub_analysis()

    provider_sub <- input$provider
    model_sub <- input$model
    scenario_sub <- input$scenario
    hazard_sub <- input$hazard
    period_sub <- input$period

    sub_analysis %>%
      filter(sector == security_mapped_sector) %>%
      r2dii.physical.risk:::plot_sector_relative_portfolio_economic_value(
        provider_sub,
        scenario_sub,
        hazard_sub,
        model_sub,
        period_sub
      ) +
      r2dii.physical.risk:::scale_fill_relative_risk()
  })


  output$number_of_assets <- renderPlot({
    sub_analysis <- sub_analysis()

    provider_sub <- input$provider
    model_sub <- input$model
    scenario_sub <- input$scenario
    hazard_sub <- input$hazard
    period_sub <- input$period

    sub_analysis %>%
      filter(sector == security_mapped_sector) %>%
      r2dii.physical.risk:::plot_sector_number_of_assets(
        provider_sub,
        scenario_sub,
        hazard_sub,
        model_sub,
        period_sub
      ) +
      r2dii.physical.risk:::scale_fill_relative_risk()
  })

  output$absolute_sector_production <- renderPlot({
    sub_analysis <- sub_analysis()

    provider_sub <- input$provider
    model_sub <- input$model
    scenario_sub <- input$scenario
    hazard_sub <- input$hazard
    period_sub <- input$period

    sub_analysis %>%
      filter(sector == security_mapped_sector) %>%
      r2dii.physical.risk:::plot_sector_absolute_portfolio_economic_value(
        provider_sub,
        scenario_sub,
        hazard_sub,
        model_sub,
        period_sub
      ) +
      r2dii.physical.risk:::scale_fill_relative_risk()
  })
}


shinyApp(ui, server)
