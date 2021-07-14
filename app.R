library(shiny)    # for shiny apps
library(leaflet)  # renderLeaflet function
library(spData)   # loads the world dataset
library(tmap)
linebreaks <- function(n){HTML(strrep(br(), n))}

distinct_geo_data <- load_distinct_geo_data()

ui = fluidPage(
  fluidRow(
    column(3,
           h2("Financial Parameter"),
           selectInput(
             "portfolio",
             label = "Portfolio",
             choices = analysis %>% filter(has_geo_data == TRUE) %>% distinct(portfolio_name) %>% pull(portfolio_name),
             multiple = TRUE
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

           sliderInput(
             "ownership_level",
             label = "Ownership Level",
             min = min(analysis %>% distinct(asset_location, .keep_all = T) %>% pull(ownership_level), na.rm = T),
             max = max(analysis %>% distinct(asset_location, .keep_all = T) %>% pull(ownership_level), na.rm = T),
             value = c(
               min(analysis %>% distinct(asset_location, .keep_all = T) %>% pull(ownership_level), na.rm = T),
               max(analysis %>% distinct(asset_location,  .keep_all = T) %>% pull(ownership_level), na.rm = T)
             )
           ),
           br(),
           h2("Climate Parameter"),

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
             choices = analysis %>% filter(has_geo_data == TRUE)%>% distinct(model)  %>% pull(model),
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
             "change",
             label = "Indicator Range",
             min = round(min(analysis$relative_change, na.rm = T),2),
             max = round(max(analysis$relative_change, na.rm = T),2),
             value = c(round(min(analysis$relative_change, na.rm = T),2), round(max(analysis$relative_change, na.rm = T),2))

           )
    ),
    column(9,
           br(),
           tmapOutput(outputId = "map", height = 1000)
    )
  ),

  linebreaks(4),



  plotOutput(outputId = "plot1"),
  br(),

  plotOutput(outputId = "plot3"),
  br(),

  plotOutput(outputId = "plot4")


)



server = function(input, output, session) {


  # observeEvent(input$company_name, {
  #
  #   sub_company_name <- input$company_name
  #
  #   sub_analysis <- analysis %>%
  #     filter(company_name %in% sub_company_name)
  #
  #   updateSelectInput(
  #     session, "portfolio",
  #     label = "Portfolio",
  #     choices = sub_analysis %>% filter(has_geo_data == TRUE) %>% distinct(portfolio_name) %>% pull(portfolio_name),
  #     selected = (sub_analysis %>% filter(has_geo_data == TRUE) %>% distinct(portfolio_name) %>% pull(portfolio_name))[1]
  #   )
  #
  #   updateSelectInput(
  #     session,"sector",
  #     label = "Sector",
  #     choices = sub_analysis %>% filter(has_geo_data == TRUE) %>% distinct(sector) %>% pull(sector),
  #   )
  #
  #   updateSelectInput(
  #     session,"country",
  #     label = "Asset Location",
  #     choices = sub_analysis %>% filter(has_geo_data == TRUE) %>% distinct(asset_location) %>% pull(asset_location),
  #   )
  #
  #   updateSliderInput(
  #     session, "ownership_level",
  #     label = "Ownership Level",
  #     min = min(sub_analysis %>% distinct(asset_location, .keep_all = T) %>% pull(ownership_level), na.rm = T),
  #     max = max(sub_analysis %>% distinct(asset_location, .keep_all = T) %>% pull(ownership_level), na.rm = T),
  #     value = c(
  #       min(sub_analysis %>% distinct(asset_location, .keep_all = T) %>% pull(ownership_level), na.rm = T),
  #       max(sub_analysis %>% distinct(asset_location,  .keep_all = T) %>% pull(ownership_level), na.rm = T)
  #     )
  #   )
  #
  #   # updateSelectInput(
  #   #   session, "hazard",
  #   #   label = "Hazard",
  #   #   choices = analysis %>% filter(has_geo_data == TRUE) %>% distinct(hazard) %>% pull(hazard),
  #   #   selected = (analysis %>% filter(has_geo_data == TRUE) %>% distinct(hazard) %>% pull(hazard))[1]
  #   # )
  #   #
  #   # updateSelectInput(
  #   #   session, "model",
  #   #   label = "Model",
  #   #   choices = analysis %>% filter(has_geo_data == TRUE)%>% distinct(model)  %>% pull(model)
  #   # )
  #   #
  #   # updateSelectInput(
  #   #   session, "period",
  #   #   label = "Period",
  #   #   choices = analysis %>% filter(has_geo_data == TRUE) %>% distinct(period) %>% pull(period)
  #   # )
  #
  #   updateSliderInput(
  #     session, "change",
  #     label = "Hazard Change",
  #     min = round(min(sub_analysis$relative_change, na.rm = T),2),
  #     max = round(max(sub_analysis$relative_change, na.rm = T),2),
  #     value = c(round(min(sub_analysis$relative_change, na.rm = T),2), round(max(sub_analysis$relative_change, na.rm = T),2))
  #
  #   )
  #
  # })

  output$map = renderTmap({

    if(isTruthy(input$portfolio)) {

      sub_analysis <- analysis

      sub_analysis <- sub_analysis %>% filter(year == 2020) %>% rename(raw_model_output = risk_level)

      if(isTruthy(input$portfolio)) {sub_analysis <- sub_analysis %>% filter(portfolio_name == input$portfolio)}
      if(isTruthy(input$sector)) {sub_analysis <- sub_analysis %>% filter(sector == input$sector)}
      if(isTruthy(input$company_name)) {sub_analysis <- sub_analysis %>% filter(company_name == input$company_name)}
      if(isTruthy(input$ownership_level)) {sub_analysis <- sub_analysis %>% filter(between(ownership_level, input$ownership_level[1], input$ownership_level[2]))}

      if(isTruthy(input$country)) {sub_analysis <- sub_analysis %>% filter(asset_location == input$country)}

      if(isTruthy(input$model)) {sub_analysis <- sub_analysis %>% filter(model == input$model)}
      if(isTruthy(input$period)) {sub_analysis <- sub_analysis %>% filter(period == input$period)}
      if(isTruthy(input$hazard)) {sub_analysis <- sub_analysis %>% filter(hazard == input$hazard)}
      if(isTruthy(input$change)) {

        if(input$indicator == "relative_change") sub_analysis <- sub_analysis %>% filter(between(relative_change, input$change[1], input$change[2]))
        if(input$indicator == "raw_model_output") sub_analysis <- sub_analysis %>% filter(between(raw_model_output, input$change[1], input$change[2]))
        if(input$indicator == "absolute_change") sub_analysis <- sub_analysis %>% filter(between(absolute_change, input$change[1], input$change[2]))

      }

      Encoding(sub_analysis$asset_name) <- "latin1"


      tm_shape(distinct_geo_data %>% inner_join(sub_analysis, by = "asset_id")) +
        tm_dots(
          id = "asset_name",
          col = input$indicator,
          size = 0.04,
          popup.vars = c("Company" = "company_name", "Technology" = "technology", "Sector" = "sector", "Production" = "economic_value", "Unit" = "economic_unit"),
          palette = rev(c(RColorBrewer::brewer.pal(11, "RdBu"))),
          #breaks = c(-1,-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1),
          style = "cont"
        ) #+
      #
      #   tm_shape(
      #     scenario %>%
      #       slice(c(500000:550000))
      #     ) +
      #   tm_polygons(col = "indicator", border.alpha = 0, alpha = 0.5)


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

  output$plot1 <- renderPlot({

    sub_analysis <- analysis

    if(isTruthy(input$portfolio)) {sub_analysis <- sub_analysis %>% filter(portfolio_name == input$portfolio)}
    if(isTruthy(input$sector)) {sub_analysis <- sub_analysis %>% filter(sector == input$sector)}
    if(isTruthy(input$company_name)) {sub_analysis <- sub_analysis %>% filter(company_name == input$company_name)}
    if(isTruthy(input$ownership_level)) {sub_analysis <- sub_analysis %>% filter(between(ownership_level, input$ownership_level[1], input$ownership_level[2]))}

    if(isTruthy(input$country)) {sub_analysis <- sub_analysis %>% filter(asset_location == input$country)}

    if(isTruthy(input$model)) {sub_analysis <- sub_analysis %>% filter(model == input$model)}
    if(isTruthy(input$period)) {sub_analysis <- sub_analysis %>% filter(period == input$period)}
    if(isTruthy(input$hazard)) {sub_analysis <- sub_analysis %>% filter(hazard == input$hazard)}
    if(isTruthy(input$change)) {sub_analysis <- sub_analysis %>% filter(between(relative_change, input$change[1], input$change[2]))}

    sub_analysis %>%
      arrange(relative_change) %>%
      ggplot() +
      geom_col(aes(x = as.character(year), y = portfolio_final_owned_economic_value_share_sector, fill = relative_change)) +
      scale_fill_gradientn(colors = rev(RColorBrewer::brewer.pal(11, "RdBu")), breaks = c(-1, -0.5, 0, 0.5, 1), limits = c(-1,1)) +
      #scale_y_continuous(labels = scales::percent) +
      facet_grid(portfolio_name ~ sector)  +
      theme_minimal() +
      theme(
        text = element_text(size = 20)
      ) +
      labs(
        title = "Share Sector Production",
        y = "",
        x = "Year"
      )
  })


  output$plot3 <- renderPlot({

    sub_analysis <- analysis

    if(isTruthy(input$portfolio)) {sub_analysis <- sub_analysis %>% filter(portfolio_name == input$portfolio)}
    if(isTruthy(input$sector)) {sub_analysis <- sub_analysis %>% filter(sector == input$sector)}
    if(isTruthy(input$company_name)) {sub_analysis <- sub_analysis %>% filter(company_name == input$company_name)}
    if(isTruthy(input$ownership_level)) {sub_analysis <- sub_analysis %>% filter(between(ownership_level, input$ownership_level[1], input$ownership_level[2]))}

    if(isTruthy(input$country)) {sub_analysis <- sub_analysis %>% filter(asset_location == input$country)}

    if(isTruthy(input$model)) {sub_analysis <- sub_analysis %>% filter(model == input$model)}
    if(isTruthy(input$period)) {sub_analysis <- sub_analysis %>% filter(period == input$period)}
    if(isTruthy(input$hazard)) {sub_analysis <- sub_analysis %>% filter(hazard == input$hazard)}
    if(isTruthy(input$change)) {sub_analysis <- sub_analysis %>% filter(between(relative_change, input$change[1], input$change[2]))}

    sub_analysis %>%
      distinct(portfolio_name, hazard, model, period, asset_id, year, .keep_all = T) %>%
      count(portfolio_name, hazard, model, period, sector, technology, year, relative_change) %>%
      arrange(relative_change) %>%
      ggplot() +
      geom_col(aes(x = as.character(year), y = n, fill = relative_change)) +
      scale_fill_gradientn(colors = rev(RColorBrewer::brewer.pal(11, "RdBu")), breaks = c(-1, -0.5, 0, 0.5, 1), limits = c(-1,1)) +
      theme_minimal() +
      facet_wrap(portfolio_name ~ sector, scales = "free", nrow = 1) +
      theme(
        text = element_text(size = 20)
      ) +
      labs(
        x = "Year",
        title = "Number of assets",
        y = ""
      )
  })

  output$plot4 <- renderPlot({

    sub_analysis <- analysis

    if(isTruthy(input$portfolio)) {sub_analysis <- sub_analysis %>% filter(portfolio_name == input$portfolio)}
    if(isTruthy(input$sector)) {sub_analysis <- sub_analysis %>% filter(sector == input$sector)}
    if(isTruthy(input$company_name)) {sub_analysis <- sub_analysis %>% filter(company_name == input$company_name)}
    if(isTruthy(input$ownership_level)) {sub_analysis <- sub_analysis %>% filter(between(ownership_level, input$ownership_level[1], input$ownership_level[2]))}

    if(isTruthy(input$country)) {sub_analysis <- sub_analysis %>% filter(asset_location == input$country)}

    if(isTruthy(input$model)) {sub_analysis <- sub_analysis %>% filter(model == input$model)}
    if(isTruthy(input$period)) {sub_analysis <- sub_analysis %>% filter(period == input$period)}
    if(isTruthy(input$hazard)) {sub_analysis <- sub_analysis %>% filter(hazard == input$hazard)}
    if(isTruthy(input$change)) {sub_analysis <- sub_analysis %>% filter(between(relative_change, input$change[1], input$change[2]))}

    sub_analysis %>%
      arrange(relative_change) %>%
      ggplot() +
      geom_col(aes(x = as.character(year), y = portfolio_final_owned_economic_value, fill = relative_change)) +
      scale_fill_gradientn(colors = rev(RColorBrewer::brewer.pal(11, "RdBu")), breaks = c(-1, -0.5, 0, 0.5, 1), limits = c(-1,1)) +
      theme_minimal() +
      facet_wrap(portfolio_name ~ sector, scales = "free", nrow = 1) +
      theme(
        text = element_text(size = 20)
      ) +
      labs(
        x = "Year",
        title = "Absolute Sector Production",
        y = ""
      )
  })

}


shinyApp(ui, server)


