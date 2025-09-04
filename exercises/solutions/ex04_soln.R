library(tidyverse)
library(shiny)

d = readr::read_csv(here::here("data/weather.csv"))

d_vars = c(
  "Average temp" = "temp_avg",   "Min temp"       = "temp_min",
  "Max temp"     = "temp_max",   "Total precip"   = "precip",
  "Snow depth"   = "snow",       "Wind direction" = "wind_direction",
  "Wind speed"   = "wind_speed", "Air pressure"   = "air_press"
)


ui = page_sidebar(
  title = "Weather Forecasts",
  sidebar = sidebar(
    selectInput(
      "region", label = "Select a region", 
      choices = c("West", "Midwest", "Northeast", "South")
    ),
    selectInput(
      "name", label = "Select an airport", 
      choices = c()
    ),
    selectInput(
      "var", label = "Select a variable",
      choices = d_vars, 
      selected = "temp_avg"
    )
  ),
  plotOutput("plot"),
  tableOutput("minmax")
)

server = function(input, output, session) {
  observe({
    updateSelectInput(
      session, "name",
      choices = d |>
        distinct(region, name) |>
        filter(region == input$region) |>
        pull(name)
    )
  })
  
  d_city = reactive({
    req(input$name)
    d |>
      filter(name %in% input$name)
  })
  
  output$plot = renderPlot({
    d_city() |>
      ggplot(aes(x=date, y=.data[[input$var]])) +
      geom_line() +
      labs(title = paste(input$name, "-", input$var))
  })
  
  output$minmax = renderTable({
    d_city() |>
      mutate(
        year = lubridate::year(date) |> as.integer()
      ) |>
      summarize(
        `min avg temp` = min(temp_min),
        `max avg temp` = max(temp_max),
        .by = year
      )
  })
}

shinyApp(ui = ui, server = server)
