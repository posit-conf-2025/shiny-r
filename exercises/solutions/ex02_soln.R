library(tidyverse)
library(shiny)
library(bslib)

d = read_csv(here::here("data/weather.csv"))

ui = page_sidebar(
  title = "Temperatures at Major Airports",
  sidebar = sidebar(
    selectInput(
      inputId = "name", 
      label = "Select an airport",
      choices = c(
        "Hartsfield-Jackson Atlanta",
        "Raleigh-Durham",
        "Denver",
        "Los Angeles",
        "John F. Kennedy"
      ),
      multiple = TRUE
    ) 
  ),
  plotOutput("plot")
)

server = function(input, output, session) {
  output$plot = renderPlot({
    d |>
      filter(name %in% input$name) |>
      ggplot(aes(x=date, y=temp_avg, color=name)) +
      geom_line()
  })
}

shinyApp(ui = ui, server = server)
