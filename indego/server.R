library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)


server <- function(input, output, session) {
  
  output$trip_dow_pass <- renderPlotly({
    p <- ggplot(alldfs) + 
      aes_string(x = "quarter") + 
      stat_count(aes_string(fill = input$color_select)) +
      scale_color_viridis(discrete = TRUE, option = "D")+
      scale_fill_viridis(discrete = TRUE)
    ggplotly(p)
  })
  
  output$station_plot <- renderPlotly({
    ss <- ggplot(alldfs) + 
      aes_string(x = "start_station") + 
      stat_count(aes_string(fill = input$var_select)) +
      #scale_fill_brewer(palette = "BuPu", direction=-1) +
      #scale_color_brewer(palette = "BuPu", direction=-1) +
      scale_color_viridis(discrete = TRUE, option = "D") +
      scale_fill_viridis(discrete = TRUE) +
      labs(
        title = "Volume of Bike Trips by Station",
        subtitle = "Fill color:",
        x = "Station",
        y = "Volume",
        fill = "input"
      ) +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    ggplotly(ss)
  })
  
}