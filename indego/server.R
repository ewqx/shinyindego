library(shiny)
library(ggplot2)
library(plotly)

colnames(alldfs)

server <- function(input, output, session) {
  
  output$trip_dow_pass <- renderPlotly({
    p <- ggplot(alldfs) + 
      aes_string(x = "quarter") + 
      stat_count(aes_string(fill = input$color_select)) +
      scale_color_viridis(discrete = TRUE, option = "D")+
      scale_fill_viridis(discrete = TRUE)
    
    ggplotly(p)
  })
  
  
  
  
  
}