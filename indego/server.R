# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
library(shiny)
#library(dplyr)

colnames(alldfs)

server <- function(input, output, session) {
  
  output$trip_dow_pass <- renderPlot({
    ggplot(alldfs) + 
      aes_string(x = "quarter") + 
      stat_count(aes_string(fill = input$color_select)) +
      scale_color_viridis(discrete = TRUE, option = "D")+
      scale_fill_viridis(discrete = TRUE)
  })
  
}