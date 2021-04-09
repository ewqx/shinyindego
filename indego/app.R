library(shiny)
library(shinydashboard)
library(viridis)

ui <- dashboardPage(
  dashboardHeader(title = "Indego Bike Share"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 500)),
      
    )
  )
)


server <- function(input, output) {
  #set.seed(122)
  #histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    
    # plot histogram of bike trips by hour, fill using dow
    hist_hr_dow <- ggplot(alldfs, aes(x = start_time_hour)) + 
      stat_count(aes(fill= start_dow1)) +
      scale_color_viridis(discrete = TRUE, option = "B")+
      scale_fill_viridis(discrete = TRUE) +
      scale_x_discrete(expand = expand=c(0.2, 0.2)) +
      labs(
        title = "Frequency of Bike Trips by Hour of the day",
        subtitle = "Fill color by Day of the week",
        x = "Hour of the day",
        y = "Volume",
        fill = "Day"
      )
    
    plot(hist_hr_dow)
    
  })
}

shinyApp(ui, server)
