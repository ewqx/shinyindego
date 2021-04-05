library(shiny)

stations <- read.csv(file = "./idgstations.csv")

# Define UI for application
# shinyUI(
fluidPage(
    # Application title
    titlePanel("Title"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h4("sidebar panel"),
            
            selectizeInput(inputId = "status",
                           label = "station status",
                           choices = unique(stations$Status))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h4("main panel"),
            fluidRow(
                column(2,
                       h5("Buttons"),
                       actionButton("action", "Action"),
                       br(),
                       br(),
                       submitButton("Submit")),
                column(2, 
                       dateInput("date", 
                        h5("date input"), 
                        value="2021-04-05"))
            ),
            fluidRow(
                br()
            ),
            fluidRow(
                column(4,
                       selectInput("select", 
                                   h5("select box"),
                                   choices = list("Choice 1" = "fuck", 
                                                  "Choice 2" = "marry", 
                                                  "Choice 3" = "die", 
                                                  "Choice 4" = "kill"))),
                column(4,
                       sliderInput("slider1", 
                                    h4("Sliders"),
                                    min = 0, max = 100, value = c(25, 75))),
                       
                column(4, 
                      textInput("text", 
                                 h5("text input"), 
                                 value="enter text"))
            )
        )
))