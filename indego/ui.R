library(shiny)
library(shinydashboard)


ui <- fluidPage(
  plotOutput("trip_dow_pass"),
  selectInput(
    inputId = "color_select",
    label = "Select Categorical Variable",
    choices = categoricalVars
  )

)
