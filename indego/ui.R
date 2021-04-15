library(shiny)
library(ggplot2)
library(plotly)


ui <- fluidPage(
  plotlyOutput("trip_dow_pass"),
  selectInput(
    inputId = "color_select",
    label = "Select Categorical Variable",
    choices = categoricalVars
  )

)
