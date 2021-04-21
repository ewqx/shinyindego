

#ui <- fluidPage(
ui <-  dashboardPage(
  
  dashboardHeader(title="Indego"),
  dashboardSidebar(
    sidebarMenu(
        menuItem("Quarter", tabName = "quartertab", icon = icon("dashboard")),
        menuItem("Stations", tabName = "stationstab", icon = icon("th"))
  )),
  
  dashboardBody(
  tabItems(
  #first tab content
  tabItem(tabName = "quartertab",
  fluidRow(
  plotlyOutput("trip_dow_pass"),
  selectInput(
    inputId = "color_select",
    label = "Select Variable",
    choices = stationVars
  ))),
  
  #second tab content
  tabItem(tabName = "stationstab",
  fluidRow(
  plotlyOutput("station_plot"),
  selectInput(
    inputId = "var_select",
    label = "Select Variable",
    choices = stationVars
  )))
  
)))
