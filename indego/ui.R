

#ui <- fluidPage(
ui <-  dashboardPage(
  
  dashboardHeader(title="Indego"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", 
               tabName = "introtab", 
               icon = icon("info")),
      menuItem("Station map", 
               tabName = "maptab", 
               icon = icon("map")),
      menuItem("Charts - by quarter", 
               tabName = "quartertab", 
               icon = icon("dashboard")),
      menuItem("Charts - by stations", 
                tabName = "stationstab", 
                icon = icon("th"))
  )),
  
  dashboardBody(
    tabItems(
      
      #tab content
      tabItem(tabName = "introtab",
              fluidRow(
                h1("Introduction")
              )),
      
      #tab content
      tabItem(tabName = "maptab",
              fluidRow(
                h1("Station Map")
              )),
      
      #tab content
      tabItem(tabName = "quartertab",
        fluidRow(
          plotlyOutput("trip_dow_pass"),
          selectInput(
            inputId = "color_select",
            label = "Select Variable",
            choices = stationVars
      ))),
  
      #tab content
      tabItem(tabName = "stationstab",
        fluidRow(
          plotlyOutput("station_plot"),
          selectInput(
            inputId = "var_select",
            label = "Select Variable",
            choices = stationVars
      )))
  
)))
