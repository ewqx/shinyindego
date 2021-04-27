

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
                icon = icon("th")),
      menuItem("Census data", 
               tabName = "censustab", 
               icon = icon("map")),
      menuItem("Test map", 
               tabName = "testtab", 
               icon = icon("map"))
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
                h1("Station Map"),
                p(),
                leafletOutput("station_map", width="100%",height="1000px")
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
      ))),
      
      #tab content
      tabItem(tabName = "censustab",
              fluidRow(
                h1("Census Tract Map"),
                p(),
                leafletOutput("census_map", width="100%",height="1000px")
              )),
      
      #tab content
      tabItem(tabName = "testtab",
              fluidRow(
                h1("_Indego Map")),
              fluidRow(
                box(width = 5,
                    uiOutput("sdropdown")),
                box(width = 5,
                uiOutput("censusdropdown")),
                box(width = 5,
                sliderInput("oprange", "Opacity", min = 0, max = 1, value = 0, step = 0.25))),
              fluidRow(
                leafletOutput("testmap", width="100%",height="500px")
              ))
  
)))
