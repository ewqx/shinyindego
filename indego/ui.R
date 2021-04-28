

#ui <- fluidPage(
ui <-  dashboardPage(
  
  dashboardHeader(title="Indego"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", 
               tabName = "introtab", 
               icon = icon("info")),
      menuItem("Charts - by quarter", 
               tabName = "quartertab", 
               icon = icon("dashboard")),
      menuItem("Charts - by stations", 
                tabName = "stationstab", 
                icon = icon("th")),
      menuItem("Indego Map", 
               tabName = "testmaptab", 
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
          plotlyOutput("quarter_plot"),
          selectInput(
            inputId = "var1_select",
            label = "Select Variable",
            choices = chartVars
      ))),
  
      #tab content
      tabItem(tabName = "stationstab",
        fluidRow(
          plotlyOutput("station_plot"),
          selectInput(
            inputId = "var2_select",
            label = "Select Variable",
            choices = chartVars
      ))),
      
      #tab content
      tabItem(tabName = "testmaptab",
              fluidRow(
                h1("_Indego Map")),
              fluidRow(
                box(width = 4,
                  h4("Click anywhere on the map to find no. of bus stops within 0.5mi radius:"),h2(textOutput("console"))),
                box(width = 4,
                    uiOutput("sdropdown")),
                box(width = 4,
                uiOutput("censusdropdown")),
                box(width = 4,
                sliderInput("oprange", "Opacity", min = 0, max = 1, value = 0.5, step = 0.25))),
              fluidRow(
                leafletOutput("testmap", width="100%",height="600px")
              ))
  
)))
