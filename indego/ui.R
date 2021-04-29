#ui <- fluidPage(
ui <-  dashboardPage(
  
  dashboardHeader(title="Indego"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", 
               tabName = "introtab", 
               icon = icon("th")),
      menuItem("Summary", 
               tabName = "summtab", 
               icon = icon("th")),
      menuItem("Charts - trips by period", 
               tabName = "periodtab", 
               icon = icon("dashboard")),
      menuItem("Charts - trips by stations", 
                tabName = "stationstab", 
                icon = icon("th")),
      menuItem("Heatmap - trips by stations", 
               tabName = "heatmaptab", 
               icon = icon("th")),
      menuItem("Map - Indego Stations", 
               tabName = "testmaptab", 
               icon = icon("map"))
  )),
  
  dashboardBody(
  
    tabItems(
      
      #tab content
      tabItem(tabName = "introtab",
                h1("intro")),
      
      #tab content
      tabItem(tabName = "summtab",
                box(width = 16, title = "duration (minutes)", plotlyOutput("dur_plot"))),
      
      #tab content
      tabItem(tabName = "periodtab",
              fluidRow(
                h1("Charts - trips by period"),
                br(),
                selectInput(
                  inputId = "var1_select",
                  label = "Select Variable",
                  choices = chartVars)),  
              fluidRow(
                box(plotlyOutput("quarter_plot")),
                box(plotlyOutput("month_plot"))),
              fluidRow(
                box(plotlyOutput("dow_plot")),
                box(plotlyOutput("hour_plot")))
      ),
  
      #tab content
      tabItem(tabName = "stationstab",
              fluidRow(
                h1("Charts - trips by stations")),
              fluidRow(
                plotlyOutput("station_plot"),
                selectInput(
                inputId = "var2_select",
                label = "Select Variable",
                choices = chartVars))
              ),
      
      #tab content
      tabItem(tabName = "heatmaptab",
              fluidRow(
                h1("_Indego Stations - Trip Heatmap"),
                br(),
                box(width = 16, tableOutput("toptentripstat"))),
              fluidRow(
                width = 16, plotlyOutput("heatmapplot")
                )
              ),
      
      #tab content
      tabItem(tabName = "testmaptab",
              fluidRow(
                h1("_Indego Stations - Map")),
              fluidRow(
                box(width = 4,
                  h4("Click anywhere to find no. of indego stations within 0.5mi radius:"),h2(textOutput("console"))),
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
