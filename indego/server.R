
server <- function(input, output, session) {
  
  
  output$testmap <- renderLeaflet({ 
    #basemap
    leaflet() %>%
    addProviderTiles("Stamen.TonerLite") %>%
    setView(lng = -75.1640, lat = 39.9520, zoom = 11.5) %>%
      addLayersControl(
        overlayGroups =c("Census Data", "Subway Stations", "Bus Stops", "Vehicular Crashes (Bikes)", "Bike Lanes"),
        options = layersControlOptions(collapsed = FALSE)
      ) 
  })
  
  output$censusdropdown <- renderUI({
    choices <- c(censusVars)
    selectInput(inputId = "censusdropdown", label = "Census Col", choices = choices, selected = "pop density")
  })
  
  observeEvent(input$censusdropdown, {
    cddsel = input$censusdropdown
    
    if (cddsel == 'pop density'){
      ctfill <- (phlctpolydata$B01003_001.POP/phlctpolydata$ALAND10)*100
    } else if (cddsel == 'pop (%black)'){
      ctfill <- (phlctpolydata$B02001_003.RACE_B/phlctpolydata$B01003_001.POP)*100
    } else if (cddsel == 'pop (%nonwhite)'){
      ctfill <- ((phlctpolydata$B01003_001.POP-phlctpolydata$B02001_002.RACE_W)/phlctpolydata$B01003_001.POP)*100
    } else if (cddsel == 'pop (%white)'){
      ctfill <- (phlctpolydata$B02001_002.RACE_W/phlctpolydata$B01003_001.POP)*100
    } else if (cddsel == 'median hh income'){
      ctfill <- phlctpolydata$B19013_001.INCOME
    } else if (cddsel == 'workers16+ (%)'){
      ctfill <- (phlctpolydata$B08016_001.WORKERS16/phlctpolydata$B01003_001.POP)*100
    } else if (cddsel == 'commute-bike(%)'){
      ctfill <- (phlctpolydata$B08134_111.CM_BIKE/phlctpolydata$B08016_001.WORKERS16)*100
    } else if (cddsel == 'commute-walk(%)'){
      ctfill <- (phlctpolydata$B08134_101.CM_WALK/phlctpolydata$B08016_001.WORKERS16)*100
    } else if (cddsel == 'commute-publictransit(%)'){
      ctfill <- (phlctpolydata$B08134_061.CM_PUB/phlctpolydata$B08016_001.WORKERS16)*100
    } else if (cddsel == 'commute-car(%)'){
      ctfill <- (phlctpolydata$B08134_011.CM_CAR/phlctpolydata$B08016_001.WORKERS16)*100
    } else { ctfill = NULL  }
    
    leafletProxy("testmap") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = phlctpolydata, stroke = TRUE, weight = 0.5, 
                  color = "white", smoothFactor = 0.3, fillOpacity = input$oprange, 
                  fillColor = ~pal2(ctfill),
                  label = ~paste0(NAMELSAD10, ": ", 
                                  formatC((ctfill), big.mark = ",")), group = "Census Data") %>%
      addLegend(pal = pal2, title = cddsel, values = ctfill, opacity = 1) %>%
      addPolylines(data = phlbikenetwork, color = "cadetblue", group = "Bike Lanes", weight = 2)
  })
  
  output$sdropdown <- renderUI({
    choices <- as.character(unique(sort(stationsdf$start_station_use)))  
    choices <- c('All', choices, 'Clear')
    selectInput(inputId = "sdropdown", label = "Station Usage", choices = choices, selected = "Clear")
  })
  
  observeEvent(input$sdropdown, {
    sddsel = input$sdropdown
    
    if (sddsel == 'All'){
      stationsdf2 <- stationsdf
    } 
    else {
      stationsdf2 <- stationsdf[stationsdf$start_station_use == sddsel, ]
    }
    
   leafletProxy("testmap") %>%
      clearMarkers() %>%
      addAwesomeMarkers(data=stationsdf2, lng = stationsdf2$start_lon, 
                        lat = stationsdf2$start_lat, 
                      
                        icon = awesomeIcons(
                         # icon = 'map-marker-alt',
                          icon = "bicycle",
                          library = "fa",
                          markerColor = setMarkerCol(stationsdf2)),
                        
                        label = paste0(
                          stationsdf2$start_station_name, 
                          " ",
                          "Trips: ", stationsdf2$start_station_volume,
                          " ",
                          "Usage: ", stationsdf2$start_station_use)) %>%
   
   addCircleMarkers(data = phlbslstations, color = "orange", radius = 3,
                    stroke = FALSE, fillOpacity = 0.8, 
                    group="Subway Stations", 
                    label = paste0(phlbslstations$Station, " ", "Minority Area: ", phlbslstations$Minority_Area,  " ", "Low-income Area: ", phlbslstations$Low_Income_Area)) %>%
     addCircleMarkers(data = phlmflstations, color = "blue", radius = 3, stroke = FALSE, fillOpacity = 0.8, group="Subway Stations", label = paste0(phlmflstations$Station, " ", "Minority Area: ", phlmflstations$Minority_Area,  " ", "Low-income Area: ", phlmflstations$Low_Income_Area)) %>%
     addCircleMarkers(data = phlvehcrashes, radius = 6, color = "brown", fillOpacity = 0.1, label = paste0("Count:",  phlvehcrashes$bicycle_death_count + phlvehcrashes$bicycle_maj_inj_count), group="Vehicular Crashes (Bikes)") %>%
     addCircleMarkers(data = phlbusshelters, lat = phlbusshelters$LAT., lng = phlbusshelters$LONG., color = "green", radius = 2, stroke = FALSE, fillOpacity = 1, group = "Bus Stops", label = paste0(phlbusshelters$ADDRESS))
  })
  

# draw circle onclick - radius=800 - approximate 0.5mi radius  
  observeEvent(input$testmap_click, {
    click <- input$testmap_click
    text <- paste("Latitude ", round(click$lat,2), "Longtitude ", round(click$lng,2))
    
    print(click)
    rlat = click$lat
    rlng = click$lng
    rcoordsdf <- data.frame(rlat, rlng)
    projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    rcoordsobj <- st_as_sf(x = rcoordsdf, coords = c('rlng', 'rlat'), crs = projcrs)
    print(rcoordsobj)
    
    #buffer in arc degrees - approx. 828.79m - around 0.5miles
    buffer <- sf::st_buffer(rcoordsobj, dist = 0.009722222222222222)
    print(buffer)
    
    sf_phlbusshelters <- st_as_sf(phlbusshelters, coords = c("LONG.", "LAT."), crs = projcrs)

    leafletProxy("testmap") %>%
      clearGroup("new_buffer") %>%
      addCircles(click$lng, click$lat, radius=1000, color="red", group = "new_buffer") 
     
    count <- sf::st_join(buffer, sf_phlbusshelters, left = F) %>% 
      nrow()
    print(count)
    
  })
  
  

  output$station_map <- renderLeaflet({ 
    stationmap <- leaflet() %>%
      addProviderTiles("Stamen.TonerLite") %>%
      setView(lng = -75.1640, lat = 39.9520, zoom = 12.5) %>%
      addAwesomeMarkers(data=stationsdf, lng = stationsdf$start_lon, 
                        lat = stationsdf$start_lat, 
                        icon=icons, 
                        popup = paste0(
                          stationsdf$start_station_name, 
                          "<br>",
                          "Trips: ", stationsdf$start_station_volume,
                          "<br>",
                          "Usage: ", stationsdf$start_station_use))
  })
  
  output$census_map <- renderLeaflet({ 
    census_map <- leaflet(phlctpolydata) %>%
      addProviderTiles("Stamen.TonerLite") %>%
      setView(lng = -75.1640, lat = 39.9520, zoom = 11.5) %>%
      addPolygons(stroke = TRUE, weight = 1, color = "#444444", 
                  smoothFactor = 0.3, fillOpacity = 0.8, 
                  fillColor = ~pal2((B01003_001.POP/ALAND10)*100), 
                  label = ~paste0(NAMELSAD10, ": ", 
                  formatC((B01003_001.POP/ALAND10)*100, big.mark = ","))) %>%
      addLegend(pal = pal2, title = "pop density", values = ~((B01003_001.POP/ALAND10)*100), opacity = 1.0)
    })
  
  
  output$trip_dow_pass <- renderPlotly({
    p <- ggplot(alldfs) + 
      aes_string(x = "quarter") + 
      stat_count(aes_string(fill = input$color_select)) +
      scale_color_viridis(discrete = TRUE, option = "D")+
      scale_fill_viridis(discrete = TRUE)
    ggplotly(p)
  })
  
  output$station_plot <- renderPlotly({
    ss <- ggplot(alldfs) + 
      aes_string(x = "start_station") + 
      stat_count(aes_string(fill = input$var_select)) +
      #scale_fill_brewer(palette = "BuPu",s direction=-1) +
      #scale_color_brewer(palette = "BuPu", direction=-1) +
      scale_color_viridis(discrete = TRUE, option = "D") +
      scale_fill_viridis(discrete = TRUE) +
      labs(
        title = "Volume of Bike Trips by Station",
        subtitle = "Fill color:",
        x = "Station",
        y = "Volume",
        fill = "input"
      ) +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    ggplotly(ss)
  })
  
}