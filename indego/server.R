
server <- function(input, output, session) {
  
  ### DURATION LINE GRAPH
  output$dur_plot <- renderPlotly({
    dp <- ggplot(alldfs, aes(x=duration)) + 
    stat_count(geom='line', aes(y=..count..)) + 
    geom_vline(xintercept = 30, color = "cadetblue", size=0.75) 
    
    ggplotly(dp) %>% layout(width = 1000)
  })
  
  
  #### INDEGO MAP OUTPUT ###
  output$testmap <- renderLeaflet({ 
    
    #load leaflet basemap
    leaflet() %>%
      #use greyscale Stamen tiles
    addProviderTiles("Stamen.TonerLite") %>%
      #set view to Philadelphia CC coords
    setView(lng = -75.1640, lat = 39.9520, zoom = 11.5) %>%
      #add radio buttons to toggle layers on and off
      addLayersControl(
        overlayGroups =c("Census Data", "Subway Stations", "Bus Stops", "Vehicular Crashes (Bikes)", "Bike Lanes", "Top 10 trips"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  ## add dropdown for options - census data columns
  output$censusdropdown <- renderUI({
    choices <- c(censusVars, "None")
    selectInput(inputId = "censusdropdown", label = "Census Column", choices = choices, selected = "None")
  })
  
  #get dropdown selection for census tract chloropleth map
  observeEvent(input$censusdropdown, {
    cddsel = input$censusdropdown
    
    #based on dropdown selection, grab census column(s) 
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
    } else { ctfill = 1 }
    
    #add layer to map
    m <- leafletProxy("testmap") %>%
      #clear polygon and legend each time user makes selection
      clearShapes() %>%
      clearControls() %>%
      #let user control fillOpacity of layer using slider input - input$oprange, fillColor controlled by user selection of census data dropdown (ctfill - see if statement above)
      addPolygons(data = phlctpolydata, stroke = TRUE, weight = 0.5, color = "white", smoothFactor = 0.3, fillOpacity = input$oprange, fillColor = ~pal2(ctfill), label = ~paste0(NAMELSAD10, ": ", formatC((ctfill), big.mark = ",")), group = "Census Data") %>%
      #legend title to match dropdown selection, values to match what is being mapped (ctfill)
      addLegend(pal = pal2, title = cddsel, values = ctfill, opacity = 1) %>%
      #because of clearShapes() - addPolylines() under here
      addPolylines(data = phlbikenetwork, color = "cadetblue", group = "Bike Lanes", weight = 2) 
    
    for (i in 1: nrow(top10trips_plot)) {
      m <- m%>%addPolylines(data=top10trips_plot, lat=c(top10trips_plot$start_lat[i],top10trips_plot$end_lat[i]),lng=c(top10trips_plot$start_lon[i],top10trips_plot$end_lon[i]), color = "lightpink", weight = 3, opacity = 3, group = "Top 10 trips", label = paste0("FROM ", top10trips_plot$start_station_name[i], " TO ", top10trips_plot$end_station_name[i]))
    }
  })
  
  #create dropdown for indego bike stations utilization categories
  output$sdropdown <- renderUI({
    #grab unique values from the utilization column - sort by levels
    choices <- as.character(unique(sort(stationsdf$start_station_use))) 
    #Add 'All" and 'Clear' to choices - to display all stations and none
    choices <- c('All', choices, 'Clear')
    #default to no markers displayed on load
    selectInput(inputId = "sdropdown", label = "Station Usage", choices = choices, selected = "Clear")
  })
  
  #grab the dropdown selection
  observeEvent(input$sdropdown, {
    sddsel = input$sdropdown
    
    if (sddsel == 'All'){
      #display all stations
      stationsdf2 <- stationsdf
    } 
    else {
      #display markers with values that match dropdown selection
      stationsdf2 <- stationsdf[stationsdf$start_station_use == sddsel, ]
    }
  
  #map out based on user selection    
   leafletProxy("testmap") %>%
     #remove markers each time user makes a different selection
      clearMarkers() %>%
     #add markers - use font awesome icons
      addAwesomeMarkers(data=stationsdf2, lng = stationsdf2$start_lon, lat = stationsdf2$start_lat, 
                        icon = awesomeIcons(
                         # icon = 'map-marker-alt',
                          icon = "bicycle",
                          library = "fa",
                          #set markerColor function - color set based on utilization value/ dropdown selection
                          markerColor = setMarkerCol(stationsdf2)),           
                          label = paste0(
                          stationsdf2$start_station_name, 
                          " ",
                          "(",stationsdf2$start_station,")",
                          " ",
                          "Trips: ", stationsdf2$start_station_volume,
                          " ",
                          "Usage: ", stationsdf2$start_station_use)) %>%
   
     #because of clearMarkers() above, add markers of various other layers/ datasets under here
     #define groups to enable toggling on/off layers
   addCircleMarkers(data = phlbslstations, color = "orange", radius = 3, stroke = FALSE, fillOpacity = 0.8, group="Subway Stations", label = paste0(phlbslstations$Station, " ", "Minority Area: ", phlbslstations$Minority_Area,  " ", "Low-income Area: ", phlbslstations$Low_Income_Area)) %>%
     addCircleMarkers(data = phlmflstations, color = "blue", radius = 3, stroke = FALSE, fillOpacity = 0.8, group="Subway Stations", label = paste0(phlmflstations$Station, " ", "Minority Area: ", phlmflstations$Minority_Area,  " ", "Low-income Area: ", phlmflstations$Low_Income_Area)) %>%
     addCircleMarkers(data = phlvehcrashes, radius = 6, color = "brown", fillOpacity = 0.1, label = paste0("Count:",  phlvehcrashes$bicycle_death_count + phlvehcrashes$bicycle_maj_inj_count), group="Vehicular Crashes (Bikes)") %>%
     addCircleMarkers(data = phlbusshelters, lat = phlbusshelters$LAT., lng = phlbusshelters$LONG., color = "purple", radius = 2, stroke = FALSE, fillOpacity = 1, group = "Bus Stops", label = paste0(phlbusshelters$ADDRESS)) %>%
     addCircleMarkers(lng = top10trips_plot$start_lon, lat = top10trips_plot$start_lat, color = "cadetblue", label = top10trips_plot$start_station_name, group = "Top 10 trips") %>%
     addCircleMarkers(lng = top10trips_plot$end_lon, lat = top10trips_plot$end_lat, color = "cadetblue", label = top10trips_plot$end_station_name, group = "Top 10 trips") %>%
     #toggle layers off on load using hideGroup()
     hideGroup("Bus Stops") %>% 
     hideGroup("Vehicular Crashes (Bikes)") %>% 
     hideGroup("Subway Stations") %>% 
     hideGroup("Bike Lanes") %>% 
     hideGroup("Top 10 trips") 
  })
  

# draw circle onclick - radius=800 - approximate 0.5mi radius  
  observeEvent(input$testmap_click, {
    click <- input$testmap_click
    text <- paste("Latitude ", round(click$lat,2), "Longtitude ", round(click$lng,2))
    
    #print(click)
    rlat = click$lat
    rlng = click$lng
    #create dataframe of coordinates clicked
    rcoordsdf <- data.frame(rlat, rlng)
    #define projection
    projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    #create sf object of coordinates
    rcoordsobj <- st_as_sf(x = rcoordsdf, coords = c('rlng', 'rlat'), crs = projcrs)
    #print(rcoordsobj)
    
    #create buffer based on point clicked by user - buffer radius in arc degrees - approx. 828.79m - around 0.5miles
    buffer <- sf::st_buffer(rcoordsobj, dist = 0.009722222222222222)
    #print(buffer)
    
    #convert csv to sf for st_join to work
    #sf_phlbusshelters <- st_as_sf(phlbusshelters, coords = c("LONG.", "LAT."), crs = projcrs)
    
    #convert csv to sf for st_join to work
    sf_idgstations <- st_as_sf(stationsdf, coords = c("start_lon", "start_lat"), crs = projcrs)

    #display circle of the clicked point
    leafletProxy("testmap") %>%
      clearGroup("new_buffer") %>%
      addCircles(click$lng, click$lat, radius=900, stroke = FALSE, color="red", group = "new_buffer") 
    
    #count number of indego stations within buffer - 0.5mi radius
    count <- sf::st_join(buffer, sf_idgstations, left = T) %>% nrow()
    print(count-1)
    
    #print count to ui
    withCallingHandlers(
      #use cat() to remove [1] before printing out on ui
      output$console <- renderPrint(cat(count-1))
    )
   
  })
  
  ### CHART - x = quarter, y = user input
  output$quarter_plot <- renderPlotly({
    qp <- ggplot(alldfs) + 
      aes_string(x = "quarter") + 
      #fill based on dropdown selection of column/variable to plot against quarter
      stat_count(aes_string(fill = input$var1_select)) +
      scale_color_viridis(discrete = TRUE, option = "D")+
      scale_fill_viridis(discrete = TRUE)
    ggplotly(qp)
  })
  
  output$month_plot <- renderPlotly({
    mp <- ggplot(alldfs) + 
      aes_string(x = "start_month") + 
      #fill based on dropdown selection of column/variable to plot against quarter
      stat_count(aes_string(fill = input$var1_select)) +
      scale_color_viridis(discrete = TRUE, option = "D")+
      scale_fill_viridis(discrete = TRUE)
    ggplotly(mp)
  })
  
  output$dow_plot <- renderPlotly({
    dp <- ggplot(alldfs) + 
      aes_string(x = "start_dow1") + 
      #fill based on dropdown selection of column/variable to plot against quarter
      stat_count(aes_string(fill = input$var1_select)) +
      scale_color_viridis(discrete = TRUE, option = "D")+
      scale_fill_viridis(discrete = TRUE)
    ggplotly(dp)
  })
  
  output$hour_plot <- renderPlotly({
    hp <- ggplot(alldfs) + 
      aes_string(x = "start_time_hour") + 
      #fill based on dropdown selection of column/variable to plot against quarter
      stat_count(aes_string(fill = input$var1_select)) +
      scale_color_viridis(discrete = TRUE, option = "D")+
      scale_fill_viridis(discrete = TRUE) +
      theme(axis.text.x = element_text(angle = 90, hjust = 10))
    ggplotly(hp)
  })
  
  
  ### CHART - x = stations, y = user input
  output$station_plot <- renderPlotly({
    ss <- ggplot(alldfs) + 
      aes_string(x = "start_station_name") + 
      stat_count(aes_string(fill = input$var2_select)) +
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
  
  ### HEATMAP - plot trips - start/end stations
  #next steps: heatmap on the left, station map on the right - display start and end stations when user clicks on a point on the heat map
  output$heatmapplot <- renderPlotly({
    hmp <- ggplot(data = station_heatmap_df, mapping = aes(x=start_station, y=end_station, fill=volume)) +
      geom_tile() + 
      scale_fill_continuous(high = "#008080", low = "#FFFFFF") +
      labs(
        title = "Heatmap of Trips Between Stations (filtered)",
        x = "Origin",
        y = "Destination",
        fill = "Trip Volume") +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank())
      #theme(text = element_text(size=0), axis.text.x = element_text(angle = 90, hjust = 10)) + 
      coord_fixed()
    ggplotly(hmp) %>% layout(height = 800, width = 800)
  })
  
  # Table of top ten trips - start/ end stations 
  output$toptentripstat <- renderTable(top10trips[2:4])
  
}