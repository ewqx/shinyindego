
server <- function(input, output, session) {
  
  
  output$testmap <- renderLeaflet({ 
    #basemap
    leaflet() %>%
    addProviderTiles("Stamen.TonerLite") %>%
    setView(lng = -75.1640, lat = 39.9520, zoom = 12.5)
  })
  
  output$sdropdown <- renderUI({
    choices <- as.character(unique(stationsdf$start_station_use))  
    choices <- c('All', choices)
    selectInput(inputId = "sdropdown", label = "Station Usage", choices = choices, selected = "All")
  })
  
  observeEvent(input$sdropdown, {
    sddsel = input$sdropdown
    
    if (sddsel != 'All'){
      stationsdf2 <- stationsdf[stationsdf$start_station_use == sddsel, ]
    } else {
      stationsdf2 <- stationsdf
    }
    
    leafletProxy("testmap") %>%
      clearMarkers() %>%
      addAwesomeMarkers(data=stationsdf2, lng = stationsdf2$start_lon, 
                        lat = stationsdf2$start_lat, 
                        icon = awesomeIcons(
                          icon = 'map-marker-alt',
                          library = 'fa',
                          markerColor = setMarkerCol(stationsdf2)
                        ),
                        popup = paste0(
                          stationsdf2$start_station_name, 
                          "<br>",
                          "Trips: ", stationsdf2$start_station_volume,
                          "<br>",
                          "Usage: ", stationsdf2$start_station_use))
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
                  formatC((B01003_001.POP/ALAND10)*100,"%", big.mark = ","))) %>%
      addLegend(pal = pal2, title = "pop density (%)", values = ~((B01003_001.POP/ALAND10)*100), opacity = 1.0)
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