
server <- function(input, output, session) {
  
  output$station_map <- renderLeaflet({ 
    leaflet() %>%
      setView(lng = -75.165222, lat = 39.952583, zoom = 6.5) %>%
      addProviderTiles("Esri.WorldStreetMap") %>%
      addCircleMarkers(
        lng = alldfs$start_lon, lat = alldfs$start_lat,
        radius = sqrt(alldfs$start_station_volume)/10,
        color = "#000000",
        fillColor = "#ffffff",
        fillOpacity = 0.5,
        popup = paste0(
          "Station Name: ", alldfs$start_station_name,
          "Volume: ", alldfs$start_station_volume,
          "Usage: ", alldfs$start_station_use
        )
      )
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