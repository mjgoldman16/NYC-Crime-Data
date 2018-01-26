shinyServer(function(input, output, session){
  
  
  ### DRAWING OF MAP
  
  ##WHEN I SELECT A NEW OUTPUT IT COVERS UP THE LAYERS, ASK TA ABOUT THIS
  output$map = renderLeaflet({
    leaflet(map_filter()) %>%
      addProviderTiles("Esri.WorldStreetMap") %>%
      setView(-73.83, 40.7, 10) %>%
      addMarkers(~Longitude, ~Latitude,
                   clusterOptions = markerClusterOptions(), 
                 popup = paste("Type of Crime:", map_filter()$OFNS_DESC, "<br>",
                               "Additional Details:", map_filter()$PD_DESC, "<br>",
                               "Date Occurred:", map_filter()$DATE, "<br>",
                               "Time Occurred:", map_filter()$TIME, "<br>"))
  })
 
  
  # ADDING IN INTERACTIVE LAYER TOGGLE
  observeEvent(input$boro_layer, {
    proxy <- leafletProxy("map")
    if(input$boro_layer) {
      proxy %>% addPolygons(data=boro_layer,
                            color = topo.colors(5,alpha = NULL),
                            fillColor = topo.colors(5,alpha = NULL),
                            smoothFactor = .5,
                            layerId = LETTERS[1:6])
    } 
    else {5
      proxy %>% removeShape(layerId = LETTERS[1:6])
    }
  })
  
  ###END OF ALL MAP DRAWING
  
  ### UPDATING SELECTIZE INPUTS
  ## INPUT FILTERS FOR THE TABLE
  updateSelectizeInput(session, "crimes", choices = unique(nyc_crimes$OFNS_DESC), server = TRUE)
  updateSelectizeInput(session, "dow", choices = unique(nyc_crimes$DOW), server = TRUE)
  updateSelectizeInput(session, "crime_time", 
                       choices = c("Early Morning (0:00-5:59)" = "Early Morning", 
                                   "Morning (6:00-11:59)" = "Morning", 
                                   "Afternoon (12:00-17:59)" = "Afternoon", 
                                   "Evening (18:00-23:59)" = "Evening"), 
                       server = TRUE)
  ##END OF FILTERS FOR THE TABLE
  
  ##START OF FILTERS FOR THE MAP
  updateSelectizeInput(session, "boro_filter", choices = unique(nyc_crimes$BORO_NM), server = TRUE)
  updateSelectizeInput(session, "date_map", choices = unique(nyc_crimes$MONTH_YEAR), server = TRUE)
  ##END OF FILTERS FOR THE MAP
  
  ##START OF FILTER FOR THE BORO STATS
  updateSelectizeInput(session, "b_boro_stats", choices = unique(nyc_crimes$BORO_NM), server = TRUE)
  ##END OF FILTER FOR BORO STATS
  
  ##START OF FILTER FOR THE CRIME STATS
  updateSelectizeInput(session, "c_crime_stats", choices = unique(nyc_crimes$OFNS_DESC), server = TRUE)
  ##END OF CRIME FILTER STATS
  ### END OF FILTERS
  
  
  ###REACTIVE FILTERS
  ##TABLE
  filtered_data = nyc_crimes
  data_filter = reactive({
    if(length(input$crimes)) {
      filtered_data = filtered_data %>% filter(.,OFNS_DESC == input$crimes)
    }
    if(length(input$dow)) {
      filtered_data = filtered_data %>% filter(.,DOW %in% input$dow)
    }
    if(length(input$crime_time)) {
      filtered_data = filtered_data %>% filter(.,TIME_OF_DAY %in% input$crime_time)
    }
    if(length(input$boro_filter)) {
      filtered_data = filtered_data %>% filter(.,BORO_NM %in% input$boro_filter)
    }
    filtered_data
  })
  ##END TABLE
  
  ##MAP REACTIVE
  filtered_map = nyc_crimes[!(is.na(Latitude) | is.na(Longitude))]
  map_filter = reactive({
    filtered_map = filtered_map %>% filter(.,MONTH_YEAR == input$date_map)
    if(input$boro_map != "ANY BOROUGH") {
      filtered_map = filtered_map %>% filter(.,BORO_NM == input$boro_map)
    }
    if(input$crime_map != "ANY CRIME") {
      filtered_map = filtered_map %>% filter(.,OFNS_DESC == input$crime_map)
    }
    return(filtered_map)
  })
  ##END MAP
  
  #realized these reactives might not be needed, since can be done in render
  ##BORO STATS
  # filtered_boro_crimes = nyc_crimes
  # boro_crimes_filter = reactive({
  #   filtered_boro_crimes = filtered_boro_crimes %>% filter(.,BORO_NM == input$b_boro_stats)
  #   filtered_boro_crimes = filtered_boro_crimes %>% filter(.,OFNS_DESC %in% input$b_crime_stats)
  #   return(filtered_boro_crimes)
  # })
  ##END BORO STATS
  
  ##CRIME STATS
  # filtered_crime_boros = nyc_crimes
  # crime_boros_filter = reactive({
  #   filtered_crime_boros = filtered_crime_boros %>% filter(.,BORO_NM == input$b_boro_stats)
  #   filtered_crime_boros = filtered_crime_boros %>% filter(.,OFNS_DESC %in% input$b_crime_stats)
  #   return(filtered_crime_boros)
  # })

  ##END OF CRIME STATS
  ###END OF REACTIVE
  
  ###RENDERING PLOTLY GRAPHS
  ##BORO TAB
  #YEAR TO YEAR
  filtered_boro_crimes = nyc_crimes
  output$boro_year_plot = renderPlot({
    filtered_boro_crimes = filtered_boro_crimes %>% filter(.,BORO_NM == input$b_boro_stats)
    filtered_boro_crimes = filtered_boro_crimes %>% filter(.,OFNS_DESC %in% input$b_crime_stats)
    filtered_boro_crimes = filtered_boro_crimes %>% 
      group_by(., OFNS_DESC, YEAR) %>%
      summarize(count = n())
    print(filtered_boro_crimes)
    
    ggplot(filtered_boro_crimes) + geom_line(aes(x = YEAR, y = count, color = OFNS_DESC), stat = "identity")
  })
  
  
  
  ##END OF BORO TAB
  
  ##CRIME TAB
  
  
  
  
  
  
  
  ##END OF CRIME TAB
    ###END OF PLOTLY GRAPHS
  
  ###OUTPUTTING THE INTERACTIVE TABLE
  ###[XX]*** WANT TO HIDE LAT/LONG/TIME OF DAY COLUMNS, KY_CD and PD_CD
  output$table <- renderDataTable({
    #DOESN"T DEACTIVATE SEARCHING. WILL COME BACK TO ***[XX]
    options = list(searching = FALSE)
    datatable(data_filter(), rownames=TRUE) %>%
      formatStyle(input$selected,
                  background="skyblue", fontWeight='bold')
    
  })
  ###END OF TABLE OUTPUT
})












####CODE TO REFERENCE IN CASE OF ERRORS


##START OF ADDING DATA PINPOINTS (CLUSTER)
# names(nyc_crimes) %>%
#   walk(function(df) {
#     l <<- l %>%
#       addMarkers(data=quakes.df[[df]],
#                  lng=~long, lat=~lat,
#                  label=~as.character(mag),
#                  popup=~as.character(mag),
#                  group = df,
#                  clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
#                  labelOptions = labelOptions(noHide = F,
#                                              direction = 'auto'))
#   })


#BUILDING OF REACTIVE FILTER
# crime_filter = reactive({
#   filtered_data = nyc_crimes %>%
#   if(length(input$crimes)) {
#     #Trying to filter on multiple different crimes seleted
#     filtered_data$c1 = grepl(paste(input$crimes, collapse = "|"), filtered_data$OFNS_CD)
#   }
#   else {
#     #if there is no length in crimes, then give all
#     filtered_data$c1 = TRUE 
#     }
# })
# 
# shinyServer(function(input, output, session) {
#   
#   updateSelectizeInput(session, 'country', choices = unique(data_test$Country), server = TRUE)
#   
#   dataset <- reactive({
#     data <- data_test
#     if (length(input$country)){
#       data$c1 <- grepl(paste(input$country, collapse = "|"), data$Country)
#     }
#     else {
#       data$c1 <- TRUE
#     }
#     
#     if (length(input$geogPref)){
#       data$c2 <- grepl(paste(input$geogPref, collapse = "|"), data$Region)
#     }
#     else {
#       data$c2 <- TRUE
#     }
#     
#     data[data$c1  & data$c2 ,c("Name", "Contact", "ContactPerson")]
#   })
#   
#   output$results <- DT::renderDataTable(
#     DT::datatable( dataset(),
#                    rownames = FALSE, options = list(searchable = FALSE)
#     )) 
# })

# leaflet() %>% addTiles() %>% # Add default OpenStreetMap map tiles
#   addMarkers(lng=-74.0059, lat=40.7128, popup="New York City")

# output$distPlot = renderPlot({
#   if(input$select == 1) {
#     x = faithful[,1]
#   } else if (input$select == 2) {
#     x = faithful[,2]
#   }
#   else {
#     print("ERROR")
#   }
#   bins = input$bins
#   hist(x, breaks = bins,
#        col = input$color,
#        border = "white")
#   output$value <- renderPrint({ input$select })
# })

#### NEW SERVER