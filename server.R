shinyServer(function(input, output, session){
  ################################## DRAWING OF INITIAL CLUSTER MAP ##################################
  output$map = renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldStreetMap") %>%
      setView(-73.945242, 40.710610, 11)
  })
  ##################################################################################################
  
  
  
  ###PURPOSE: take in a data frame and assigns colors to map icons based on crime type
  ###OUTPUT: Strings (of various colors)
  ###NOTE: Function had to be unnamed because of interaction between addingAwesomeMarkers and named vectors
  getColor = function(df) {
    unname(sapply(df$OFNS_DESC, function(offense) {
      if(offense == "MURDER & NON-NEGL. MANSLAUGHTER") {
        "red"
      } 
      else if(offense == "FELONY ASSAULT") {
        "orange"
      } 
      else if(offense == "ROBBERY") {
        "green"
      } 
      else if(offense == "BURGLARY") {
        "lightblue"
      } 
      else if(offense == "GRAND LARCENY") {
        "blue"
      } 
      else if(offense == "GRAND LARCENY OF MOTOR VEHICLE") {
        "purple"
      } 
      else {
        "black"
      }
    }))
  }
  
  
  ################################## ADDING OF CLUSTER MARKERS ##################################
  filtered_map = nyc_crimes[!(is.na(Latitude) | is.na(Longitude))]
  
  #Check if any of the cluster map filters or borough layers have been triggered
  observeEvent(c(input$boro_layer, input$date_map, input$crime_map, input$boro_map), {
    #Filter the data accordingly
    if(length(input$date_map)) {
      filtered_map = filtered_map %>% filter(.,MONTH_YEAR == input$date_map) %>% na.omit()
    }
    if(input$boro_map != "ANY BOROUGH") {
      filtered_map = filtered_map %>% filter(.,BORO_NM == input$boro_map)
    }
    if(input$crime_map != "ANY CRIME") {
      filtered_map = filtered_map %>% filter(.,OFNS_DESC == input$crime_map)
    }
    
    #Initialize icons, calling getColor to find the proper color
    icons = awesomeIcons(
      icon = "ion-alert-circled",
      library = "ion",
      markerColor = getColor(filtered_map)
    )
    
    #Add markers based on the filtered data
    leafletProxy("map", data = filtered_map) %>% 
      clearMarkerClusters() %>%
      addAwesomeMarkers(~Longitude, ~Latitude, icon = icons,
                        clusterOptions = markerClusterOptions(),
                        popup = paste("Type of Crime:", filtered_map$OFNS_DESC, "<br>",
                                      "Additional Details:", filtered_map$PD_DESC, "<br>",
                                      "Location:", filtered_map$PREM_TYP_DESC, "<br>",
                                      "Date Occurred:", filtered_map$DATE, "<br>",
                                      "Time Occurred:", filtered_map$TIME, "<br>")) %>%
                                      {ifelse(input$boro_layer, 
                                              leafletProxy("map") %>% 
                                                addPolygons(data=boro_layer,
                                                            color = topo.colors(5,alpha = NULL),
                                                            fillColor = topo.colors(5,alpha = NULL),
                                                            smoothFactor = .5,
                                                            layerId = LETTERS[1:6]),
                                              leafletProxy("map") %>% removeShape(layerId = LETTERS[1:6]))}
    
  })
  
  #Check to see if an address was put into the search bar
  observeEvent(c(input$search), {
    if(input$location != "") {
      loc = geocode(input$location)
      leafletProxy("map") %>%
        setView(loc$lon,loc$lat,17)
    }
  })
  ###############################################################################################
  
  
  
  ################################## DRAWING OF INITIAL HEAT MAP ##################################
  output$heat = renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(-73.945242, 40.710610, 11) %>%
      addPolygons(data = boro_layer,
                  stroke = FALSE, 
                  smoothFactor = 0.5)
  })
  #################################################################################################
  
  
  
  ################################## ADDING OF HEAT INDICATORS ##################################
  filtered_map = nyc_crimes[!(is.na(Latitude) | is.na(Longitude))]
  #See if any of the heat map filters were triggered
  observeEvent(c(input$date_heat, input$crime_heat, input$boro_heat), {
    #Filter the data accordingly
    if(length(input$date_heat)) {
      filtered_map = filtered_map %>% filter(.,MONTH_YEAR == input$date_heat) %>% na.omit()
    }
    if(input$boro_heat != "ANY BOROUGH") {
      filtered_map = filtered_map %>% filter(.,BORO_NM == input$boro_heat)
    }
    if(input$crime_heat != "ANY CRIME") {
      filtered_map = filtered_map %>% filter(.,OFNS_DESC == input$crime_heat)
    }
    
    #Add heat indicators based on the filtered data
    leafletProxy("heat", data = filtered_map) %>%
      clearWebGLHeatmap() %>%
      addWebGLHeatmap(~Longitude, ~Latitude,
                      size=15, units = "px", alphaRange = .5)
  })
  
  #Check to see if an address was put into the search bar
  observeEvent(c(input$search_heat), {
    if(input$location != "") {
      loc = geocode(input$location_heat)
      leafletProxy("heat") %>%
        setView(loc$lon,loc$lat,17)
    }
  })
  ###############################################################################################
  
  
  
  ################################## PROVIDING CHOICES FOR SELECTIZEINPUT ##################################
  
  ##### INPUT FILTERS FOR THE TABLE#####
  updateSelectizeInput(session, "crimes", choices = unique(nyc_crimes$OFNS_DESC), server = TRUE)
  updateSelectizeInput(session, "dow", choices = unique(nyc_crimes$DOW), server = TRUE)
  updateSelectizeInput(session, "crime_time", 
                       choices = c("Early Morning (0:00-5:59)" = "Early Morning", 
                                   "Morning (6:00-11:59)" = "Morning", 
                                   "Afternoon (12:00-17:59)" = "Afternoon", 
                                   "Evening (18:00-23:59)" = "Evening"), 
                       server = TRUE)
  updateSelectizeInput(session, "boro_filter", choices = unique(nyc_crimes$BORO_NM), server = TRUE)
  ######################################
  
  ##### INPUT FILTERS FOR THE CLUSTER AND HEAT MAPS #####
  updateSelectizeInput(session, "date_map", choices = unique(nyc_crimes$MONTH_YEAR), server = TRUE)
  updateSelectizeInput(session, "date_heat", choices = unique(nyc_crimes$MONTH_YEAR), server = TRUE)
  #######################################################
  
  ##### INPUT FILTERS FOR THE BOROUGH STASTITICS #####
  updateSelectizeInput(session, "b_boro_stats", choices = unique(nyc_crimes$BORO_NM), server = TRUE)
  updateSelectizeInput(session, "b_crime_stats", choices = unique(nyc_crimes$OFNS_DESC), server = TRUE)
  ####################################################
  
  ##### INPUT FILTERS FOR THE CRIME STASTITICS #####
  updateSelectizeInput(session, "c_crime_stats", choices = unique(nyc_crimes$OFNS_DESC), server = TRUE)
  ####################################################
  
  ##########################################################################################################
  
  
  
  ################################## FILTERING OF THE DATATABLE ##################################
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
    return(filtered_data)
  })
  ################################################################################################
  
  
  
  ################################## FILTERING AND PLOTTING OF BOROUGH STATISTICS ##################################
  filtered_boro_crimes = nyc_crimes
  observeEvent(c(input$b_graph), {
    #This filtering and grouping will need to occur for all of the four graphs
    filtered_boro_crimes = filtered_boro_crimes %>% filter(.,BORO_NM == input$b_boro_stats)
    filtered_boro_crimes = filtered_boro_crimes %>% filter(.,OFNS_DESC %in% input$b_crime_stats) %>%
      group_by(., OFNS_DESC)
    
    ##### YEAR TO YEAR #####
    output$boro_year_plot = renderPlot({
      withProgress({
        setProgress(message = "Drawing Year plot...")
        #Want account for population when counting total number of crimes
        filtered_boro_crimes = filtered_boro_crimes %>%
          group_by(., OFNS_DESC, YEAR, POPULATION) %>%
          mutate(ratio = n()*10000/POPULATION) %>%
          distinct()
        ggplot(filtered_boro_crimes) + 
          geom_line(aes(x = YEAR, y = ratio, color = str_wrap(OFNS_DESC,16), group = OFNS_DESC), stat = "identity", size = 1) + 
          theme_bw() + 
          theme(text = element_text(size=16), legend.position = "bottom") +
          guides(color = guide_legend((title = "Types of Crimes:"))) +
          labs(title = "Total Crimes by Year (per 10,000)", x = "Year", y = "Number of Crimes (per 10,000 People)")
      })
    })
    ########################
    
    ###### MONTH TO MONTH #####
    output$boro_month_plot = renderPlot({
      withProgress({
        setProgress(message = "Drawing Month plot...")
        filtered_boro_crimes = filtered_boro_crimes %>% 
          #Display the percentage of a specific type of crime that occur within that month (eg: 8% of assualts occur in january vs 14% in march)
          mutate(count_total = n()) %>%
          group_by(., OFNS_DESC, MONTH, count_total) %>%
          mutate(ratio = n()*100/count_total) %>%
          distinct()
        filtered_boro_crimes$MONTH = factor(filtered_boro_crimes$MONTH, levels=month.name[1:12])
        ggplot(filtered_boro_crimes) + 
          geom_line(aes(x = MONTH, y = ratio, color = str_wrap(OFNS_DESC,16), group = OFNS_DESC), stat = "identity", size = 1) + 
          theme_bw() + 
          guides(color = guide_legend((title = "Types of Crimes:"))) +
          theme(text = element_text(size=16), axis.text.x = element_text(angle=-45, hjust = -.05), legend.position = "bottom") +
          labs(title = "Crime Trends by Month", x = "Month", y = "Percentage of Crimes that Occurred")
      })
    })
    ###########################
    
    ##### DAY OF THE WEEK #####
    output$boro_DOW_plot = renderPlot({
      withProgress({
        setProgress(message = "Drawing DOW plot...")
        filtered_boro_crimes = filtered_boro_crimes %>%
          #Display the percentage of a specific type of crime that occur within that day (eg: 3% of murders occur on monday vs 9% in wednesday)
          mutate(count_total = n()) %>%
          group_by(., OFNS_DESC, DOW, count_total) %>%
          mutate(ratio = n()*100/count_total) %>%
          distinct()
        filtered_boro_crimes$DOW = factor(filtered_boro_crimes$DOW, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
        ggplot(filtered_boro_crimes) + 
          geom_line(aes(x = DOW, y = ratio, color = str_wrap(OFNS_DESC,16), group = OFNS_DESC), stat = "identity", size = 1)+ 
          theme_bw() + 
          theme(text = element_text(size=16), axis.text.x = element_text(angle=-45, hjust = -.05), legend.position = "bottom") +
          guides(color = guide_legend((title = "Types of Crimes:"))) +
          labs(title = "Crime Trends by Day of the Week", x = "Days of the Week", y = "Percentage of Crimes that Occurred")
      })
    })
    ###########################
    
    ##### TIME OF DAY #####
    output$boro_time_plot = renderPlot({
      withProgress({
        setProgress(message = "Drawing Tinme of Day plot...")
        filtered_boro_crimes = filtered_boro_crimes %>%
          #Display the percentage of a specific type of crime that occur a certain time of day (eg: 3% of murders occur at 6:00 vs 9% at 20:00)
          mutate(count_total = n()) %>%
          group_by(., OFNS_DESC, HOUR, count_total) %>%
          mutate(ratio = n()*100/count_total) %>%
          distinct()
        ggplot(filtered_boro_crimes) + 
          geom_line(aes(x = HOUR, y = ratio, color = str_wrap(OFNS_DESC,16), group = OFNS_DESC), stat = "identity", size = 1) + 
          theme_bw() + 
          guides(color = guide_legend((title = "Types of Crimes:"))) +
          theme(text = element_text(size=16), legend.position = "bottom") +
          labs(title = "Crime Trends by Time of Day", x = "Time of Day (Military Time)", y = "Percentage of Crimes that Occurred")
      })
    })
    #######################
  })
  ##################################################################################################################
  
  
  ################################## FILTERING AND PLOTTING OF CRIME STATISTICS ##################################
  filtered_crime_boros = nyc_crimes
  grouped_crime_boros = reactive({
    #All four graphs will have to be filtered and grouped initially
    filtered_crime_boros = filtered_crime_boros %>% filter(., OFNS_DESC == input$c_crime_stats) %>%
      group_by(., BORO_NM)
    return(filtered_crime_boros)
  })
  
  ##### YEAR TO YEAR #####
  output$crime_year_plot = renderPlot({
    withProgress({
      setProgress(message = "Drawing Year plot...")
      #See boro stats for explanation of calculation
      filtered_crime_boros = filtered_crime_boros %>% 
        filter(., OFNS_DESC == input$c_crime_stats) %>%
        group_by(., BORO_NM, YEAR, POPULATION) %>%
        summarize(count = n())
      ggplot(filtered_crime_boros) + 
        geom_line(aes(x = YEAR, y = (count/POPULATION)*10000, color = str_wrap(BORO_NM,16), group = BORO_NM), stat = "identity", size = 1) +
        theme_bw() + 
        theme(text = element_text(size=16), legend.position = "bottom") +
        guides(color = guide_legend((title = "Boroughs:"))) +
        labs(title = "Total Crimes by Year (per 10,000)", x = "Year", y = "Number of Crimes (per 10,000 People)")
    })
  })
  ########################
  
  ##### MONTH TO MONTH #####
  output$crime_month_plot = renderPlot({
    withProgress({
      setProgress(message = "Drawing Month plot...")
      
      filtered_crime_boros = grouped_crime_boros() %>%
        mutate(count_total = n()) %>%
        group_by(., OFNS_DESC, MONTH, count_total) %>%
        mutate(ratio = n()*100/count_total) %>%
        distinct()
      filtered_crime_boros$MONTH = factor(filtered_crime_boros$MONTH, levels=month.name[1:12])
      ggplot(filtered_crime_boros) + 
        geom_line(aes(x = MONTH, y = ratio, color = str_wrap(BORO_NM,16), group = BORO_NM), stat = "identity", size = 1) +
        theme_bw() + 
        guides(color = guide_legend((title = "Boroughs:"))) +
        theme(text = element_text(size=16), axis.text.x = element_text(angle=-45, hjust = -.05), legend.position = "bottom") +
        labs(title = "Crime Trends by Month", x = "Months", y = "Percentage of Crimes that Occurred")
    })
  })
  ##########################
  
  
  ##### DAY OF THE WEEK #####
  output$crime_DOW_plot = renderPlot({
    withProgress({
      setProgress(message = "Drawing DOW plot...")
      filtered_crime_boros = grouped_crime_boros() %>%
        mutate(count_total = n()) %>%
        group_by(., OFNS_DESC, DOW, count_total) %>%
        mutate(ratio = n()*100/count_total) %>%
        distinct()
      filtered_crime_boros$DOW = factor(filtered_crime_boros$DOW, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
      ggplot(filtered_crime_boros) + 
        geom_line(aes(x = DOW, y = ratio, color = str_wrap(BORO_NM,16), group = BORO_NM), stat = "identity", size = 1) +
        theme_bw() + 
        guides(color = guide_legend((title = "Boroughs:"))) +
        theme(text = element_text(size=16), axis.text.x = element_text(angle=-45, hjust = -.05), legend.position = "bottom") +
        labs(title = "Crime Trends by Day of the Week", x = "Days of the Week", y = "Percentage of Crimes that Occurred")
    })
  })
  ###########################
  
  ##### TIME OF DAY #####
  output$crime_time_plot = renderPlot({
    withProgress({
      setProgress(message = "Drawing Time of Day plot...")
      filtered_crime_boros = grouped_crime_boros() %>%
        mutate(count_total = n()) %>%
        group_by(., OFNS_DESC, HOUR, count_total) %>%
        mutate(ratio = n()*100/count_total) %>%
        distinct()
      ggplot(filtered_crime_boros) + 
        geom_line(aes(x = HOUR, y = ratio, color = str_wrap(BORO_NM,16), group = BORO_NM), stat = "identity", size = 1) +
        theme_bw() + 
        guides(color = guide_legend((title = "Boroughs:"))) +
        theme(text = element_text(size=16), legend.position = "bottom") +
        labs(title = "Crime Trends by Time of Day", x = "Time of Day (Military Time)", y = "Percentage of Crimes that Occurred")
    })
  })
  #######################
  ################################## FILTERING AND PLOTTING OF CRIME STATISTICS ##################################
  
  
  ################################## DISPLAYING OF DATA TABLE ##################################
  output$table <- renderDataTable({
    datatable(data_filter(), rownames=TRUE, options = list(columnDefs = list(list(visible = FALSE, targets = c(2,3,4,6,7,13,14,15))))) %>%
      formatStyle(input$selected,
                  background="skyblue", fontWeight='bold')
    
  })
  ##############################################################################################
})