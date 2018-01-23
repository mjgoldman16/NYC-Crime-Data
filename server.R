shinyServer(function(input, output, session){
  
  
  # CREATION OF THE MAP
  output$map = renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldStreetMap") %>%
      setView(-73.83, 40.7, 10) %>%
      addTiles(group = "OSM default)")
  })
  
  # CREATION OF THE CRIME FILTER FOR AN INTERACTIVE DATATABLE
  updateSelectizeInput(session, "crimes", choices = unique(nyc_crimes$OFNS_DESC), server = TRUE)
  
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
  filtered_data = nyc_crimes
  crime_filter = reactive({
    if(length(input$crimes)) {
      filtered_data = filtered_data %>% filter(.,OFNS_DESC == input$crimes)
    } else {
      filtered_data
    }
    
    # if(length(input$borough)) {
    #   filtered_data = filtered_data %>% filter(.,BORO_NM == input$borough) 
    # } else {
    #   filtered_data
    # }
  })
  
  
  
  
  #OUTPUTTING THE INTERACTIVE DATA
  output$table <- renderDataTable({
    options = list(scrollX = TRUE)
    datatable(crime_filter(), rownames=FALSE) %>%
      formatStyle(input$selected,
                  background="skyblue", fontWeight='bold')
    
  })
  
  output$crime <- renderPrint({ input$crime })
  
})

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