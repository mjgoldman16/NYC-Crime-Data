shinyServer(function(input, output){
  
  output$map = renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldStreetMap")
  })
  
  output$table <- renderDataTable({
    datatable(nyc_crimes, rownames=FALSE) %>%
      formatStyle(input$selected,
                  background="skyblue", fontWeight='bold')
  })
})



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