library(shiny)
library(shinydashboard)
library(data.table)
library(leaflet)
library(maps)
library(DT)
library(rgdal)
library(ggthemes)

#READ THE INITIAL DATA
nyc_crimes = fread(input="D:/NYC-Data-Science/Shiny-Project/Data/NYC_CRIMES_SEMICLEAN.csv", drop = "V1",
                   header=TRUE) 
#MAKING THE OPTIONS 
capitalizeFirst= function(x) { gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(x), perl=TRUE)}
boro_list = c("Citywide", capitalizeFirst(unique(nyc_crimes$BORO_NM)))

#THIS SEEMS INEFFICIENT. WILL COME BACK *** [XX]
#Need map_crimes and crime_list
map_crimes = c("Any Crime", capitalizeFirst(unique(nyc_crimes$OFNS_DESC[nyc_crimes$OFNS_DESC != "RAPE"])))


boro_layer <- readOGR(path.expand("D:/NYC-Data-Science/Shiny-Project/Data/nybb_15d"), "nybb")
boro_layer <- spTransform(boro_layer, CRS("+proj=longlat +datum=WGS84"))

###NEW GLOBAL