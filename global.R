library(shiny)
library(shinydashboard)
library(data.table)
library(leaflet)
library(DT)
library(rgdal)
library(plotly)
library(lubridate)
library(dplyr)
library(ggmap)
library(leaflet.extras)
library(stringr)


#Reading of the formatted data
nyc_crimes = fread(input="D:/NYC-Data-Science/Shiny-Project/Data/NYC_CRIMES.csv", drop = "V1",
                   header=TRUE) 

#Function to clean the underlying data
# capitalizeFirst= function(x) { gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(x), perl=TRUE)}

#Initialize boroughs
boro_list = c("ANY BOROUGH", unique(nyc_crimes$BORO_NM))

#Create a list of crimes for the map (since rape is not geolocated)
map_crimes = c("ANY CRIME", unique(nyc_crimes$OFNS_DESC[nyc_crimes$OFNS_DESC != "RAPE"]))

#Open and transform the shape file for the boroughs
boro_layer <- readOGR(path.expand("D:/NYC-Data-Science/Shiny-Project/Data/nybb_15d"), "nybb")
boro_layer <- spTransform(boro_layer, CRS("+proj=longlat +datum=WGS84"))

#register google API key
register_google(key = "AIzaSyCd7c8Dy5-eJV2uJnNTSvqZFo36fRyOZWM")
