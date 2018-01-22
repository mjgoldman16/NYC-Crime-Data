library(shiny)
library(shinydashboard)
library(leaflet)
library(maps)
library(DT)


capitalizeFirst= function(x) { gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(x), perl=TRUE)}
boroughs = c("Citywide", capitalizeFirst(unique(nyc_crimes$BORO_NM)))
type = c("Any Crime", capitalizeFirst(unique(nyc_crimes$OFNS_DESC)))
crimetime = c("Any Time","Early Morning (0:00-5:59)", "Morning (6:00-11:59)", "Afternoon (12:00-17:59)", "Evening (18:00-23:59)")