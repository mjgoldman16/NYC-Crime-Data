library(shiny)
library(shinydashboard)
library(data.table)
library(leaflet)
library(DT)
library(rgdal)
library(ggthemes)
library(plotly)
library(lubridate)
library(dplyr)
library(ggmap)
library(leaflet.extras)
library(stringr)


#READ THE INITIAL DATA
#readRDS
nyc_crimes = fread(input="D:/NYC-Data-Science/Shiny-Project/Data/NYC_CRIMES_SEMICLEAN.csv", drop = "V1",
                   header=TRUE) 
#MAKING THE OPTIONS 
capitalizeFirst= function(x) { gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(x), perl=TRUE)}
boro_list = c("ANY BOROUGH", unique(nyc_crimes$BORO_NM))

#THIS SEEMS INEFFICIENT. WILL COME BACK *** [XX]
#Need map_crimes and crime_list
map_crimes = c("ANY CRIME", unique(nyc_crimes$OFNS_DESC[nyc_crimes$OFNS_DESC != "RAPE"]))

boro_layer <- readOGR(path.expand("D:/NYC-Data-Science/Shiny-Project/Data/nybb_15d"), "nybb")
boro_layer <- spTransform(boro_layer, CRS("+proj=longlat +datum=WGS84"))

#register google API key
register_google(key = "AIzaSyCd7c8Dy5-eJV2uJnNTSvqZFo36fRyOZWM")

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
}

###NEW GLOBAL