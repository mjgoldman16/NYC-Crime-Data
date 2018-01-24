shinyUI(dashboardPage(
  dashboardHeader(
    title = "NYC Crime Statistics"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About the Data", tabName = "about", icon = icon("database")),
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Borough Statistics", tabName = "boro_stats", icon = icon("bar-chart")),
      menuItem("Crime Statistics", tabName = "crime_stats", icon = icon("user")),
      menuItem("About Me", tabName = "me", icon = icon("bath")),
      menuItem("TEMP", tabName = "temp", icon = icon("warning"))
      #will likely shift these over to main bodies since it will make more sense when comparing each other
      #will have a select for maps, will have a checkbox group for stats
      # selectizeInput("location",
      #                "Select a Borough:",
      #                boroughs),
      # selectizeInput("type",
      #                "Select a Type of Crime:",
      #                type),
      # selectizeInput("time",
      #                "Select a Time the Crime Occurred",
      #                crime_time),
      # dateInput("date", label = h3("Date input"), value = "2014-01")
    )
  ),
  
  #START OF DASHBOARD BODY
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              leafletOutput("map", height = "900"),
              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                            width = 330, height = "auto",
                            
                            h2("Data Filter"),
                            dateInput("date_map", label = "Select a Date:",
                                      format = "mm/yyyy",
                                      value = "2006-01"
                            ),
                            selectInput("crime_map", label = h3("Select Crime:"), map_crimes),
                            # selectInput("time", label = h3("Select a Time:"), crime_time),
                            checkboxInput("boro_layer", "Show Boroughs", value = FALSE)

              )
      ),
      tabItem(tabName = "boro_stats",
              h3("Filters: date, time, type, borough"),
              h3("want a way to compare individual but also select total 7. "),
              h3("without regard to others: DOW, year to year, month to month, time."),
              h3("want a way to see (ir)respective to other filters. if you select any, dyon't take into account"),
              h3("")
      ),
      tabItem(tabName = "crime_stats",
              "test"),
      tabItem(tabName = "about",
              "NOTE ON RAPE: To further protect victim identities, rape and sex crime offenses are not geocoded."),
      tabItem(tabName = "me",
              "Who.... who am I?"),
      tabItem(tabName = "temp",
              column(4,selectizeInput("crimes",label="Pick a Crime:",choices= NULL, multiple = TRUE)),
              column(4,selectizeInput("dow",label="Pick a Day of the Week:",choices= NULL, multiple = TRUE)),
              column(4,selectizeInput("crime_time", label="Pick a Time:", choices = NULL, multiple = TRUE)),
              column(4,selectizeInput("boro_filter", label="Pick a Borough:", choices = NULL, multiple = TRUE)),
              dataTableOutput("table")
      )
    )
  )
))

# fluidPage(
#   titlePanel("Old Faithful Geyser Data"),
#   sidebarLayout(
#     sidebarPanel(
#       selectInput("select", label = h3("Data displayed:"), 
#                   choices = list("Eruptions Length" = 1, "Waiting Time" = 2), 
#                   selected = 1),
#       selectInput("color", label = h3("Color of bars:"), 
#                   choices = list("Blue", "Red", "Green", "Yellow", "Black"), 
#                   selected = "Blue"),
#       sliderInput("bins", "Number of bins:",
#                   min = 1, max = 50, value = 30)
#     ),
#     mainPanel(
#       plotOutput("distPlot")
#     )
#   ),
#   fluidRow(column(3, verbatimTextOutput("value")))
# )



###NEW UI