shinyUI(dashboardPage(
  dashboardHeader(
    title = "NYC Crime Statistics"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Stasitics", tabName = "stats", icon = icon("bar-chart")),
      menuItem("About the Data", tabName = "about", icon = icon("database")),
      menuItem("About Me", tabName = "me", icon = icon("bath")),
      menuItem("TEMP", tabName = "temp", icon = icon("warning")),
      #will likely shift these over to main bodies since it will make more sense when comparing each other
      #will have a select for maps, will have a checkbox group for stats
      selectizeInput("location",
                     "Select a Borough:",
                     boroughs),
      selectizeInput("type",
                     "Select a Type of Crime:",
                     type),
      selectizeInput("time",
                     "Select a Time the Crime Occurred",
                     crimetime)
      )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              fluidPage(leafletOutput("map")),
              #want the map to be the entire page, not just a row
              checkboxInput("borough_layer", "Show Boroughs", value = FALSE),
              checkboxInput("zip_layer", "Show Neighborhoods", value = FALSE),
              dateRangeInput("dates", label = h3("Date range"))
      ),
      tabItem(tabName = "stats",
              h3("Filters: date, time, type, borough"),
              h3("want a way to compare individual but also select total 7. "),
              h3("without regard to others: DOW, year to year, month to month, time."),
              h3("want a way to see (ir)respective to other filters")
              ),
      tabItem(tabName = "about",
              "About to come"),
      tabItem(tabName = "me",
              "Who.... who am I?"),
      tabItem(tabName = "temp",
              # datatable
              fluidRow(box(DT::dataTableOutput("table")))
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
