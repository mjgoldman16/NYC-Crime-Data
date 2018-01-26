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
      menuItem("View the Data", tabName = "table", icon = icon("warning"))
    )
  ),
  
  #START OF DASHBOARD BODY
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              leafletOutput("map", height = "900"),
              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                            draggable = TRUE, top = 160, right = "auto", bottom = "auto",
                            width = 330, height = "auto", style = "padding: 8px; opacity: 0.92; background: #f7f6fc;",
                            h2("Data Filter"),
                            selectizeInput("date_map", label = "Select a Month and Year:", choices = NULL, multiple = FALSE),
                            selectInput("crime_map", label = h3("Select Crime:"), map_crimes),
                            selectInput("boro_map", label = h3("Select a Borough:"), boro_list),
                            checkboxInput("boro_layer", "Show Boroughs", value = FALSE)
              )
      ),
      tabItem(tabName = "boro_stats",
              absolutePanel(top = 45, left = 0, right = 0,
                            fixed = TRUE,
                            selectizeInput("b_boro_stats",label="Pick a Borough to focus on:", choices = NULL, multiple = FALSE),
                            style = "padding: 8px; padding-left: 240px; border-bottom: 1px solid #CCC; background: #FFFFEE;"),
              h1("_"), #***[XX] used to pad the text/graphs underneath the absolute panel. Will fix later
              box(checkboxGroupInput("b_crime_stats", label="Pick the Crimes you would like to graph:", 
                                 unique(nyc_crimes$OFNS_DESC),
                                 selected = NULL)),
              plotOutput("boro_year_plot")
      ),
      tabItem(tabName = "crime_stats",
              absolutePanel(top = 45, left = 0, right = 0,
                            fixed = TRUE,
                            selectizeInput("c_crime_stats",label="Pick a Crime to focus on:", choices = NULL, multiple = FALSE),
                            style = "padding: 8px; padding-left: 240px; border-bottom: 1px solid #CCC; background: #FFFFEE;"),
              h1("_"), #***[XX] used to pad the text/graphs underneath the absolute panel. Will fix later
              box(checkboxGroupInput("c_boro_stats", label="Pick the Boroughs you would like to graph:", 
                                 unique(nyc_crimes$BORO_NM),
                                 selected = NULL))
      ),
      tabItem(tabName = "about",
              "NOTE ON RAPE: To further protect victim identities, rape and sex crime offenses are not geocoded."),
      tabItem(tabName = "me",
              "Who.... who am I?"),
      
      #View the entire data
      tabItem(tabName = "table",
              column(4,selectizeInput("crimes",label="Pick a Crime:",choices= NULL, multiple = TRUE)),
              column(4,selectizeInput("dow",label="Pick a Day of the Week:",choices= NULL, multiple = TRUE)),
              column(4,selectizeInput("crime_time", label="Pick a Time:", choices = NULL, multiple = TRUE)),
              column(4,selectizeInput("boro_filter", label="Pick a Borough:", choices = NULL, multiple = TRUE)),
              dataTableOutput("table")
      )
    )
  )
))


###NEW UI