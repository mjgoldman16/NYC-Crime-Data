shinyUI(dashboardPage(
  dashboardHeader(
    title = "NYC Crime Statistics"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About the Data", tabName = "about", icon = icon("database")),
      menuItem("Maps", tabName = "map_dropdown", icon = icon("bars"),
               menuItem("Cluster-map", tabName = "map", icon = icon("map")),
               menuItem("Heat-map", tabName = "heatmap", icon = icon("fire"))),
      menuItem("Statistics", tabName = "stats_dropdown", icon = icon("bars"),
               menuItem("Borough Statistics", tabName = "boro_stats", icon = icon("bar-chart")),
               menuItem("Crime Statistics", tabName = "crime_stats", icon = icon("user"))),
      menuItem("View the Data", tabName = "table", icon = icon("warning"))
    )
  ),
  
  #START OF DASHBOARD BODY
  dashboardBody(
    tags$head(
      tags$link(href = "custom.css"),
      tags$script(HTML("$('body').addClass('fixed');"))
    ),
    tabItems(
      tabItem(tabName = "about",
              "NOTE ON RAPE: To further protect victim identities, rape and sex crime offenses are not geocoded."),
      tabItem(tabName = "map",
              leafletOutput("map", height = "900", width="100%"),
              absolutePanel(id = "controls", class = "panel panel-default",
                            draggable = TRUE, top = 160, right = "auto", bottom = "auto",
                            width = 330, height = "auto", style = "padding: 8px; opacity: 0.92; background: #f7f6fc;",
                            h2("Data Filter"),
                            selectizeInput("date_map", label = "Select a Month and Year:", choices = NULL, multiple = FALSE),
                            selectInput("crime_map", label = h3("Select Crime:"), map_crimes),
                            selectInput("boro_map", label = h3("Select a Borough:"), boro_list),
                            checkboxInput("boro_layer", "Show Boroughs", value = FALSE)
              )
      ),
      tabItem(tabName = "heatmap",
              "TBD"),
      tabItem(tabName = "boro_stats",
              absolutePanel(top = 45, left = 0, right = 0,
                            selectizeInput("b_boro_stats",label="Pick a Borough to focus on:", choices = NULL, multiple = FALSE),
                            style = "padding: 8px; padding-left: 240px; border-bottom: 1px solid #CCC; background: #FFFFEE;"),
              h1("_"), #***[XX] used to pad the text/graphs underneath the absolute panel. Will fix later
              box(checkboxGroupInput("b_crime_stats", label="Pick the Crimes you would like to graph:", 
                                     unique(nyc_crimes$OFNS_DESC),
                                     selected = NULL)),
              ##[xx]***
              #actionButton("boro_action", label = "GO!"),
              box(plotOutput("boro_year_plot")),
              box(plotOutput("boro_month_plot")),
              box(plotOutput("boro_DOW_plot")),
              box(plotOutput("boro_time_plot"))
      ),
      tabItem(tabName = "crime_stats",
              absolutePanel(top = 45, left = 0, right = 0,
                            selectizeInput("c_crime_stats",label="Pick a Crime to focus on:", choices = NULL, multiple = FALSE),
                            style = "padding: 8px; padding-left: 240px; border-bottom: 1px solid #CCC; background: #FFFFEE;"),
              h1("_"), #***[XX] used to pad the text/graphs underneath the absolute panel. Will fix later
              box(plotOutput("crime_year_plot")),
              box(plotOutput("crime_month_plot")),
              box(plotOutput("crime_DOW_plot")),
              box(plotOutput("crime_time_plot"))
      ),
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