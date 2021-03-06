###CREATION OF UI
shinyUI(dashboardPage(
  skin = "black",
  dashboardHeader(
    title = "NYC Crime Statistics",
    titleWidth = 250
  ),
  
  
  dashboardSidebar(
    width = 250,
    sidebarUserPanel("Michael Goldman", "mjgoldman16@gmail.com",
                     image = "https://media-exp2.licdn.com/mpr/mpr/shrinknp_400_400/AAIA_wDGAAAAAQAAAAAAAApdAAAAJGQ0NmY1Y2I2LTA4OWQtNGFiNy1hMDI3LTk0ZjU2YzA1NDQ4MA.jpg"),
    sidebarMenu(
      menuItem("Maps", tabName = "map_dropdown", icon = icon("bars"),
               menuItem("Cluster Map", tabName = "map", icon = icon("map")),
               menuItem("Heat Map", tabName = "heatmap", icon = icon("fire"))),
      menuItem("Statistics", tabName = "stats_dropdown", icon = icon("bars"),
               menuItem("Borough Statistics", tabName = "boro_stats", icon = icon("bar-chart")),
               menuItem("Crime Statistics", tabName = "crime_stats", icon = icon("user"))),
      menuItem("View the Data", tabName = "table", icon = icon("table"))
    )
  ),
  
  
  dashboardBody(
    #Creation of CSS custom edits (altering header text)
    tags$head(
      tags$link(rel = "stylesheet", type = 'text/css', href = "custom.css")
    ),
    
    #Creation of menu items
    tabItems(
      tabItem(tabName = "map",
              #Creation and outputting of cluster map
              leafletOutput("map", height = "900", width="100%"),
              
              #Creation of data filter panel for the cluster map
              absolutePanel(id = "controls", class = "panel panel-default",
                            draggable = TRUE, top = 160, right = "auto", bottom = "auto",
                            width = 330, height = "auto", style = "padding: 8px; opacity: 0.92; background: #f7f6fc;",
                            h2("Data Filter"),
                            selectizeInput("date_map", label = "Select a Month and Year:", choices = NULL, multiple = FALSE),
                            selectInput("crime_map", label = "Select Crime:", map_crimes),
                            selectInput("boro_map", label = "Select a Borough:", boro_list),
                            textInput("location", label = "Type an address below:", value = ""),
                            actionButton("search", label = "Find Address"),
                            checkboxInput("boro_layer", "Show Boroughs", value = FALSE)
              )
      ),
      
      tabItem(tabName = "heatmap",
              #Creation and outputting of heat map
              leafletOutput("heat", height = "900", width="100%"),
              
              #Creation of data filter panel for the heat map
              absolutePanel(id = "controls", class = "panel panel-default",
                            draggable = TRUE, top = 160, right = "auto", bottom = "auto",
                            width = 330, height = "auto", style = "padding: 8px; opacity: 0.92; background: #f7f6fc;",
                            h2("Data Filter"),
                            selectizeInput("date_heat", label = "Select a Month and Year:", choices = NULL, multiple = FALSE),
                            selectInput("crime_heat", label = h3("Select Crime:"), map_crimes),
                            selectInput("boro_heat", label = h3("Select a Borough:"), boro_list),
                            textInput("location_heat", label = h3("Type an address below:"), value = ""),
                            actionButton("search_heat", label = "Find Address")
              )
      ),
      
      tabItem(tabName = "boro_stats",
              #A row of filters for the borough statistics.
              fluidRow(column(3, selectizeInput("b_boro_stats",label="Pick a Borough to focus on:", choices = NULL, multiple = FALSE)),
                       column(3, selectizeInput("b_crime_stats", label = "Pick the Crimes you would like to graph", choices = NULL, multiple = TRUE)),
                       
                       #Graph button to avoid chaining functions together from fast input from the user
                       column(3,actionButton("b_graph", "Graph"),  style = "padding-top: 20px")),
              
              #Plotting of borough graphs
              fluidRow(box(plotOutput("boro_year_plot")),
                       box(plotOutput("boro_month_plot")),
                       box(plotOutput("boro_DOW_plot")),
                       box(plotOutput("boro_time_plot")))
      ),
      
      tabItem(tabName = "crime_stats",
              #Filter for the crime statistics
              column(12,selectizeInput("c_crime_stats",label="Pick a Crime to focus on:", choices = NULL, multiple = FALSE)),
              
              #Plotting of crime graphs
              fluidRow(box(plotOutput("crime_year_plot")),
                       box(plotOutput("crime_month_plot")),
                       box(plotOutput("crime_DOW_plot")),
                       box(plotOutput("crime_time_plot")))
      ),
      
      #Tab to view the entire datatable
      tabItem(tabName = "table",
              #A row of filters for the datatable
              fluidRow(column(3,selectizeInput("crimes",label="Pick a Crime:",choices= NULL, multiple = TRUE)),
                       column(3,selectizeInput("dow",label="Pick a Day of the Week:",choices= NULL, multiple = TRUE)),
                       column(3,selectizeInput("crime_time", label="Pick a Time:", choices = NULL, multiple = TRUE)),
                       column(3,selectizeInput("boro_filter", label="Pick a Borough:", choices = NULL, multiple = TRUE)),
                       dataTableOutput("table"))
      )
    )
  )
))