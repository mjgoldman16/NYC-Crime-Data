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
      menuItem("TEMP", tabName = "temp", icon = icon("warning")))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              fluidPage(leafletOutput("map"))),
      tabItem(tabName = "stats",
              "Soon to come"),
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
