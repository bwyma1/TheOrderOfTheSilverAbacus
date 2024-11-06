library(shiny)
library(leaflet)

# Define the UI
ui <- fluidPage(
  titlePanel("Leaflet Map Display"),
  sidebarLayout(
    sidebarPanel(
      actionButton("show_map", "Show Map")
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  observeEvent(input$show_map, {
    # Create the leaflet map and render it when the button is pressed
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addMarkers(lng = -122.4194, lat = 37.7749, popup = "San Francisco") %>%
        addMarkers(lng = -122.431297, lat = 37.773972, popup = "Marker 2")
    })
  })
}

# Run the app
shinyApp(ui, server)
