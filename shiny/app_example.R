library(shiny)
library(DT)
library(leaflet)
library(sf)

# Example data (Replace with your actual data)
table_data <- data.frame(activity_name = c("Hiking", "Cycling", "Kayaking"))
spatial_data <- st_as_sf(data.frame(
  activity_name = c("Hiking", "Cycling", "Kayaking"),
  lon = c(-122.4, -122.3, -122.2),
  lat = c(37.8, 37.7, 37.6)
), coords = c("lon", "lat"), crs = 4326)

ui <- fluidPage(
  titlePanel("Activity Viewer"),
  fluidRow(
    column(
      width = 4,
      DTOutput("activity_table")
    ),
    column(
      width = 8,
      leafletOutput("activity_map", height = 600)
    )
  )
)

server <- function(input, output, session) {
  # Replace with your real data
  table_data <- table_data          # Replace with your actual table
  spatial_data <- spatial_data   # Replace with your actual sf object
  
  output$activity_table <- renderDT({
    datatable(table_data, selection = "single", rownames = FALSE)
  })
  
  output$activity_map <- renderLeaflet({
    leaflet() %>%
      addTiles()
  })
  
  observeEvent(input$activity_table_rows_selected, {
    selected_row <- input$activity_table_rows_selected
    if (length(selected_row) == 0) return()
    
    selected_name <- table_data$activity_name[selected_row]
    selected_geom <- spatial_data[spatial_data$activity_name == selected_name, ]
    
    leafletProxy("activity_map") %>%
      clearMarkers() %>%
      clearShapes() %>%
      addTiles() %>%
      addCircleMarkers(data = selected_geom, radius = 6, color = "blue", popup = ~activity_name) %>%
      fitBounds(
        lng1 = st_bbox(selected_geom)$xmin,
        lat1 = st_bbox(selected_geom)$ymin,
        lng2 = st_bbox(selected_geom)$xmax,
        lat2 = st_bbox(selected_geom)$ymax
      )
  })
}

shinyApp(ui, server)
