# app.R (or dashboard.R)

library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)

ui <- fluidPage(
  titlePanel("Draw polygons and store as sf with crop type"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "crop_type",
        label   = "Crop type:",
        choices = c("Winter wheat"),
        selected = "Winter wheat"
      ),
      h4("Current sf object:"),
      verbatimTextOutput("sf_print")
    ),
    mainPanel(
      leafletOutput("map", height = 600)
    )
  )
)

server <- function(input, output, session) {
  
  # Will hold all drawn polygons as a single sf object
  drawn_polygons <- reactiveVal(NULL)
  
  # Base map with draw toolbar, initial view over Germany
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      # Option 1: center + zoom over Germany
      setView(lng = 10.5, lat = 51.2, zoom = 6) %>%
      # (or use fitBounds(...) instead if you prefer a bbox)
      addDrawToolbar(
        targetGroup = "drawn",
        polygonOptions   = drawPolygonOptions(),
        rectangleOptions = drawRectangleOptions(),
        circleOptions    = FALSE,
        markerOptions    = FALSE,
        circleMarkerOptions = FALSE,
        polylineOptions  = FALSE,
        editOptions      = editToolbarOptions()
      ) %>%
      addLayersControl(
        overlayGroups = c("drawn"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
  # Listen for newly drawn features and convert to sf
  observeEvent(input$map_draw_new_feature, {
    feature <- input$map_draw_new_feature
    req(feature)
    req(feature$geometry$type %in% c("polygon", "Polygon", "rectangle", "Rectangle"))
    
    # Leaflet gives coordinates as list(list(c(lng, lat), ...))
    coords_list <- feature$geometry$coordinates[[1]]
    
    # Convert to matrix [lng lat] for sf::st_polygon()
    coords_mat <- do.call(
      rbind,
      lapply(coords_list, function(pt) c(pt[[1]], pt[[2]]))
    )
    
    # Create sf polygon (assuming EPSG:4326 from leaflet)
    poly_sfc <- st_sfc(st_polygon(list(coords_mat)), crs = 4326)
    sf_obj   <- st_sf(geometry = poly_sfc)
    
    # Add crop type as attribute
    sf_obj$croptype <- input$crop_type
    
    # Append to existing polygons
    current <- drawn_polygons()
    if (is.null(current)) {
      drawn_polygons(sf_obj)
    } else {
      # ensure columns align; this will keep croptype + geometry
      drawn_polygons(rbind(current, sf_obj))
    }
  })
  
  # Print the sf object so you can see what's being stored
  output$sf_print <- renderPrint({
    drawn_polygons()
  })
}

shinyApp(ui, server)
