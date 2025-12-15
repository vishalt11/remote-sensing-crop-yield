# dashboard.R (or app.R)

library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(giscoR)

ui <- fluidPage(
  titlePanel("Get Yield for your Farm"),
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
  
  # Will hold the (single) valid polygon as an sf object
  drawn_polygons <- reactiveVal(NULL)
  
  # ---- Bavaria boundaries (NUTS2) ----
  nuts2_all <- giscoR::gisco_get_nuts(year = "2021", resolution = "03", nuts_level = 2, epsg = 4326)
  
  # Bavaria NUTS2: NUTS_ID starts with "DE2"
  bavaria_nuts2 <- subset(nuts2_all, grepl("^DE2", NUTS_ID))
  
  # Bavaria outline (NUTS1) as one geometry derived from NUTS2 union
  bavaria_outline_geom <- sf::st_union(sf::st_make_valid(bavaria_nuts2))
  bavaria_outline <- sf::st_as_sf(sf::st_sfc(bavaria_outline_geom), crs = 4326)
  
  # Initial view: fit to Bavaria
  bav_bbox <- sf::st_bbox(bavaria_outline)
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      fitBounds(
        lng1 = bav_bbox[["xmin"]], lat1 = bav_bbox[["ymin"]],
        lng2 = bav_bbox[["xmax"]], lat2 = bav_bbox[["ymax"]]
      ) %>%
      # NUTS2 borders in Bavaria (no fill)
      addPolygons(
        data = bavaria_nuts2,
        fill = FALSE,
        weight = 2,
        opacity = 1,
        group = "bavaria_nuts2"
      ) %>%
      # Bavaria outline (no fill)
      addPolygons(
        data = bavaria_outline,
        fill = FALSE,
        weight = 4,
        opacity = 1,
        group = "bavaria_outline"
      ) %>%
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
        overlayGroups = c("drawn", "bavaria_outline", "bavaria_nuts2"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
  observeEvent(input$map_draw_new_feature, {
    
    # Only one valid selection allowed per workflow
    if (!is.null(drawn_polygons())) {
      leafletProxy("map") %>% clearGroup("drawn")
      showNotification("Only one selection allowed. Refresh or add a reset later if needed.", type = "warning")
      return()
    }
    
    feature <- input$map_draw_new_feature
    req(feature)
    req(feature$geometry$type %in% c("polygon", "Polygon", "rectangle", "Rectangle"))
    
    # Leaflet gives coordinates as list(list(c(lng, lat), ...))
    coords_list <- feature$geometry$coordinates[[1]]
    
    coords_mat <- do.call(
      rbind,
      lapply(coords_list, function(pt) c(pt[[1]], pt[[2]]))
    )
    
    poly_sfc <- st_sfc(st_polygon(list(coords_mat)), crs = 4326)
    sf_obj   <- st_sf(geometry = poly_sfc)
    
    # Make valid using sf (avoid lwgeom export issues)
    sf_obj <- sf::st_make_valid(sf_obj)
    
    # Validate selection is within Bavaria outline
    inside <- sf::st_within(sf_obj, bavaria_outline, sparse = FALSE)[1, 1]
    
    if (!inside) {
      leafletProxy("map") %>% clearGroup("drawn")
      drawn_polygons(NULL)
      showNotification("Select within Bavaria only.", type = "error")
      return()
    }
    
    # Add crop type
    sf_obj$croptype <- input$crop_type
    
    # --- Determine which NUTS2 region contains the selection ---
    # (Assumption: polygon is fully inside exactly one NUTS2 region)
    idx <- sf::st_within(sf_obj, bavaria_nuts2, sparse = TRUE)[[1]]
    
    if (length(idx) != 1) {
      leafletProxy("map") %>% clearGroup("drawn")
      drawn_polygons(NULL)
      showNotification("Selection must be inside exactly one NUTS2 region.", type = "error")
      return()
    }
    
    
    nuts2_name <- bavaria_nuts2$NUTS_NAME[idx]
    sf_obj$NUTS_NAME <- nuts2_name
    sf_obj$year <- 2024
    
    # --- Pull 2024 model features for that NUTS_NAME from df ---
    # df must exist in your environment and contain NUTS_NAME + year
    df <- arrow::read_parquet("../data/model_data_all.parquet")
    row_2024 <- df[df$NUTS_NAME == nuts2_name & df$year == 2024, , drop = FALSE]
    
    if (nrow(row_2024) < 1) {
      leafletProxy("map") %>% clearGroup("drawn")
      drawn_polygons(NULL)
      showNotification(paste0("No 2024 data found for: ", nuts2_name), type = "error")
      return()
    }
    
    # Choose which columns to copy into the sf object:
    # - all mean_sif_* and mean_c3_share_* columns (as you requested)
    sif_cols <- grep("^mean_sif_", names(row_2024), value = TRUE)
    c3_cols  <- grep("^mean_c3_share_", names(row_2024), value = TRUE)
    feature_cols <- c(sif_cols, c3_cols)
    
    # Attach them to sf_obj
    sf_obj[, feature_cols] <- row_2024[1, feature_cols, drop = FALSE]
    
    # Store the enriched sf object
    drawn_polygons(sf_obj)
    
    # Keep only the valid polygon visible
    leafletProxy("map") %>%
      clearGroup("drawn") %>%
      addPolygons(data = sf_obj, group = "drawn")
  })
  
  output$sf_print <- renderPrint({
    drawn_polygons()
  })
}


shinyApp(ui, server)
