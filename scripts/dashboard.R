# dashboard.R (or app.R)

library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(giscoR)

library(tidyverse)
library(tidymodels)
library(mice)
library(rsi)
library(terra)

month_specs <- tibble::tribble(
  ~month_name, ~start_date,    ~end_date,
  "march",     "2024-03-01",   "2024-03-31",
  "april",     "2024-04-01",   "2024-04-30",
  "may",       "2024-05-01",   "2024-05-31",
  "june",      "2024-06-01",   "2024-06-30"
)

compute_mean_nirv_month <- function(aoi_sf_4326, start_date, end_date, out_tif) {
  # 1) ensure AOI is valid + in EPSG:4326
  aoi_sf_4326 <- sf::st_make_valid(aoi_sf_4326)
  aoi_sf_4326 <- sf::st_transform(aoi_sf_4326, 4326)
  
  # 2) get composite tif (use existing file if already downloaded)
  if (!file.exists(out_tif)) {
    lcpri <- rsi::get_stac_data(
      aoi_sf_4326,
      start_date = start_date,
      end_date   = end_date,
      asset_names = c("B04", "B08", "SCL"),
      stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/",
      collection  = "sentinel-2-l2a",
      output_filename = out_tif,
      cloud_cover_threshold = 100,
      mask_function = "s2_mask",
      composite_function = "mean"
    )
    tif_path <- lcpri  # rsi returns the written file path
  } else {
    tif_path <- out_tif
  }
  
  # 3) read raster + compute NIRV
  sentinel_raster <- terra::rast(tif_path) / 10000
  
  nirv_raster <- ((sentinel_raster[[2]] - sentinel_raster[[1]]) /
                    (sentinel_raster[[2]] + sentinel_raster[[1]])) * sentinel_raster[[2]]
  
  # 4) mask/crop to AOI + mean
  aoi_v <- terra::vect(aoi_sf_4326)
  nirv_in <- terra::mask(terra::crop(nirv_raster, aoi_v), aoi_v)
  
  terra::global(nirv_in, fun = "mean", na.rm = TRUE)[1, 1]
}

df_model <- arrow::read_parquet("../data/model_data_all.parquet") %>%
  dplyr::select(
    NUTS_NAME, year, Winterweizen,
    dplyr::starts_with("mean_sif"),
    dplyr::starts_with("mean_c3"),
    dplyr::contains("NIRv", ignore.case = TRUE)
  )

train_df <- df_model %>% dplyr::filter(year < 2024)
train_ids <- paste(train_df$NUTS_NAME, train_df$year, sep = "_")

train_df <- train_df %>%
  dplyr::select(-NUTS_NAME, -year) %>%
  as.data.frame()
rownames(train_df) <- train_ids

# mice imputation on train only
imp <- mice::mice(train_df, m = 5, maxit = 50, meth = "pmm", seed = 123, printFlag = FALSE)
train_df_imp <- mice::complete(imp, 1)

# recipe + model
yield_recipe <- recipes::recipe(Winterweizen ~ ., data = train_df_imp) %>%
  recipes::step_normalize(recipes::all_numeric_predictors())

lm_model <- parsnip::linear_reg() %>% parsnip::set_engine("lm")

lm_workflow <- workflows::workflow() %>%
  workflows::add_recipe(yield_recipe) %>%
  workflows::add_model(lm_model)

lm_final_fit <- fit(lm_workflow, data = train_df_imp)

# Save the predictor names the model expects (very important)
model_predictors <- setdiff(names(train_df_imp), "Winterweizen")




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
      actionButton("run_pred", "Predict yield (t/ha)"),
      h4("Predicted yield (t/ha):"),
      verbatimTextOutput("pred_print"),
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
      addProviderTiles("Esri.WorldImagery") %>%
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
    
    #nirv calculation and appending
    # Ensure AOI is your selected polygon (sf_obj) in EPSG:4326
    aoi_sel_4326 <- sf::st_transform(sf_obj, 4326)
    
    # Make sure folder exists
    dir.create("./nirv_data", showWarnings = FALSE, recursive = TRUE)
    
    nirv_values <- list()
    
    for (i in seq_len(nrow(month_specs))) {
      m <- month_specs[i, ]
      
      out_tif <- file.path("./nirv_data", paste0("sentinel2_lcpri_composite_", m$month_name, "_2024.tif"))
      
      mean_nirv <- compute_mean_nirv_month(
        aoi_sf_4326 = aoi_sel_4326,
        start_date  = m$start_date,
        end_date    = m$end_date,
        out_tif     = out_tif
      )
      
      nirv_values[[paste0("NIRv_", m$month_name)]] <- mean_nirv
    }
    
    # Attach to sf object as new columns: NIRv_march, NIRv_april, NIRv_may, NIRv_june
    for (nm in names(nirv_values)) {
      sf_obj[[nm]] <- nirv_values[[nm]]
    }
    
    # then store it (same as you already do)
    #drawn_polygons(sf_obj)
    
    
    
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
    #nuts2_name <- grep("^NUTS_", names(row_2024), value = TRUE)
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
  
  pred_value <- reactiveVal(NULL)
  
  observeEvent(input$run_pred, {
    req(drawn_polygons())
    
    sf_obj <- drawn_polygons()
    
    # Build 1-row model input (drop geometry + irrelevant cols)
    newdata <- sf::st_drop_geometry(sf_obj) %>%
      tibble::as_tibble()
    
    # Remove columns the model should not see (safe even if they don't exist)
    newdata <- newdata %>%
      dplyr::select(-dplyr::any_of(c("geometry", "croptype", "NUTS_NAME", "year")))
    
    # Ensure we have exactly the columns the model expects:
    # - add any missing predictors as NA
    missing_cols <- setdiff(model_predictors, names(newdata))
    for (mc in missing_cols) newdata[[mc]] <- NA_real_
    
    # - drop any extras
    newdata <- newdata %>% dplyr::select(dplyr::all_of(model_predictors))
    
    # Predict (returns tibble with .pred)
    test_predictions <- predict(lm_final_fit, new_data = newdata)
    
    pred_value(test_predictions$.pred[[1]])
  })
  
  output$sf_print <- renderPrint({
    drawn_polygons()
  })
  
  output$pred_print <- renderPrint({
    pred_value()
  })
  
}


shinyApp(ui, server)
