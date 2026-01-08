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
library(arrow)

library(bslib)

# ----------------------------
# Month specs (user-selectable)
# ----------------------------
month_specs <- tibble::tribble(
  ~month_name, ~start_date,    ~end_date,
  "march",     "2024-03-01",   "2024-03-31",
  "april",     "2024-04-01",   "2024-04-30",
  "may",       "2024-05-01",   "2024-05-31",
  "june",      "2024-06-01",   "2024-06-30"
)

compute_mean_nirv_month <- function(aoi_sf_4326, start_date, end_date, out_tif) {
  
  print('entered stac routine')
  
  # ensure AOI is valid + in EPSG:4326
  aoi_sf_4326 <- sf::st_make_valid(aoi_sf_4326)
  aoi_sf_4326 <- sf::st_transform(aoi_sf_4326, 4326)
  
  # get composite tif (use existing file if already downloaded)
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
    tif_path <- lcpri
  } else {
    tif_path <- out_tif
  }
  
  print('stac routine for month: DONE')
  print(end_date)
  
  sentinel_raster <- terra::rast(tif_path) / 10000
  
  # NIRV = NDVI * NIR
  nirv_raster <- ((sentinel_raster[[2]] - sentinel_raster[[1]]) /
                    (sentinel_raster[[2]] + sentinel_raster[[1]])) * sentinel_raster[[2]]
  
  aoi_v <- terra::vect(aoi_sf_4326)
  nirv_in <- terra::mask(terra::crop(nirv_raster, aoi_v), aoi_v)
  
  terra::global(nirv_in, fun = "mean", na.rm = TRUE)[1, 1]
}

# # ----------------------------
# # Train model once (app startup)
# # ----------------------------
# df_model <- arrow::read_parquet("../data/model_data_all_v1.parquet") %>%
#   dplyr::select(
#     NUTS_NAME, year, Winterweizen,
#     dplyr::starts_with("mean_sif"),
#     dplyr::starts_with("mean_c3"),
#     dplyr::contains("NIRv", ignore.case = TRUE)
#   )
# 
# train_df <- df_model %>% dplyr::filter(year < 2024)
# train_ids <- paste(train_df$NUTS_NAME, train_df$year, sep = "_")
# 
# train_df <- train_df %>%
#   dplyr::select(-NUTS_NAME, -year) %>%
#   as.data.frame()
# rownames(train_df) <- train_ids
# 
# imp <- mice::mice(train_df, m = 5, maxit = 50, meth = "pmm", seed = 123, printFlag = FALSE)
# train_df_imp <- mice::complete(imp, 1)
# 
# yield_recipe <- recipes::recipe(Winterweizen ~ ., data = train_df_imp) %>%
#   recipes::step_normalize(recipes::all_numeric_predictors())
# 
# lm_model <- parsnip::linear_reg() %>% parsnip::set_engine("lm")
# 
# lm_workflow <- workflows::workflow() %>%
#   workflows::add_recipe(yield_recipe) %>%
#   workflows::add_model(lm_model)
# 
# lm_final_fit <- fit(lm_workflow, data = train_df_imp)
# 
# model_predictors <- setdiff(names(train_df_imp), "Winterweizen")


cols_for_months <- function(months_selected) {
  month_to_abbr <- c(march = "Mar", april = "Apr", may = "May", june = "Jun")
  abbrs <- unname(month_to_abbr[months_selected])
  
  c(
    paste0("mean_sif_", abbrs),
    paste0("mean_c3_share_", abbrs),
    paste0("NIRv_", months_selected)
  )
}


# ----------------------------
# UI
# ----------------------------
ui <- fluidPage(
  theme = bs_theme(
    bg = "#282B33",
    fg = "#dee2e6",
    primary = "#ffd60a",
    base_font = font_google("Lato"),
    bootswatch = 'lux',
    
    
  ),
  
  tags$head(tags$link(rel = "stylesheet", href = "custom.css")),
  
  #titlePanel("Crop Yield Predictor"),
  
  tabsetPanel(
    tabPanel(
      "Crop yield predictor",
      sidebarLayout(
        sidebarPanel(
      
          # --- Section 1: Inputs (crop + months) ---
          div(
            class = "panel-section",
            h6("Inputs"),
            selectInput(
              inputId = "crop_type",
              label   = "Crop type:",
              choices = c("None", "Winter wheat"),
              selected = "None"
            ),
            
            conditionalPanel(
              condition = "input.crop_type != 'None'",
              checkboxGroupInput(
                inputId = "months",
                label   = "Select months to use:",
                choices = c("march", "april", "may", "june"),
                selected = c("march", "april", "may", "june")
              )
            )
          ),
          
          # --- Section 2: Build + sf table ---
          div(
            class = "panel-section",
            h6("Farm Data:"),
            conditionalPanel(
              condition = "input.crop_type != 'None'",
              div(
                style = "margin-bottom: 12px;",
                actionButton("build_sf", "Build sf object", class = "btn-primary")
              ),
            ),
            #h4("sf object:"),
            div(
              style = "overflow-x: auto; width: 100%;",
              tableOutput("sf_table")
            )
          ),
          
          # --- Section 3: Predict + output ---
          div(
            class = "panel-section-last",
            h6("Predicted Yield (tons/ha):"),
            #h6("Predicted Yield (tons/ha):"),
            verbatimTextOutput("pred_print"),
            actionButton("run_pred", "Predict Yield", class = "btn-success"),
          )
        ),
        mainPanel(leafletOutput("map", height = 600))
      )
    ),
    tabPanel(
      "Historical data",
      fluidRow(
        column(
          12,
          h4("Historical data"),
          p("Coming soon.")
        )
      )
    )
  )
)

# ----------------------------
# Server
# ----------------------------
server <- function(input, output, session) {
  
  drawn_polygons <- reactiveVal(NULL)
  pred_value <- reactiveVal(NULL)
  
  # ---- Bavaria boundaries (NUTS2) ----
  nuts2_all <- giscoR::gisco_get_nuts(year = "2021", resolution = "03", nuts_level = 2, epsg = 4326)
  bavaria_nuts2 <- subset(nuts2_all, grepl("^DE2", NUTS_ID))
  
  bavaria_outline_geom <- sf::st_union(sf::st_make_valid(bavaria_nuts2))
  bavaria_outline <- sf::st_as_sf(sf::st_sfc(bavaria_outline_geom), crs = 4326)
  
  bav_bbox <- sf::st_bbox(bavaria_outline)
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      fitBounds(
        lng1 = bav_bbox[["xmin"]], lat1 = bav_bbox[["ymin"]],
        lng2 = bav_bbox[["xmax"]], lat2 = bav_bbox[["ymax"]]
      ) %>%
      addPolygons(
        data = bavaria_nuts2,
        fill = FALSE,
        weight = 2,
        opacity = 1,
        color = '#ffd60a',
        group = "bavaria_nuts2"
      ) %>%
      addPolygons(
        data = bavaria_outline,
        fill = FALSE,
        weight = 4,
        opacity = 1,
        color = '#ffd60a',
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
  
  # ----------------------------
  # Draw polygon only
  # ----------------------------
  observeEvent(input$map_draw_new_feature, {
    
    if (!is.null(drawn_polygons())) {
      leafletProxy("map") %>% clearGroup("drawn")
      showNotification("Only one selection allowed. Refresh or add a reset later if needed.", type = "warning")
      return()
    }
    
    feature <- input$map_draw_new_feature
    req(feature)
    req(feature$geometry$type %in% c("polygon", "Polygon", "rectangle", "Rectangle"))
    
    coords_list <- feature$geometry$coordinates[[1]]
    coords_mat <- do.call(
      rbind,
      lapply(coords_list, function(pt) c(pt[[1]], pt[[2]]))
    )
    
    poly_sfc <- st_sfc(st_polygon(list(coords_mat)), crs = 4326)
    sf_obj   <- st_sf(geometry = poly_sfc)
    sf_obj   <- sf::st_make_valid(sf_obj)
    
    inside <- sf::st_within(sf_obj, bavaria_outline, sparse = FALSE)[1, 1]
    if (!inside) {
      leafletProxy("map") %>% clearGroup("drawn")
      drawn_polygons(NULL)
      showNotification("Select within Bavaria only.", type = "error")
      return()
    }
    
    idx <- sf::st_within(sf_obj, bavaria_nuts2, sparse = TRUE)[[1]]
    if (length(idx) != 1) {
      leafletProxy("map") %>% clearGroup("drawn")
      drawn_polygons(NULL)
      showNotification("Selection must be inside exactly one NUTS2 region.", type = "error")
      return()
    }
    
    nuts2_name <- bavaria_nuts2$NUTS_NAME[idx]
    
    sf_obj$croptype <- input$crop_type
    sf_obj$NUTS_NAME <- nuts2_name
    sf_obj$year <- 2024
    
    drawn_polygons(sf_obj)
    
    leafletProxy("map") %>%
      clearGroup("drawn") %>%
      addPolygons(data = sf_obj, group = "drawn")
  })
  
  # ----------------------------
  # Build sf object (months + crop selected)
  # ----------------------------
  observeEvent(input$build_sf, {
    req(drawn_polygons())
    req(input$crop_type != "None")
    req(length(input$months) > 0)
    
    withProgress(message = "Building sf object...", value = 0, {
      sf_obj <- drawn_polygons()
      sf_obj$croptype <- input$crop_type
      
      incProgress(0.05, detail = "Preparing AOI and inputs...")
      
      months_selected <- input$months
      ms <- month_specs[month_specs$month_name %in% months_selected, , drop = FALSE]
      
      # ---- NIRv via STAC for selected months ----
      incProgress(0.05, detail = "Setting up NIRv processing...")
      aoi_sel_4326 <- sf::st_transform(sf_obj, 4326)
      dir.create("./nirv_data", showWarnings = FALSE, recursive = TRUE)
      
      n_months <- nrow(ms)
      if (n_months < 1) {
        showNotification("No months selected.", type = "error")
        return()
      }
      
      # Reserve 70% of the bar for STAC/month loop
      per_month <- 0.70 / n_months
      
      for (i in seq_len(n_months)) {
        m <- ms[i, ]
        incProgress(per_month * 0.15, detail = paste0("Requesting STAC composite for ", m$month_name, "..."))
        
        out_tif <- file.path("./nirv_data", paste0("sentinel2_lcpri_composite_", m$month_name, "_2024.tif"))
        
        # The heavy call happens here
        mean_nirv <- compute_mean_nirv_month(
          aoi_sf_4326 = aoi_sel_4326,
          start_date  = m$start_date,
          end_date    = m$end_date,
          out_tif     = out_tif
        )
        
        sf_obj[[paste0("NIRv_", m$month_name)]] <- mean_nirv
        
        incProgress(per_month * 0.85, detail = paste0("Finished NIRv for ", m$month_name))
      }
      
      # ---- SIF + C3 from parquet for selected months ----
      incProgress(0.10, detail = "Loading SIF/C3 features from parquet...")
      
      df <- arrow::read_parquet("../data/model_data_all.parquet")
      row_2024 <- df[df$NUTS_NAME == sf_obj$NUTS_NAME[1] & df$year == 2024, , drop = FALSE]
      
      if (nrow(row_2024) < 1) {
        showNotification(paste0("No 2024 data found for: ", sf_obj$NUTS_NAME[1]), type = "error")
        return()
      }
      
      month_to_abbr <- c(march = "Mar", april = "Apr", may = "May", june = "Jun")
      abbrs <- unname(month_to_abbr[months_selected])
      
      sif_cols <- paste0("mean_sif_", abbrs)
      c3_cols  <- paste0("mean_c3_share_", abbrs)
      feature_cols <- c(sif_cols, c3_cols)
      feature_cols <- feature_cols[feature_cols %in% names(row_2024)]
      
      if (length(feature_cols) > 0) {
        sf_obj[, feature_cols] <- row_2024[1, feature_cols, drop = FALSE]
      }
      
      incProgress(0.05, detail = "Saving updated sf object...")
      drawn_polygons(sf_obj)
      
      incProgress(0.05, detail = "Done.")
    })
    
    showNotification("sf object built with selected months.", type = "message")
  })
  
  
  # ----------------------------
  # Predict yield
  # ----------------------------
  observeEvent(input$run_pred, {
    req(drawn_polygons())
    req(input$crop_type != "None")
    req(length(input$months) > 0)
    
    sf_obj <- drawn_polygons()
    
    # Optional safety: ensure sf object was built (has at least one NIRv_ column)
    # If you prefer, replace this with a dedicated flag set in build_sf.
    nirv_cols_present <- any(grepl("^NIRv_", names(sf::st_drop_geometry(sf_obj))))
    if (!nirv_cols_present) {
      showNotification("Build sf object first (fill NIRv/SIF/C3) before predicting.", type = "error")
      return()
    }
    
    # ----------------------------
    # Load & restrict model data to selected months
    # ----------------------------
    df_model <- arrow::read_parquet("../data/model_data_all_v1.parquet") %>%
      dplyr::select(
        NUTS_NAME, year, Winterweizen,
        dplyr::starts_with("mean_sif"),
        dplyr::starts_with("mean_c3"),
        dplyr::contains("NIRv", ignore.case = TRUE)
      )
    
    months_selected <- input$months
    keep_feats <- cols_for_months(months_selected)
    
    keep_cols <- c("NUTS_NAME", "year", "Winterweizen", keep_feats)
    keep_cols <- keep_cols[keep_cols %in% names(df_model)]  # only existing cols
    
    df_model_month <- df_model %>% dplyr::select(dplyr::all_of(keep_cols))
    
    # ----------------------------
    # Train (2017-2023) with restricted months
    # ----------------------------
    train_df <- df_model_month %>% dplyr::filter(year < 2024)
    if (nrow(train_df) == 0) {
      showNotification("No training rows found (year < 2024).", type = "error")
      return()
    }
    
    train_ids <- paste(train_df$NUTS_NAME, train_df$year, sep = "_")
    train_df <- train_df %>%
      dplyr::select(-NUTS_NAME, -year) %>%
      as.data.frame()
    rownames(train_df) <- train_ids
    
    print('Model training data:')
    print(train_df)
    
    # mice imputation on train only
    imp <- mice::mice(train_df, m = 5, maxit = 50, meth = "pmm", seed = 123, printFlag = FALSE)
    train_df_imp <- mice::complete(imp, 1)
    
    yield_recipe <- recipes::recipe(Winterweizen ~ ., data = train_df_imp) %>%
      recipes::step_normalize(recipes::all_numeric_predictors())
    
    lm_model <- parsnip::linear_reg() %>% parsnip::set_engine("lm")
    
    lm_workflow <- workflows::workflow() %>%
      workflows::add_recipe(yield_recipe) %>%
      workflows::add_model(lm_model)
    
    lm_final_fit <- fit(lm_workflow, data = train_df_imp)
    
    model_predictors <- setdiff(names(train_df_imp), "Winterweizen")
    
    # ----------------------------
    # Build prediction row from sf object (drop geometry + ids)
    # ----------------------------
    newdata <- sf::st_drop_geometry(sf_obj) %>%
      tibble::as_tibble() %>%
      dplyr::select(-dplyr::any_of(c("geometry", "croptype", "NUTS_NAME", "year")))
    
    # ensure the same columns the trained model expects
    missing_cols <- setdiff(model_predictors, names(newdata))
    for (mc in missing_cols) newdata[[mc]] <- NA_real_
    
    newdata <- newdata %>% dplyr::select(dplyr::all_of(model_predictors))
    
    # ----------------------------
    # Predict
    # ----------------------------
    test_predictions <- predict(lm_final_fit, new_data = newdata)
    
    pred_value(test_predictions$.pred[[1]])
  })
  
  
  # observeEvent(input$run_pred, {
  #   req(drawn_polygons())
  #   
  #   sf_obj <- drawn_polygons()
  #   
  #   newdata <- sf::st_drop_geometry(sf_obj) %>%
  #     tibble::as_tibble() %>%
  #     dplyr::select(-dplyr::any_of(c("geometry", "croptype", "NUTS_NAME", "year")))
  #   
  #   missing_cols <- setdiff(model_predictors, names(newdata))
  #   for (mc in missing_cols) newdata[[mc]] <- NA_real_
  #   
  #   newdata <- newdata %>% dplyr::select(dplyr::all_of(model_predictors))
  #   
  #   test_predictions <- predict(lm_final_fit, new_data = newdata)
  #   pred_value(test_predictions$.pred[[1]])
  # })
  
  
  
  # output$sf_print <- renderPrint({
  #   drawn_polygons()
  # })
  
  output$sf_table <- renderTable({
    req(drawn_polygons())
    
    sf_obj <- drawn_polygons()
    
    # Drop geometry
    df <- sf::st_drop_geometry(sf_obj)
    
    # Optional: nicer formatting
    df <- tibble::as_tibble(df)
    
    df
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE,
  spacing = "xs"
  )
  
  
  
  output$pred_print <- renderPrint({
    pred_value()
  })
}

shinyApp(ui, server)
