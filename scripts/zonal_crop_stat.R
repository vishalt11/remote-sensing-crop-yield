library(ncdf4) 
library(readr)

library(terra)
library(sf) 

library(rnaturalearth)
library(rnaturalearthdata)
library(giscoR)

library(leaflet)

library(tidyverse)
library(ggrepel)
library(purrr)
#library(furrr)

library(viridis)

# read the pre-processed oco-2 SIF files
df <- base::readRDS(file = '../data/SIF_v1.rds')

df1 <- base::readRDS(file = '../data/SIF_v2_corrected.rds')
df1$Delta_date <- format(df1$Delta_Time, "%Y-%m-%d")

df2 <- base::readRDS(file = '../data/SIF_v3_corrected.rds')
df2$Delta_date <- format(df2$Delta_Time, "%Y-%m-%d")

df_main <- bind_rows(df, df1, df2)

rm(df, df1, df2)

# filter the df to include only months feb - may for each year
df_main <- df_main %>%
  filter(
    lubridate::month(Delta_Time) %in% 2:7
  )

#-------------------------------------------------------------------------------

# load the raster file for a particular year
r <- rast("../data/crop_type_tif/croptypes_2024.tif")

# filter the df to include only dates for the corresponding year
df_y <- df_main %>% filter(lubridate::year(Delta_Time) == 2024 & lubridate::month(Delta_Time) %in% c(6,7))
range(df_y$Delta_Time)
#-------------------------------------------------------------------------------

# germany_nuts1 <- gisco_get_nuts(
#   year = 2024,
#   resolution = "20",  # high resolution
#   nuts_level = 1
# ) %>% 
#   filter(CNTR_CODE == "DE")   # Germany only
# 
# #---------------------------------
# # 2. Convert your df to sf points
# #---------------------------------
# df_points <- st_as_sf(
#   df_y,
#   coords = c("Longitude", "Latitude"),
#   crs = 4326   # WGS84
# )
# 
# #---------------------------------
# # 3. Plot with ggplot2
# #---------------------------------
# ggplot() +
#   geom_sf(data = germany_nuts1, fill = "grey95", color = "black") +
#   geom_sf(data = df_points,
#           aes(color = Daily_SIF_740nm),
#           size = 2, alpha = 0.8) +
#   scale_color_viridis_c(name = "Daily SIF (740 nm)") +
#   coord_sf() +
#   theme_minimal() +
#   labs(
#     title = "Daily SIF Observations over Germany",
#     subtitle = "Points colored by Daily_SIF_740nm"
#   )

#-------------------------------------------------------------------------------

# get bavaria border in sf
germany <- ne_states(country = "Germany", returnclass = "sf")
bavaria <- germany[germany$name_en == "Bavaria",]

# convertthe sif df to sf geometry 
plot_sf <- st_as_sf(df_y, coords = c("Longitude", "Latitude"), crs = 4326)
# crop the sif sf to include bavaria only 
plot_bavaria <- plot_sf[bavaria,]


# EU NUTS level:2 - Regierungsbezirke
nuts2 <- gisco_get_nuts(year = 2024, resolution = "3", epsg = 4326, nuts_level = 2)
# filter Bavaria subdivisions 
bavaria_rb <- nuts2 |> filter(CNTR_CODE == "DE" & grepl("^DE2[1-7]$", NUTS_ID))

#bin every point into their corresponding nuts lvl 2
plot_bavaria <- st_join(plot_bavaria, bavaria_rb[, c("NUTS_NAME")], left = TRUE)

# remove NAs
colSums(is.na(plot_bavaria))

#plot_bavaria$Metadata.MeasurementMode <- 1

plot_bavaria <- plot_bavaria |> drop_na()

# Polygon creator function using the lat, lon corners (SIF polygon / 1 sounding)
make_poly <- function(lon1, lon2, lon3, lon4, lat1, lat2, lat3, lat4) {
  coords <- matrix(
    c(lon1, lat1,
      lon2, lat2,
      lon3, lat3,
      lon4, lat4,
      lon1, lat1),   
    ncol = 2,
    byrow = TRUE
  )
  st_polygon(list(coords))
}

# Build sf polygons
polys_sf <- plot_bavaria |>
  st_drop_geometry() |>
  mutate(geometry = st_sfc(
    purrr::pmap(
      list(Lon_corner1, Lon_corner2, Lon_corner3, Lon_corner4,
           Lat_corner1, Lat_corner2, Lat_corner3, Lat_corner4),
      make_poly
    ),
    crs = st_crs(plot_bavaria)
  )) |>
  st_as_sf()

#-------------------------------------------------------------------------------

# set the levels for the raster file (crop name and code)
crop_classes <- read_delim("../data/crop_type_tif/LEGEND_CropTypes.txt", delim = "\t")
colnames(crop_classes) <- c("code", "label")

levels(r)
levels(r) <- data.frame(value = crop_classes$code, crop = crop_classes$label)

# colors for each crop type for plotting purposes
n_classes <- nrow(crop_classes)
palette_crop <- c("#FFFFFF","#0070FF","#00BFFF","#87CEFA","#ADD8E6","#F4A460","#FFA500","#FF8C00","#FF00FF","#D2B48C",
                  "#8B4513","#8400A8","#FFFF99","#D1FF73","#89CD66","#4E7500","#FFBEBE","#FF7F50","#004C70","#CCCCCC")
palette_crop <- palette_crop[1:n_classes]


#-------------------------------------------------------------------------------
# EU NUTS level:1
nuts1 <- gisco_get_nuts(year = 2024, resolution = "3", epsg = 4326, nuts_level = 1)
# filter Bavaria border
bavaria_border  <- nuts1 |> filter(CNTR_CODE == "DE", NUTS_ID == "DE2")


# raster is in crs: EPSG:32632
bavaria_border_utm <- st_transform(bavaria_border, crs = st_crs(r))
#st_crs(bavaria_border_utm)

# crop raster to bavaria borders
r_crop_utm <- terra::crop(r, terra::vect(bavaria_border_utm))
r_crop_utm <- terra::mask(r_crop_utm, terra::vect(bavaria_border_utm))
#-------------------------------------------------------------------------------


# get back in wgs 84 format for visualization
# dont do this for the crop composition run, not needed
#r_crop <- project(r_crop_utm, "EPSG:4326")

#-------------------------------------------------------------------------------
#terra::writeRaster(r_crop_utm, "../data/bavaria_croptype_utm.tif", filetype = "GTiff", overwrite = TRUE)
#terra::writeRaster(r_crop, "../data/bavaria_croptype.tif", filetype = "GTiff", overwrite = TRUE)

#r_crop <- rast("../data/bavaria_croptype.tif")
#r_crop_utm <- rast("../data/bavaria_croptype_utm.tif")

#-------------------------------------------------------------------------------
# Task 1: calculate area of each crop class in raster/kml selection
# in this case, Composition of Crop Types Bavaria

#r_crop_utm <- project(r_crop, "EPSG:32632")
#cell_areas <- cellSize(r_crop_utm, unit = "m")
#crop_areas <- as.data.frame(zonal(cell_areas, r_crop_utm, fun = "sum", na.rm = TRUE))
#colnames(crop_areas) <- c("code", "area_m2")
#crop_areas$area_km2 <- round(crop_areas$area_m2 / 1e6, 3)

#test_leaflet_utm <- st_transform(test_leaflet, st_crs(r_crop_utm))
#total_area_m2 <- as.numeric(st_area(bavaria_border_utm))
#crop_areas <- crop_areas |> mutate(area_pct = round(100 * area_m2 / total_area_m2, 3))

#-------------------------------------------------------------------------------
# Task 1.1: for each SIF polygon, find the composition of crop classes by area

# r crop utm is the bavaria cropped version of the full germany raster in utm format
# polys_sf contains the entire SIF polygons in bavaria

#single test
# single_sif <- polys_sf[1,]
# 
# single_sif_utm <- st_transform(single_sif, crs = st_crs(r_crop_utm))
# 
# r_singlsif_utm <- terra::crop(r_crop_utm, terra::vect(single_sif_utm))
# r_singlsif_utm <- terra::mask(r_singlsif_utm, terra::vect(single_sif_utm))
# 
# crop_areas <- as.data.frame(zonal(cellSize(r_singlsif_utm, unit = "m"), r_singlsif_utm, fun = "sum", na.rm = TRUE))
# colnames(crop_areas) <- c("code", "area_m2")
# 
# total_area_m2 <- as.numeric(st_area(single_sif_utm))
# crop_areas <- crop_areas |> mutate(area_pct = round(100 * area_m2 / total_area_m2, 3))
# 
# single_sif$crop_stats <- list(crop_areas)
# single_sif$total_area_m2 <- total_area_m2

#-------------------------------------------------------------------------------
# Looped Version
# subset (for testing)

# Calculate crop composition for each SIF polygon using the raster which contains crop composition pixels in 10m resolution
# basically crop and mask SIF polygon with raster, use cellsize to calculate the composition of crop specific pixels
# prepare an empty list to store results
result_list <- vector("list", nrow(polys_sf))

start_time <- Sys.time()

for (i in seq_len(nrow(polys_sf))) {
  message("Processing polygon ", i, " of ", nrow(polys_sf))
  
  # wrap each iteration in tryCatch
  result_list[[i]] <- tryCatch({
    single_sif <- polys_sf[i, ]
    
    # transform to match raster CRS
    single_sif_utm <- st_transform(single_sif, crs = st_crs(r_crop_utm))
    
    # crop and mask raster
    r_singlsif_utm <- terra::crop(r_crop_utm, terra::vect(single_sif_utm))
    r_singlsif_utm <- terra::mask(r_singlsif_utm, terra::vect(single_sif_utm))
    
    # compute crop areas
    crop_areas <- as.data.frame(zonal(cellSize(r_singlsif_utm, unit = "m"), r_singlsif_utm, fun = "sum", na.rm = TRUE))
    
    colnames(crop_areas) <- c("code", "area_m2")
    
    total_area_m2 <- as.numeric(st_area(single_sif_utm))
    
    crop_areas <- crop_areas |> mutate(area_pct = round(100 * area_m2 / total_area_m2, 3))
    
    # attach results
    single_sif$crop_stats <- list(crop_areas)
    single_sif$total_area_m2 <- total_area_m2
    
    single_sif  # return the processed polygon
  },
  error = function(e) {
    message("Skipping polygon ", i, " due to error: ", e$message)
    return(NULL)  # skip this polygon
  })
  
  # free memory every 100 iterations
  #if (i %% 100 == 0) gc()
}

# combine all successfully processed rows
#result_sf <- do.call(rbind, result_list[!sapply(result_list, is.null)])
result_sf <- bind_rows(result_list[!sapply(result_list, is.null)])

end_time <- Sys.time()
print(end_time - start_time)

head(result_sf$crop_stats,10)

base::saveRDS(result_sf, file = '../data/crop_composition/2024_67_cropstat_sf.rds')

# ------------------------------------------------------------------------------
# Vectorized terra::extract CALCULATION

start_time_optimized <- Sys.time()

# 1. Prepare Polygons: Transform ALL polygons at once (Vectorized)
# r_crop_utm is in EPSG:32632, so we transform all SIF polygons to 32632.
polys_sf_utm <- st_transform(polys_sf, crs = st_crs(r_crop_utm))

# 2. Extract Crop Areas (Zonal Statistics in one go)
# This step uses terra::extract, which is highly optimized (usually C/C++ based).
# We calculate the area of each pixel ONCE using cellSize (as r_crop_utm is 32632).
area_raster <- terra::cellSize(r_crop_utm, unit = "m")

# This creates a SpatRaster where cell values are the area (m2)
r_crop_area_utm <- terra::as.int(r_crop_utm) # Convert to integer to ensure correct classification
r_crop_area_utm <- c(r_crop_area_utm, area_raster) # Stack crop value and area

# The function to apply during extraction: Sum the area (Layer 2) for each crop type (Layer 1)
# Note: terra::extract only allows simple functions. Let's use aggregate on the extracted values.

# Extract raw data: crop code (L1) and area (L2) for ALL polygons
# This returns a data frame with columns: ID, Layer.1 (Crop Code), Layer.2 (Area m2)
extracted_data <- terra::extract(r_crop_area_utm, terra::vect(polys_sf_utm), df = TRUE)

#extracted_data <- base::readRDS(file = '../data/extracted_data.rds')

# 3. Aggregate and Calculate Composition (Vectorized using dplyr)
crop_stats_list <- extracted_data %>%
  # Drop the initial 'ID' column from terra::extract
  select(-ID) %>%
  # Add the polygon ID back, using the row number of the polygon
  mutate(PolyID = rep(1:nrow(polys_sf_utm), times = rle(extracted_data$ID)$lengths)) %>%
  # Group by polygon and crop type
  group_by(PolyID, Layer.1) %>%
  # Sum the area for each crop type within each polygon
  summarise(area_m2 = sum(Layer.2, na.rm = TRUE), .groups = 'drop') %>%
  # Rename columns
  rename(code = Layer.1) %>%
  # Calculate Total Polygon Area (Vectorized)
  mutate(total_area_m2 = as.numeric(st_area(polys_sf_utm))[PolyID]) %>%
  # Calculate Percentage
  mutate(area_pct = round(100 * area_m2 / total_area_m2, 3)) %>%
  # Nest the results for easy attachment to the sf object
  group_by(PolyID) %>%
  nest(crop_stats = c(code, area_m2, area_pct)) %>%
  ungroup()

# 4. Attach Results back to sf object
polys_sf_utm$crop_stats <- crop_stats_list$crop_stats
polys_sf_utm$total_area_m2 <- crop_stats_list$crop_stats[[1]]$total_area_m2[1] # A bit tricky, but gets the total area

end_time_optimized <- Sys.time()
print(end_time_optimized - start_time_optimized)


#-------------------------------------------------------------------------------

result_sf <- base::readRDS(file = '../data/crop_composition/2017_cropstat_sf.rds')


#-------------------------------------------------------------------------------
# parallel version
# use only the first 10 polygons
# polys_subset <- polys_sf[1:10, ]
# 
# # set up parallel processing plan
# plan(multisession, workers = parallel::detectCores() - 1)
# 
# # function to process a single polygon
# get_crop_composition <- function(single_sif) {
#   single_sif_utm <- st_transform(single_sif, crs = st_crs(r_crop_utm))
#   
#   r_singlsif_utm <- terra::crop(r_crop_utm, terra::vect(single_sif_utm))
#   r_singlsif_utm <- terra::mask(r_singlsif_utm, terra::vect(single_sif_utm))
#   
#   crop_areas <- as.data.frame(zonal(cellSize(r_singlsif_utm, unit = "m"),
#                                     r_singlsif_utm, fun = "sum", na.rm = TRUE))
#   colnames(crop_areas) <- c("code", "area_m2")
#   
#   total_area_m2 <- as.numeric(st_area(single_sif_utm))
#   
#   crop_areas <- crop_areas %>%
#     mutate(area_pct = round(100 * area_m2 / total_area_m2, 3))
#   
#   single_sif$crop_stats <- list(crop_areas)
#   single_sif$total_area_m2 <- total_area_m2
#   return(single_sif)
# }
# 
# # run the extraction in parallel
# result_list <- future_map(1:nrow(polys_subset), function(i) {get_crop_composition(polys_subset[i, ])}, .progress = TRUE)
# 
# # bind results back into one sf object
# result_sf <- do.call(rbind, result_list)

#-------------------------------------------------------------------------------
# Task 2:Modelling: region wise averaged and temporally averaged feb to may

# for one year
# Extract year and month to group by
# sif_summary <- as.data.frame(polys_sf) |>
#   mutate(month = month(Delta_date, label = TRUE, abbr = TRUE)) |>
#   group_by(NUTS_NAME, month) |>
#   summarise(mean_sif = mean(Daily_SIF_740nm, na.rm = TRUE), .groups = "drop")
# 
# sif_summary <- sif_summary |>
#   tidyr::pivot_wider(names_from = month, values_from = mean_sif)
# 
# sif_summary

# for multiple years
# Build monthly SIF averages per NUTS_NAME per year (Feb–May only)
