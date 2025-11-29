# https://tech.popdata.org/dhs-research-hub/posts/2024-08-01-ndvi-data/
# 
library(rsi)
library(rstac)

library(leaflet)
library(tidyverse)

library(sf)
library(terra)
library(rnaturalearth)

#48.349362, 12.150977
aoi_point <- sf::st_point(c(-74.912131, 44.080410))

aoi_point <- sf::st_point(c(12.150977, 48.349362))
aoi_sfc <- sf::st_set_crs(sf::st_sfc(aoi_point), 4326)
aoi_projected <- sf::st_transform(aoi_sfc, 5070)
aoi_buffer_projected <- sf::st_buffer(aoi_projected, 1000)

aoi_wgs84 <- sf::st_transform(aoi_buffer_projected, 4326)

# Get the centroid for map view
centroid <- sf::st_coordinates(aoi_point)
center_lon <- centroid[1, 1]
center_lat <- centroid[1, 2]

# bavaria
aoi_point <- c(xmin = 8.9, ymin = 47.2, xmax = 13.9, ymax = 50.5)
aoi_sfc <- sf::st_bbox(aoi_point, crs = 4326) |> sf::st_as_sfc()
aoi_projected <- sf::st_transform(aoi_sfc, 5070)


# world_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
# 
# ggplot() +
#   geom_sf(data = world_map, fill = "antiquewhite", color = "gray60", linewidth = 0.2) +
#   geom_sf(data = aoi_wgs84, fill = "red", color = "darkred", alpha = 0.8) +
#   coord_sf(xlim = c(-80, -70), ylim = c(40, 50), expand = FALSE) +
#   theme_bw() +
#   theme(
#     panel.background = element_rect(fill = "lightblue"), 
#     plot.title = element_text(hjust = 0.5),
#     plot.subtitle = element_text(hjust = 0.5)
#   )

# https://docs.ropensci.org/rsi/reference/get_stac_data.html

output_file_name <- "sentinel2_ndvi_composite.tif"
lcpri <- get_stac_data(
  aoi_buffer_projected,
  start_date = "2021-01-01", 
  end_date = "2021-01-10",
  asset_names = c("B04", "B08", "SCL"), # BAND NAMES
  stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/",
  collection = "sentinel-2-l2a", # the name after collections https://planetarycomputer.microsoft.com/catalog
  output_filename = tempfile(fileext = ".tif"),
  #output_filename = output_file_name,
  cloud_cover_threshold = 100,
  #composite_function = NULL,
  mask_function = "s2_mask", # This tells rsi to use the SCL band to mask clouds
  composite_function = "mean", # Only the cloud-free pixels within your AOI are used for the mean composite calculation.
)

lcpri

terra::plot(terra::rast(lcpri[8]))
terra::plot(terra::rast(lcpri))
terra::res(terra::rast(lcpri))

# bands
names(terra::rast(lcpri))

# store raster
sentinel_raster <- terra::rast(lcpri)

# calc NDVI
# NDVI = (NIR - RED) / (NIR + RED)
# B08 is the second band, B04 is the first band in the SpatRaster
ndvi_raster <- (sentinel_raster[[2]] - sentinel_raster[[1]]) / 
  (sentinel_raster[[2]] + sentinel_raster[[1]])

terra::plot(ndvi_raster)

# color palette for NDVI (check range from above plot)
d1 <- c(-0.4, 0.8)
ndvi_pal <- colorNumeric(
  #palette = "RdYlGn", 
  palette = viridis::viridis(256, option = "plasma"),
  domain = d1,
  na.color = "transparent"
)


leaflet() |>
  #addTiles(group = "OSM Base") |>
  addProviderTiles("Esri.WorldImagery") |>
  #setView(lng = center_lon, lat = center_lat, zoom = 14) |>
  addRasterImage(
    x = ndvi_raster,
    colors = ndvi_pal,
    opacity = 0.8,
    group = "NDVI Raster"
  ) |>
  addPolygons(
    data = aoi_wgs84,
    fillColor = "transparent",
    color = "black",
    weight = 3,
    popup = "100-meter AOI Buffer"
  ) |>
  addLegend(
    pal = ndvi_pal,
    values = d1,
    title = "NDVI (0 to 0.03)",
    position = "bottomright",
    group = "NDVI Raster"
  ) |>
  addLayersControl(
    overlayGroups = c("NDVI Raster"),
    baseGroups = c("Esri world imagery Base"),
    options = layersControlOptions(collapsed = FALSE)
  )

#-------------------------------------------------------------------------------


red_path <- "../data/backup/SENTINEL2X_20200615-000000-000_L3A_T31UGS_C_V1-2_FRC_B4.tif"
nir_path <- "../data/backup/SENTINEL2X_20200615-000000-000_L3A_T31UGS_C_V1-2_FRC_B8.tif"

b4 <- rast(red_path)
b8 <- rast(nir_path)


# NDVI: (B8 - B4) / (B8 + B4)
start_time <- Sys.time()

ndvi <- (b8 - b4) / (b8 + b4)
names(ndvi) <- "NDVI"

end_time <- Sys.time()
print(end_time - start_time)

# Fetch the Germany boundary as an sf object
germany_border <- ne_countries(country = "Germany", returnclass = "sf")

# (The ndvi raster retains the CRS of the Sentinel-2 TIF files)
germany_border_proj <- st_transform(germany_border, crs(ndvi))


terra::plot(
  ndvi,
  col = hcl.colors(100, "Army"), 
  main = "NDVI Overlayed with Germany Border",
  legend = TRUE,
  # Set the range of the color scale
  range = c(-1, 1)
)

plot(
  germany_border_proj,
  add = TRUE,          
  border = "black",   
  lwd = 1.5,          
  col = NA             
)


ext(ndvi)


# Your provided Bounding Box (SpatExtent) in UTM Zone 31N:
bbox_data <- list(
  xmin = 699960,
  xmax = 809760,
  ymin = 5590200,
  ymax = 5700000
)

ndvi_crs <- "EPSG:32631"


# Create a matrix of coordinates for the four corners
bbox_coords <- matrix(c(
  bbox_data$xmin, bbox_data$ymin,
  bbox_data$xmin, bbox_data$ymax,
  bbox_data$xmax, bbox_data$ymax,
  bbox_data$xmax, bbox_data$ymin,
  bbox_data$xmin, bbox_data$ymin
), ncol = 2, byrow = TRUE)

# Create an sf polygon and assign its original CRS (UTM 31N)
bbox_sf <- st_polygon(list(bbox_coords)) %>%
  st_sfc(crs = ndvi_crs)

# Transform the polygon to WGS84 (EPSG:4326) for plotting on a global map
bbox_wgs84 <- st_transform(bbox_sf, crs = 4326)



world_map <- ne_countries(scale = "medium", returnclass = "sf")


bbox_ext_wgs84 <- st_bbox(bbox_wgs84)

ggplot() +
  geom_sf(data = world_map, fill = "gray90", color = "gray50") +
  geom_sf(data = bbox_wgs84, fill = NA, color = "red", linewidth = 1.2, linetype = "solid") +
  coord_sf(
    xlim = c(bbox_ext_wgs84["xmin"] - 1.5, bbox_ext_wgs84["xmax"] + 1.5),
    ylim = c(bbox_ext_wgs84["ymin"] - 1, bbox_ext_wgs84["ymax"] + 1),
    expand = FALSE
  ) +
  labs(
    title = "Geographic Location of Sentinel-2 Tile T31UGS",
    subtitle = "Bounding Box in Red",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw()

#-------------------------------------------------------------------------------

# aggregation to lower resolution
terra::minmax(ndvi)
terra::plot(ndvi,col = hcl.colors(100, "Army"),legend = TRUE,range = c(-1, 1))

# 30 is 300m, 100 is 1km/1000m
agg_factor <- 30

start_time <- Sys.time()

ndvi_agg <- aggregate(ndvi, fact = agg_factor, fun = "mean", na.rm = TRUE)

end_time <- Sys.time()
print(end_time - start_time)

terra::minmax(ndvi_agg)
terra::plot(ndvi_agg,col = hcl.colors(100, "Army"),legend = TRUE,range = c(-1, 1))

writeRaster(ndvi_agg, "../data/backup/ndvi_300m.tif", overwrite = TRUE)

# 300m resolution ndvi raster of 1 utm tile somewhere in bavaria
#ndvi_agg

ct_2020 <- rast('../data/crop_type_tif/croptypes_2020.tif')

# set the levels for the raster file (crop name and code)
crop_classes <- read_delim("../data/crop_type_tif/LEGEND_CropTypes.txt", delim = "\t")
colnames(crop_classes) <- c("code", "label")

levels(ct_2020)
levels(ct_2020) <- data.frame(value = crop_classes$code, crop = crop_classes$label)

# colors for each crop type for plotting purposes
n_classes <- nrow(crop_classes)
palette_crop <- c("#FFFFFF","#0070FF","#00BFFF","#87CEFA","#ADD8E6","#F4A460","#FFA500","#FF8C00","#FF00FF","#D2B48C",
                  "#8B4513","#8400A8","#FFFF99","#D1FF73","#89CD66","#4E7500","#FFBEBE","#FF7F50","#004C70","#CCCCCC")
palette_crop <- palette_crop[1:n_classes]


st_crs(ct_2020)
st_crs(ndvi_agg)

bbox_coords <- as.list(ext(ndvi_agg))

coords_matrix <- matrix(c(
  bbox_coords$xmin, bbox_coords$ymin,
  bbox_coords$xmin, bbox_coords$ymax,
  bbox_coords$xmax, bbox_coords$ymax,
  bbox_coords$xmax, bbox_coords$ymin,
  bbox_coords$xmin, bbox_coords$ymin
), ncol = 2, byrow = TRUE)

bbox_sf_31n <- st_polygon(list(coords_matrix)) %>% st_sfc(crs = st_crs(ndvi_agg))
  
bbox_sf_32n <- st_transform(bbox_sf_31n, crs = st_crs(ct_2020))

#ct_cropped <- crop(x = ct_2020, y = ext(bbox_sf_32n))


# crop raster to bavaria borders
ct_cropped <- terra::crop(ct_2020, terra::vect(bbox_sf_32n))
ct_cropped <- terra::mask(ct_cropped, terra::vect(bbox_sf_32n))




st_crs(ct_cropped)
st_crs(ndvi_agg)


ct_cropped_32631 <- terra::project(ct_cropped, terra::crs(ndvi_agg), method = "near")

terra::plot(ct_cropped)
terra::plot(ct_cropped_32631)
terra::plot(ndvi_agg,col = hcl.colors(100, "Army"),legend = TRUE,range = c(-1, 1))


# ndvi_agg raster
ndvi_agg
# cropped crop type raster
ct_cropped_32631
# bbox of the cropped crop type raster
ct_bbox_sf <- as.polygons(terra::ext(ct_cropped_32631), crs=crs(ct_cropped_32631)) %>% st_as_sf()
# bbox of the ndvi_agg
bbox_sf_31n



























