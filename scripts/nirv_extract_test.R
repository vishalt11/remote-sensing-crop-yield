library(rsi)
library(rstac)
library(sf)
library(terra)
library(tidyverse)


# aoi_point <- sf::st_point(c(12.150977, 48.349362))
# aoi_sfc <- sf::st_set_crs(sf::st_sfc(aoi_point), 4326)
# aoi_projected <- sf::st_transform(aoi_sfc, 5070)
# aoi_buffer_projected <- sf::st_buffer(aoi_projected, 100)


aoi_kml <- st_read("./test_kml.kml")
aoi_kml <- st_make_valid(aoi_kml)
aoi_kml_4326<- st_transform(aoi_kml, 4326)


# https://docs.ropensci.org/rsi/reference/get_stac_data.html

output_file_name <- "./nirv_data/sentinel2_lcpri_composite.tif"

lcpri <- get_stac_data(
  #aoi_buffer_projected,
  aoi_kml_4326,
  start_date = "2024-04-01", 
  end_date = "2024-04-30",
  asset_names = c("B04", "B08", "SCL"), # BAND NAMES
  stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/",
  collection = "sentinel-2-l2a", # the name after collections https://planetarycomputer.microsoft.com/catalog
  #output_filename = tempfile(fileext = ".tif"),
  output_filename = output_file_name,
  #output_filename = output_file_name,
  cloud_cover_threshold = 100,
  #composite_function = NULL,
  mask_function = "s2_mask", # This tells rsi to use the SCL band to mask clouds
  composite_function = "mean", # Only the cloud-free pixels within your AOI are used for the mean composite calculation.
)

#dirname(lcpri)
#lcpri
#terra::plot(terra::rast(lcpri))

sentinel_raster <- terra::rast(lcpri)
sentinel_raster <- sentinel_raster / 10000
# calc NIRV
# NIRV = ((NIR - RED) / (NIR + RED))*NIR
# B08 is the second band, B04 is the first band in the SpatRaster
nirv_raster <- ((sentinel_raster[[2]] - sentinel_raster[[1]]) / 
                  (sentinel_raster[[2]] + sentinel_raster[[1]]))*sentinel_raster[[2]]


terra::plot(nirv_raster)

aoi_v <- terra::vect(aoi_kml_4326)

nirv_in <- mask(crop(nirv_raster, aoi_v), aoi_v)

mean_nirv <- global(nirv_in, fun = "mean", na.rm = TRUE)[1,1]






