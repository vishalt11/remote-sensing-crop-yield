library(terra)
library(readr)
library(sf)
library(tidyverse)

tile_vec <- c(
  "32UNA", "32UPA", "32UQA", 
  "32UNV", "32UPV", "32UQV", "33UUQ",
  "32UNU", "32UPU", "32UQU", "33UUP",
  "32TNT", "32TPT", "32TQT"
)

# Calculate NDVI for each tile
base_dir <- "../data/sentinel-2-tiles"

for (tile in tile_vec) {
  tile_dir <- file.path(base_dir, tile)
  
  # Files end with _B4.tif and _B8.tif
  b4_file <- list.files(tile_dir, pattern = "_B4\\.tif$", full.names = TRUE)
  b8_file <- list.files(tile_dir, pattern = "_B8\\.tif$", full.names = TRUE)
  
  if (length(b4_file) == 0 || length(b8_file) == 0) {
    warning("Missing B4 or B8 in ", tile_dir, " – skipping.")
    next
  }
  
  b4 <- rast(b4_file[1])
  b8 <- rast(b8_file[1])
  
  # NDVI formula
  ndvi <- (b8 - b4) / (b8 + b4)
  names(ndvi) <- "NDVI"
  
  # Save inside the tile folder
  out_file <- file.path(tile_dir, paste0(tile, "_NDVI.tif"))
  writeRaster(ndvi, out_file, overwrite = TRUE)
  
  message("Saved NDVI for ", tile)
}




#-------------------------------------------------------------------------------

r <- rast("../data/sentinel-2-tiles/32UPV/32UPV_NDVI.tif")
ct <- rast("../data/crop_type_tif/croptypes_2021.tif")

crop_classes <- read_delim("../data/crop_type_tif/LEGEND_CropTypes.txt", delim = "\t")
colnames(crop_classes) <- c("code", "label")
levels(r) <- data.frame(value = crop_classes$code, crop = crop_classes$label)

#- - - - - - - - - - - - - - - - - - - -

terra::plot(r)
crs(r)


ct_11 <- mask(ct, ct == 11, maskvalues = FALSE)
ct_11_mask <- (ct == 11)

#-------------------------------------------------------------------------------
nuts1_de <- giscoR::gisco_get_nuts(
  year    = "2021",   
  epsg    = 4326,     
  resolution = "3",   
  nuts_level = 1,
  country = "DE"
)

nuts1_de <- st_transform(nuts1_de, crs(ct_11))

nuts2_de <- giscoR::gisco_get_nuts(
  year    = "2021",   
  epsg    = 4326,     
  resolution = "3",   
  nuts_level = 2,
  country = "DE"
)

nuts2_de <- st_transform(nuts2_de, crs(ct_11))

plot(ct_11_mask, main = "Crop Type = 11 (Winter Wheat)", background = "black")
plot(st_geometry(nuts2_de), add = TRUE, border = "red", lwd = 2, alpha = 0.4)
plot(st_geometry(nuts1_de), add = TRUE, border = "white", lwd = 2)
















