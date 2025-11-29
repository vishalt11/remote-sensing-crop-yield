
library(tidyverse)

tile_vec <- c(
  "32UNA", "32UPA", "32UQA", 
  "32UNV", "32UPV", "32UQV", "33UUQ",
  "32UNU", "32UPU", "32UQU", "33UUP",
  "32TNT", "32TPT", "32TQT"
)

# base URL 
base_url <- "https://download.geoservice.dlr.de/S2_L3A_WASP/files"

year  <- 2021L
month <- 5L


# assume the same pattern for all tiles
date_stamp <- sprintf("%04d%02d15-000000-000", year, month)
# -> "20210515-000000-000"

bands <- c("B4", "B8")

out_dir <- "../data/sentinel-2-tiles/"
#dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

download_wasp_tile <- function(tile_id) {
  # tile_id, e.g. "32UNA"
  zone      <- substr(tile_id, 1, 2)  # "32"
  lat_band  <- substr(tile_id, 3, 3)  # "U" or "T"
  grid_sq   <- substr(tile_id, 4, 5)  # "NA", "PA", ...
  
  # Remote path pieces: /32/U/NA/2021/
  path_prefix <- paste(base_url, zone, lat_band, grid_sq, year, sep = "/")
  
  # Folder name for that month/tile, e.g.:
  # SENTINEL2X_20210515-000000-000_L3A_T32UNA_C_V1-2
  prod_name <- sprintf("SENTINEL2X_%s_L3A_T%s_C_V1-2", date_stamp, tile_id)
  
  prod_dir  <- paste(path_prefix, prod_name, sep = "/")
  
  # Local subfolder per tile
  tile_out_dir <- file.path(out_dir, tile_id)
  dir.create(tile_out_dir, showWarnings = FALSE, recursive = TRUE)
  
  for (b in bands) {
    # File name, e.g.:
    # SENTINEL2X_20210515-000000-000_L3A_T32UNA_C_V1-2_FRC_B4.tif
    fname <- sprintf("%s_FRC_%s.tif", prod_name, b)
    
    url   <- paste(prod_dir, fname, sep = "/")
    dest  <- file.path(tile_out_dir, fname)
    
    message("Downloading ", tile_id, " ", b, " from:\n  ", url)
    
    tryCatch(
      {
        download.file(url, destfile = dest, mode = "wb", quiet = TRUE)
        message("  -> saved to ", dest)
      },
      error = function(e) {
        message("  !! Failed for ", tile_id, " ", b, ": ", conditionMessage(e))
      }
    )
  }
}


for (tile in tile_vec) {
  print(tile)
  download_wasp_tile(tile)
}


