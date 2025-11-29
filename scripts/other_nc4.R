library(ncdf4) 
library(readr)

library(lubridate)
library(tidyverse)

setwd("D:/UE_Applied_Sciences/3_Semester/Capstone/scripts")

nc_data <- nc_open('../data/oco2SIF10r_2022.01.01_2024.03.31/oco2_LtSIF_211231_B11012Ar_220627183447s.nc4')

attributes(nc_data$var)$names

lon_corners <- ncvar_get(nc_data,"Longitude_Corners")


#-------------------------------------------------------------------------------
# bulk load
files <- list.files('../data/oco2SIF10r_2022.01.01_2024.03.31//', pattern = "\\.nc4$", full.names = TRUE)

nc_list <- list()

counter <- 1
for(f in files){
  cat("Reading:", f, "\n")
  nc <- tryCatch(ncdf4::nc_open(f), error = function(e) NULL)
  
  if(!is.null(nc)){
    if (length(nc$var) > 0) {
      short_name <- paste0("a", counter)
      nc_list[[short_name]] <- nc
      counter <- counter + 1
    } else {
      cat("Skipping (no variables):", f, "\n")
      ncdf4::nc_close(nc)
    }
  } else {
    cat("Could not open:", f, "\n")
  }
}

cat("loaded", length(nc_list), "files successfully.\n")
length(files)


#-------------------------------------------------------------------------------
# test variable structure
# a23 has only one sounding
attributes(nc_list$a1$var)$names
nc_list$a1$dim$sounding_dim

lon_corners <- ncvar_get(nc_list$a36,"Longitude_Corners")
lon <- ncvar_get(nc_list$a36,"Longitude")

#-------------------------------------------------------------------------------
# Germany bounding box
lat_min <- 47.27
lat_max <- 55.06
lon_min <- 5.87
lon_max <- 15.04

vars_to_extract <- c("Daily_SIF_740nm", "Delta_Time", "Latitude", "Longitude", 
                     "Latitude_Corners", "Longitude_Corners", "Quality_Flag", 
                     "Meteo/specific_humidity", "Meteo/surface_pressure",
                     "Meteo/temperature_skin", "Meteo/temperature_two_meter", 
                     "Meteo/vapor_pressure_deficit", "Metadata/MeasurementMode")

df_list <- list()

for (i in seq_along(nc_list)) {
  
  nc <- nc_list[[i]]
  short_name <- names(nc_list)[i]
  cat("Processing:", short_name, "\n")
  
  # --- STEP 1: read lat/lon only
  lat <- ncvar_get(nc, "Latitude")
  lon <- ncvar_get(nc, "Longitude")
  
  if (is.null(lat) | is.null(lon)) {
    cat("Missing lat/lon in file:", short_name, "\n")
    next
  }
  
  # --- STEP 2: create mask for soundings inside Germany
  mask <- lat >= lat_min & lat <= lat_max & 
    lon >= lon_min & lon <= lon_max
  
  # if nothing inside Germany, skip file
  if (sum(mask) == 0) {
    cat("No soundings within Germany in", short_name, "\n")
    next
  }
  
  dtime <- ncvar_get(nc, "Delta_Time")
  dtime <- as.POSIXct(dtime, origin = "1990-01-01", tz = "UTC")
  
  if (unique(month(dtime)) < 2 | unique(month(dtime)) > 7) {
    cat("Not within timeframe", short_name, "\n")
    next
  }
  
  vars <- list()
  #remove if else, it has been already checked for missing
  for (v in vars_to_extract) {
    if (v %in% names(nc$var)) {
      vals <- ncvar_get(nc, v)
      
      # correction for lat, lon corners as matrix
      # Handle 4xN matrix
      if (v == "Latitude_Corners") {
        if (length(dim(vals)) == 1) {
          # Single sounding (1D array of 4 corners)
          vars$Lat_corner1 <- vals[1]
          vars$Lat_corner2 <- vals[2]
          vars$Lat_corner3 <- vals[3]
          vars$Lat_corner4 <- vals[4]
        } else if (length(dim(vals)) == 2) {
          # Multi-sounding (4 × N)
          vars$Lat_corner1 <- as.vector(vals[1, ])
          vars$Lat_corner2 <- as.vector(vals[2, ])
          vars$Lat_corner3 <- as.vector(vals[3, ])
          vars$Lat_corner4 <- as.vector(vals[4, ])
        } else {
          cat("Unexpected shape for Latitude_Corners in", short_name, "\n")
        }
        
      } else if (v == "Longitude_Corners") {
        if (length(dim(vals)) == 1) {
          vars$Lon_corner1 <- vals[1]
          vars$Lon_corner2 <- vals[2]
          vars$Lon_corner3 <- vals[3]
          vars$Lon_corner4 <- vals[4]
        } else if (length(dim(vals)) == 2) {
          vars$Lon_corner1 <- as.vector(vals[1, ])
          vars$Lon_corner2 <- as.vector(vals[2, ])
          vars$Lon_corner3 <- as.vector(vals[3, ])
          vars$Lon_corner4 <- as.vector(vals[4, ])
        } else {
          cat("Unexpected shape for Longitude_Corners in", short_name, "\n")
        }
        
      } else {
        vars[[v]] <- as.vector(vals)
      }
      
      #vars[[v]] <- as.vector(vals)
    } else {
      cat("Variable", v, "missing in", short_name, "\n")
      vars[[v]] <- NA
    }
  }
  
  # Combine into a data frame for this file
  df <- as.data.frame(vars)
  df$file_id <- short_name  
  
  df_list[[short_name]] <- df
}

# Combine all into one large data frame
combined_df <- bind_rows(df_list)
head(combined_df, 3)
rm(df_list)

combined_df$Delta_Time <- as.POSIXct(combined_df$Delta_Time, origin = "1990-01-01", tz = "UTC")

#combined_df$Delta_date <- format(combined_df$Delta_Time, "%Y-%m-%d")
# filter to germany
combined_df <- combined_df %>% filter(
    Latitude  >= lat_min,
    Latitude  <= lat_max,
    Longitude >= lon_min,
    Longitude <= lon_max
  )
base::saveRDS(combined_df, file = '../data/SIF_v2_corrected.rds')



