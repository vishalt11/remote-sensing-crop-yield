library(terra)
library(sf)
library(tidyverse)
library(giscoR)
library(data.table)

#--------------------------TEST-------------------------------------------------

#ger_admin <- geodata::gadm(country = "DEU", level = 4, path = tempdir())

kreise <- st_read("../data/enhanced_sif/2_hoch.geo.json")
munich_sf <- kreise[kreise$NAME_3 == "Munich Städte", ]

plot(munich_sf, max.plot = 1)

munich_poly <- vect(munich_sf)

r <- rast('../data/enhanced_sif/GOSIF_2024.M05.tif/GOSIF_2024.M05.tif')

nlyr(r)
names(r)
is.factor(r)
levels(r)

terra::plot(r)


global(r, fun = "min", na.rm = TRUE)
global(r, fun = "max", na.rm = TRUE)

r[r >= 32766] <- NA
r_real <- r * 0.0001

ger_extent <- ext(5.8, 15.1, 47.2, 55.1)

munich_extent <- ext(11.5, 11.7, 47.9, 48.4)
munich_coords <- data.frame(lon = 11.576, lat = 48.137)


terra::plot(r_real, ext = munich_extent)
grid(nx = 10, ny = 10, col = "black", lty = "dotted")
points(munich_coords, pch = 3, col = "red", lwd = 2)
text(11.576, 48.15, "Munich", col = "red", pos = 3)
lines(munich_poly, col = "black", lwd = 2)


r_cropped <- crop(r_real, munich_poly)

terra::plot(r_cropped)
points(munich_coords, pch = 3, col = "red", lwd = 2)
text(11.576, 48.15, "Munich", col = "red", pos = 3)
lines(munich_poly, col = "black", lwd = 2)


pixel_area <- cellSize(r_cropped, unit = "km")

print(paste("Pixel Area:", round(values(pixel_area)[1], 2), "km^2"))

#---------crop type raster (UTM) cropped to munich boundary------
crop_type <- rast('../data/crop_type_tif/croptypes_2024.tif')

crop_classes <- readr::read_delim("../data/crop_type_tif/LEGEND_CropTypes.txt", delim = "\t")
colnames(crop_classes) <- c("code", "label")
levels(crop_type)
levels(crop_type) <- data.frame(value = crop_classes$code, crop = crop_classes$label)
# colors for each crop type for plotting purposes
n_classes <- nrow(crop_classes)
palette_crop <- c("#FFFFFF","#0070FF","#00BFFF","#87CEFA","#ADD8E6","#F4A460","#FFA500","#FF8C00","#FF00FF","#D2B48C",
                  "#8B4513","#8400A8","#FFFF99","#D1FF73","#89CD66","#4E7500","#FFBEBE","#FF7F50","#004C70","#CCCCCC")
palette_crop <- palette_crop[1:n_classes]



munich_poly_utm <- project(munich_poly, crs(crop_type))

crop_cropped <- crop(crop_type, munich_poly_utm)

terra::plot(crop_cropped)
lines(munich_poly_utm, col = "black", lwd = 2)


#-------------- convert crop raster to sif's crs------------ 

crop_cropped_wgs84 <- project(crop_cropped, crs(r_cropped))


plot(r_cropped, alpha = 0.6, main = "GOSIF and Crop Type Overlay", legend = FALSE)

# Plot the second raster on top
# (Adjust its alpha if needed, or leave at 1 to see it clearly)
plot(crop_cropped_wgs84, col = palette_crop, 
     plg = list(cex = 0.4, ncol = 1), add = TRUE)

# Add vector overlays
lines(munich_poly, col = "black", lwd = 2)
points(munich_coords, pch = 3, col = "red", lwd = 2)
text(11.576, 48.15, "Munich", col = "red", pos = 3)





################################################################################
#---------------------zonal stat 1 file-----------------------------------------

nuts1 <- gisco_get_nuts(year = 2021,  resolution = "10", nuts_level = 1, epsg = 4326)

bavaria_sf <- subset(nuts1, NUTS_ID == "DE2")
bavaria_v  <- vect(bavaria_sf) 

# gosif raster
gosif_202405 <- rast('../data/enhanced_sif/gosif/GOSIF_2024.M05.tif')

# necessary transformations to correct sif value's range
gosif_202405[gosif_202405 >= 32766] <- NA
gosif_202405 <- gosif_202405 * 0.0001

#crs(gosif_202405)


gosif_bav_crop <- crop(gosif_202405, bavaria_v)
gosif_bav_crop <- mask(gosif_bav_crop, bavaria_v)

#plot(bavaria_v)
#plot(gosif_bav_crop, ext = ext(8.5, 14, 47.0, 50.8))

# raster has different fields in germany marked, colored (winter wheat, maize, soyabean etc.)
crop_type <- rast('../data/crop_type_tif/croptypes_2024.tif')

# set levels for crop type raster
crop_classes <- readr::read_delim("../data/crop_type_tif/LEGEND_CropTypes.txt", delim = "\t")
colnames(crop_classes) <- c("code", "label")
levels(crop_type) <- data.frame(value = crop_classes$code, crop = crop_classes$label)

#crs(crop_type)

# crop down crop type raster to bavaria's border

bavaria_utm <- project(bavaria_v, "EPSG:32632")

# Crop the Germany-wide crop raster down to Bavaria first 
crop_type_bav <- crop(crop_type, bavaria_utm)
crop_type_bav <- mask(crop_type_bav, bavaria_utm)

# Project the (already Bavaria-masked) SIF raster to UTM
# (continuous values -> bilinear is appropriate)
gosif_bav_utm <- project(gosif_bav_crop, "EPSG:32632", method = "bilinear")

# (optional) ensure it’s still strictly within Bavaria border after reprojection
gosif_bav_utm <- mask(gosif_bav_utm, bavaria_utm)

# make each sif pixel into polygon
zones <- as.polygons(gosif_bav_utm, dissolve = FALSE, values = TRUE, na.rm = TRUE)
zones$zone_id <- 1:nrow(zones)

# method 1: extract
# Too slow
# Exact extraction returns a row per (zone_id, crop_value) combination at 10m level,
# plus a coverage weight/fraction for partial overlaps.
# ex <- terra::extract(crop_type_bav, zones, exact = TRUE)

# Terra column naming can differ by version; these are typical:
# - "ID" for polygon id
# - "crop" or the raster layer name for class value
# - "fraction" (or sometimes "weight") for coverage
#names(ex)


# method 1: use data.table to get crop stats for each pixel using readValues

zone_r <- rasterize(zones, crop_type_bav, field = "zone_id", touches = TRUE)
#tab <- crosstab(zone_r, crop_type_bav, long = TRUE)   # columns like: zone_id, crop, freq
#names(tab) <- c("zone_id", "code", "n")

compareGeom(zone_r, crop_type_bav, stopOnError = TRUE)


s <- c(zone_r, crop_type_bav)
names(s) <- c("zone_id", "code")
# s: two-layer SpatRaster, names = c("zone_id","code")

nr <- nrow(s)

chunk_rows <- 2000L
rows  <- seq.int(1L, nr, by = chunk_rows)
nrows <- pmin(chunk_rows, nr - rows + 1L)

out_list <- vector("list", length(rows))

# IMPORTANT: open files for reading
readStart(s)
on.exit(readStop(s), add = TRUE)

for (i in seq_along(rows)) {
  m <- readValues(s, row = rows[i], nrows = nrows[i], mat = TRUE)
  
  z <- m[, 1]
  c <- m[, 2]
  
  keep <- !is.na(z) & !is.na(c)
  if (!any(keep)) next
  
  dt <- data.table(
    zone_id = as.integer(z[keep]),
    code    = as.integer(c[keep])
  )
  
  out_list[[i]] <- dt[, .(count = .N), by = .(zone_id, code)]
  
  if (i %% 10 == 0) message("chunk ", i, "/", length(rows))
  print(i)
}

tab <- rbindlist(out_list, use.names = TRUE, fill = TRUE)
tab <- tab[, .(count = sum(count)), by = .(zone_id, code)]
setorder(tab, zone_id, -count)

head(tab)

legend_df <- levels(crop_type)[[1]]  # columns: value, crop

comp <- dplyr::as_tibble(tab) %>%
  dplyr::group_by(zone_id) %>%
  dplyr::mutate(
    total_cells = sum(count),
    pct = count / total_cells
  ) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(legend_df, by = c("code" = "value"))

comp %>%
  group_by(zone_id) %>%
  summarise(sum_pct = sum(pct)) %>%
  summary()

# Compute “vegetated total” per zone (exclude code 0)
veg_totals <- comp %>%
  filter(code != 0) %>%           # exclude no_data
  group_by(zone_id) %>%
  summarise(veg_cells = sum(count), .groups = "drop")

# Compute winter wheat share among vegetation
ww_share_veg <- comp %>%
  dplyr::filter(code == 11) %>%           # winter wheat
  dplyr::select(zone_id, ww_cells = count) %>%
  dplyr::left_join(veg_totals, by = "zone_id") %>%
  dplyr::mutate(ww_pct_veg = ww_cells / veg_cells)


ww_zones <- ww_share_veg %>%
  dplyr::filter(!is.na(ww_pct_veg), ww_pct_veg >= 0.05) %>%
  dplyr::pull(zone_id)

# ww_zones <- comp %>%
#   dplyr::filter(code == 11, pct >= 0.1) %>%
#   dplyr::pull(zone_id) %>%
#   unique()

length(ww_zones)





# comp <- comp %>%
#   select(-ww_pct_veg)
# 
# comp <- comp %>%
#   left_join(
#     ww_share_veg %>% select(zone_id, ww_pct_veg),
#     by = "zone_id"
#   )


#-----------plot the gosif pixels that have wheat share > 5%

# 1) Download NUTS3 in EPSG:4326 (lightweight) and filter to Bavaria (DE2...)
nuts3 <- gisco_get_nuts(year = 2021, resolution = "10", nuts_level = 3, epsg = 4326)
bav_nuts3_sf <- nuts3[grepl("^DE2", nuts3$NUTS_ID), ]
# Convert to terra vector + project to UTM 
bav_nuts3_v <- terra::vect(bav_nuts3_sf)
bav_nuts3_v <- terra::project(bav_nuts3_v, "EPSG:32632")


zones_ww <- zones[zones$zone_id %in% ww_zones, ]

# Plot SIF background + outline qualifying pixels
plot(gosif_bav_utm, main = "GOSIF May 2024 (Bavaria) – pixels with ≥5% winter wheat")
lines(zones, col = "grey60", lwd = 0.3)      # optional: show all pixel boundaries faintly
lines(zones_ww, col = "red", lwd = 1.8)      # highlight qualifying pixels
lines(bav_nuts3_v, col = "blue", lwd = 1.8)







#------------------------gosif zonal stats--------------------------------------















