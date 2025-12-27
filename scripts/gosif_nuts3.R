library(terra)
library(sf)
library(tidyverse)
library(lubridate)
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
levels(crop_type) <- data.frame(value = crop_classes$code, crop = crop_classes$label)
# colors for each crop type for plotting purposes
n_classes <- nrow(crop_classes)
palette_crop <- c("#FFFFFF","#0070FF","#00BFFF","#87CEFA","#ADD8E6","#F4A460",
                  "#FFA500","#FF8C00","#FF00FF","#D2B48C","#8B4513","#8400A8",
                  "#FFFF99","#D1FF73","#89CD66","#4E7500","#FFBEBE","#FF7F50",
                  "#004C70","#CCCCCC")
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

#------------ SIF area, how many polygons to make for oco2?

df <- readRDS('../data/oco2_sif.rds')

df_2024 <- df |> dplyr::filter(year(Delta_Time) == 2024)

sum(df_2024$total_area_m2/1e+6)

#----------------- distribution of bavaria wheat farm sizes---------------------

#----------------- find major clusters of wheat farms bavaria-------------------


################################################################################
#---------------------gosif zonal stats-----------------------------------------
################################################################################


nuts1 <- gisco_get_nuts(year = 2024,  resolution = "03", nuts_level = 1, epsg = 4326)
nuts3 <- gisco_get_nuts(year = 2024, resolution = "03", nuts_level = 3, epsg = 4326)

bavaria_sf <- subset(nuts1, NUTS_ID == "DE2")
bavaria_v  <- vect(bavaria_sf) 


# raster has different fields in germany marked, colored (winter wheat, maize, soyabean etc.)
crop_type <- rast('../data/crop_type_tif/croptypes_2024.tif')

# set levels for crop type raster
crop_classes <- readr::read_delim("../data/crop_type_tif/LEGEND_CropTypes.txt", delim = "\t")
colnames(crop_classes) <- c("code", "label")
levels(crop_type) <- data.frame(value = crop_classes$code, crop = crop_classes$label)

# Convert gosif to UTM to do zonal stats with crop type raster
# convert vec object to utm
bavaria_utm <- project(bavaria_v, "EPSG:32632")

# Crop the Germany-wide crop raster down to Bavaria first 
crop_type_bav <- crop(crop_type, bavaria_utm)
crop_type_bav <- mask(crop_type_bav, bavaria_utm)


# gosif raster for 2024 May (month 5)
gosif_202405 <- rast('../data/enhanced_sif/gosif/GOSIF_2024.M05.tif')

# necessary transformations to correct sif value's range
gosif_202405[gosif_202405 >= 32766] <- NA
gosif_202405 <- gosif_202405 * 0.0001

gosif_bav_crop <- crop(gosif_202405, bavaria_v)
gosif_bav_crop <- mask(gosif_bav_crop, bavaria_v)

#plot(bavaria_v)
#plot(gosif_bav_crop, ext = ext(8.5, 14, 47.0, 50.8))

# Project the (already Bavaria-masked) SIF raster to UTM
# (continuous values -> bilinear)
gosif_bav_utm <- project(gosif_bav_crop, "EPSG:32632", method = "bilinear")

# ensure it’s still strictly within Bavaria border after reprojection
gosif_bav_utm <- mask(gosif_bav_utm, bavaria_utm)

# make each sif pixel into polygon
zones <- as.polygons(gosif_bav_utm, dissolve = FALSE, values = TRUE, na.rm = TRUE)
zones$zone_id <- 1:nrow(zones)

# Use data.table to get crop stats for each pixel using readValues

zone_r <- rasterize(zones, crop_type_bav, field = "zone_id", touches = TRUE)

# check if rasterize gosif polygons match
compareGeom(zone_r, crop_type_bav, stopOnError = TRUE)

s <- c(zone_r, crop_type_bav)
names(s) <- c("zone_id", "code")

nr <- nrow(s)
chunk_rows <- 2000L
rows  <- seq.int(1L, nr, by = chunk_rows)
nrows <- pmin(chunk_rows, nr - rows + 1L)

out_list <- vector("list", length(rows))

# open files for reading
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

head(tab,4)

# do gc() here to clear unecessary RAM from the for loop operations

# correction use total area instead of total cells
# columns: value, crop
legend_df <- levels(crop_type)[[1]]  

comp <- dplyr::as_tibble(tab) %>%
  dplyr::group_by(zone_id) %>%
  dplyr::mutate(
    total_cells = sum(count),
    pct = count / total_cells
  ) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(legend_df, by = c("code" = "value"))

# hist(comp$total_cells)

# comp %>%
#   dplyr::group_by(zone_id) %>%
#   dplyr::summarise(sum_pct = sum(pct)) %>%
#   summary()

# Compute “vegetated total” per zone (exclude code 0 = no_data)
veg_totals <- comp %>%
  dplyr::filter(code != 0) %>%           
  dplyr::group_by(zone_id) %>%
  dplyr::summarise(veg_cells = sum(count), .groups = "drop") 

# Compute winter wheat share among vegetation
ww_share_veg <- comp %>%
  dplyr::filter(code == 11) %>%          
  dplyr::select(zone_id, ww_cells = count) %>%
  dplyr::left_join(veg_totals, by = "zone_id") %>%
  dplyr::mutate(ww_pct_veg = ww_cells / veg_cells)

# 5% wheat share
ww_zones <- ww_share_veg %>%
  dplyr::filter(!is.na(ww_pct_veg), ww_pct_veg >= 0.20) %>%
  dplyr::pull(zone_id)

length(ww_zones)

# maize (C4 proxy) cells per zone
maize_cells <- comp %>%
  dplyr::filter(code == 30) %>%
  dplyr::select(zone_id, maize_cells = count)

# combine zone-level counts needed
zone_counts <- veg_totals %>%
  dplyr::left_join(ww_share_veg %>% dplyr::select(zone_id, ww_cells), by = "zone_id") %>%
  dplyr::left_join(maize_cells, by = "zone_id") %>%
  dplyr::mutate(
    ww_cells = dplyr::coalesce(ww_cells, 0L),
    maize_cells = dplyr::coalesce(maize_cells, 0L),
    c3_cells = veg_cells - maize_cells
  )


nuts3_bav_sf <- nuts3[grepl("^DE2", nuts3$NUTS_ID), ]
# convert to terra vec + utm
nuts3_bav_v <- terra::vect(nuts3_bav_sf)
nuts3_bav_utm <- terra::project(nuts3_bav_v, "EPSG:32632")

# keep only zones that qualify ww share criteria
zones_ww <- zones[zones$zone_id %in% ww_zones,]

gosif_col <- names(gosif_bav_utm)[1]

# 1. centroid assignment mean---------------------------------------------------

pts <- centroids(zones_ww)

join <- terra::extract(nuts3_bav_utm, pts)

pix_df <- as.data.frame(zones_ww)[, c("zone_id", gosif_col)]
pix_df <- bind_cols(pix_df, join[, c("NUTS_ID", "NUTS_NAME")])

# pix_df2 <- pix_df %>%
#   dplyr::left_join(ww_share_veg %>% 
#                      dplyr::select(zone_id, ww_cells, veg_cells), by = "zone_id")

pix_df2 <- pix_df %>%
  dplyr::left_join(zone_counts, by = "zone_id")


#mean aggregation code with ww share(qualifying pixels only)
nuts3_gosif_ww <- pix_df2 %>%
  dplyr::filter(!is.na(.data[[gosif_col]]), !is.na(NUTS_ID), !is.na(veg_cells), veg_cells > 0) %>%
  dplyr::group_by(NUTS_ID, NUTS_NAME) %>%
  dplyr::summarise(
    n_pixels   = dplyr::n(),
    mean_gosif = mean(.data[[gosif_col]], na.rm = TRUE),
    
    ww_cells   = sum(ww_cells, na.rm = TRUE),
    maize_cells = sum(maize_cells, na.rm = TRUE),
    veg_cells  = sum(veg_cells, na.rm = TRUE),
    
    ww_share   = ww_cells / veg_cells,
    c4_share   = maize_cells / veg_cells,
    c3_share   = 1 - (maize_cells / veg_cells),  # or sum(c3_cells)/sum(veg_cells)
    
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(n_pixels))

nuts3_gosif_ww


# 2. area weighted mean---------------------------------------------------------

# nuts3_bav_utm is a SpatVector with multiple features
# zones_ww is qualifying pixels (SpatVector)

ov_list <- lapply(seq_len(nrow(nuts3_bav_utm)), function(i) {
  n3 <- nuts3_bav_utm[i, ]
  
  # clip pixels to THIS NUTS3 polygon -> splits pixels at boundaries
  x <- terra::intersect(zones_ww, n3)
  
  if (is.null(x) || nrow(x) == 0) return(NULL)
  
  x$NUTS_ID   <- n3$NUTS_ID
  x$NUTS_NAME <- n3$NUTS_NAME
  x
})

ov_list <- Filter(Negate(is.null), ov_list)
ov <- do.call(rbind, ov_list)   # SpatVector of overlap pieces

# areas
ov$area_m2 <- expanse(ov, unit = "m")

# df for aggregation
gosif_col <- names(gosif_bav_utm)[1]
ov_df <- as.data.frame(ov)[, c(gosif_col, "zone_id", "NUTS_ID", "NUTS_NAME", "area_m2")]

# sanity check
ov_df %>%
  count(zone_id) %>%
  summarise(max_pieces_per_pixel = max(n), pixels_split = sum(n > 1), total_pixels = n())

ov_df2 <- ov_df %>%
  dplyr::left_join(ww_share_veg %>% dplyr::select(zone_id, ww_cells, veg_cells), by = "zone_id") %>%
  dplyr::left_join(maize_cells, by = "zone_id") %>%
  dplyr::mutate(
    maize_cells = dplyr::coalesce(maize_cells, 0L)
  ) %>%
  dplyr::filter(!is.na(ww_cells), !is.na(veg_cells), veg_cells > 0, area_m2 > 0) %>%
  dplyr::mutate(
    veg_alloc   = area_m2,
    ww_alloc    = area_m2 * (ww_cells / veg_cells),
    maize_alloc = area_m2 * (maize_cells / veg_cells)   # C4 allocation
  )

nuts3_gosif_ww_aw <- ov_df2 %>%
  dplyr::filter(!is.na(.data[[gosif_col]]), !is.na(NUTS_ID)) %>%
  dplyr::group_by(NUTS_ID, NUTS_NAME) %>%
  dplyr::summarise(
    n_pieces   = dplyr::n(),
    n_pixels   = dplyr::n_distinct(zone_id),
    area_km2   = sum(area_m2) / 1e6,
    mean_gosif = sum(.data[[gosif_col]] * area_m2) / sum(area_m2),
    
    ww_share = sum(ww_alloc) / sum(veg_alloc),
    
    c4_share = sum(maize_alloc) / sum(veg_alloc),
    c3_share = 1 - (sum(maize_alloc) / sum(veg_alloc)),
    
    .groups = "drop"
  )

nuts3_gosif_ww_aw

#-------------------crop type nuts 3 composition--------------------------------

crop_type_bav
nuts3_bav_utm

library(terra)
library(data.table)
library(dplyr)

# --- 0) Prep: add an integer ID per NUTS3 polygon ---
nuts3_bav_utm$nuts3_i <- seq_len(nrow(nuts3_bav_utm))

nuts3_key <- as.data.frame(nuts3_bav_utm)[, c("nuts3_i", "NUTS_ID", "NUTS_NAME")]

# --- 1) Rasterize NUTS3 polygons to the crop grid (10m) ---
nuts3_id_r <- rasterize(nuts3_bav_utm, crop_type_bav, field = "nuts3_i", touches = TRUE)

# sanity: same geometry
compareGeom(nuts3_id_r, crop_type_bav, stopOnError = TRUE)

# --- 2) Stack: (nuts3_id, crop_code) ---
s <- c(nuts3_id_r, crop_type_bav)
names(s) <- c("nuts3_i", "code")

# --- 3) Block-wise joint counts: (nuts3_i, code) -> count ---
nr <- nrow(s)
chunk_rows <- 2000L
rows  <- seq.int(1L, nr, by = chunk_rows)
nrows <- pmin(chunk_rows, nr - rows + 1L)

out_list <- vector("list", length(rows))

readStart(s)
on.exit(readStop(s), add = TRUE)

for (i in seq_along(rows)) {
  m <- readValues(s, row = rows[i], nrows = nrows[i], mat = TRUE)
  
  n3 <- m[, 1]
  cd <- m[, 2]
  
  keep <- !is.na(n3) & !is.na(cd)
  if (!any(keep)) next
  
  dt <- data.table(
    nuts3_i = as.integer(n3[keep]),
    code    = as.integer(cd[keep])
  )
  
  out_list[[i]] <- dt[, .(count = .N), by = .(nuts3_i, code)]
  
  if (i %% 10 == 0) message("chunk ", i, "/", length(rows))
}

tab_n3 <- rbindlist(out_list, use.names = TRUE, fill = TRUE)
tab_n3 <- tab_n3[, .(count = sum(count)), by = .(nuts3_i, code)]
setorder(tab_n3, nuts3_i, code)

# --- 4) Compute veg totals + wheat + C4 (maize) shares (veg-only: exclude code 0) ---
# winter wheat = 11, maize (C4) = 30, no_data = 0

veg_totals <- tab_n3[code != 0, .(veg_cells = sum(count)), by = nuts3_i]
ww_cells   <- tab_n3[code == 11, .(ww_cells = sum(count)), by = nuts3_i]
maize_cells <- tab_n3[code == 30, .(maize_cells = sum(count)), by = nuts3_i]

# merge + compute shares
nuts3_shares <- veg_totals %>%
  as_tibble() %>%
  left_join(as_tibble(ww_cells), by = "nuts3_i") %>%
  left_join(as_tibble(maize_cells), by = "nuts3_i") %>%
  mutate(
    ww_cells = coalesce(ww_cells, 0L),
    maize_cells = coalesce(maize_cells, 0L),
    ww_share = ww_cells / veg_cells,
    c4_share = maize_cells / veg_cells,
    c3_share = 1 - c4_share,
    veg_area_km2 = (veg_cells * 100) / 1e6   # 10m cell = 100 m^2
  ) %>%
  left_join(nuts3_key, by = "nuts3_i") %>%
  select(NUTS_ID, NUTS_NAME, veg_cells, veg_area_km2, ww_cells, ww_share, maize_cells, c4_share, c3_share) %>%
  arrange(desc(veg_area_km2))

nuts3_shares

saveRDS(nuts3_shares, '../data/gosif_nuts3/nuts3_shares.rds')

saveRDS(nuts3_gosif_ww, '../data/gosif_nuts3/nuts3_gosif_ww_centroid.rds')
saveRDS(nuts3_gosif_ww_aw, '../data/gosif_nuts3/nuts3_gosif_ww_aw.rds')
#-----------plot the gosif pixels that have wheat share > 5%--------------------

# filter bavaria
nuts3_bav_sf <- nuts3[grepl("^DE2", nuts3$NUTS_ID), ]
# convert to terra vec + utm
nuts3_bav_v <- terra::vect(nuts3_bav_sf)
nuts3_bav_utm <- terra::project(nuts3_bav_v, "EPSG:32632")

zones_ww <- zones[zones$zone_id %in% ww_zones, ]

# plot sif + qualifying pixels
plot(gosif_bav_utm, main = "GOSIF May 2024 (Bavaria) – pixels with ≥20% winter wheat")
lines(zones, col = "grey60", lwd = 0.3)      
lines(zones_ww, col = "red", lwd = 1.8)     
lines(nuts3_bav_utm, col = "blue", lwd = 1.8)


#------------------------gosif zonal stats--------------------------------------






