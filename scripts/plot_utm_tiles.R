library(tidyverse)
library(terra)
library(sf)
library(rnaturalearth)

b4 <- rast("../data/backup/SENTINEL2X_20210515-000000-000_L3A_T33UUQ_C_V1-2_FRC_B4.tif")
b8 <- rast("../data/backup/SENTINEL2X_20210515-000000-000_L3A_T33UUQ_C_V1-2_FRC_B8.tif")


# NDVI: (B8 - B4) / (B8 + B4)
ndvi <- (b8 - b4) / (b8 + b4)
names(ndvi) <- "NDVI"


germany_border <- ne_countries(country = "Germany", returnclass = "sf")
germany_border_proj <- st_transform(germany_border, crs(ndvi))


terra::plot(
  ndvi,
  col = hcl.colors(100, "Army"), 
  main = "NDVI Overlayed with Germany Border",
  legend = TRUE,
  #range = c(-1, 1)
)

bbox_data <- as.list(ext(ndvi))

# bbox coords
bbox_coords <- matrix(c(
  bbox_data$xmin, bbox_data$ymin,
  bbox_data$xmin, bbox_data$ymax,
  bbox_data$xmax, bbox_data$ymax,
  bbox_data$xmax, bbox_data$ymin,
  bbox_data$xmin, bbox_data$ymin
), ncol = 2, byrow = TRUE)

# sf polygon bbox
bbox_sf <- st_polygon(list(bbox_coords)) %>%
  st_sfc(crs = crs(ndvi))

# wgs84 for plotting
bbox_wgs84 <- st_transform(bbox_sf, crs = 4326)



world_map <- ne_countries(scale = "medium", returnclass = "sf")


bbox_ext_wgs84 <- st_bbox(bbox_wgs84)

ggplot() +
  geom_sf(data = world_map, fill = "gray90", color = "gray50") +
  geom_sf(data = bbox_wgs84, fill = NA, color = "red", linewidth = 1.2, linetype = "solid") +
  coord_sf(
    xlim = c(bbox_ext_wgs84["xmin"] - 4, bbox_ext_wgs84["xmax"] + 4),
    ylim = c(bbox_ext_wgs84["ymin"] - 4, bbox_ext_wgs84["ymax"] + 4),
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

ndvi <- rast("../data/copernicus_ndvi_300m/NDVI_20250201T000000Z.tif")

nuts1_de <- giscoR::gisco_get_nuts(
  year    = "2021",   
  epsg    = 4326,     
  resolution = "3",   
  nuts_level = 1,
  country = "DE"
)

nuts1_de <- st_transform(nuts1_de, crs(ndvi))

nuts2_de <- giscoR::gisco_get_nuts(
  year    = "2021",   
  epsg    = 4326,     
  resolution = "3",   
  nuts_level = 2,
  country = "DE"
)

nuts2_de <- st_transform(nuts2_de, crs(ndvi))

plot(ndvi)
plot(st_geometry(nuts2_de), add = TRUE, border = "red", lwd = 2, alpha = 0.4)
plot(st_geometry(nuts1_de), add = TRUE, border = "blue", lwd = 2)

#-------------------------------------------------------------------------------

tile_vec <- c(
  "32UNA", "32UPA", "32UQA", 
  "32UNV", "32UPV", "32UQV", "33UUQ",
  "32UNU", "32UPU", "32UQU", "33UUP",
  "32TNT", "32TPT", "32TQT"
)

base_dir <- "../data/sentinel-2-tiles/"  

b4_files <- list.files(base_dir, pattern = "FRC_B4\\.tif$", full.names = TRUE, recursive = TRUE)

length(b4_files)  

common_crs <- 4326  # EPSG:3035 / 4326

bbox_list <- lapply(b4_files, function(f) {
  r <- terra::rast(f)
  ext <- terra::ext(r)
  
  poly <- terra::as.polygons(ext, crs = terra::crs(r))
  sf_poly <- sf::st_as_sf(poly)
  
  sf_poly <- sf::st_transform(sf_poly, common_crs)
  
  sf_poly$tile_file <- basename(f)
  sf_poly$tile_id   <- sub(".*_L3A_T(.*)_C.*", "\\1", sf_poly$tile_file)
  
  sf_poly
})

tiles_sf <- do.call(rbind, bbox_list)

tiles_sf$tile_id <- factor(tiles_sf$tile_id, levels = tile_vec)
tiles_sf <- tiles_sf[order(tiles_sf$tile_id), ]

target_crs <- st_crs(tiles_sf)

nuts1_de <- giscoR::gisco_get_nuts(year = "2021", epsg = 4326, nuts_level = 1, 
                                   resolution = "01", country = "DE")

nuts2_de <- giscoR::gisco_get_nuts(year = "2021", epsg = 4326, nuts_level = 3, 
                                   resolution = "01", country = "DE")
nuts2_centroids <- sf::st_centroid(nuts2_de)
# nuts1_de <- st_transform(nuts1_de, target_crs)
# nuts2_de <- st_transform(nuts2_de, target_crs)


ggplot() +
  geom_sf(data = nuts1_de, fill = NA, color = "blue", linewidth = 1) +
  geom_sf(data = nuts2_de, fill = NA, color = "black", linewidth = 0.4) +
  geom_sf(data = tiles_sf, aes(color = tile_id, fill = tile_id), linewidth = 0.8, alpha = 0.3 ) +
  coord_sf(xlim = c(8, 15), ylim = c(46.5, 51), expand = FALSE) +
  theme_minimal() +
  labs(title = "14 tiles covering bavaria", x = "Lon", y = "Lat") +
  # --- labels for NUTS level 3 ---
  geom_sf_text(
    data = nuts2_centroids,
    aes(label = NUTS_ID),   # or NUTS_NAME, depending on the column you want
    size = 2.5,
    check_overlap = TRUE
  ) +
  coord_sf(xlim = c(8, 15), ylim = c(46.5, 51), expand = FALSE) +
  theme_minimal() +
  labs(
    title = "14 tiles covering Bavaria",
    x = "Lon",
    y = "Lat"
  )


