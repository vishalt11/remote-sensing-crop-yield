library(tidyverse)
library(sf)
library(lubridate)
library(terra)


library(rcartocolor)
library(leaflet)


result_all <- readRDS(file = '../data/oco2_sif.rds')



#result_all$area <-  as.numeric(st_area(result_all))

crs0 <- st_crs(result_all)
result_all$area_m2 <- vapply(
  seq_len(nrow(result_all)),
  function(i) {
    message("i = ", i)
    g <- st_sfc(st_geometry(result_all)[[i]], crs = crs0)
    tryCatch(as.numeric(st_area(g)), error = function(e) NA_real_)
  },
  numeric(1)
)

result_all$area_km2 <- result_all$area_m2/1e6

saveRDS(result_all, '../data/oco2_sif_areacorrected.rds')

nuts1_de <- giscoR::gisco_get_nuts(year = "2024", epsg = 4326, nuts_level = 1, resolution = "01", country = "DE")
nuts3_de <- giscoR::gisco_get_nuts(year = "2024", epsg = 4326, nuts_level = 3, resolution = "01", country = "DE")
nuts3_de <- nuts3_de |> filter(startsWith(NUTS_ID, "DE2"))

saveRDS(nuts1_de, '../data/nuts1_de.rds')
saveRDS(nuts3_de, '../data/nuts3_de.rds')

#-------------------------------------------------------------------------------
result_all <- readRDS('../data/oco2_sif_areacorrected.rds')

#result_all$year <- year(result_all$Delta_Time)
#result_all$month <- month(result_all$Delta_Time)
#unique(result_all[result_all$year == 2024 & result_all$Metadata.MeasurementMode %in% c(0,1),]$month)


nuts1_de <- readRDS('../data/nuts1_de.rds')
nuts3_de <- readRDS('../data/nuts3_de.rds')


bad_polys <- result_all[is.na(result_all$area_km2),]

plot(bad_polys[4,], max.plot=1)




#-------------------------------------------------------------------------------
#Verify area of a single polygon is correct compared to external source

nuts2_all <- giscoR::gisco_get_nuts(year = "2021", resolution = "03", nuts_level = 2, epsg = 4326)
bavaria_nuts2 <- subset(nuts2_all, grepl("^DE2", NUTS_ID))

leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
  addProviderTiles(providers$CartoDB.VoyagerOnlyLabels, group = "Labels", 
                   options = providerTileOptions(opacity = 0.9)) |>
  addPolygons(
    data = result_all[250,],
    color = "red",
    weight = 2,
    fill = "red",
    group = "Polygons",
    smoothFactor = 0.5,
    fillOpacity = 0.8,
    popup = ~paste0(
      "<b>NUTS:</b> ", NUTS_NAME, "<br/>",
      "<b>Date:</b> ", Delta_date, "<br/>",
      "<b>file_id:</b> ", file_id)) |>
  addPolygons(data = bavaria_nuts2, fill = FALSE, weight = 2, opacity = 1, 
              color = '#ffd60a', group = "bavaria_nuts2") 




colSums(is.na(result_all))

#-------------------------------------------------------------------------------
# Measurement Mode Analysis
mode0 <- result_all %>% filter(Metadata.MeasurementMode %in% c(0,1))


mode0 %>%
  mutate(mode = factor(
    Metadata.MeasurementMode,
    levels = c(0, 1),
    labels = c("Nadir (0)", "Glint (1)")
  )) %>%
  ggplot(aes(x = Daily_SIF_740nm, fill = mode, color = mode)) +
  geom_density(alpha = 0.4, linewidth = 1) +
  theme_bw() +
  labs(
    x = "Daily SIF (740 nm)",
    y = "Density",
    fill = "Measurement mode",
    color = "Measurement mode"
  )


mean(mode0$area_km2, na.rm=TRUE)
range(mode0$area_km2, na.rm = TRUE)
median(mode0$area_km2, na.rm=TRUE)


mode0 %>%
  ggplot(aes(x=area_km2)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  theme_bw()

plot(st_geometry(mode0[1:200, ]), col = "lightblue", border = "black", reset = FALSE)


#-------------------------------------------------------------------------------
mode0 <- mode0 |>
  mutate(
    center_lat = (Lat_corner1 + Lat_corner2 + Lat_corner3 + Lat_corner4) / 4,
    center_lon = (Lon_corner1 + Lon_corner2 + Lon_corner3 + Lon_corner4) / 4
  )

mode0$month <- lubridate::month(mode0$Delta_Time)
mode0$year <- lubridate::year(mode0$Delta_Time)

#high_ww <- mode0[mode0$wheat_share > 0.25,]

ggplot() +
  geom_sf(data = nuts1_de, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = nuts3_de, fill = NA, color = "navyblue", linewidth = 0.4) +
  geom_point(data = mode0 |> filter(year == 2022), 
             aes(x = center_lon, y = center_lat, color = factor(month)),size = 0.3) +
  theme_bw() +
  #scale_color_brewer(name = "Mode",palette = "Set1") +
  scale_color_carto_d(name = "Mode", palette = "Bold") +
  #coord_sf(xlim = c(10, 13), ylim = c(47.2, 49.5), expand = FALSE) +
  coord_sf(xlim = c(8.8, 14), ylim = c(47, 50.7), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude") +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  theme(legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10),
        axis.ticks = element_blank())


#-------------------------------------------------------------------------------

result_all <- result_all %>% filter(Metadata.MeasurementMode %in% c(0,1))
result_all <- result_all |>
  mutate(
    center_lat = (Lat_corner1 + Lat_corner2 + Lat_corner3 + Lat_corner4) / 4,
    center_lon = (Lon_corner1 + Lon_corner2 + Lon_corner3 + Lon_corner4) / 4
  )

# make a point sf from centers (keep row order via row_id)
centers <- st_as_sf(
  result_all |> st_drop_geometry() |> mutate(row_id = row_number()),
  coords = c("center_lon", "center_lat"),
  crs = 4326,
  remove = FALSE
)

# spatial join: point -> NUTS3 polygon
centers_n3 <- st_join(
  centers,
  nuts3_de |> select(NUTS_ID, NUTS_NAME),
  join = st_within,
  left = TRUE
) |> st_drop_geometry()

centers_n3 <- centers_n3 |>
  rename(
    NUTS2_NAME = NUTS_NAME.x,
    NUTS3_ID   = NUTS_ID,
    NUTS3_NAME = NUTS_NAME.y
  )



# attach back to result_all
result_all <- result_all |>
  mutate(row_id = dplyr::row_number()) |>
  left_join(centers_n3 |> select(row_id, NUTS3_ID, NUTS3_NAME), by = "row_id") |>
  select(-row_id)

result_all <- drop_na(result_all)

idx <- sapply(result_all$crop_stats, function(df) {
  if (is.null(df) || nrow(df) == 0) return(FALSE)
  any(df$code == "winter_wheat" & df$area_pct >= 0, na.rm = TRUE)
})

winterwheat_sf <- result_all[idx,]

c4_codes <- c("maize")
`%!in%` <- Negate(`%in%`)

winterwheat_sf <- winterwheat_sf |>
  mutate(
    wheat_share_rel = map_dbl(crop_stats, \(cs) {
      wheat_area_pct <- cs$area_pct[cs$code == "winter_wheat"][1]
      total_pct <- sum(cs$area_pct, na.rm = TRUE)
      wheat_area_pct / total_pct
    }),
    
    # Winter_wheat area_m2 inside crop_stats divided by THIS ROW's area_m2
    wheat_share_abs = map2_dbl(crop_stats, area_m2, \(cs, a_m2) {
      wheat_area <- cs$area_m2[cs$code == "winter_wheat"][1]
      wheat_area / a_m2
    }),
    
    c3_share = map_dbl(crop_stats, \(cs) {
      total_pct <- sum(cs$area_pct, na.rm = TRUE)
      c3_pct <- sum(cs$area_pct[cs$code %!in% c4_codes], na.rm = TRUE)
      c3_pct / total_pct
    })
  )


sif_monthly <- as.data.frame(winterwheat_sf) |>
  mutate(
    year = year(Delta_date),
    month = month(Delta_date, label = TRUE, abbr = TRUE)
  ) |>
  filter(year %in% 2017:2021, month(Delta_date) %in% 2:7) |> # march to july
  group_by(NUTS3_NAME, year, month) |>
  summarise(
    mean_sif         = mean(Daily_SIF_740nm, na.rm = TRUE),
    mean_humidity    = mean(Meteo.specific_humidity, na.rm = TRUE),
    mean_pressure    = mean(Meteo.surface_pressure, na.rm = TRUE),
    mean_temp_skin   = mean(Meteo.temperature_skin, na.rm = TRUE),
    mean_temp_2m     = mean(Meteo.temperature_two_meter, na.rm = TRUE),
    mean_vpd         = mean(Meteo.vapor_pressure_deficit, na.rm = TRUE),
    mean_c3_share    = mean(c3_share, na.rm = TRUE),
    .groups = "drop"
  )

sif_monthly_nut2 <- as.data.frame(winterwheat_sf) |>
  mutate(
    year = year(Delta_date),
    month = month(Delta_date, label = TRUE, abbr = TRUE)
  ) |>
  filter(year %in% 2017:2024, month(Delta_date) %in% 2:7) |> # march to july
  group_by(NUTS_NAME, year, month) |>
  summarise(
    mean_sif         = mean(Daily_SIF_740nm, na.rm = TRUE),
    mean_humidity    = mean(Meteo.specific_humidity, na.rm = TRUE),
    mean_pressure    = mean(Meteo.surface_pressure, na.rm = TRUE),
    mean_temp_skin   = mean(Meteo.temperature_skin, na.rm = TRUE),
    mean_temp_2m     = mean(Meteo.temperature_two_meter, na.rm = TRUE),
    mean_vpd         = mean(Meteo.vapor_pressure_deficit, na.rm = TRUE),
    mean_c3_share    = mean(c3_share, na.rm = TRUE),
    .groups = "drop"
  )


X_all <- sif_monthly |>
  mutate(month = as.character(month)) |>
  pivot_wider(
    names_from = month,
    values_from = c(
      mean_sif, mean_humidity, mean_pressure,
      mean_temp_skin, mean_temp_2m, mean_vpd, mean_c3_share
    ),
    names_glue = "{.value}_{month}"
  )

colSums(is.na(X_all))









# Check how the SIF polygons and raster overlap
#-------------------------------------------------------------------------------

wheat_1 <- winterwheat_sf[winterwheat_sf$wheat_share >= 0.9,]
wheat_1 <- wheat_1 |> filter(year(wheat_1$Delta_Time) == 2024)

ctr <- rast('../data/crop_type_tif/croptypes_2024.tif')

crop_classes <- readr::read_delim("../data/crop_type_tif/LEGEND_CropTypes.txt", delim = "\t")
colnames(crop_classes) <- c("code", "label")
levels(ctr) <- data.frame(value = crop_classes$code, crop = crop_classes$label)

# ctr: SpatRaster (EPSG:32632)
# wheat_1: sf (EPSG:4326)

wheat_utm <- st_transform(wheat_1, crs(terra::crs(ctr)))
wheat_v <- terra::vect(wheat_utm)


crop_list_4326 <- lapply(seq_len(nrow(wheat_1)), function(i) {
  v_i <- wheat_v[i]
  r_i <- terra::crop(ctr, v_i)
  r_i <- terra::mask(r_i, v_i)
  
  # reproject to WGS84 for leaflet; "near" preserves categorical classes
  r_i_ll <- terra::project(r_i, "EPSG:4326", method = "near")
  r_i_ll
})

terra::plot(crop_list_4326[[1]])

levs <- levels(ctr)[[1]]
vals   <- levs[[names(levs)[1]]]
labs   <- levs[[names(levs)[2]]]

pal <- colorFactor(palette = "viridis", domain = vals, na.color = "#00000000")

m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
  addProviderTiles(
    providers$CartoDB.VoyagerOnlyLabels,
    group = "Labels",
    options = providerTileOptions(opacity = 0.9)
  ) |>
  addPolygons(
    data = wheat_1,
    color = "red",
    weight = 2,
    fill = FALSE,
    group = "Polygons",
    smoothFactor = 0.5,
    popup = ~paste0(
      "<b>NUTS:</b> ", NUTS_NAME, "<br/>",
      "<b>Date:</b> ", Delta_date, "<br/>",
      "<b>file_id:</b> ", file_id)
  )

m

for (i in seq_along(crop_list_4326)) {
  m <- m |>
    addRasterImage(
      crop_list_4326[[i]],
      colors = pal,
      opacity = 0.65,
      group = paste0("Croptype #", i),
      project = FALSE  
    )
}

m <- m |>
  addLayersControl(
    baseGroups = c("Satellite"),
    overlayGroups = c("Polygons", paste0("Croptype #", seq_along(crop_list_4326))),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  fitBounds(lng1 = st_bbox(wheat_1)[["xmin"]], lat1 = st_bbox(wheat_1)[["ymin"]],
            lng2 = st_bbox(wheat_1)[["xmax"]],lat2 = st_bbox(wheat_1)[["ymax"]])

m

grep("Label", names(unlist(leaflet::providers)), value = TRUE)


# Check average wheat farm size in each NUTS 3, bavaria.
#-------------------------------------------------------------------------------

ctr <- rast('../data/crop_type_tif/croptypes_2024.tif')
crop_classes <- readr::read_delim("../data/crop_type_tif/LEGEND_CropTypes.txt", delim = "\t")
colnames(crop_classes) <- c("code", "label")
levels(ctr) <- data.frame(value = crop_classes$code, crop = crop_classes$label)
head(levels(ctr)[[1]])
head(nuts3_de)

nuts257 <- nuts3_de %>% 
  filter(NUTS_ID == "DE257")

nuts257 <- st_transform(nuts257, 32632)

nuts257 <- vect(nuts257)

# crop & mask raster to NUTS3
ctr_257 <- crop(ctr, nuts257)
ctr_257 <- mask(ctr_257, nuts257)

#  winter wheat code
ww_code <- crop_classes %>% 
  filter(label == "winter_wheat") %>% 
  pull(code)

# Binary raster: winter wheat = 1, else NA
ww_bin <- ifel(ctr_257 == ww_code, 1, NA)
# Label connected components (farms)
# directions=8 => pixels touching by edges OR corners are considered connected
farms <- patches(ww_bin, directions = 8)

# Per-farm cell counts
freq_tbl <- as.data.frame(freq(farms)) |> select(farm_id = value, n_cells = count)

#number of farms
nrow(freq_tbl)

# Cell area (km^2) from raster resolution (ctr_257 is in meters, EPSG:32632)
cell_area_km2 <- prod(res(ctr_257)) / 1e6
freq_tbl$area_km2 <- freq_tbl$n_cells * cell_area_km2

area_summary_ha <- freq_tbl %>%
  summarise(
    n_farms = n(),
    total_area_ha  = sum(area_km2) * 100,
    mean_area_ha   = mean(area_km2) * 100,
    median_area_ha = median(area_km2) * 100,
    min_area_ha    = min(area_km2) * 100,
    max_area_ha    = max(area_km2) * 100
  )

area_summary_ha

# For all nuts3 regions
#-------------------------

ctr <- rast("../data/crop_type_tif/croptypes_2024.tif")
nuts3_de <- readRDS('../data/nuts3_de.rds')

crop_classes <- readr::read_delim("../data/crop_type_tif/LEGEND_CropTypes.txt", delim = "\t")
colnames(crop_classes) <- c("code", "label")
levels(ctr) <- data.frame(value = crop_classes$code, crop = crop_classes$label)

# winter wheat code
ww_code <- crop_classes %>%
  filter(label == "winter_wheat") %>%
  pull(code)

# --- prep NUTS3 in raster CRS once ---
nuts3_32632 <- st_transform(nuts3_de, crs(ctr))

# pixel area in hectares (EPSG:32632 => meters)
cell_area_ha <- prod(res(ctr)) / 10000

# --- per-NUTS3 function ---
calc_ww_farms_one <- function(nuts3_row_sf) {
  nuts_v <- vect(nuts3_row_sf)
  
  ctr_clip <- crop(ctr, nuts_v) |> mask(nuts_v)
  
  # winter wheat binary
  ww_bin <- ifel(ctr_clip == ww_code, 1, NA)
  
  # if no winter wheat pixels, return zeros/NA safely
  if (all(is.na(values(ww_bin)))) {
    return(tibble(
      n_farms = 0L,
      total_area_ha = 0,
      mean_area_ha = NA_real_,
      median_area_ha = NA_real_,
      min_area_ha = NA_real_,
      max_area_ha = NA_real_
    ))
  }
  
  farms <- patches(ww_bin, directions = 8)
  
  freq_tbl <- as.data.frame(freq(farms)) %>%
    transmute(farm_id = value, n_cells = count,
              area_ha = n_cells * cell_area_ha)
  
  tibble(
    n_farms = nrow(freq_tbl),
    total_area_ha  = sum(freq_tbl$area_ha),
    mean_area_ha   = mean(freq_tbl$area_ha),
    median_area_ha = median(freq_tbl$area_ha),
    min_area_ha    = min(freq_tbl$area_ha),
    max_area_ha    = max(freq_tbl$area_ha)
  )
}

# run for all NUTS3 and bind back to sf 
stats_all <- map_dfr(
  seq_len(nrow(nuts3_32632)),
  ~ calc_ww_farms_one(nuts3_32632[.x, ]),
  .progress = TRUE
)

nuts3_ww_farms <- nuts3_32632 %>%
  select(NUTS_ID, NUTS_NAME, geometry) %>%
  bind_cols(stats_all)

nuts3_ww_farms

# Parallel version
#----------------------------------------
library(purrr)
library(furrr)
library(future)

ctr_path <- "../data/crop_type_tif/croptypes_2024.tif"
nuts3_de <- readRDS('../data/nuts3_de.rds')

ctr <- rast("../data/crop_type_tif/croptypes_2024.tif")
crop_classes <- readr::read_delim("../data/crop_type_tif/LEGEND_CropTypes.txt", delim = "\t")
colnames(crop_classes) <- c("code", "label")
levels(ctr) <- data.frame(value = crop_classes$code, crop = crop_classes$label)
ww_code <- crop_classes |> dplyr::filter(label == "winter_wheat") |> dplyr::pull(code)


# prep NUTS3 in raster CRS
nuts3_32632 <- st_transform(nuts3_de, crs(rast(ctr_path)))

# pixel area
cell_area_ha <- {
  r <- rast(ctr_path)
  prod(res(r)) / 10000
}

calc_ww_farms_one_parallel <- function(nuts3_row_sf, ww_code, ctr_path, cell_area_ha, crop_classes) {
  ctr <- rast(ctr_path)
  levels(ctr) <- data.frame(value = crop_classes$code, crop = crop_classes$label)
  
  nuts_v <- vect(nuts3_row_sf)
  ctr_clip <- crop(ctr, nuts_v) |> mask(nuts_v)
  
  ww_bin <- ifel(ctr_clip == ww_code, 1, NA)
  
  if (all(is.na(values(ww_bin)))) {
    return(tibble(
      n_farms = 0L,
      total_area_ha = 0,
      mean_area_ha = NA_real_,
      median_area_ha = NA_real_,
      min_area_ha = NA_real_,
      max_area_ha = NA_real_
    ))
  }
  
  farms <- patches(ww_bin, directions = 8)
  
  freq_tbl <- as.data.frame(freq(farms)) %>%
    transmute(area_ha = count * cell_area_ha)
  
  tibble(
    n_farms = nrow(freq_tbl),
    total_area_ha  = sum(freq_tbl$area_ha),
    mean_area_ha   = mean(freq_tbl$area_ha),
    median_area_ha = median(freq_tbl$area_ha),
    min_area_ha    = min(freq_tbl$area_ha),
    max_area_ha    = max(freq_tbl$area_ha)
  )
}

# set worker numbers for parallel
#plan(multisession, workers = max(1, parallel::detectCores() - 1))
plan(multisession, workers = 10)

# parallel with progress
stats_all <- future_map_dfr(
  seq_len(nrow(nuts3_32632)),
  ~ calc_ww_farms_one_parallel(nuts3_32632[.x, ], ww_code, ctr_path, cell_area_ha, crop_classes),
  .progress = TRUE,
  .options = furrr_options(packages = c("sf", "terra", "dplyr", "readr"))
)


# bind back to sf
nuts3_ww_farms <- nuts3_32632 %>%
  select(NUTS_ID, NUTS_NAME, geometry) %>%
  bind_cols(stats_all)

nuts3_ww_farms


saveRDS(nuts3_ww_farms, '../data/nuts3_bav_farm_summary.rds')

# Find high density wheat farm clusters and focus on these regions
#-------------------------------------------------------------------------------

nuts3_farms <- readRDS('../data/nuts3_bav_farm_summary.rds')

head(nuts3_farms, 2)

library(rcartocolor)
library(patchwork)

base_map <- function(var, title) {
  ggplot(nuts3_farms) +
    geom_sf(aes(fill = .data[[var]]), color = NA) +
    coord_sf(datum = NA) +
    labs(title = title, fill = NULL) +
    scale_fill_carto_c(palette = "ag_GrnYl", direction = -1) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      legend.position = "right"
    )
}

p1 <- base_map("total_area_ha",  "Total area (ha)")
p2 <- base_map("max_area_ha",   "Max area (ha)")
p3 <- base_map("median_area_ha", "Median area (ha)")
p4 <- base_map("n_farms",        "Number of farms")

(p1 | p2) / (p3 | p4) 


# Find high density wheat farm clusters and focus on these regions
#-------------------------------------------------------------------------------

ctr <- rast('../data/crop_type_tif/croptypes_2024.tif')
crop_classes <- readr::read_delim("../data/crop_type_tif/LEGEND_CropTypes.txt", delim = "\t")
colnames(crop_classes) <- c("code", "label")
levels(ctr) <- data.frame(value = crop_classes$code, crop = crop_classes$label)

nuts1_de <- readRDS('../data/nuts1_de.rds')
nuts1_de  <- subset(nuts1_de, grepl("^DE2", NUTS_ID))
nuts3_de <- readRDS('../data/nuts3_de.rds')

bav_utm <- st_transform(nuts1_de, crs(ctr))
bav_utm <- vect(bav_utm)

ctr_bav <- crop(ctr, bav_utm)
ctr_bav <- mask(ctr_bav, bav_utm)

writeRaster(ctr_bav,"../data/crop_type_tif/croptypes_2024_bavaria.tif", overwrite = TRUE)
ctr_bav <- rast('../data/crop_type_tif/croptypes_2024_bavaria.tif')
levels(ctr_bav)


plot(ctr_bav)

ctr_bav_1km <- terra::aggregate(ctr_bav, fact = 1000, fun  = 'modal', na.rm = TRUE)
nuts3_utm <- st_transform(nuts3_de, crs(ctr))
nuts3_utm <- vect(nuts3_utm)


plot(ctr_bav_1km)
lines(nuts3_utm, col = "black", lwd = 2)

res(ctr_bav_1km)

wheat <- ctr_bav == 11
wheat_1km  <- terra::aggregate(wheat, fact = 100, fun  = 'mean', na.rm = TRUE)
high_wheat <- wheat_1km >= 0.4   #  >40% wheat

plot(wheat_1km)
plot(high_wheat)

# Where are spatially coherent, high-density wheat systems that form real farming regions?
# High Density Wheat farm cluster
#--------------------------

library(spdep)

# points at cell centers = one feature per cell (no dissolving)
pts <- as.points(wheat_1km, na.rm=TRUE) |> st_as_sf()
names(pts)[names(pts) == names(pts)[1]] <- "wheat_share"
pts$wheat_share <- as.numeric(pts$wheat_share)

# k-nearest neighbors (good for regular grids)
coords <- st_coordinates(pts)
nb <- knn2nb(knearneigh(coords, k = 8))
lw <- nb2listw(nb, style="W")

mi <- localmoran(pts$wheat_share, lw)

pts$Ii   <- mi[, "Ii"]
pts$Z    <- mi[, "Z.Ii"]
pts$pval <- mi[, "Pr(z != E(Ii))"]

pts$lisa <- "Not significant"
pts$lisa[pts$wheat_share >= 0.6 & pts$Ii > 0 & pts$pval < 0.05] <- "High–High"
pts$lisa[pts$wheat_share <  0.4 & pts$Ii > 0 & pts$pval < 0.05] <- "Low–Low"

mean(pts$Ii > 0 & pts$pval < 0.05, na.rm = TRUE)

hotspots <- pts[pts$wheat_share >= 0.6 & pts$Ii > 0 & pts$pval < 0.05,]
plot(hotspots, max.plot = 1)

#Red → high–high clusters (wheat hotspots)
#Blue → low–low clusters
#Near zero → spatially random


ggplot(pts) +
  geom_sf(aes(color = Z), size = 0.6) +
  geom_sf(data = nuts3_de, fill = NA, color = "black", linewidth = 0.3) +
  scale_color_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0
  ) +
  labs(color = "Local Moran Z") +
  theme_bw()

# With LISA High–High, you are explicitly keeping the cells that satisfy all three:
  
# High wheat density (your threshold, e.g. ≥ 60%)

# Surrounded by high wheat neighbors (positive local Moran’s I)

# Statistically significant (p-value threshold, e.g. 0.05)
# Conceptually: “This cell has a lot of wheat, and so do the cells around it — more than expected by chance.”


saveRDS(pts, '../data/local_Moran_wheatclusters.rds')



hh_pts <- pts %>%
  mutate(hh = wheat_share >= 0.6 & Ii > 0 & pval < 0.05)

nuts3_de <- st_transform(nuts3_de, st_crs(hh_pts))

pts_n3 <- st_join(
  hh_pts,
  nuts3_de[, c("NUTS_ID", "NUTS_NAME")],
  join = st_within,
  left = FALSE
)

# 3) summarize per district
nuts3_stats <- pts_n3 %>%
  st_drop_geometry() %>%
  group_by(NUTS_ID, NUTS_NAME) %>%
  summarise(
    n_cells_total = n(),
    n_cells_hh    = sum(hh, na.rm = TRUE),
    hh_share      = n_cells_hh / n_cells_total,
    hh_area_km2   = n_cells_hh * 1  # 1 km² per 1km cell
  ) %>%
  arrange(desc(hh_share))

nuts3_map <- nuts3_de %>% left_join(nuts3_stats, by = c("NUTS_ID","NUTS_NAME"))

saveRDS(nuts3_map, '../data/nuts3_local_moran_HH_summary.rds')





