# https://excalidraw.com/#json=7ZHLSFM96vFcDuIt015t8,Vab-cWU0LlP5LirkOegdHA
library(ncdf4) 
library(readr)

library(terra)
library(sf) 
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet)
library(ggrepel)
library(viridis)
library(giscoR)

library(tidyverse)


#-------------------------------------------------------------------------------
# 1 file test
nc_data <- nc_open('../data/oco2SIF10r_2022.01.01_2024.03.31/oco2_LtSIF_211231_B11012Ar_220627183447s.nc4')

attributes(nc_data$var)$names

lon_corners <- ncvar_get(nc_data,"Longitude_Corners")

#-------------------------------------------------------------------------------
# bulk load
files <- list.files('../data/oco2SIF10r_2015.01.01_2021.12.31/', pattern = "\\.nc4$", full.names = TRUE)

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

lon_corners <- ncvar_get(nc_list$a663,"Longitude_Corners")
lon <- ncvar_get(nc_list$a663,"Longitude")

#-------------------------------------------------------------------------------
# Variables of interest
vars_to_extract <- c("Daily_SIF_740nm", "Delta_Time", "Latitude", "Longitude", "Latitude_Corners",
                     "Longitude_Corners", "Quality_Flag", "Meteo/specific_humidity", "Meteo/surface_pressure",
                     "Meteo/temperature_skin", "Meteo/temperature_two_meter", "Meteo/vapor_pressure_deficit")

df_list <- list()

for (i in seq_along(nc_list)) {
  nc <- nc_list[[i]]
  short_name <- names(nc_list)[i]
  cat("Processing:", short_name, "\n")
  
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
head(combined_df,1)

combined_df$Delta_Time <- as.POSIXct(combined_df$Delta_Time, origin = "1990-01-01", tz = "UTC")
combined_df$Delta_date <- format(combined_df$Delta_Time, "%Y-%m-%d")
combined_df$Delta_date <- as.factor(combined_df$Delta_date)
#unique(format(combined_df$Delta_Time, "%Y-%m-%d"))

#write.csv(combined_df, file = '../data/SIF_v1.csv', row.names=FALSE)
base::saveRDS(combined_df, file = '../data/SIF_v1.rds')

#-------------------------------------------------------------------------------

colnames(combined_df)

plot_data <- combined_df[combined_df$Delta_Time >= as.Date('2020-02-01') & combined_df$Delta_Time <= as.Date('2020-05-31'),]

unique(plot_data$Delta_date)

germany <- ne_states(country = "Germany", returnclass = "sf")
bavaria <- germany[germany$name_en == "Bavaria",]

plot_sf <- st_as_sf(plot_data, coords = c("Longitude", "Latitude"), crs = 4326)
# crop the sf to include bavaria only 
plot_bavaria <- plot_sf[bavaria,]

# merge to include back the lat, lon from main df
#coords <- st_coordinates(plot_bavaria)
#plot_bavaria_df <- cbind(st_drop_geometry(plot_bavaria), coords)

# plot for dataframe
# ggplot() +
#   annotation_borders("world", colour = "gray40", fill = "gray90") +
#   geom_point(data = plot_bavaria_df, aes(x = Longitude, y = Latitude, color=Delta_date), , size = 0.5) +
#   scale_color_viridis_d(option = "plasma", na.value = "transparent") +
#   #scale_color_viridis_c(option = "plasma", na.value = "transparent") +
#   coord_quickmap(xlim = c(5, 16), ylim = c(47, 55), expand = FALSE) +
#   #coord_quickmap(xlim = c(11, 12.5), ylim = c(48, 49), expand = FALSE) +
#   theme_minimal()

# convert sf to df
coords <- st_coordinates(plot_bavaria)
plot_bavaria_df <- cbind(st_drop_geometry(plot_bavaria), coords)

# Group and compute mean coordinates per track (file_id)
# this is for gg_text positioning on the map
label_data <- plot_bavaria_df %>%
  group_by(file_id, Delta_date) %>%
  summarise(
    Longitude = mean(X, na.rm = TRUE),
    Latitude = mean(Y, na.rm = TRUE),
    .groups = "drop"
  )

# Convert to sf object for plotting
label_sf <- st_as_sf(label_data, coords = c("Longitude", "Latitude"), crs = 4326)

label_sf$Delta_date <- format(as.Date(label_sf$Delta_date), "%b-%d")

world <- ne_countries(scale = "large", returnclass = "sf")

# Bund Germany
# Land Bayern
# Regierungsbezirk OberBayern
# Landkreis (Lkr) Freising
# Kreisfreie Stadt(Krfr.St) Ingolstadt

# EU NUTS level:2 - Regierungsbezirke
nuts2 <- gisco_get_nuts(year = 2024, resolution = "3", epsg = 4326, nuts_level = 2)

# filter Bavaria
bavaria_rb <- nuts2 |> filter(CNTR_CODE == "DE" & grepl("^DE2[1-7]$", NUTS_ID))

# plot for sf object
ggplot() +
  geom_sf(data = world, colour = "gray40", fill = "gray90") +
  geom_sf(data = bavaria, fill = NA, color = "dodgerblue3", linewidth = 0.8) +
  #geom_sf(data = plot_bavaria, aes(color = Delta_date), size = 0.5) +
  geom_sf(data = plot_bavaria, aes(color = Daily_SIF_740nm), size = 0.5) +
  geom_sf(data = bavaria_rb, fill = NA, color = "dodgerblue3", linewidth = 0.5) +
  #scale_color_viridis_d(option = "plasma", na.value = "transparent") +
  scale_color_viridis_c(option = "plasma", na.value = "transparent") +
  #coord_sf(xlim = c(5, 16), ylim = c(47, 55), expand = FALSE) +
  coord_sf(xlim = c(8, 14.5), ylim = c(47, 51), expand = FALSE) +
  geom_text_repel(data = label_sf, aes(label = Delta_date, geometry = geometry), 
                  stat = "sf_coordinates", size = 5, color = "black", segment.color = "gray50",
                  segment.size = 1, force = 5, max.overlaps = Inf, min.segment.length = 0) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray80", linetype = "dotted")) +
  #guides(color = guide_legend(override.aes = list(size = 3))) +
  labs(title = "Satellite tracks over Bayern - 2020 Feb-May",x = "Longitude", y = "Latitude", color = "SIF_740nm")


#-------------------------------------------------------------------------------
# Leaflet plot: Visualize the blocks in given kml selection
# add the crop type raster too

test_leaflet <- st_read("../data/upper_bavaria.kml")

st_crs(test_leaflet)
st_crs(plot_bavaria)

test_leaflet <- st_transform(test_leaflet, crs = st_crs(plot_bavaria))
plot_kml <- st_intersection(plot_bavaria, test_leaflet)

#-------------------------------------------------------------------------------
# plot polygons (formed by 4 corners lat/lon corners) & centroids (lat, lon)

# --- 1 Polygon creator function ---
make_poly <- function(lon1, lon2, lon3, lon4, lat1, lat2, lat3, lat4) {
  coords <- matrix(
    c(lon1, lat1,
      lon2, lat2,
      lon3, lat3,
      lon4, lat4,
      lon1, lat1),   # close polygon
    ncol = 2,
    byrow = TRUE
  )
  st_polygon(list(coords))
}

# --- 2 Build sf polygons (corrected) ---
polys_sf <- plot_kml |>
  st_drop_geometry() |>
  mutate(geometry = st_sfc(
    purrr::pmap(
      list(Lon_corner1, Lon_corner2, Lon_corner3, Lon_corner4,
           Lat_corner1, Lat_corner2, Lat_corner3, Lat_corner4),
      make_poly
    ),
    crs = st_crs(plot_kml)
  )) |>
  st_as_sf()

st_crs(polys_sf)

# Difference between 4326 vs 3035
#st_area() automatically computes polygon area based on the CRS.
#If your CRS is geographic (EPSG:4326) → it returns planar areas that aren’t meaningful (in degrees²).
#So you should transform to an equal-area projection first.
#For Germany, use EPSG:3035 (ETRS89 / LAEA Europe) or EPSG:32632 (UTM Zone 32N).

#polys_sf$area_m2 <- st_area(polys_sf)

#CRS info confirms your polys_sf polygons are in geographic coordinates (latitude/longitude, EPSG:4326).

#That’s fine for mapping, but not for computing area — because degrees aren’t uniform distances (1° ≠ same length everywhere).
#So before computing area, you should reproject to an equal-area CRS.

#For Europe / Germany, the most common and accurate choices are:
  
#  EPSG:3035 → ETRS89 / LAEA Europe (equal-area projection for Europe)

#EPSG:32632 → WGS84 / UTM zone 32N (good for Central Europe)

#!!!!!!!!!!!!!!!!!!!!!!!!!!
# IMPORTANT to check
#!!!!!!!!!!!!!!!!!!!!!!!!!!
#Task	                    Recommended CRS
#Compute area / distance	Projected (e.g., EPSG:3035 or 32632)
#Spatial joins / buffers	Projected
#Plot with ggplot2	      Either (3035 or 4326)
#Plot with leaflet	      Always EPSG:4326

polys_sf <- polys_sf |>
  st_transform(3035) |>                        # reproject to equal-area CRS
  mutate(area_km2 = as.numeric(st_area(polys_sf) / 1e6))  # convert m² → km²

# ggplot(polys_sf) +
#   geom_sf(aes(fill = Daily_SIF_740nm), color = "gray20", linewidth = 0.1) +
#   scale_fill_viridis_c(option = "plasma") +
#   coord_sf(crs = st_crs(3035)) +
#   theme_minimal() +
#   labs(title = "SIF Polygons in (EPSG:3035)")

polys_sf <- st_transform(polys_sf, 4326)

# --- 3 Define palette and plot ---
pal <- colorNumeric(
  palette = viridis(256, option = "plasma"),
  domain = polys_sf$Daily_SIF_740nm,
  na.color = "transparent"
)

leaflet(polys_sf) |>
  addProviderTiles("Esri.WorldImagery") |>
  addPolygons(
    fillColor = ~pal(Daily_SIF_740nm),
    color = "black",
    weight = 0.5,
    opacity = 0.8,
    fillOpacity = 0.6,
    popup = ~paste0(
      "<b>Date:</b> ", Delta_date, "<br>",
      "<b>SIF_740nm:</b> ", round(Daily_SIF_740nm, 4), "<br>",
      "<b>File ID:</b> ", file_id
    )
  ) |>
  addCircleMarkers(
    data = plot_kml,
    radius = 5,
    stroke = FALSE,
    fillOpacity = 0.7,
    color = ~pal(Daily_SIF_740nm),
    popup = ~paste0(
      "<b>Date:</b> ", Delta_date, "<br>",
      "<b>SIF_740nm:</b> ", round(Daily_SIF_740nm, 4), "<br>",
      "<b>File ID:</b> ", file_id
    )
  ) |>
  addLegend(
    pal = pal,
    values = ~Daily_SIF_740nm,
    title = "SIF 740nm",
    position = "bottomright"
  )

#-------------------------------------------------------------------------------
# project the raster on to kml selection: leaflet visualization

r <- rast("../data/crop_types_DE_2020.tif")

crop_classes <- read_delim("../data/LEGEND_CropTypes.txt", delim = "\t")
colnames(crop_classes) <- c("code", "label")

levels(r)
# attach class table
levels(r) <- data.frame(value = crop_classes$code, crop = crop_classes$label)

n_classes <- nrow(crop_classes)
#palette_crop <- viridis::viridis(n_classes, option = "D")

#palette_crop <- c(RColorBrewer::brewer.pal(12, "Paired"), RColorBrewer::brewer.pal(8, "Set3"))

palette_crop <- c("#FFFFFF","#0070FF","#00BFFF","#87CEFA","#ADD8E6","#F4A460","#FFA500","#FF8C00","#FF00FF","#D2B48C",
                  "#8B4513","#8400A8","#FFFF99","#D1FF73","#89CD66","#4E7500","#FFBEBE","#FF7F50","#004C70","#CCCCCC")

palette_crop <- palette_crop[1:n_classes]

st_crs(plot_kml)$epsg
st_crs(r)

# resource intensive instead convert kml to raster's crs (utm)
# then crop and reconvert to 4326/wgs84
#r_wgs84 <- project(r, "EPSG:4326")

test_leaflet <- st_read("../data/upper_bavaria.kml")
st_crs(test_leaflet)
st_geometry_type(test_leaflet)

test_leaflet_utm <- st_transform(test_leaflet, crs = st_crs(r))
st_crs(test_leaflet_utm)

r_crop_utm <- terra::crop(r, terra::vect(test_leaflet_utm))
r_crop_utm <- terra::mask(r_crop_utm, terra::vect(test_leaflet_utm))

# get back in wgs 84 format for visualization
r_crop <- project(r_crop_utm, "EPSG:4326")

# make the kml plottable on leaflet
test_leaflet <- st_make_valid(test_leaflet)
test_leaflet <- st_transform(test_leaflet, 4326)

pal_crop <- colorFactor(
  palette = palette_crop,
  domain  = crop_classes$code
)

leaflet(polys_sf) |>
  addProviderTiles("Esri.WorldImagery") |>
  addPolygons(
    fillColor = ~pal(Daily_SIF_740nm),
    color = "black",
    weight = 0.5,
    opacity = 0.8,
    fillOpacity = 0.6,
    popup = ~paste0(
      "<b>Date:</b> ", Delta_date, "<br>",
      "<b>SIF_740nm:</b> ", round(Daily_SIF_740nm, 4), "<br>",
      "<b>File ID:</b> ", file_id
    )
  ) |>
  addCircleMarkers(
    data = plot_kml,
    radius = 5,
    stroke = FALSE,
    fillOpacity = 0.4,
    color = ~pal(Daily_SIF_740nm),
    popup = ~paste0(
      "<b>Date:</b> ", Delta_date, "<br>",
      "<b>SIF_740nm:</b> ", round(Daily_SIF_740nm, 4), "<br>",
      "<b>File ID:</b> ", file_id)
  ) |>
  addRasterImage(r_crop, colors = pal_crop, opacity = 0.8, project = TRUE) |>
  addPolygons(
    data = test_leaflet,
    fill = NA,
    color = "red",
    weight = 2,
    opacity = 0.9,
    dashArray = "4"
  ) |>
  addLegend(
    pal = pal,
    values = ~Daily_SIF_740nm,
    title = "SIF 740nm",
    position = "bottomright"
  ) |>
  addLegend(
    pal = pal_crop,
    values = crop_classes$code,
    labels = crop_classes$label,
    #labels = crop_classes$label[match(crop_classes$code, crop_classes$code)],
    title = "Crop types",
    position = "bottomleft"
  )

#-------------------------------------------------------------------------------

# polys_sf
# r_crop

# Task 1: calculate area of each crop class in raster/kml selection

r_crop_utm <- project(r_crop, "EPSG:32632")
cell_areas <- cellSize(r_crop_utm, unit = "m")
crop_areas <- as.data.frame(zonal(cell_areas, r_crop_utm, fun = "sum", na.rm = TRUE))
colnames(crop_areas) <- c("code", "area_m2")
crop_areas$area_km2 <- round(crop_areas$area_m2 / 1e6, 3)

test_leaflet_utm <- st_transform(test_leaflet, st_crs(r_crop_utm))
total_area_m2 <- as.numeric(st_area(test_leaflet_utm))
crop_areas <- crop_areas |> mutate(area_pct = round(100 * area_m2 / total_area_m2, 3))

#-------------------------------------------------------------------------------
# Task 2: for each SIF polygon, find the composition of crop classes by area

head(polys_sf, 1)

# Reproject raster to UTM (meter units) for area calculation
r_crop_utm <- project(r_crop, "EPSG:32632", method = "near")

# Make a second raster with cell areas
cell_area <- cellSize(r_crop_utm, unit = "m")

# Extract *both* crop and cell area in one go
crop_extract <- terra::extract(c(r_crop_utm, cell_area), vect(st_transform(polys_sf, 32632)), exact = TRUE)

# convert NAs to no data
crop_extract[is.na(crop_extract$crop),]$crop <- 'no_data'

summary(crop_extract)

crop_summary <- crop_extract %>%
  filter(!is.na(crop)) %>%
  mutate(area_m2 = area * fraction) %>%     # correct column name
  group_by(ID, crop) %>%
  summarise(area_m2 = sum(area_m2, na.rm = TRUE), .groups = "drop") %>%
  group_by(ID) %>%
  mutate(
    total_area_m2 = sum(area_m2, na.rm = TRUE),
    area_pct = 100 * area_m2 / total_area_m2
  )

crop_nested <- crop_summary %>%
  group_by(ID) %>%
  arrange(desc(area_pct)) %>%
  nest(crop_stats = c(crop, area_m2, area_pct))

polys_with_crop <- polys_sf %>%
  mutate(ID = 1:n()) %>%
  left_join(crop_nested, by = "ID")



# plot static combination
ccolors <- c("#0070FF","#00BFFF","#87CEFA","#ADD8E6","#F4A460","#FFA500","#FF8C00","#FF00FF","#D2B48C",
                  "#8B4513","#8400A8","#FFFF99","#D1FF73","#89CD66","#4E7500","#FFBEBE","#FF7F50","#004C70","#CCCCCC")
plot(r_crop, col = ccolors, legend = TRUE, axes = FALSE, box = FALSE)
plot(st_geometry(polys_with_crop), add = TRUE, border = "red", lwd = 2)
centroids <- st_centroid(polys_with_crop)
text(st_coordinates(centroids), labels = seq_len(nrow(polys_with_crop)), cex = 0.7, col = "blue")

polys_with_crop$area_m2 <- as.numeric(st_area(st_transform(polys_with_crop, 32632)))

# reason for not matching exactly? pixels of 10mx10m resolution, if they were finer, we would get even closer match
# NA as "no_data" those areas were still counted in total_area_m2 (since they have valid overlap fractions).
# Depending on how the polygon crosses the raster extent, a few boundary cells might be partially outside the raster or fall on NA cells, 
# trimming the summed area a bit.
polys_with_crop %>% select(area_m2, total_area_m2) %>% print(n=2)

# sanity check if the composition sums to 100% percent
crop_summary %>%
  group_by(ID) %>%
  summarise(sum_pct = sum(area_pct))

#-------------------------------------------------------------------------------
# Task 3: Choose SIf polygons that have at least 60%+ crop coverage

polys_with_crop <- polys_with_crop %>%
  mutate(
    crop_covered_pct = map_dbl(
      crop_stats, # each value in the column is a dataframe
      ~ {
        df <- .x # one polygon's crop_stats / one element
        if (is.null(df) || nrow(df) == 0) return(NA_real_) # handle empty mini dataframes
        df %>%
          filter(crop != "no_data") %>% summarise(sum_pct = sum(area_pct, na.rm = TRUE)) %>% pull(sum_pct)
      }
    )
  )

# choose polygons with more than 10% any crop coverage
nrow(polys_with_crop[polys_with_crop$crop_covered_pct > 10,])

#-------------------------------------------------------------------------------
# Task 4: check how many polygons in bavaria have 50% winter wheat (color code 11)
# select only these polygons for SIF modelling
polys_with_crop <- polys_with_crop %>%
  mutate(
    winter_wheat_covered_pct = map_dbl(
      crop_stats,
      ~ {
        df <- .x
        if (is.null(df) || nrow(df) == 0) return(NA_real_)
        df %>% filter(crop == "winter_wheat") %>% summarise(sum_pct = sum(area_pct, na.rm = TRUE)) %>% pull(sum_pct)
      }
    )
  )














