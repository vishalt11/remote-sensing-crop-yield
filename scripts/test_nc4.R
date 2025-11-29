# https://excalidraw.com/#json=7ZHLSFM96vFcDuIt015t8,Vab-cWU0LlP5LirkOegdHA
library(ncdf4) 
library(raster)
library(sf) 
library(ggplot2) 
library(rnaturalearth)


nc_data <- nc_open('../data/oco2SIF10r_2021.03.01_2021.03.31/oco2_LtSIF_210321_B10206r_210917171735s.SUB.nc4')

attributes(nc_data$dim)$names

nc_data$dim$footprint_dim
nc_data$dim$sounding_dim
nc_data$dim$vertex_dim

attributes(nc_data$var)$names


lon <- ncvar_get(nc_data, "Longitude")
lat <- ncvar_get(nc_data, "Latitude")

#sif_740nm <- ncvar_get(nc_data, "SIF_740nm")
sif_740nm_daily <- ncvar_get(nc_data, "Daily_SIF_740nm")

dtime <- ncvar_get(nc_data, "Delta_Time")
dtime <- as.POSIXct(dtime, origin = "1990-01-01", tz = "UTC")

unique(dtime)

#sum(is.na(sif_740nm))

#-------------------------------------------------------------------------------
#that tells us:
#The first dimension (4) = number of vertices (corner points) per footprint polygon.
#The second dimension (122 546) = number of soundings (your sounding_dim).
#So in this specific OCO-2 Lite SIF v10r product:
#each of those 122 546 soundings has one footprint (not eight, as in the raw Level 2 version),
#and each footprint is defined by 4 corner coordinates (lat/lon pairs).
#This is because the Lite SIF product is already aggregated to one footprint per sounding — 
#NASA simplified it by flattening the across-track dimension.


lon_corner <- ncvar_get(nc_data, "Longitude_Corners")
lat_corner <- ncvar_get(nc_data, "Latitude_Corners")

dim(lon_corner)

n_soundings <- dim(lon_corner)[2]
areas_km2 <- numeric(n_soundings)

for (i in seq_len(n_soundings)) {
  lon_poly <- c(lon_corner[, i], lon_corner[1, i])
  lat_poly <- c(lat_corner[, i], lat_corner[1, i])
  
  # skip if any missing or invalid coordinates
  if (any(is.na(lon_poly)) || any(is.na(lat_poly))) {
    areas_km2[i] <- NA
    next
  }
  
  #  create polygon compute area
  poly_try <- try({
    fp_poly <- st_polygon(list(cbind(lon_poly, lat_poly))) |> st_sfc(crs = 4326)
    as.numeric(st_area(fp_poly)) / 1e6  # km²
  }, silent = TRUE)
  
  # polygon creation fails
  if (inherits(poly_try, "try-error")) {
    areas_km2[i] <- NA
  } else {
    areas_km2[i] <- poly_try
  }
}

summary(areas_km2)

area_df <- data.frame(
  lon = lon,
  lat = lat,
  areas_km2 = areas_km2,
  dtime = dtime
)



#-------------------------------------------------------------------------------
# run this in a loop 1-122546
i <- 500
lon_poly <- c(lon_corner[, i], lon_corner[1, i])  # close polygon
lat_poly <- c(lat_corner[, i], lat_corner[1, i])

fp_poly <- st_polygon(list(cbind(lon_poly, lat_poly))) |> st_sfc(crs = 4326)
# true footprint area
as.numeric(st_area(fp_poly)) / 1e6

# world basemap
world <- ne_countries(scale = "medium", returnclass = "sf")

# zoom to viewing box
bbox_sector <- c(
  xmin = -120,  # 120 W
  xmax = -60,   # 60 W
  ymin = -90,   # 90 S
  ymax = -75    # 75 S
)

ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray70") +
  geom_sf(data = fp_poly, fill = "skyblue", color = "blue", size = 1) +
  coord_sf(xlim = c(bbox_sector["xmin"], bbox_sector["xmax"]), ylim = c(bbox_sector["ymin"], bbox_sector["ymax"]), expand = FALSE) +
  theme_minimal() +
  labs(title = "OCO-2 SIF Footprint of 4.48 km^2— Antarctica (120°W to 60°W) (76°S to 90°S)",x = "Longitude",y = "Latitude")


#-------------------------------------------------------------------------------

sif_df <- data.frame(
  lon = lon,
  lat = lat,
  #sif = sif_740nm,
  sif_daily = sif_740nm_daily,
  dtime = dtime
)

range(sif_df$dtime)

head(sif_df)
#lat ≈ –90 to –60 → Antarctica
#lat ≈ –60 to 0 → Southern Hemisphere (South America, Africa, Australia)
#lat ≈ 0 to 60 → Tropics / mid-latitudes
#lat ≈ >60 → Northern Hemisphere (Canada, Russia, etc.)
range(sif_df$lat)
range(sif_df$lon)

sif_sf <- st_as_sf(sif_df, coords = c("lon", "lat"), crs = 4326)

ggplot() +
  borders("world", colour = "gray40", fill = "gray90") +
  geom_point(data = sif_df, aes(x = lon, y = lat), color = "red", size = 0.5) +
  coord_quickmap() +
  theme_minimal()

ggplot() +
  borders("world", colour = "gray40", fill = "gray90") +
  geom_point(data = sif_df, aes(x = lon, y = lat, color=sif_daily), , size = 0.2) +
  scale_color_viridis_c(option = "plasma", na.value = "transparent") +
  coord_quickmap(xlim = c(5, 16), ylim = c(47, 55), expand = FALSE) +
  theme_minimal()

