library(sf)
library(rnaturalearth)
library(ggplot2)
library(dplyr)

# South-pole orthographic view
crs_string <- "+proj=ortho +lat_0=-50 +lon_0=50"

# World data
world <- ne_countries(scale = "medium", returnclass = "sf")
st_crs(world) <- 4326

india       <- world %>% filter(admin == "India")
antarctica  <- world %>% filter(admin == "Antarctica")
world_other <- world %>% filter(!admin %in% c("India", "Antarctica"))

# --- Move India in lon/lat (it will be squished, acceptable) ---
india_cent <- st_coordinates(st_centroid(india))
shift <- c(70 - india_cent[1],
           -70 - india_cent[2])
st_geometry(india) <- st_geometry(india) + shift

# Project world layers
world_other_ortho <- st_transform(world_other, crs_string)
antarctica_ortho  <- st_transform(antarctica,  crs_string)
st_crs(india) <- 4326
india_ortho <- st_transform(india, crs_string)

# Ocean circle
ocean <- st_point(c(0,0)) |>
  st_sfc(crs = crs_string) |>
  st_buffer(dist = 6371000)

# ---- Graticules ----
graticules_ll <- st_graticule(
  lon = seq(-180, 180, 30),
  lat = seq(-90, 90, 30)
)

graticules <- st_transform(graticules_ll, crs_string)

# ---- Labels ----
# Convert gridlines to labeled points near the edge
lon_labels <- data.frame(
  lon = seq(-180, 180, 30),
  lat = -20 # pick a visible latitude
)

lat_labels <- data.frame(
  lat = seq(-90, 90, 30),
  lon = 150 # pick a visible longitude
)

lon_points <- st_as_sf(lon_labels, coords = c("lon","lat"), crs = 4326) |> 
  st_transform(crs_string)
lat_points <- st_as_sf(lat_labels, coords = c("lon","lat"), crs = 4326) |> 
  st_transform(crs_string)

# ---- Plot ----
ggplot() +
  geom_sf(data = ocean, fill = "#a4dee9", color = NA) +
  
  geom_sf(data = world_other_ortho,
          fill = "#fae1b0", color = "#edc48a", linewidth = 0.3) +
  geom_sf(data = antarctica_ortho,
          fill = "#fae1b0", color = "#edc48a", linewidth = 0.3) +
  geom_sf(data = india_ortho,
          fill = "firebrick1", color = "firebrick3", linewidth = 0.4, alpha = 0.8) +
  
  # Graticule lines
  geom_sf(data = graticules,
          color = "grey30", linewidth = 0.25, alpha = 0.6) +
  
  # Labels
  geom_sf_text(data = lon_points,
               aes(label = sprintf("%d°", lon_labels$lon)),
               size = 3, color = "grey30") +
  geom_sf_text(data = lat_points,
               aes(label = sprintf("%d°", lat_labels$lat)),
               size = 3, color = "grey30") +
  
  coord_sf(crs = crs_string, clip = "on") +
  theme_void() +
  theme(panel.background = element_rect(fill = "aliceblue", colour = NA))


ggplot() +
  geom_sf(data = world, fill = "#fae1b0", color = "#edc48a", linewidth = 0.3) +
  coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ") +
  theme_bw() +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = 'dashed', linewidth = 0.5), 
        panel.background = element_rect(fill = 'aliceblue'))
