

set.seed(42)

n <- 182

train_NUTS3_lt40_GOSIF <- tibble::tibble(
  Winterweizen        = rnorm(n, mean = 7.5, sd = 0.8),   # yield (t/ha)
  
  mean_sif_Mar        = runif(n, 0.2, 1.5),
  mean_sif_Apr        = runif(n, 0.3, 2.0),
  mean_sif_May        = runif(n, 0.4, 2.5),
  mean_sif_Jun        = runif(n, 0.4, 2.8),

  mean_c3_share_Mar   = runif(n, 0.2, 0.8),
  mean_c3_share_Apr   = runif(n, 0.3, 0.9),
  mean_c3_share_May   = runif(n, 0.4, 1.0),
  mean_c3_share_Jun   = runif(n, 0.3, 1.0),
  
  mean_nirv_Mar       = runif(n, 0.4, 2.8),
  mean_nirv_Apr        = runif(n, 0.4, 2.8),
  mean_nirv_May        = runif(n, 0.4, 2.8),
  mean_nirv_Jun        = runif(n, 0.4, 2.8),  

  #mean_vpd_Mar        = runif(n, 0.3, 1.2),
  #mean_vpd_Apr        = runif(n, 0.4, 1.6),
  #mean_vpd_May        = runif(n, 0.6, 2.2),
  #mean_vpd_Jun        = runif(n, 0.8, 2.8),
)

rmse = c('rmse : 0.766')


dim(train_NUTS3_lt40_GOSIF)
colnames(train_NUTS3_lt40_GOSIF)
rmse


# --- Packages ---
# install.packages(c("giscoR", "sf", "dplyr", "units"))
library(giscoR)
library(sf)
library(dplyr)
library(units)

# --- 1) Download NUTS3 for Germany and keep Bavaria (NUTS1 = DE2) ---
# Year and resolution can be changed; 2016 is commonly available in GISCO
nuts3_bavaria <- gisco_get_nuts(
  year = 2021,
  resolution = "10",
  nuts_level = 3,
  country = "DE"
) %>%
  filter(NUTS_ID %in% c("DE2")) # keep Bavaria (NUTS1 = DE2)

# NOTE: Some GISCO versions include NUTS1 in a different field; the robust approach is:
# nuts3_bavaria <- gisco_get_nuts(year=2016, resolution="20", nuts_level=3, country="DE") %>%
#   filter(substr(NUTS_ID, 1, 3) == "DE2")  # NUTS3 within Bavaria all start with DE2

# Use robust filter (overwrite):
nuts3_bavaria <- gisco_get_nuts(
  year = 2021,
  resolution = "20",
  nuts_level = 3,
  country = "DE"
) %>%
  filter(substr(NUTS_ID, 1, 3) == "DE2")

# --- 2) Calculate area (km^2) using an equal-area CRS ---
nuts3_bavaria_area <- nuts3_bavaria %>%
  st_transform(3035) %>%                 # ETRS89 / LAEA Europe (equal-area)
  mutate(
    area_m2  = st_area(geometry),
    area_km2 = set_units(area_m2, km^2) %>% drop_units()
  )

# --- 3) Summary stats: average + range ---
area_summary <- nuts3_bavaria_area %>%
  st_drop_geometry() %>%
  summarise(
    n_regions   = n(),
    mean_km2    = mean(area_km2, na.rm = TRUE),
    min_km2     = min(area_km2, na.rm = TRUE),
    max_km2     = max(area_km2, na.rm = TRUE),
    range_km2   = max_km2 - min_km2
  )

print(area_summary)

# (Optional) inspect biggest/smallest regions
nuts3_bavaria_area %>%
  st_drop_geometry() %>%
  select(NUTS_ID, NAME_LATN, area_km2) %>%
  arrange(area_km2) %>%
  print(n = 10)


