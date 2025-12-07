library(tidyverse)
library(sf)
library(mice)

result_all <- readRDS(file = '../data/model_data.rds')

idx <- sapply(result_all$crop_stats, function(df) {
  if (is.null(df) || nrow(df) == 0) return(FALSE)
  any(df$code == "winter_wheat" & df$area_pct >= 0, na.rm = TRUE)
})

winterwheat_sf <- result_all[idx,]

c4_codes <- c("maize")
`%!in%` <- Negate(`%in%`)

winterwheat_sf <- winterwheat_sf |>
  mutate(
    wheat_share = purrr::map_dbl(crop_stats, function(cs) {
      wheat_row <- cs[cs$code == "winter_wheat", ]
      wheat_pct <- wheat_row$area_pct
      total_pct <- sum(cs$area_pct)
      wheat_pct / total_pct
    }),
    
    c3_share = purrr::map_dbl(crop_stats, function(cs) {
      # total crop pct inside polygon
      total_pct <- sum(cs$area_pct)
      # sum of area_pct for all C3 crops
      c3_pct <- sum(cs$area_pct[cs$code %!in% c4_codes])
      # normalized C3 fraction
      c3_pct / total_pct
    })
  )

winterwheat_sf %>%
  ggplot(aes(x=wheat_share)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  theme_bw()

winterwheat_sf <- winterwheat_sf[winterwheat_sf$wheat_share >= 0.1,]

sif_monthly <- as.data.frame(winterwheat_sf) |>
  mutate(
    year = year(Delta_date),
    month = month(Delta_date, label = TRUE, abbr = TRUE)
  ) |>
  filter(year %in% 2017:2024, month(Delta_date) %in% 3:7) |> # march to july
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



sen2_indices <- read.csv('../data/2017_2024_nuts3_ndvi_nirv.csv')

sen2_indices <- sen2_indices |> select(year, month, index, nuts3_id, mean)

nuts2_de <- giscoR::gisco_get_nuts(year = "2021", epsg = 4326, nuts_level = 2, resolution = "01", country = "DE")
nuts2_de <- nuts2_de |> filter(startsWith(NUTS_ID, "DE2"))

sen2_indices$nuts2_id <- substr(sen2_indices$nuts3_id, 1, 4)

sen2_indices <- sen2_indices |> select(-c(nuts3_id))

sen2_indices <- sen2_indices |>
  left_join(nuts2_de |> st_drop_geometry() |> 
              select(NUTS_ID, NUTS_NAME), by = c("nuts2_id" = "NUTS_ID"))


sen2_indices <- sen2_indices |>
  group_by(year, month, index, NUTS_NAME) |>
  summarise(mean_value = mean(mean, na.rm = TRUE), .groups = "drop")
  
  
sen2_wide <- sen2_indices |>
  mutate(month = tolower(month)) |>  
  unite("index_month", index, month, sep = "_") |>  
  pivot_wider(
    names_from = index_month,
    values_from = mean_value
  ) |>
  arrange(NUTS_NAME, year)
  
  
X_all <- X_all |> left_join(sen2_wide, by = c("NUTS_NAME", "year"))  
  
  
sort(colSums(is.na(X_all)))

# List all CSV files in the folder
files <- list.files("../data/bayern_yield_formatted/", pattern = "\\.csv$", full.names = TRUE)
target_nuts <- c("Mittelfranken", "Niederbayern", "Oberbayern", "Oberfranken", "Oberpfalz", "Schwaben", "Unterfranken")

# clean column names
clean_names1 <- function(x) {
  x <- iconv(x, from = "", to = "UTF-8")  
  x <- gsub("\\s+", " ", x)              
  trimws(x)
}

Y_all <- lapply(files, function(f) {
  #f <- "../data/bayern_yield_formatted/2016.csv"
  year <- as.numeric(str_extract(basename(f), "\\d{4}"))
  y <- read.csv(f, sep = ";", fileEncoding = "latin1", stringsAsFactors = FALSE)
  
  
  y <- y |> mutate(name = clean_names1(name))
  names(y) <- make.names(names(y))
  
  y <- y |>
    filter(name %in% target_nuts) |>
    select(name, Winterweizen) |>
    mutate(year = year)
  
  return(y)
}) |>
  bind_rows()

# (German decimals use commas)
Y_all <- Y_all |> mutate(Winterweizen = as.numeric(gsub(",", ".", Winterweizen)))

XY_all <- X_all |> inner_join(Y_all, by = c("NUTS_NAME" = "name", "year" = "year"))

XY_all$Winterweizen <- XY_all$Winterweizen/10

arrow::write_parquet(XY_all, "../data/model_data_all.parquet")

#-------------------------------------------------------------------------------

XY_all <- arrow::read_parquet("../data/model_data_all.parquet")

# train test split
train_df <- XY_all |> filter(year <= 2023)
test_df  <- XY_all |> filter(year == 2024)

