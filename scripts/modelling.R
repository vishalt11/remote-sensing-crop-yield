library(tidyverse)
library(sf)
library(mice)


#-------------------------------------------------------------------------------

c4_codes <- c("maize")
`%!in%` <- Negate(`%in%`)

rds_files <- list.files("../data/crop_composition", pattern = "\\.rds$", full.names = TRUE)
df_list <- lapply(rds_files, readRDS)
result_all <- bind_rows(df_list)

result_all <- result_all[order(result_all$Delta_Time),]
rm(df_list)


saveRDS(result_all, file = '../data/model_data.rds')
result_all <- readRDS(file = '../data/model_data.rds')




unique(lubridate::month(result_all[lubridate::year(result_all$Delta_Time) %in% c(2024),]$Delta_Time))


idx <- sapply(result_all$crop_stats, function(df) {
  if (is.null(df) || nrow(df) == 0) return(FALSE)
  any(df$code == "winter_wheat" & df$area_pct >= 10, na.rm = TRUE)
})

# get the polygons with x pct wheat
winterwheat_sf <- result_all[idx, ]

winterwheat_sf <- winterwheat_sf %>%
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



#head(winterwheat_sf$crop_stats,10)
# wheat share
nrow(winterwheat_sf[winterwheat_sf$wheat_share >= 0.5,])
# negative SIF values
# nadir=1
# qualityflag

winterwheat_sf <- winterwheat_sf[winterwheat_sf$Quality_Flag %in% c(0, 1),]

wheat_l <- winterwheat_sf[winterwheat_sf$wheat_share < 0.2,]
wheat_u <- winterwheat_sf[winterwheat_sf$wheat_share >= 0.8,]



wheat_l <- st_drop_geometry(wheat_l)
wheat_u <- st_drop_geometry(wheat_u)

wheat_l <- wheat_l %>%
  mutate(date = as.Date(Delta_date), month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(mean_sif = mean(Daily_SIF_740nm, na.rm = TRUE), .groups = "drop")

wheat_u <- wheat_u %>%
  mutate(date = as.Date(Delta_date), month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(mean_sif = mean(Daily_SIF_740nm, na.rm = TRUE), .groups = "drop")


ggplot() +
  geom_point(data = wheat_l, aes(x = month, y = mean_sif), color = 'grey', size = 3, alpha = 0.8) +
  geom_point(data = wheat_u, aes(x = month, y = mean_sif), color = 'firebrick', size = 3, alpha = 0.8) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_bw() +
  facet_wrap(~ year(month), scales = "free_x") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# check if these two time series are the same

#---------------------------MODELLING-------------------------------------------
sif_monthly <- as.data.frame(winterwheat_sf) |>
  mutate(
    year = year(Delta_date),
    month = month(Delta_date, label = TRUE, abbr = TRUE)
  ) |>
  filter(year %in% 2017:2024, month(Delta_date) %in% 2:5) |>
  group_by(NUTS_NAME, year, month) |>
  summarise(
    mean_sif         = mean(Daily_SIF_740nm, na.rm = TRUE),
    mean_humidity    = mean(Meteo.specific_humidity, na.rm = TRUE),
    mean_pressure    = mean(Meteo.surface_pressure, na.rm = TRUE),
    mean_temp_skin   = mean(Meteo.temperature_skin, na.rm = TRUE),
    mean_temp_2m     = mean(Meteo.temperature_two_meter, na.rm = TRUE),
    mean_vpd         = mean(Meteo.vapor_pressure_deficit, na.rm = TRUE),
    #mean_c3_share    = mean(c3_share, na.rm = TRUE),
    .groups = "drop"
  )

# Pivot wider to get Feb–May columns
# X_all <- sif_monthly |>
#   tidyr::pivot_wider(
#     names_from = month,
#     values_from = mean_sif,
#     names_prefix = "SIF_"
#   ) |>
#   arrange(NUTS_NAME, year)

X_all <- sif_monthly %>%
  mutate(month = as.character(month)) %>%
  pivot_wider(
    names_from = month,
    values_from = c(
      mean_sif, mean_humidity, mean_pressure,
      mean_temp_skin, mean_temp_2m, mean_vpd
    ),
    names_glue = "{.value}_{month}"
  )

X_all

# List all CSV files in the folder
files <- list.files("../data/bayern_yield_formatted/", pattern = "\\.csv$", full.names = TRUE)
target_nuts <- c("Mittelfranken", "Niederbayern", "Oberbayern", 
                 "Oberfranken", "Oberpfalz", "Schwaben", "Unterfranken")

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
Y_all <- Y_all |>
  mutate(Winterweizen = as.numeric(gsub(",", ".", Winterweizen)))

Y_all

XY_all <- X_all |>
  inner_join(Y_all, by = c("NUTS_NAME" = "name", "year" = "year"))

XY_all$Winterweizen <- XY_all$Winterweizen/10

# train test split
train_df <- XY_all |> filter(year <= 2023)
test_df  <- XY_all |> filter(year == 2024)

#-------------IMPUTATION--------------------------------------------------------


imp <- mice(XY_all, m = 5, method = "pmm")
XY_all <- complete(imp)
#-------------------------------------------------------------------------------


# linear regression
model <- lm(Winterweizen ~ ., data = train_df[, -c(1,2)])

# model summary
summary(model)

#test_df <- test_df %>% drop_na(SIF_Feb, SIF_Mar, SIF_Apr, SIF_May)
test_df$predicted_yield <- predict(model, newdata = test_df)
test_df |> select(NUTS_NAME, year, Winterweizen, predicted_yield)


test_df <- test_df |>
  mutate(
    predicted_yield = predict(model, newdata = test_df),
    pct_diff = 100 * (predicted_yield - Winterweizen) / Winterweizen
  )

rmse <- sqrt(mean((test_df[test_df$NUTS_NAME != 'Oberfranken',]$predicted_yield - test_df[test_df$NUTS_NAME != 'Oberfranken',]$Winterweizen)^2, na.rm = TRUE))
rmse
rmse <- sqrt(mean((test_df$predicted_yield - test_df$Winterweizen)^2, na.rm = TRUE))
rmse
