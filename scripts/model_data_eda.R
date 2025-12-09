library(tidyverse)
library(sf)
library(mice)


#-------------------------------------------------------------------------------



rds_files <- list.files("../data/crop_composition", pattern = "\\.rds$", full.names = TRUE)
df_list <- lapply(rds_files, readRDS)
result_all <- bind_rows(df_list)

result_all <- result_all[order(result_all$Delta_Time),]
rm(df_list)


saveRDS(result_all, file = '../data/model_data.rds')

#-------------------------------------------------------------------------------


result_all <- readRDS(file = '../data/model_data.rds')

#unique(lubridate::month(result_all[lubridate::year(result_all$Delta_Time) %in% c(2024),]$Delta_Time))

idx <- sapply(result_all$crop_stats, function(df) {
  if (is.null(df) || nrow(df) == 0) return(FALSE)
  any(df$code == "winter_wheat" & df$area_pct >= 0, na.rm = TRUE)
})

# get the polygons with x pct wheat
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
#---------------------------Eda Plots-------------------------------------------

winterwheat_sf |> 
  st_drop_geometry() |> 
  select(Daily_SIF_740nm, wheat_share,c3_share,Meteo.vapor_pressure_deficit) |> 
  pivot_longer(cols = everything(),names_to = "variable",values_to = "value") |> 
  ggplot(aes(x = value)) +
  geom_density(na.rm = TRUE) +
  facet_wrap(~ variable, scales = "free") +
  theme_bw()

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

wheat_l <- wheat_l |>
  mutate(date = as.Date(Delta_date), month = floor_date(date, "month")) |>
  group_by(month) |>
  summarise(mean_sif = mean(Daily_SIF_740nm, na.rm = TRUE), .groups = "drop")

wheat_u <- wheat_u |>
  mutate(date = as.Date(Delta_date), month = floor_date(date, "month")) |>
  group_by(month) |>
  summarise(mean_sif = mean(Daily_SIF_740nm, na.rm = TRUE), .groups = "drop")


ggplot() +
  geom_point(data = wheat_l, aes(x = month, y = mean_sif), color = 'grey', size = 3, alpha = 0.8) +
  geom_point(data = wheat_u, aes(x = month, y = mean_sif), color = 'firebrick', size = 3, alpha = 0.8) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_bw() +
  facet_wrap(~ year(month), scales = "free_x") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#---------------------------date coverage---------------------------------------

winterwheat_sf %>%
  ggplot(aes(x=wheat_share)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  theme_bw()

head(winterwheat_sf |> select(-c(crop_stats)),1)

winterwheat_sf <- winterwheat_sf |>
  mutate(
    center_lat = (Lat_corner1 + Lat_corner2 + Lat_corner3 + Lat_corner4) / 4,
    center_lon = (Lon_corner1 + Lon_corner2 + Lon_corner3 + Lon_corner4) / 4
  )

winterwheat_sf$month <- lubridate::month(winterwheat_sf$Delta_Time)
winterwheat_sf$year <- lubridate::year(winterwheat_sf$Delta_Time)

high_ww <- winterwheat_sf[winterwheat_sf$wheat_share > 0.25,]

nuts1_de <- giscoR::gisco_get_nuts(year = "2021", epsg = 4326, nuts_level = 1, resolution = "01", country = "DE")
nuts2_de <- giscoR::gisco_get_nuts(year = "2021", epsg = 4326, nuts_level = 3, resolution = "01", country = "DE")
nuts2_centroids <- sf::st_centroid(nuts2_de)

nuts3_bavaria <- nuts2_de |> filter(startsWith(NUTS_ID, "DE2"))


ggplot() +
  geom_sf(data = nuts1_de, fill = NA, color = "blue", linewidth = 1) +
  geom_sf(data = nuts3_bavaria, fill = NA, color = "black", linewidth = 0.4) +
  geom_point(data = winterwheat_sf |> filter(NUTS_NAME == "Oberbayern"), 
             aes(x = center_lon, y = center_lat, color = factor(month)),size = 1, alpha=0.8) +
  theme_bw() +
  scale_color_brewer(name = "Month",palette = "Set2") +
  coord_sf(xlim = c(8.8, 14), ylim = c(47, 50.7), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude") +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  theme(legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10),
        axis.ticks = element_blank())

#---------which nuts 3 have all the months + years data-------------------------

nuts3_bavaria <- nuts3_bavaria |> select(NUTS3_ID = NUTS_ID, NUTS3_NAME = NUTS_NAME)

# Create point sf from center_lon, center_lat
winter_centers <- winterwheat_sf |>
  st_drop_geometry() |>
  mutate(id = row_number()) |>
  st_as_sf(
    coords = c("center_lon", "center_lat"),
    crs    = 4326
  )

# which NUTS3 polygon contains the center point?
winter_centers_nuts3 <- st_join(
  winter_centers,
  nuts3_bavaria,
  join = st_within,   
  left = TRUE
)

# Drop point geometry and keep the NUTS3 labels
nuts3_labels <- winter_centers_nuts3 |>
  st_drop_geometry() |>
  select(id, NUTS3_ID, NUTS3_NAME)

# Join back to original polygon sf
winterwheat_sf <- winterwheat_sf |>
  mutate(id = row_number()) |>
  left_join(nuts3_labels, by = "id") |>
  select(-id)


# nuts3 for each year, which months does it have data for?

# Start from attribute table only
ww_attr <- winterwheat_sf |>
  st_drop_geometry()

# Unique combinations of NUTS3, year, month
nuts3_year_month <- ww_attr |>
  distinct(NUTS3_ID, NUTS3_NAME, year, month) |>
  arrange(NUTS3_ID, year, month)

# For each NUTS3 & year: which months have data, which are missing (2–7)
nuts3_coverage <- nuts3_year_month |>
  group_by(NUTS3_ID, NUTS3_NAME, year) |>
  summarise(
    months_with_data   = list(sort(unique(month))),
    months_missing     = list(setdiff(2:7, unique(month))),
    n_months_with_data = dplyr::n(),
    .groups = "drop"
  )

# one column per month
nuts3_coverage_wide <- nuts3_year_month |>
  mutate(has_data = TRUE) |>
  tidyr::pivot_wider(
    id_cols = c(NUTS3_ID, NUTS3_NAME, year),
    names_from = month,
    names_prefix = "m",
    values_from = has_data,
    values_fill = FALSE
  )

coverage_nuts3_heatmap <- ww_attr |>
  # keep only the relevant period & months
  filter(
    dplyr::between(year, 2017, 2023),
    month %in% 2:7
  ) |>
  # unique NUTS3–year–month combos
  distinct(NUTS3_ID, NUTS3_NAME, year, month) |>
  # count number of month-year combos with data per NUTS3
  count(NUTS3_ID, NUTS3_NAME, name = "n_months_with_data") |>
  mutate(
    total_possible = 8 * 6,                             # 2017–2024, months 2–7
    coverage_pct   = 100 * n_months_with_data / total_possible
  )

nuts3_map <- nuts3_bavaria |>
  left_join(coverage_nuts3_heatmap, by = c("NUTS3_ID" = "NUTS3_ID")) |>
  # NUTS3 with no data at all → 0% coverage
  mutate(
    coverage_pct = replace_na(coverage_pct, 0)
  )

ggplot(nuts3_map) +  
  geom_sf(aes(fill = coverage_pct), color = NA) +
  scale_fill_viridis_c(
    name   = "SIF coverage\n(%)",
    limits = c(0, 100)
  ) +
  coord_sf() +
  theme_minimal() +
  labs(
    title    = "OCO-2 SIF Coverage: NUTS3 Regions (2017–2024, Feb–Jul)",
    subtitle = "Color shows % of month–year combinations with at least one sounding"
  )

# plot the labelling arrow

max_region <- nuts3_map |>
  filter(coverage_pct == max(coverage_pct)) |>
  slice(1)
# Get centroid for labeling placement
centroid <- st_coordinates(st_centroid(max_region)) |> as.data.frame()
label_x <- centroid$X
label_y <- centroid$Y

ggplot(nuts3_map) +
  geom_sf(aes(fill = coverage_pct), color = NA) +
  scale_fill_viridis_c(
    name   = "SIF coverage\n(%)",
    limits = c(0, 100)
  ) +
  # Arrow pointing to region centroid
  geom_segment(
    aes(
      x = label_x + 1,  
      y = label_y + 0.1,
      xend = label_x - 0.1,
      yend = label_y - 0.1),
    arrow = arrow(length = unit(0.25, "cm")),
    linewidth = 0.8
  ) +
  # Label with ID and % coverage
  geom_label(
    aes(x = label_x + 1, y = label_y + 0.1, 
        label = paste0(max_region$NUTS3_ID, "\n", round(max_region$coverage_pct), "%")),
    fill = "white", label.size = 0.3, size = 3) +
  coord_sf() +
  theme_minimal() +
  labs(
    x = 'Lon', y = 'Lat',
    title = "OCO-2 SIF Coverage: NUTS3 Regions (2017–2024, Feb–Jul)",
    subtitle = "Color shows % of month–year combinations with at least one sounding"
  )



#---------------------------MODELLING-------------------------------------------
sif_monthly <- as.data.frame(winterwheat_sf) |>
  mutate(
    year = year(Delta_date),
    month = month(Delta_date, label = TRUE, abbr = TRUE)
  ) |>
  filter(year %in% 2017:2024, month(Delta_date) %in% 3:7) |>
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

# Pivot wider to get Feb–May columns
# X_all <- sif_monthly |>
#   tidyr::pivot_wider(
#     names_from = month,
#     values_from = mean_sif,
#     names_prefix = "SIF_"
#   ) |>
#   arrange(NUTS_NAME, year)

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

# train test split
train_df <- XY_all |> filter(year <= 2023)
test_df  <- XY_all |> filter(year == 2024)

train_df <- train_df |>
  mutate(
    NUTS_NAME = factor(NUTS_NAME),
    year      = factor(year)
  )



#------------------------------IMPUTATION---------------------------------------
md.pattern(train_df, rotate.names = TRUE)

imp <- mice(train_df,m=5,maxit=50,meth='pmm',seed=123)
summary(imp)

imp$meth

train_complete <- complete(imp,1)
#----------------------------------linear regression----------------------------
fmla <- Winterweizen ~ mean_sif_Mar + mean_sif_Apr + mean_sif_May + mean_sif_Jun +
  mean_sif_Jul + mean_c3_share_Mar + mean_c3_share_Apr + mean_c3_share_May + mean_c3_share_Jun +
  mean_c3_share_Jul + mean_temp_2m_Mar + mean_temp_2m_Apr + mean_temp_2m_May + mean_temp_2m_Jun +
  mean_temp_2m_Jul
model <- lm(fmla, data = train_complete[, -c(1,2)])

# model summary
summary(model)

#test_df <- test_df |> drop_na(SIF_Feb, SIF_Mar, SIF_Apr, SIF_May)
test_df$predicted_yield <- predict(model, newdata = test_df)

test_df <- test_df |>
  mutate(
    predicted_yield = predict(model, newdata = test_df),
    pct_diff = 100 * (predicted_yield - Winterweizen) / Winterweizen
  )

test_df |> select(NUTS_NAME, year, Winterweizen, predicted_yield, pct_diff)

# rmse <- sqrt(mean((test_df[test_df$NUTS_NAME != 'Oberfranken',]$predicted_yield - test_df[test_df$NUTS_NAME != 'Oberfranken',]$Winterweizen)^2, na.rm = TRUE))
# rmse
rmse <- sqrt(mean((test_df$predicted_yield - test_df$Winterweizen)^2, na.rm = TRUE))
rmse

#----------------------------------glmnet----------------------------
library(glmnet)


glm_train <- train_complete |> select(-c(NUTS_NAME, year))

X <- model.matrix(Winterweizen ~ ., data = glm_train)[, -1]  # drop intercept column
y <- train_complete$Winterweizen

set.seed(123)
cv_fit <- cv.glmnet(
  x      = X,
  y      = y,
  alpha  = 1,      # 1 = lasso, 0 = ridge, in between = elastic net
  nfolds = 5       # you can use 10, but with ~50 obs 5 is fine
)

## 5. Inspect results
plot(cv_fit)                           # CV curve vs log(lambda)
cv_fit$lambda.min                      # lambda with minimum CV error
cv_fit$lambda.1se                      # more regularized lambda

## 6. Coefficients at best lambda
coef_min  <- coef(cv_fit, s = "lambda.min")
coef_1se  <- coef(cv_fit, s = "lambda.1se")

coef_min
coef_1se

glm_test <- test_df |> select(-c(NUTS_NAME, year))

X_test <- model.matrix(Winterweizen ~ ., data = glm_test)[, -1]
y_pred <- predict(cv_fit, newx = X_test, s = "lambda.min")

test_df <- test_df |> mutate(
      pred_Winterweizen = as.numeric(y_pred),
      pct_diff = 100 * (pred_Winterweizen - Winterweizen) / Winterweizen) |> 
      select(NUTS_NAME, pred_Winterweizen, Winterweizen, pct_diff)

rmse <- sqrt(mean((test_df[test_df$NUTS_NAME != 'Oberfranken',]$pred_Winterweizen - test_df[test_df$NUTS_NAME != 'Oberfranken',]$Winterweizen)^2, na.rm = TRUE))
rmse

#----------------------------------xgboost----------------------------

train_df <- XY_all |> filter(year <= 2023)
test_df  <- XY_all |> filter(year == 2024)

train_df_mean_imp <- train_df |>
  mutate(
    across(
      where(is.numeric),
      ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)
    )
  )







