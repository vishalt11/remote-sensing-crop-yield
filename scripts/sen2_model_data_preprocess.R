sen2_indices <- read.csv('../data/2017_2024_nuts3_ndvi_nirv.csv')

sen2_indices <- sen2_indices |> select(year, month, index, nuts3_id, mean)



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
    #filter(name %in% target_nuts) |>
    select(name, Winterweizen) |>
    mutate(year = year)
  
  return(y)
}) |>
  bind_rows()

# (German decimals use commas)
Y_all <- Y_all |> mutate(Winterweizen = as.numeric(gsub(",", ".", Winterweizen)))

nuts3_de <- giscoR::gisco_get_nuts(year = "2021", epsg = 4326, nuts_level = 3, resolution = "01", country = "DE")
nuts3_de <- nuts3_de |> filter(startsWith(NUTS_ID, "DE2"))

Y_all <- Y_all |>
  mutate(
    NUTS_NAME = name |>
      str_replace("\\(Krfr\\.St\\)", ", Kreisfreie Stadt") |>
      str_replace("\\(Lkr\\)", ", Landkreis") |>
      str_trim() |>
      str_replace_all("\\s+,", ",")
  )


unclean_names <- unique(Y_all$NUTS_NAME)
regions_to_remove <- c("Bayern", "Oberbayern", "Niederbayern", "Oberpfalz","Oberfranken", 
                       "Mittelfranken", "Unterfranken", "Schwaben")

unclean_names <- unclean_names[!unclean_names %in% regions_to_remove]

clean_names <- nuts3_de |> st_drop_geometry() |> select(NUTS_NAME, NUTS_ID) |> arrange(NUTS_NAME)
clean_names$bad_names <- sort(unclean_names)

#writeLines(sort(unclean_names), con = "bavstat.txt")
write.csv(clean_names, file = "nuts3.csv", row.names = FALSE)


match_table <- read.csv('nuts3_cleaned.csv')

colnames(Y_all) <- c("name","Winterweizen","year","bad_names")  

Y_all <- Y_all |>
  left_join(match_table |> select(bad_names, NUTS_ID, NUTS_NAME), by = "bad_names")

colSums(is.na(Y_all))

Y_final <- Y_all |> drop_na()


saveRDS(Y_final, file = 'y_final_sen2.rds')


sen2_wide <- sen2_indices |>
  mutate(month = tolower(month)) |>  
  unite("index_month", index, month, sep = "_") |>  
  pivot_wider(
    names_from = index_month,
    values_from = mean
  ) |>
  arrange(nuts3_id, year)


XY_all <- sen2_wide |> inner_join(Y_final, by = c("nuts3_id" = "NUTS_ID", "year" = "year"))
colSums(is.na(XY_all))

arrow::write_parquet(XY_all, "../data/model_data_sen2.parquet")
XY_all <- arrow::read_parquet("../data/model_data_sen2.parquet")

XY_all <- XY_all |> select(year, nuts3_id, NUTS_NAME, NDVI_march, NDVI_april, NDVI_may, NDVI_june,
                           NIRv_march, NIRv_april, NIRv_may, NIRv_june, Winterweizen)

train_df <- XY_all |> filter(year <= 2023)
train_df <- train_df |> drop_na()
test_df  <- XY_all |> filter(year == 2024)



model <- lm(Winterweizen ~ ., data = train_df[, -c(1,2,3)])
summary(model)

test_df$predicted_yield <- predict(model, newdata = test_df)
test_df <- test_df |>
  mutate(
    predicted_yield = predict(model, newdata = test_df),
    pct_diff = round(100 * (predicted_yield - Winterweizen) / Winterweizen,2)
  )


test_df |> select(nuts3_id, NUTS_NAME, Winterweizen, predicted_yield, pct_diff) |> print(n=70)


rmse <- sqrt(mean((test_df$predicted_yield - test_df$Winterweizen)^2, na.rm = TRUE))
rmse



XY_all |> filter(nuts3_id == 'DE249')

sen2_indices |> filter(nuts3_id == 'DE249')


