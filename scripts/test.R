library(tidyverse)
library(mice)


df <- arrow::read_parquet("../data/model_data_all.parquet")

df <- df |>
  select(
    NUTS_NAME, year, Winterweizen,
    starts_with("mean_sif"),
    #starts_with("mean_temp_2m"),
    #starts_with("mean_temp_skin"),
    #starts_with("mean_pressure"),
    #starts_with("mean_vpd"),
    starts_with("mean_c3"),
    #starts_with("NDVI"),
    starts_with("NIRv"),
    #contains("mean", ignore.case = TRUE),
    #contains("NDVI", ignore.case = TRUE),
    #contains("NIRv", ignore.case = TRUE)
  )


# train test split
train_df <- df |> filter(year <= 2023)
test_df  <- df |> filter(year == 2024)


# Optional IDs (for inspection only)
train_ids <- paste(train_df$NUTS_NAME, train_df$year, sep = "_")
test_ids  <- paste(test_df$NUTS_NAME,  test_df$year,  sep = "_")

# Remove ID columns from modeling data
train_df <- train_df %>%
  select(-NUTS_NAME, -year) %>%
  as.data.frame()
rownames(train_df) <- train_ids

test_df <- test_df %>%
  select(-NUTS_NAME, -year) %>%
  as.data.frame()
rownames(test_df) <- test_ids

#----------mean imputation---------------------
x_cols <- setdiff(names(train_df), "Winterweizen")

# compute train means (na.rm=TRUE)
train_means <- sapply(train_df[, x_cols, drop = FALSE], function(x) mean(x, na.rm = TRUE))

mean_impute <- function(dat, means_vec) {
  out <- dat
  for (nm in names(means_vec)) {
    idx <- is.na(out[[nm]])
    if (any(idx)) out[[nm]][idx] <- means_vec[[nm]]
  }
  out
}

train_df_imp <- mean_impute(train_df, train_means)

#----------mice imputation---------------------

imp <- mice(train_df,m=5,maxit=50,meth='pmm',seed=123)
train_df_imp <- complete(imp,1)


#-----------------------------------LR-----------------------------------

model <- lm(Winterweizen ~ ., data = train_df_imp)

# model summary
summary(model)

test_df$predicted_yield <- predict(model, newdata = test_df)

test_df <- test_df |>
  mutate(
    predicted_yield = predict(model, newdata = test_df),
    pct_diff = 100 * (predicted_yield - Winterweizen) / Winterweizen
  )

test_df |> select(Winterweizen, predicted_yield, pct_diff)

# rmse <- sqrt(mean((test_df[-c(4),]$predicted_yield - test_df[-c(4),]$Winterweizen)^2))
# rmse
rmse <- sqrt(mean((test_df$predicted_yield - test_df$Winterweizen)^2, na.rm = TRUE))
rmse

y_test <- test_df$Winterweizen
rmse <- sqrt(mean((test_df$predicted_yield - y_test)^2))
mae  <- mean(abs(test_df$predicted_yield - y_test))
r2   <- 1 - sum((test_df$predicted_yield - y_test)^2) / sum((y_test - mean(y_test))^2)

c(RMSE = rmse, MAE = mae, R2 = r2)

#------------------------baseline model-----------------------------------------

baseline_pred <- rep(mean(train_df$Winterweizen), nrow(test_df))

baseline_rmse <- sqrt(mean((baseline_pred - y_test)^2))
baseline_mae  <- mean(abs(baseline_pred - y_test))

c(RMSE = baseline_rmse, MAE = baseline_mae)


