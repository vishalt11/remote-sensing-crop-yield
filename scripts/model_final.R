library(tidyverse)
library(tidymodels)
library(mice)


df <- arrow::read_parquet("../data/model_data_all.parquet")

df <- df |>
  select(
    NUTS_NAME, year, Winterweizen,
    starts_with("mean_sif"),
    #starts_with("mean_temp_2m"),
    #starts_with("mean_pressure"),
    #starts_with("mean_vpd"),
    starts_with("mean_c3"),
    #starts_with("NDVI"),
    #starts_with("NIRv"),
    #contains("mean", ignore.case = TRUE),
    #contains("NDVI", ignore.case = TRUE),
    contains("NIRv", ignore.case = TRUE)
  )
# 
# cor_mat <- cor(df[, -c(1,2)], use = "pairwise.complete.obs")
# 
# corrplot::corrplot(
#   cor_mat,
#   method = "color",
#   type = "upper",
#   order = "hclust",
#   tl.cex = 0.7,
#   addCoef.col = "black",  # ← numbers
#   number.cex = 0.6        # size of numbers
# )
# 
# 
# colSums(is.na(df[df$year < 2024,]))
# (sum(is.na(df))/(sum(is.na(df)) + sum(!is.na(df))))*100

# -----------------------------
# 1) Train / test split by year
# -----------------------------
train_df <- df %>% filter(year < 2024)
test_df  <- df %>% filter(year == 2024)

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

#--------------------miceimputation---------------------------------------------
imp <- mice(train_df,m=5,maxit=50,meth='pmm',seed=123)
train_df_imp <- complete(imp,1)


# -----------------------------
# 2) Recipe: imputation + scaling
# -----------------------------
yield_recipe <- recipe(Winterweizen ~ ., data = train_df_imp) %>%
  step_normalize(all_numeric_predictors())
  #step_impute_median(all_numeric_predictors()) 
#%>%
 

set.seed(42)

#cv_folds <- vfold_cv(train_df_imp, v = 5)


# -----------------------------
# 3) Plain linear regression
# -----------------------------
lm_model <- linear_reg() %>%
  set_engine("lm")

lm_workflow <- workflow() %>%
  add_recipe(yield_recipe) %>%
  add_model(lm_model)

#lm_cv_results <- fit_resamples(
#  lm_workflow,
#  resamples = cv_folds,
#  metrics = metric_set(yardstick::rmse, yardstick::rsq, yardstick::mae)
#)
#collect_metrics(lm_cv_results)


lm_final_fit <- fit(
  lm_workflow,
  data = train_df_imp
)


test_predictions <- predict(lm_final_fit, new_data = test_df) %>%
  bind_cols(test_df %>% select(Winterweizen))

yardstick::metrics(
  test_predictions,
  truth = Winterweizen,
  estimate = .pred
)

#-----------------------xgboost-------------------------------------------------
library(xgboost)

head(df,1)


# -----------------------------
# 1) Split by year (train: 2017-2023, test: 2024)
# -----------------------------
train_df <- df %>% filter(year < 2024)
test_df  <- df %>% filter(year == 2024)

# -----------------------------
# 2) Build X / y (drop NUTS_NAME and year from features)
# -----------------------------
y_train <- train_df$Winterweizen
y_test  <- test_df$Winterweizen

X_train <- train_df %>%
  select(-Winterweizen, -NUTS_NAME, -year)

X_test <- test_df %>%
  select(-Winterweizen, -NUTS_NAME, -year)

# -----------------------------
# 3) Simple imputation (NO mice): median per column (fit on train, apply to both)
# -----------------------------
medians <- sapply(X_train, function(x) median(x, na.rm = TRUE))

impute_with_medians <- function(X, med) {
  X_imp <- X
  for (nm in names(med)) {
    idx <- is.na(X_imp[[nm]])
    if (any(idx)) X_imp[[nm]][idx] <- med[[nm]]
  }
  X_imp
}

X_train_imp <- impute_with_medians(X_train, medians)
#X_test_imp  <- impute_with_medians(X_test,  medians)

# Convert to numeric matrix for xgboost
train_mat <- data.matrix(X_train_imp)
test_mat  <- data.matrix(X_test)

dtrain <- xgb.DMatrix(data = train_mat, label = y_train)
dtest  <- xgb.DMatrix(data = test_mat,  label = y_test)

# -----------------------------
# 4) 5-fold cross-validation to choose nrounds
# -----------------------------
set.seed(42)

params <- list(
  booster = "gbtree",
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.05,
  max_depth = 4,
  min_child_weight = 1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  lambda = 1,
  alpha = 0
)

params <- list(
  booster = "gbtree",
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.02,
  max_depth = 2,          # CRITICAL
  min_child_weight = 5,   # CRITICAL
  subsample = 0.7,
  colsample_bytree = 0.6,
  lambda = 5,
  alpha = 1
)


cv <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 2000,
  nfold = 5,
  early_stopping_rounds = 25,
  verbose = 1
)

elog <- cv$evaluation_log

best_nrounds <- elog$iter[which.min(elog$test_rmse_mean)]
best_nrounds


# -----------------------------
# 5) Train final model on all training years
# -----------------------------
final_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = best_nrounds,
  verbose = 0
)

# -----------------------------
# 6) Predict on 2024 test + metrics
# -----------------------------
pred_test <- predict(final_model, dtest)

rmse <- sqrt(mean((pred_test - y_test)^2))
mae  <- mean(abs(pred_test - y_test))
r2   <- 1 - sum((pred_test - y_test)^2) / sum((y_test - mean(y_test))^2)

c(RMSE = rmse, MAE = mae, R2 = r2)

# Optional: feature importance
imp <- xgb.importance(model = final_model)
print(head(imp, 15))
xgb.plot.importance(imp, top_n = 20)


#----------------------------ridge/lasso----------------------------------------

















