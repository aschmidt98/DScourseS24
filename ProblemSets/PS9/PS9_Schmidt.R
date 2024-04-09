library(tidyverse)
library(tidymodels)
library(glmnet)

# Install tidymodels package
if (!requireNamespace("tidymodels", quietly = TRUE)) {
  install.packages("tidymodels")
}

# Install glmnet package
if (!requireNamespace("glmnet", quietly = TRUE)) {
  install.packages("glmnet")
}

# Load the housing data from UCI
housing <- read_table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", col_names = FALSE)
names(housing) <- c("crim", "zn", "indus", "chas", "nox", "rm", "age", "dis", "rad", "tax", "ptratio", "b", "lstat", "medv")

# Display the first few rows of the data
head(housing)

# Set the seed for reproducibility
set.seed(123456)

# Split the data into training and testing sets
housing_split <- initial_split(housing, prop = 0.8)

# Extract the training and testing sets
housing_train <- training(housing_split)
housing_test  <- testing(housing_split)

# Define the recipe
housing_recipe <- recipe(medv ~ ., data = housing_train) %>%
  # Convert outcome variable to logs
  step_log(all_outcomes()) %>%
  # Convert 0/1 chas to a factor
  step_bin2factor(chas) %>%
  # Create 6th-degree polynomials for continuous variables
  step_poly(crim, zn, indus, rm, age, rad, tax, ptratio, b, lstat, dis, nox, degree = 6)

# Run the recipe
housing_prep <- prep(housing_recipe, training = housing_train, retain = TRUE)
housing_train_prepped <- juice(housing_prep)
housing_test_prepped <- bake(housing_prep, new_data = housing_test)

# Define the model specification
lasso_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Define the cross-validation folds
cv_folds <- vfold_cv(housing_train_prepped, v = 6)

# Define the grid of penalty values to try
lambda_grid <- grid_regular(penalty(range = c(-3, 0)), levels = 30)

# Tune the LASSO model
lasso_tune_results <- tune_grid(
  lasso_spec,
  medv ~ .,
  resamples = cv_folds,
  grid = lambda_grid,
  metrics = metric_set(rmse)
)

# Extract the best penalty value
best_lambda <- select_best(lasso_tune_results, metric = "rmse")
cat("Optimal value of lambda:", best_lambda$penalty, "\n")

# Fit the final LASSO model with the best penalty value
final_lasso_spec <- finalize_model(lasso_spec, best_lambda)
final_lasso_fit <- fit(final_lasso_spec, medv ~ ., data = housing_train_prepped)

# Calculate in-sample RMSE
in_sample_results <- tibble(
  truth = housing_train_prepped$medv,
  estimate = in_sample_preds
)
in_sample_rmse <- rmse(in_sample_results, truth = truth, estimate = estimate)
cat("In-sample RMSE:", in_sample_rmse$.estimate, "\n")

# Predict on the test set
out_sample_preds <- predict(final_lasso_fit, new_data = housing_test_prepped)$.pred

# Calculate out-of-sample RMSE
out_sample_results <- tibble(
  truth = housing_test_prepped$medv,
  estimate = out_sample_preds
)
out_sample_rmse <- rmse(out_sample_results, truth = truth, estimate = estimate)
cat("Out-of-sample RMSE:", out_sample_rmse$.estimate, "\n")


# Calculate out-of-sample RMSE
out_sample_results <- tibble(
  truth = housing_test_prepped$medv,
  estimate = out_sample_preds
)
out_sample_rmse <- rmse(out_sample_results, truth = truth, estimate = estimate)
cat("Out-of-sample RMSE:", out_sample_rmse$.estimate, "\n")

# Define the model specification for ridge regression
ridge_spec <- linear_reg(penalty = tune(), mixture = 0) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Tune the ridge regression model
ridge_tune_results <- tune_grid(
  ridge_spec,
  medv ~ .,
  resamples = cv_folds,
  grid = lambda_grid,
  metrics = metric_set(rmse)
)

# Extract the best penalty value (optimal lambda) for ridge regression
best_lambda_ridge <- select_best(ridge_tune_results, metric = "rmse")
cat("Optimal value of lambda for ridge regression:", best_lambda_ridge$penalty, "\n")

# Fit the final ridge regression model with the best penalty value
final_ridge_spec <- finalize_model(ridge_spec, best_lambda_ridge)
final_ridge_fit <- fit(final_ridge_spec, medv ~ ., data = housing_train_prepped)

# Predict on the test set using the final ridge regression model
ridge_out_sample_preds <- predict(final_ridge_fit, new_data = housing_test_prepped)$.pred

# Calculate out-of-sample RMSE for ridge regression
ridge_out_sample_results <- tibble(
  truth = housing_test_prepped$medv,
  estimate = ridge_out_sample_preds
)
ridge_out_sample_rmse <- rmse(ridge_out_sample_results, truth = truth, estimate = estimate)
cat("Out-of-sample RMSE for ridge regression:", ridge_out_sample_rmse$.estimate, "\n")
