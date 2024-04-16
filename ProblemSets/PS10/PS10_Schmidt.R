library(tidymodels)  # Load the tidymodels suite
library(rpart)       # For decision trees
library(e1071)       # Support Vector Machines and other tools
library(kknn)        # k-Nearest Neighbors
library(nnet)        # For neural networks
library(kernlab)     # For SVM with RBF kernel
library(dials)       # For managing model hyperparameters

# Define model specifications with explicit modes
model_list <- list(
  logistic = logistic_reg(penalty = tune(), mixture = 1) %>% 
    set_engine("glmnet") %>% 
    set_mode("classification"),
  tree = decision_tree(min_n = tune(), tree_depth = tune(), cost_complexity = tune()) %>% 
    set_engine("rpart") %>% 
    set_mode("classification"),
  nnet = mlp(hidden_units = tune(), penalty = tune()) %>% 
    set_engine("nnet") %>% 
    set_mode("classification"),
  knn = nearest_neighbor(neighbors = tune()) %>% 
    set_engine("kknn") %>% 
    set_mode("classification"),
  svm = svm_rbf(cost = tune(), rbf_sigma = tune()) %>% 
    set_engine("kernlab") %>% 
    set_mode("classification")
)

# Example of tuning grid setup (ensure your grids are correctly defined based on previous steps)
grids <- list(
  logistic = grid_regular(penalty(), levels = 50),
  tree = grid_regular(min_n(), tree_depth(), cost_complexity(), levels = 10),
  nnet = grid_regular(hidden_units(), penalty(), levels = 10),
  knn = tibble(neighbors = seq(1, 30)),  # Discrete values in a tibble
  svm = grid_regular(cost(), rbf_sigma(), levels = 10)
)

# Example of setting up cross-validation
cv_folds <- vfold_cv(income_train, v = 3, strata = high.earner)

# Example of tuning models
tuned_results <- map2(model_list, grids, ~ {
  workflow() %>%
    add_model(.x) %>%
    add_formula(high.earner ~ age + workclass + education + marital.status + occupation + relationship + race + sex + capital.gain + capital.loss + hours) %>%
    tune_grid(resamples = cv_folds, grid = .y, metrics = metric_set(accuracy))
})

# Check tuned_results to debug further if needed
print(tuned_results)
