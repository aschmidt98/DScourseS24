# Load necessary packages
library(tidyverse)
library(mice)
library(modelsummary)
library(readr)

# Load the wages.csv file as a data frame
wages_df <- read_csv("C:/Users/alex_/Downloads/wages.csv")

# Drop observations where either hgc or tenure are missing
wages_df_clean <- na.omit(wages_df, select = c(hgc, tenure))

summary(wages_df)

sum(is.na(wages_df$logwage))
sum(is.na(wages_df_clean$logwage))
sum(is.na(wages_df$age))
sum(is.na(wages_df_clean$age))


# Create a summary table of the original wages_df data frame
summary_table_original <- datasummary_skim(wages_df)

# Display the summary table for the original data frame
summary_table_original

library(modelsummary)

# Create a summary table of the wages_df_clean data frame
summary_table <- datasummary_skim(wages_df_clean)

# Display the summary table
summary_table

mod <- lm(logwage ~ hgc + tenure, data = wages_df_clean)

modelsummary(mod, output = "markdown")

library(tidyverse)
library(mice)
library(modelsummary)
library(readr)

# Define a linear regression model using the cleaned data frame
mod <- lm(logwage ~ hgc + tenure, data = wages_df_clean)

# Use modelsummary to create a summary table of the regression model
modelsummary(mod, output = "markdown")

# Calculate the proportion of missing values in the logwage variable
missing_rate_logwage <- sum(is.na(wages_df$logwage)) / nrow(wages_df)

# Print the missing rate
missing_rate_logwage

# Estimate the regression using only complete cases
model_complete <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = na.omit(wages_df))

# Perform mean imputation to fill in missing log wages
mean_logwage <- mean(wages_df$logwage, na.rm = TRUE)
wages_df_mean <- wages_df %>% 
  mutate(logwage = ifelse(is.na(logwage), mean_logwage, logwage))

# Estimate the regression with mean imputation
model_mean <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = wages_df_mean)

# Impute missing log wages as their predicted values from the complete cases regression
predicted_logwage <- predict(model_complete, newdata = wages_df)
wages_df_pred <- wages_df %>% 
  mutate(logwage = ifelse(is.na(logwage), predicted_logwage, logwage))

# Estimate the regression with predicted imputation
model_pred <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = wages_df_pred)

# Perform multiple imputation using the mice package
mice_data <- mice(wages_df, m = 5, method = 'pmm', maxit = 5)
mice_model <- with(mice_data, lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married))
mice_summary <- pool(mice_model)

# Create a regression table with the estimates of the four regression models
models <- list("Complete Cases" = model_complete, "Mean Imputation" = model_mean, "Predicted Imputation" = model_pred, "Multiple Imputation" = mice_summary)
modelsummary(models)


#tested code ends







#untested code begins

# Create a LaTeX-formatted summary table of the cleaned-up data frame
datasummary_skim(wages_df_clean, output = "latex")

# Define the four regression models
model_complete <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, 
                     data = na.omit(wages_df))
model_mean <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, 
                 data = wages_df_mean)
model_pred <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, 
                 data = wages_df_pred)
mice_data <- mice(wages_df, m = 5, method = 'pmm', maxit = 5)
mice_model <- with(mice_data, lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married))
mice_summary <- pool(mice_model)

# Create a list of models
models <- list("Complete Cases" = model_complete, 
               "Mean Imputation" = model_mean, 
               "Predicted Imputation" = model_pred, 
               "Multiple Imputation" = mice_summary)

# Create a LaTeX-formatted regression table
modelsummary(models, output = "latex")