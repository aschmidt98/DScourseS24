# Load required packages
library(sparklyr)
library(tidyverse)

# Set up a connection to Spark
sc <- spark_connect(master = "local")

# Create a tibble called df1 that loads in the iris data
df1 <- as_tibble(iris)

# Copy this tibble into Spark, calling it df
df <- copy_to(sc, df1)

# Verify that the two dataframes are different types
cat("Class of df1:", class(df1), "\n")
cat("Class of df:", class(df), "\n")

# Check if the column names are different across the two objects
cat("Column names of df1:", colnames(df1), "\n")
cat("Column names of df:", colnames(df), "\n")

# List the first 6 rows of the Sepal_Length and Species columns of df
df %>%
  select(Sepal_Length, Species) %>%
  head() %>%
  print()

# List the first 6 rows of all columns of df where Sepal_Length is larger than 5.5
df %>%
  filter(Sepal_Length > 5.5) %>%
  head() %>%
  print()

# Combine the select and filter operations into one line
df %>%
  filter(Sepal_Length > 5.5) %>%
  select(Sepal_Length, Species) %>%
  head() %>%
  print()

# Compute the average sepal length and count by species
df2 <- df %>%
  group_by(Species) %>%
  summarize(mean_sepal_length = mean(Sepal_Length), count = n()) %>%
  head() %>%
  print()

# Sort the grouped by RDD ascending by species name
df2 %>%
  arrange(Species) %>%
  head() %>%
  print()

# Disconnect from Spark
spark_disconnect(sc)