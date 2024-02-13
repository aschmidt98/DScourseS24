# Load necessary libraries
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("RSQLite", quietly = TRUE)) install.packages("RSQLite")
library(readr)
library(RSQLite)
library(DBI)

# Function to download and extract the CSV file
download_and_extract_csv <- function(zip_url, output_dir) {
  zip_file_path <- tempfile()
  download.file(zip_url, zip_file_path)
  unzip(zip_file_path, exdir = output_dir)
  files <- list.files(output_dir, full.names = TRUE)
  csv_file <- files[grepl("\\.csv$", files)]
  return(csv_file)
}

# Define the URL of the ZIP file
zip_url <- "http://spatialkeydocs.s3.amazonaws.com/FL_insurance_sample.csv.zip"

# Download and extract the CSV file
csv_file <- download_and_extract_csv(zip_url, tempdir())

# Read the CSV file
florida_insurance_data <- read_csv(csv_file[1])

# Create an in-memory SQLite database
db <- dbConnect(RSQLite::SQLite(), ":memory:")

# Import the data frame into the SQLite database
dbWriteTable(db, "insurance_data", florida_insurance_data, overwrite = TRUE)

# Execute SQL queries
# a) Print out the first 10 rows of the data set
print(dbGetQuery(db, "SELECT * FROM insurance_data LIMIT 10"))

# b) List which counties are in the sample
print(dbGetQuery(db, "SELECT DISTINCT county FROM insurance_data"))

# c) Compute the average property appreciation from 2011 to 2012
print(dbGetQuery(db, "SELECT AVG(tiv_2012 - tiv_2011) AS avg_appreciation FROM insurance_data"))

# d) Create a frequency table of the construction variable
print(dbGetQuery(db, "SELECT construction, COUNT(*) AS count, COUNT(*) * 1.0 / (SELECT COUNT(*) FROM insurance_data) AS fraction FROM insurance_data GROUP BY construction"))

# Disconnect from the database
dbDisconnect(db)
