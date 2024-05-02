##########################################
## Section 1: Extracting the Crime Data ##
##########################################


# Define the URL of the webpage
url <- "https://oklahomawatch.opalstacked.com/crime/"

# Load the rvest package
library(rvest)

# Read the HTML content of the webpage
page <- read_html(url)

# Extract data using CSS selectors
Name <- page %>% html_nodes("#sort a") %>% html_text()
Population <- page %>% html_nodes(".gray:nth-child(2)") %>% html_text()
Offenses <- page %>% html_nodes(".searchable .r:nth-child(3)") %>% html_text()
Rate <- page %>% html_nodes(".searchable .gray:nth-child(4)") %>% html_text()
Violent <- page %>% html_nodes(".searchable .r:nth-child(5)") %>% html_text()
V_rate <- page %>% html_nodes(".searchable .r:nth-child(6)") %>% html_text()
Property <- page %>% html_nodes(".searchable .gray:nth-child(7)") %>% html_text()
P_Rate <- page %>% html_nodes(".searchable .gray+ .gray") %>% html_text()

# Print out extracted vectors for debugging
print("Name")
print("Population")
print("Offenses")
print("Rate")
print("Violent")
print("V_rate")
print("Property")
print("P_rate")

# Check lengths of vectors
lengths <- c(length(Name), length(Population), length(Offenses), length(Rate), length(Violent), length(V_rate), length(Property), length(P_Rate))
print(lengths)

##########################################
## Section 1.1: Cleaning the Crime Data ##
##########################################

# Create a data frame only if all vectors have the same length
if (all(lengths == lengths[1])) {
  data_frame <- data.frame(Town_Name = Name, Population = Population, Total_Offenses = Offenses, Offense_Rate = Rate, Violent_Crimes = Violent, Violent_Crime_Rate = V_rate, Property_Crimes = Property, Property_Crime_Rate = P_Rate, stringsAsFactors = FALSE)
  
  # Convert empty cells to NA
  data_frame[data_frame == ""] <- NA
  
  # Remove rows with missing values (NA)
  data_frame <- na.omit(data_frame)
  
  # Remove the strange character Â, which appears to have been from the dot the website used for color indication. For some reason only Unicode identification works on it.
  data_frame <- data.frame(lapply(data_frame, function(x) gsub("\xC2\xA0", "", x)))
  
  # Write the cleaned data frame to a CSV file
  write.csv(data_frame, "Oklahoma.crime.rates.cleaned.csv", row.names = FALSE)
  print("Successfully created file.")
} else {
  print("Vectors have differing lengths. Cannot create data frame.")
}

# Load the ggplot2 package
library(ggplot2)

# Convert the Population, Offense_Rate, Violent_Crime_Rate, and Property_Crime_Rate columns to numeric
data_frame$Population <- as.numeric(gsub(",", "", data_frame$Population))
data_frame$Offense_Rate <- as.numeric(gsub(",", "", data_frame$Offense_Rate))
data_frame$Violent_Crime_Rate <- as.numeric(gsub(",", "", data_frame$Violent_Crime_Rate))
data_frame$Property_Crime_Rate <- as.numeric(gsub(",", "", data_frame$Property_Crime_Rate))

# Ensure all necessary columns are converted to numeric
data_frame$Population <- as.numeric(gsub(",", "", data_frame$Population))
data_frame$Offense_Rate <- as.numeric(gsub(",", "", data_frame$Offense_Rate))

# Remove rows with NA values in Offense_Rate
data_frame <- na.omit(data_frame)

# Plot Offense Rate vs. Population with adjusted axes
ggplot(data_frame, aes(x = Population, y = Offense_Rate)) +
  geom_point() +
  scale_x_continuous(limits = c(0, max(data_frame$Population, na.rm = TRUE)), breaks = seq(0, max(data_frame$Population, na.rm = TRUE), by = 10000)) +
  scale_y_continuous(limits = c(0, max(data_frame$Offense_Rate, na.rm = TRUE)), breaks = seq(0, max(data_frame$Offense_Rate, na.rm = TRUE), by = 50)) +
  labs(title = "Offense Rate vs. Population", x = "Population", y = "Offense Rate") +
  theme_minimal()

# Plot Violent Crime Rate vs. Population with adjusted axes
ggplot(data_frame, aes(x = Population, y = Violent_Crime_Rate)) +
  geom_point() +
  scale_x_continuous(limits = c(0, max(data_frame$Population, na.rm = TRUE)), breaks = seq(0, max(data_frame$Population, na.rm = TRUE), by = 10000)) +
  scale_y_continuous(limits = c(0, max(data_frame$Violent_Crime_Rate, na.rm = TRUE)), breaks = seq(0, max(data_frame$Violent_Crime_Rate, na.rm = TRUE), by = 0.5)) +
  labs(title = "Violent Crime Rate vs. Population", x = "Population", y = "Violent Crime Rate") +
  theme_minimal()

# Plot Property Crime Rate vs. Population with adjusted axes
ggplot(data_frame, aes(x = Population, y = Property_Crime_Rate)) +
  geom_point() +
  scale_x_continuous(limits = c(0, max(data_frame$Population, na.rm = TRUE)), breaks = seq(0, max(data_frame$Population, na.rm = TRUE), by = 10000)) +
  scale_y_continuous(limits = c(0, max(data_frame$Property_Crime_Rate, na.rm = TRUE)), breaks = seq(0, max(data_frame$Property_Crime_Rate, na.rm = TRUE), by = 50)) +
  labs(title = "Property Crime Rate vs. Population", x = "Population", y = "Property Crime Rate") +
  theme_minimal()

# Calculate the interquartile range (IQR) for the population
IQR_Population <- IQR(data_frame$Population, na.rm = TRUE)

# Define the upper limit for the population to exclude outliers
upper_limit_Population <- quantile(data_frame$Population, 0.75, na.rm = TRUE) + 1.5 * IQR_Population

# Calculate the interquartile range (IQR) for each rate
IQR_Offense_Rate <- IQR(data_frame$Offense_Rate, na.rm = TRUE)
IQR_Violent_Crime_Rate <- IQR(data_frame$Violent_Crime_Rate, na.rm = TRUE)
IQR_Property_Crime_Rate <- IQR(data_frame$Property_Crime_Rate, na.rm = TRUE)

# Define the upper limits for each rate to exclude outliers
upper_limit_Offense_Rate <- quantile(data_frame$Offense_Rate, 0.75, na.rm = TRUE) + 1.5 * IQR_Offense_Rate
upper_limit_Violent_Crime_Rate <- quantile(data_frame$Violent_Crime_Rate, 0.75, na.rm = TRUE) + 1.5 * IQR_Violent_Crime_Rate
upper_limit_Property_Crime_Rate <- quantile(data_frame$Property_Crime_Rate, 0.75, na.rm = TRUE) + 1.5 * IQR_Property_Crime_Rate

# Plot Offense Rate vs. Population excluding outliers in both axes
ggplot(data_frame, aes(x = Population, y = Offense_Rate)) +
  geom_point() +
  scale_x_continuous(limits = c(0, upper_limit_Population), breaks = seq(0, upper_limit_Population, by = 10000)) +
  scale_y_continuous(limits = c(0, upper_limit_Offense_Rate), breaks = seq(0, upper_limit_Offense_Rate, by = 50)) +
  labs(title = "Offense Rate vs. Population (Excluding Outliers)", x = "Population", y = "Offense Rate") +
  theme_minimal()

# Plot Violent Crime Rate vs. Population excluding outliers in both axes
ggplot(data_frame, aes(x = Population, y = Violent_Crime_Rate)) +
  geom_point() +
  scale_x_continuous(limits = c(0, upper_limit_Population), breaks = seq(0, upper_limit_Population, by = 10000)) +
  scale_y_continuous(limits = c(0, upper_limit_Violent_Crime_Rate), breaks = seq(0, upper_limit_Violent_Crime_Rate, by = 0.5)) +
  labs(title = "Violent Crime Rate vs. Population (Excluding Outliers)", x = "Population", y = "Violent Crime Rate") +
  theme_minimal()

# Plot Property Crime Rate vs. Population excluding outliers in both axes
ggplot(data_frame, aes(x = Population, y = Property_Crime_Rate)) +
  geom_point() +
  scale_x_continuous(limits = c(0, upper_limit_Population), breaks = seq(0, upper_limit_Population, by = 10000)) +
  scale_y_continuous(limits = c(0, upper_limit_Property_Crime_Rate), breaks = seq(0, upper_limit_Property_Crime_Rate, by = 50)) +
  labs(title = "Property Crime Rate vs. Population (Excluding Outliers)", x = "Population", y = "Property Crime Rate") +
  theme_minimal()

############################################
## Section 2: Assigning Towns to Counties ##
## and Collecting Population Density      ##
############################################

library(rvest)
library(dplyr)
library(purrr)

# Function to fetch county information for a town
get_county_info <- function(town) {
  # Construct the URL
  url <- sprintf("https://en.wikipedia.org/wiki/%s,_Oklahoma", gsub(" ", "_", town))
  
  # Attempt to read the HTML content of the page
  webpage <- tryCatch({
    read_html(url)
  }, error = function(e) return(NA))
  
  # Check if webpage was successfully read
  if (is.na(webpage)) {
    return(NA)
  } else {
    # Extract the county information using the CSS selector
    county_info <- webpage %>% 
      html_node(".mergedrow+ .mergedrow .infobox-data") %>%
      html_text(trim = TRUE)
    
    # Return the county information, or NA if not found
    return(ifelse(is.na(county_info), NA, county_info))
  }
}

# List of towns
towns <- c("Achille", "Adair", "Alfalfa", "Allen", "Altus", "Alva", "Amber", "Anadarko", "Antlers", "Apache",
           "Arkoma", "Atoka", "Avant", "Barnsdall", "Beaver", "Beckham", "Beggs", "Bernice", "Big Cabin", "Billings",
           "Binger", "Bixby", "Blackwell", "Blaine", "Blanchard", "Boise City", "Bokoshe", "Boley", "Boswell",
           "Bristow", "Broken Bow", "Bryan", "Burns Flat", "Cache", "Caddo", "Calera", "Canadian", "Caney", "Carnegie",
           "Carney", "Carter", "Cashion", "Cement", "Chandler", "Chattanooga", "Checotah", "Chelsea", "Cherokee",
           "Chouteau", "Cimarron", "Clayton", "Cleveland", "Clinton", "Coal", "Coalgate", "Colbert", "Colcord",
           "Collinsville", "Comanche", "Commerce", "Cordell", "Cotton", "Covington", "Coweta", "Coyle", "Craig",
           "Creek", "Crescent", "Cushing", "Custer", "Cyril", "Davenport", "Davis", "Delaware", "Depew", "Dewar",
           "Dewey", "Dibble", "Dickson", "Drumright", "Duncan", "Earlsboro", "Elgin", "Elk City", "Ellis", "Elmore City",
           "Erick", "Eufaula", "Fairfax", "Fairland", "Fairview", "Fletcher", "Forest Park", "Fort Gibson", "Fort Towson",
           "Frederick", "Garfield", "Garvin", "Geary", "Goodwell", "Gore", "Grady", "Grandfield", "Granite", "Grant",
           "Greer", "Grove", "Guthrie", "Guymon", "Haileyville", "Harmon", "Harper", "Harrah", "Hartshorne", "Haskell",
           "Healdton", "Heavener", "Hennessey", "Henryetta", "Hinton", "Hobart", "Holdenville", "Hollis", "Hominy",
           "Hooker", "Howe", "Hughes", "Hugo", "Hulbert", "Hydro", "Jackson", "Jay", "Jefferson", "Jenks", "Jennings",
           "Johnston", "Jones", "Kellyville", "Kiefer", "Kingfisher", "Kingston", "Kiowa", "Konawa", "Krebs", "Lahoma",
           "Lamont", "Langley", "Langston", "Latimer", "Le Flore", "Lexington", "Lincoln", "Lindsay", "Locust Grove",
           "Logan", "Lone Grove", "Love", "Luther", "Madill", "Major", "Mangum", "Mannford", "Marietta", "Marlow",
           "Marshall", "Maud", "Mayes", "Maysville", "McClain", "McCurtain", "McIntosh", "McLoud", "Medicine Park",
           "Meeker", "Miami", "Minco", "Moffett", "Mooreland", "Morris", "Mounds", "Mountain View", "Muldrow", "Murray",
           "Nash", "Newcastle", "Newkirk", "Nichols Hills", "Nicoma Park", "Ninnekah", "Noble", "Norman", "North Enid",
           "Nowata", "Oilton", "Okarche", "Okemah", "Okfuskee", "Okmulgee", "Olustee", "Oologah", "Osage", "Ottawa",
           "Owasso", "Paoli", "Pauls Valley", "Pawhuska", "Pawnee", "Payne", "Perkins", "Perry", "Piedmont", "Pittsburg",
           "Pocola", "Pond Creek", "Porum", "Poteau", "Pottawatomie", "Prague", "Purcell", "Pushmataha", "Quinton",
           "Ramona", "Ringling", "Roger Mills", "Roland", "Rush Springs", "Salina", "Sallisaw", "Sapulpa", "Savanna",
           "Sawyer", "Sayre", "Seiling", "Seminole", "Sequoyah", "Shady Point", "Shattuck", "Skiatook", "Snyder",
           "South Coffeyville", "Sparks", "Spencer", "Sperry", "Spiro", "Sportsmen Acres", "Stephens", "Stigler",
           "Stillwater", "Stilwell", "Stonewall", "Stratford", "Stringtown", "Stroud", "Sulphur", "Tahlequah", "Talala")

# Fetch and store county information for each town
county_info <- map(towns, get_county_info)

# Create a data frame
county_df <- data.frame(Town = towns, County = unlist(county_info), stringsAsFactors = FALSE)

# Write the data to a CSV file
write.csv(county_df, "Oklahoma_Towns_and_Counties.csv", row.names = FALSE)

# Print completion message
print("Data extraction complete and saved to Oklahoma_Towns_and_Counties.csv")



########################################
## Section 2.1: Combining the list of ##
## Town-County and Town-Criminality   ##
########################################

library(dplyr)

# Read the CSV files into data frames
town_county <- read.csv("Oklahoma_Towns_and_Counties.csv", stringsAsFactors = FALSE)
crime_rates <- read.csv("Oklahoma.crime.rates.cleaned.csv", stringsAsFactors = FALSE)

# Rename the column in town_county that corresponds to the town names to match the crime_rates data frame
# This is necessary for the merging process. Adjust "Town" to the actual column name if different.
town_county <- rename(town_county, Town_Name = Town)

# Merge the data frames by town name
combined_data <- merge(town_county, crime_rates, by = "Town_Name", all = TRUE)

# Reorder columns to place the County column after the Town_Name column
combined_data <- combined_data %>%
  select(Town_Name, County, everything())

# Write the combined data frame to a new CSV file
write.csv(combined_data, "OK_Town_County_Crime.csv", row.names = FALSE)

# Print a completion message
print("Merged data has been saved to OK_Town_County_Crime.csv")

#############################################
## Section 2.2: Cleaning the Combined Data ##
#############################################

# Load necessary library
library(dplyr)

# Read the CSV file into a data frame
data <- read.csv("OK_Town_County_Crime.csv", stringsAsFactors = FALSE)

# Remove rows where the 'County' column is NA
# Assuming 'County' is column B, which is the second column
cleaned_data <- data %>%
  filter(!is.na(County))

# Alternatively, if you don't know the name and just want to use column index:
# cleaned_data <- data %>%
#   filter(!is.na(data[,2]))

# Write the cleaned data frame back to a CSV file
write.csv(cleaned_data, "OK_Town_County_Crime_Cleaned.csv", row.names = FALSE)

# Print a message indicating completion

print("Entries with NA in column 'B' have been removed and the cleaned data is saved.")

#########################################
## Section 2.3: Splitting and Counting ##
## the Counties  (should be <= 77)     ##
#########################################

# Load the necessary libraries
library(readr)
library(dplyr)
library(tidyr)

# Read the CSV file
data <- read_csv("C:/Users/alex_/Documents/OK_Town_County_Crime.csv")

# Create a new dataframe with the town names (assuming column A is Towns)
towns_counties <- data.frame(Town = data[[1]], County = data[[2]])

# Remove rows where County is "NA" or "county"
towns_counties <- towns_counties[!towns_counties$County %in% c("NA", "county"), ]

# Split the County entries at commas and expand into multiple rows
towns_counties <- towns_counties %>%
  separate_rows(County, sep = ",\\s*")

# Get unique town-county pairs
towns_counties <- unique(towns_counties)

# Write the cleaned and expanded data back to a new CSV file
write_csv(towns_counties, "C:/Users/alex_/Documents/Updated_OK_Town_County_Crime_Cleaned.csv")

# Optionally, print a message indicating completion
print("The CSV file has been updated and saved.")

# Load the necessary library
library(readr)
library(dplyr)

# Read the CSV file
data <- read_csv("C:/Users/alex_/Documents/Updated_OK_Town_County_Crime_Cleaned.csv")

# Access the "County" column
counties <- data$County

# Filter out "NA" and any placeholder that might be "County"
filtered_counties <- counties[!counties %in% c("NA", "County")]

# Get unique county names
unique_counties <- unique(filtered_counties)

# Count of unique counties
number_of_unique_counties <- length(unique_counties)

# Print the number of unique county entries
print(number_of_unique_counties)

########################################
## Section 3: Collecting County-Level ##
## Financial Data                     ##
########################################

library(httr)

# Retrieve the API key from environment variable
api_key <- Sys.getenv("US_Bureau_of_Economic_Analysis_API_Key")

# Check if the API key has been successfully retrieved
if (api_key == "") {
  stop("API Key not found. Please make sure it is set correctly in your environment variables.")
}

# Define the URL for the PDF
pdf_url <- "https://www.bea.gov/sites/default/files/2023-11/lapi1123.pdf"

# File path to save the downloaded PDF
output_file <- "Personal_Income_by_County_and_Metropolitan_Area_2022.pdf"

# Download the PDF file
download.file(pdf_url, destfile = output_file, mode = "wb")

# Print a message to confirm download
print(paste("The PDF has been successfully downloaded and saved as:", output_file))

# Install and load the pdftools package
if (!require(pdftools)) install.packages("pdftools")
library(pdftools)

# Specify the path to your PDF file using double backslashes
file_path <- "C:\\Users\\alex_\\Documents\\Personal_Income_by_County_and_Metropolitan_Area_2022.pdf"

# Correctly call pdf_text to extract text from the PDF
text <- pdf_text(file_path)

# Combine the text from all pages into a single character string
full_text <- paste(text, collapse = "\n")

# Write the text to a .txt file
write_file_path <- "Personal_Income_by_County_and_Metropolitan_Area_2022.txt"
writeLines(full_text, write_file_path)

# Print a message to indicate the file has been written
cat("Text has been extracted and saved to", write_file_path)

########################################
## Section 3.1: Reading the Text File ##
########################################

# Load necessary libraries
library(tidyverse)

# Specify the path to the text file
text_path <- "Personal_Income_by_County_and_Metropolitan_Area_2022.txt"

# Read the entire text file
text_data <- read_file(text_path)

# Find the position of the first "Oklahoma" and extract text after that
oklahoma_pos <- regexpr("Oklahoma", text_data)
text_after_oklahoma <- substring(text_data, oklahoma_pos + attr(oklahoma_pos, "match.length"))

# List of unique county names (you need to define this based on actual data you have)
unique_counties <- c("Adair", "Alfalfa", "Atoka", "Beaver", "Beckham", "Blaine", "Bryan", "Caddo", "Canadian", "Carter", "Cherokee", "Choctaw", "Cimarron", "Cleveland", "Coal", "Comanche", "Cotton", "Craig", "Creek", "Custer", "Delaware", "Dewey", "Ellis", "Garfield", "Garvin", "Grady", "Grant", "Greer", "Harmon", "Harper", "Haskell", "Hughes", "Jackson", "Jefferson", "Johnston", "Kay", "Kingfisher", "Kiowa", "Latimer", "Le Flore", "Lincoln", "Logan", "Love", "Major", "Marshall", "Mayes", "McClain", "McCurtain", "McIntosh", "Murray", "Muskogee", "Noble", "Nowata", "Okfuskee", "Oklahoma", "Okmulgee", "Osage", "Ottawa", "Pawnee", "Payne", "Pittsburg", "Pontotoc", "Pottawatomie", "Pushmataha", "Roger Mills", "Rogers", "Seminole", "Sequoyah", "Stephens", "Texas", "Tillman", "Tulsa", "Wagoner", "Washington", "Washita", "Woods", "Woodward")

# Pattern to match county names followed by numerical values (accounting for commas and optional decimals)
pattern <- paste(unique_counties, "\\s+([\\d,]+(?:\\.\\d+)?)", collapse = "|")
matches <- str_extract_all(text_after_oklahoma, pattern)[[1]]

# Create a dataframe from matches
county_data <- map_df(matches, ~{
  parts <- str_split(.x, "\\s+")[[1]]
  tibble(County = parts[1], Income = as.numeric(gsub(",", "", parts[2])))  # Remove commas and convert to numeric
})

# Print the dataframe
print(county_data)

#####################################
## Section 3.2: Adding Income Data ##
#####################################

# Load necessary libraries
library(readr)
library(dplyr)

# Read the CSV file
original_data <- read_csv("OK_Town_County_Crime_Cleaned.csv")

# Check the names of the columns to confirm their order
print(colnames(original_data))

# Swap the first and second columns, add an "Income" column, rename it, and sort by County
sorted_swapped_data <- original_data %>%
  select(County, Town_Name = Town_Name, everything()) %>%
  left_join(county_data, by = "County") %>%  # Join with income data
  relocate(Income, .after = County) %>%  # Move Income data right after the County column
  rename(`Per_Capita_Income` = Income) %>%  # Rename Income to Per Capita Income
  arrange(County)  # Sort the rows alphabetically by the County column

# Write the modified and sorted data to a new CSV file
write_csv(sorted_swapped_data, "3.2_County_Town_Crime_Income.csv")

# Print a message to confirm completion
print("The new CSV file with switched, sorted, and 'Per Capita Income' correctly labeled has been created.")

#########################################################
## Section 4: Constructing a Hierarchical Linear Model ##
#########################################################

# Load necessary libraries
library(lme4)
library(xtable)

# Load your data
crime_data <- read.csv("3.2_County_Town_Crime_Income.csv")

################################################
## Section 4.1: Model 1: Overall Offense Rate ##
################################################

# Define your model
model_offense <- lmer(Offense_Rate ~ Per_Capita_Income + Population + (1 | County), data = crime_data)

# Get the summary of the model
summary_offenses <- summary(model_offense)

# Create a dataframe to hold coefficients, including degrees of freedom
coefficients_df <- data.frame(
  Coefficient = rownames(summary_offense$coefficients),
  Estimate = summary_offense$coefficients[, "Estimate"],
  SE = summary_offense$coefficients[, "Std. Error"],
  df = summary_offense$coefficients[, "df"],
  T_value = summary_offense$coefficients[, "t value"],
  P_value = summary_offense$coefficients[, "Pr(>|t|)"]
)

# Use xtable to create a LaTeX table
latex_table <- xtable(coefficients_df, caption = "Summary of total crime rate model", align = "rlrrrrr")

# Print the table to console (or save to file)
print(latex_table, include.rownames = FALSE, sanitize.text.function = identity, hline.after = c(-1, 0, nrow(coefficients_df)))

# Optionally save the table to a .tex file
print.xtable(latex_table, file = "summary_offenses.tex", include.rownames = FALSE, caption.placement = "top")

##############################################
## Section 4.2: Model 2: Violent Crime Rate ##
##############################################

# Define your model
model_violent <- lmer(Violent_Crime_Rate ~ Per_Capita_Income + Population + (1 | County), data = crime_data)

# Get the summary of the model
summary_violent <- summary(model_violent)

# Create a dataframe to hold coefficients, including degrees of freedom
coefficients_df <- data.frame(
  Coefficient = rownames(summary_violent$coefficients),
  Estimate = summary_violent$coefficients[, "Estimate"],
  SE = summary_violent$coefficients[, "Std. Error"],
  df = summary_violent$coefficients[, "df"],
  T_value = summary_violent$coefficients[, "t value"],
  P_value = summary_violent$coefficients[, "Pr(>|t|)"]
)

# Use xtable to create a LaTeX table
latex_table <- xtable(coefficients_df, caption = "Summary of violent crime rate model", align = "rlrrrrr")

# Print the table to console (or save to file)
print(latex_table, include.rownames = FALSE, sanitize.text.function = identity, hline.after = c(-1, 0, nrow(coefficients_df)))

# Optionally save the table to a .tex file
print.xtable(latex_table, file = "summary_violent.tex", include.rownames = FALSE, caption.placement = "top")


###############################################
## Section 4.3: Model 3: Property Crime Rate ##
###############################################


# Define your model
model_property <- lmer(Property_Crime_Rate ~ Per_Capita_Income + Population + (1 | County), data = crime_data)

# Get the summary of the model
summary_property <- summary(model_property)

# Create a dataframe to hold coefficients, including degrees of freedom
coefficients_df <- data.frame(
  Coefficient = rownames(summary_property$coefficients),
  Estimate = summary_property$coefficients[, "Estimate"],
  SE = summary_property$coefficients[, "Std. Error"],
  df = summary_property$coefficients[, "df"],
  T_value = summary_property$coefficients[, "t value"],
  P_value = summary_property$coefficients[, "Pr(>|t|)"]
)

# Use xtable to create a LaTeX table
latex_table <- xtable(coefficients_df, caption = "Summary of property crime rate model", align = "rlrrrrr")

# Print the table to console (or save to file)
print(latex_table, include.rownames = FALSE, sanitize.text.function = identity, hline.after = c(-1, 0, nrow(coefficients_df)))

# Optionally save the table to a .tex file
print.xtable(latex_table, file = "summary_property.tex", include.rownames = FALSE, caption.placement = "top")
