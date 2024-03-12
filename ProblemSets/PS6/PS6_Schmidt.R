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