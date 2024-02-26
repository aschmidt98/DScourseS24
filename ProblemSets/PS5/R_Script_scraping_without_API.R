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
  data_frame <- data.frame(Town_Name = Name, Population = Population, Total_Offenses = Offenses, Offense_Rate = Rate, Violent_Crimes = Violent, Violent_Crime_Rate = V_rate, Property_Crimes = Property, Property_Crime_Rate = P_Rate)
  # Write the data frame to a CSV file
  write.csv(data_frame, "my_data.csv", row.names = FALSE)
  print("Successfully created file.")
} else {
  print("Vectors have differing lengths. Cannot create data frame.")
}



