# Install and load required packages
install.packages("jsonlite")
install.packages("tidyverse")
library(jsonlite)
library(tidyverse)

# Download the JSON file
system('wget -O dates.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20240209&lang=en"')

# Print the JSON file to the console
cat(readLines("dates.json"), sep = "\n")

# Convert the JSON file to a data frame
mylist <- fromJSON('dates.json')
mydf <- bind_rows(mylist$result[-1])

# Check the type of mydf and mydf$date
print(class(mydf))
print(class(mydf$date))

# List the first n rows of the mydf dataframe
n <- 5
print(head(mydf, n))
