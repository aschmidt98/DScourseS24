library(httr)

api_key <- Sys.getenv("EXCHANGE_RATE_API")

if (api_key != "") {
  print(paste("API key is:", api_key))
  
  # Construct the URL for the API request
  url <- paste0("https://v6.exchangerate-api.com/v6/", api_key, "/latest/USD")
  
  # Make the GET request
  response <- GET(url)
  
  # Check the status code of the response
  if (status_code(response) == 200) {
    # Parse the response content if the request was successful
    content <- content(response, "text")
    print(content)
  } else {
    print(paste("Request failed with status code", status_code(response)))
  }
  
} else {
  print("API key is not loaded.")
}

library(jsonlite)

# Parse the JSON response
parsed_response <- fromJSON(content)

# Access the conversion rates
conversion_rates <- parsed_response$conversion_rates

# Convert the conversion rates to a data frame
conversion_rates_df <- as.data.frame(conversion_rates)

# Write the data frame to a CSV file
write.csv(conversion_rates_df, "conversion_rates.csv", row.names = FALSE)

# Print a message to indicate that the file has been saved
print("Conversion rates saved to 'conversion_rates.csv'.")
