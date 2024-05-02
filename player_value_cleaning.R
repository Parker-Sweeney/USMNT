# Load necessary libraries
library(dplyr)
library(lubridate)
library(readr)

# Read data from CSV file
player_valuations <- read_csv("player_valuations.csv")

# Define a function to adjust and format the date
adjust_and_format_date <- function(date) {
  # Convert to Date object if it's not already
  if (!inherits(date, "Date")) {
    date <- as.Date(date, format = "%Y-%m-%d")
  }
  
  # Check if the date conversion was successful
  if (is.na(date)) {
    return(NA)  # Return NA if the date conversion fails
  }
  
  # Determine the formatted date based on the month
  if (month(date) < 8) {
    # Subtract one year
    year_adjusted <- year(date) - 1
    # Format as year to year (previous to current)
    formatted_date <- paste0(year_adjusted, "/", format(date, "%y"))
  } else {
    # Add one year
    year_adjusted <- year(date) + 1
    # Format as year to year (current to next)
    formatted_date <- paste0(year(date), "/", format(as.Date(paste0(year_adjusted, "-01-01")), "%y"))
  }
  return(formatted_date)
}

# Apply the function to the date column
player_valuations <- player_valuations %>%
  mutate(date = sapply(date, adjust_and_format_date, USE.NAMES = FALSE))

# Drop the current_club_id column
player_valuations <- player_valuations %>%
  select(-current_club_id)
