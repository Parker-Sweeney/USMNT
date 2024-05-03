# This code cleans the player valuations file so that it can be further used for analysis

# Load necessary libraries
library(dplyr)
library(lubridate)
library(readr)

# Read data from CSV file
player_valuations <- read_csv("player_valuations.csv")

# Function To Format Date
adjust_and_format_date <- function(date) {
  # Convert to Date type if not already
  if (!inherits(date, "Date")) {
    date <- as.Date(date, format = "%Y-%m-%d")
  }
  
  # Make sure conversion happened
  if (is.na(date)) {
    return(NA)
  }
  
  # Format differently depending on month of year (better season allocation)
  if (month(date) < 8) {
    year_adjusted <- year(date) - 1
    # Format as year to year (previous to current)
    formatted_date <- paste0(year_adjusted, "/", format(date, "%y"))
  } else {
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
