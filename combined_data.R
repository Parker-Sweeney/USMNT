# The purpose of this code is to combine the data collected from other codes for this project

library(readr)
library(dplyr)
library(stringr)
library(stringdist)
library(fuzzyjoin)
library(scales)


# Read data
elo <- read_csv("player_elo_and_points.csv")
valuations <- read_csv("modified_player_valuations.csv")
player_profiles <- read_csv("player_profiles.csv")

# Convert names to lower case
elo$Name <- tolower(elo$Name)
player_profiles$Name <- tolower(player_profiles$Name)

# Change Names
player_profiles$Name <- gsub("\\bmatt\\b", "matthew", player_profiles$Name)
player_profiles$Name <- gsub("\\bjoe\\b", "joseph", player_profiles$Name)
elo$Name <- gsub("\\bjoo\\b", "johnny", elo$Name)
elo$Name <- gsub("\\bjoão lucas de souza\\b", "johnny", elo$Name)
player_profiles$Name <- gsub("\\bchris\\b", "christopher", player_profiles$Name)
player_profiles$Name <- gsub("\\btim\\b", "timothy", player_profiles$Name)
player_profiles$Name <- gsub("\\bsergino\\b", "sergio", player_profiles$Name)

# Function to clean and standardize names
clean_names <- function(name) {
  name %>%
    str_to_lower() %>%
    str_replace_all("[^a-z\\s]", "") %>%
    str_squish()
}

# Apply cleaning
elo$Name <- sapply(elo$Name, clean_names)
player_profiles$Name <- sapply(player_profiles$Name, clean_names)

# Extract first and last names
get_first_last <- function(name) {
  parts <- unlist(str_split(name, "\\s+"))
  if (length(parts) > 1) {
    paste0(parts[1], " ", tail(parts, n=1))
  } else {
    name
  }
}

elo$Name <- sapply(elo$Name, get_first_last)
player_profiles$Name <- sapply(player_profiles$Name, get_first_last)

# Merge Data
exact_match <- merge(elo, player_profiles, by = "Name")

# Fuzzy match on unmatched names
unmatched_elo <- anti_join(elo, exact_match, by = "Name")
unmatched_profiles <- anti_join(player_profiles, exact_match, by = "Name")

fuzzy_match <- stringdist_inner_join(unmatched_elo, unmatched_profiles, by = "Name", max_dist = 2)

# Combine exact matches and fuzzy matches
elo_player_matched <- bind_rows(exact_match, fuzzy_match)

# Rename Column Names
valuations <- rename(valuations, Player_ID = player_id)
valuations <- rename(valuations, Season = date)

# Left Join
final_df <- left_join(elo_player_matched, valuations, by = c("Player_ID", "Season"))

# Drop Unused columns
final_df <- final_df %>%
  select(-Name.x, -Name.y)

# Reorder Columns
final_df <- final_df %>%
  select(Player_ID, Name, Season, Team, player_club_domestic_competition_id, Avg_Pts, Avg_ELO, market_value_in_eur, Position, Height, Birthdate, BirthPlace, Nationality, player_profile_links, everything())

# Round Numbers
final_df <- final_df %>%
  mutate(Avg_ELO = round(Avg_ELO, 2),
         Avg_Pts = round(Avg_Pts, 2))

# Rename Columns
final_df <- rename(final_df, League = player_club_domestic_competition_id)

final_df <- final_df %>%
  rename_with(tolower, .cols = everything())

# Change MV to currency (Ended up commenting out)
#final_df <- final_df %>%
#  mutate(market_value_in_eur = dollar_format(prefix = "€", big.mark = ",", decimal.mark = ".")(market_value_in_eur))
