# The purpose of this code is to merge data frames created from different codes used for this project

library(dplyr)

#Read in csv files
all_player_stats <- read.csv("all_player_stats.csv")
player_stat_links <- read.csv("player_stat_links.csv")
player_profiles <- read.csv("player_profiles.csv")

### Change Season formatting
# Show unique instances in the Season
unique(all_player_stats$season)

# Convert full years to yy/yy format
all_player_stats <- mutate(all_player_stats,
                           season = ifelse(nchar(season) == 4,
                                           paste(substr(season, 3, 4), "/", as.numeric(substr(season, 3, 4)) + 1, sep = ""),
                                           season))

# Show unique instances in Season
unique(all_player_stats$season)

# Convert yy/yy to yyyy/yy format
all_player_stats <- mutate(all_player_stats,
                           season = ifelse(nchar(season) == 5,
                                           paste0("20", substr(season, 1, 2), "/", substr(season, 4, 5)),
                                           season))

# Show unique instances in Season
unique(all_player_stats$season)

# Rename Birthdate column to Birthdate (Age)
player_profiles <- rename(player_profiles, `Birthdate (Age)` = Birthdate)


# Remove special characters and make lower case for name matching
player_profiles$Name <- tolower(gsub("[^a-zA-Z0-9 ]", "", player_profiles$Name))
all_player_stats$name <- tolower(gsub("[^a-zA-Z0-9 ]", "", all_player_stats$name))

# Merge the data frames
player_stats_profile <- merge(all_player_stats, player_profiles, by.x = "name", by.y = "Name", all = TRUE)

# View the first few rows of the merged data frame
head(player_stats_profile)

# Write CSV
write.csv(player_stats_profile, "player_stats_profile.csv", row.names=FALSE)
