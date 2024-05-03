# This code loads the combined data and adjusts it for easier analysis
# This code analyzes data about the datasets Market Values, Ages, Positions, Seasons, and Appearances

library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(scales)
library(lubridate)

# Read TM and BeSoccer Combined Table
combined_data <- read_csv("combined_data.csv", na = c("NA", "missing", "-"))

# Drop Unused columns and Group
combined_data <- combined_data %>%
  select(-league, -birthplace, -nationality, -player_profile_links) %>%
  group_by(player_id, season, name, team, position, height, birthdate) %>%
  summarise(
    avg_points = mean(avg_pts, na.rm = TRUE),
    avg_elos = mean(avg_elo, na.rm = TRUE),
    avg_market_value = mean(market_value_in_eur, na.rm = TRUE),
    .groups = 'drop'
  )

# Edit BD
combined_data$birthdate <- as.Date(sub(" \\(.*\\)$", "", combined_data$birthdate), format="%b %d, %Y")

### Create Age Column
# Get the year from the season
combined_data$season_year <- as.numeric(substr(combined_data$season, 1, 4))

# Get the year from the birthdate
combined_data$birth_year <- as.numeric(format(combined_data$birthdate, "%Y"))

# Calculate the age
combined_data$age <- combined_data$season_year - combined_data$birth_year

# Format Season Year
combined_data$season_year <- as.Date(paste(combined_data$season_year, "-01-01", sep=""), format="%Y-%m-%d")

# Get Rid of Columns
combined_data <- combined_data %>%
  select(-birth_year, -birthdate)

# Set nulls
combined_data$avg_market_value <- ifelse(combined_data$avg_market_value == "NaN", NA, combined_data$avg_market_value)

# Read Stats File
stats_data <- read_csv("player_stats_profile.csv")

# Correct Col Name
stats_data <- rename(stats_data, player_id = Player_ID)

# Get Rid of "-"
stats_data$appearances <- ifelse(stats_data$appearances == "-", NA, stats_data$appearances)
stats_data$goals <- ifelse(stats_data$goals == "-", NA, stats_data$goals)
stats_data$cards <- ifelse(stats_data$cards == "-", NA, stats_data$cards)

# Change Data Types
stats_data$appearances <- as.numeric(stats_data$appearances)
stats_data$goals <- as.numeric(stats_data$goals)

# Drop Columns
stats_data <- stats_data %>%
  select(-name, 
         -player_profile_links,
         -`Birthdate (Age)`,
         -BirthPlace,
         -Nationality,
         -Height,
         -Position,
         -cards) %>%
  group_by(player_id, season) %>%
  summarise(
    total_goals = sum(goals, na.rm = TRUE),
    total_appearances = sum(appearances, na.rm = TRUE),
    competition_count = n(),
    #.groups = 'drop'
  )

# Get Dataset to Analyze
data_with_stats <- left_join(combined_data, stats_data, by = c("player_id", "season"))

##############
# Analysis

# Omit NAs
clean_data <- na.omit(data_with_stats)

# Clean the Height Variable
clean_data$height <- as.numeric(gsub(",", ".", gsub("m", "", clean_data$height)))  # Remove 'm' and replace comma

lm_data <- clean_data

### Overall Linear Regression ###
# Create LR Model
linear_model <- lm(avg_market_value ~ position + height + avg_elos + avg_points + total_goals + total_appearances + competition_count + age, data = lm_data)

# Summarize Model
summary(linear_model)

# Predict market values using the model
lm_data$predicted_market_value <- predict(linear_model, newdata = lm_data)

# Add actual market value to data frame
lm_data$actual_market_value <- lm_data$avg_market_value

# Plot actual vs. predicted market values
plot <- ggplot(lm_data, aes(x = actual_market_value, y = predicted_market_value)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", linetype = "dashed") +
  ggtitle("Actual vs. Predicted Market Values") +
  xlab("Actual Market Value") +
  ylab("Predicted Market Value") +
  theme_minimal() +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", color = "red") +
  scale_x_continuous(labels = dollar_format(prefix = "$", suffix = "", big.mark = ",")) +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "", big.mark = ",")) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(vjust = -0.2),
    axis.title.y = element_text(vjust = 1.1)
  )

# Print the plot
print(plot)

###############################

#summary statistics of data_clean (as a whole)
str(clean_data)
summary(clean_data)

#summary statistics for each position (that we have data for)
position_summary <- clean_data %>%
  group_by(position) %>%
  summarise(
    avg_market_value = mean(avg_market_value, na.rm = TRUE),
    med_market_value = median(avg_market_value, na.rm = TRUE),
    avg_points = mean(avg_points, na.rm = TRUE),
    avg_elos = mean(avg_elos, na.rm = TRUE),
    total_appearances = sum(total_appearances, na.rm = TRUE),
    competition_count = sum(competition_count, na.rm = TRUE)
  )

# Boxplot for market value by position
ggplot(clean_data, aes(x = position, y = avg_market_value)) +
  geom_boxplot() +
  labs(title = "Market Value by Position") +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "", big.mark = ",")) +
  theme_minimal()

# Scatter plot of points vs. market value, colored by position
ggplot(clean_data, aes(x = avg_points, y = avg_market_value, color = position)) +
  geom_point() +
  labs(title = "Avg Points vs. Market Value by Position") +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "", big.mark = ",")) +
  theme_minimal()

# Line plot of market value over seasons by position
ggplot(clean_data, aes(x = season, y = avg_market_value, color = position, group = position)) +
  geom_line() +
  labs(title = "Market Value Over Seasons by Position") +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "", big.mark = ",")) +
  theme_minimal()

###

#correlation analysis
cor_results <- cor(clean_data %>%
                     select(avg_market_value, avg_points, avg_elos, total_appearances), use = "complete.obs")
cor_results

# Multiple linear regression with other factors
lm2 <- lm(avg_market_value ~ avg_points + total_appearances + position, data = clean_data)
summary(lm2)



##############################
segment_data <- clean_data %>%
  select(-player_id,
         -season,
         -name,
         -team,
         -height,
         -avg_points,
         -avg_elos,
         -season_year,
         -total_goals,
         -competition_count)

### Segmented Analysis
# MV Segmented Analysis by Appearances and Position
plot_segmented_apps <- ggplot(segment_data, aes(x = total_appearances, y = avg_market_value, color = age)) +
  geom_point() +
  facet_wrap(~position) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "", big.mark = ",")) +
  labs(title = "Market Value by Appearances, Split by Position")
print(plot_segmented_apps)

# MV Segmented Analysis by Age and Position
plot_segmented_age <- ggplot(segment_data, aes(x = age, y = avg_market_value, color = total_appearances)) +
  geom_point() +
  facet_wrap(~position) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "", big.mark = ",")) +
  labs(title = "Market Value by Age, Split by Position")
print(plot_segmented_age)

# MV Analysis of Age
plot_age <- ggplot(segment_data, aes(x = age, y = avg_market_value, color = total_appearances)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "", big.mark = ",")) +
  labs(title = "Market Value by Age")
print(plot_age)

# Ages Per Season Box Plot
ggplot(clean_data, aes(x = factor(season), y = age)) +
  geom_boxplot() +
  labs(title = "Distribution of Player Ages per Season",
       x = "Season",
       y = "Age") +
  theme_minimal()
