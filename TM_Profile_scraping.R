# The purpose of this code is to scrape player information from Transfermarkt's profile section

library(xml2)

# Set User Agent
user_agent <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.2.1 Safari/605.1.15"

# Set Current Page
current_page <- "https://www.transfermarkt.com/vereinigte-staaten/startseite/verein/3505"

# Read HTML
page<-read_html(current_page, user_agent)
Sys.sleep(5)

# Make sure page loads
class(page)
page

# Get the first player name and link in player list
player_profile_links <- xml_attr(xml_find_all(page, "//*[@id='yw1']//tr/td[contains(@class, 'hauptlink')]/a"), "href")

# Combine player names and links into a df
player_profile_links <- data.frame(player_profile_links = player_profile_links)

# Print extracted player data
print(player_profile_links)

# This code contains two links (profile and market value) 
# we only need the profile one, so this will clean the data frame for the links we need

# Remove unnecessary rows (even rows)
player_profile_links <- player_profile_links[c(TRUE, FALSE), , drop = FALSE]
player_profile_links <- as.data.frame(player_profile_links)

# Make full link
base_url <- "https://www.transfermarkt.com"
player_profile_links$player_profile_links <- paste(base_url, player_profile_links$player_profile_links, sep = "")

###########################################

# Function to Scrape Player Details
scrape_player_details <- function(url) {
  # Read the HTML
  page <- read_html(url, user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36")
  
  # Set variables to store scraped data
  birth_date <- NA
  nationality <- NA
  birthplace <- NA
  height <- NA
  position <- NA
  
  # Scrape Each Element (Note: Contains Error Handlers for Debugging)
  # Had issues with this section, hence the error handlers
  tryCatch({
    birth_date <- trimws(page %>%
                           html_nodes("span[itemprop='birthDate']") %>%
                           html_text())
  }, error = function(e) {
    cat("Error while scraping birth date for URL:", url, "\n")
  })
  
  tryCatch({
    nationality <- trimws(page %>%
                            html_nodes("span[itemprop='nationality']") %>%
                            html_text())
  }, error = function(e) {
    cat("Error while scraping nationality for URL:", url, "\n")
  })
  
  tryCatch({
    birthplace <- trimws(page %>%
                           html_nodes("span[itemprop='birthPlace']") %>%
                           html_text())
  }, error = function(e) {
    cat("Error while scraping birthplace for URL:", url, "\n")
  })
  
  tryCatch({
    height <- trimws(page %>%
                       html_nodes("span[itemprop='height']") %>%
                       html_text())
  }, error = function(e) {
    cat("Error while scraping height for URL:", url, "\n")
  })
  
  tryCatch({
    position <- trimws(page %>%
                         html_nodes("ul:nth-of-type(2) li:nth-of-type(2) span.data-header__content") %>%
                         html_text())
  }, error = function(e) {
    cat("Error while scraping position for URL:", url, "\n")
  })
  
  # Create a data frame for player details
  player_details <- data.frame(
    Birthdate = ifelse(length(birth_date) > 0, birth_date, NA),
    BirthPlace = ifelse(length(birthplace) > 0, birthplace, NA),
    Nationality = ifelse(length(nationality) > 0, nationality, NA),
    Height = ifelse(length(height) > 0, height, NA),
    Position = ifelse(length(position) > 0, position, NA),
    stringsAsFactors = FALSE
  )
  
  return(player_details)
}

# Set an empty list to store player details data frames
all_player_details <- list()

# Loop through each player profile link
for (url in player_profile_links$player_profile_links) {
  # Scrape player details
  player_details <- scrape_player_details(url)
  
  # Append player details to the list
  all_player_details <- c(all_player_details, list(player_details))
  
  # Pause
  Sys.sleep(3)
}

# Combine all player details into a single data frame
all_player_details_df <- do.call(rbind, all_player_details)

# Print the data frame
print(all_player_details_df)

###############################################################

# Reset row names of player_profile_links
rownames(player_profile_links) <- NULL

# Merge the data frames
merged_profile_data <- merge(player_profile_links, all_player_details_df, by = "row.names")

# Remove the row.names column
merged_profile_data <- merged_profile_data[, -c(which(names(merged_profile_data) == "Row.names"))]

# Add new name column
merged_profile_data$Name <- gsub("https://www.transfermarkt.com/|/profil/spieler/.*", "", merged_profile_data$player_profile_links)
merged_profile_data$Name <- gsub("-", " ", merged_profile_data$Name)
merged_profile_data$Name <- tolower(merged_profile_data$Name)
merged_profile_data$Name <- sapply(strsplit(merged_profile_data$Name, " "), function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "", collapse = " "))

# Print the modified data frame
print(merged_profile_data)

# Extract Player ID from player_profile_links column
merged_profile_data$Player_ID <- gsub(".*/", "", merged_profile_data$player_profile_links)

# Print the modified data frame
print(merged_profile_data)
