#Haleigh Allyn, Parker Sweeney
#Final Project: Transfermarkt Statistics Scraping Code


library(xml2)

#Haleigh Allyn User Agent
user_agent <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.2.1 Safari/605.1.15"

#Trying one page, attempt loop later
current_page <- "https://www.transfermarkt.com/vereinigte-staaten/startseite/verein/3505"

page<-read_html(current_page, user_agent)
Sys.sleep(5)

#Check to make sure the page loads
class(page)
page

#Attempting to get the first player name and link in the list of players

player_links <- xml_attr(xml_find_all(page, "//*[@id='yw1']//tr/td[contains(@class, 'hauptlink')]/a"), "href")

# Combine player names and links into a data frame
player_links <- data.frame(Link = player_links)

# Print the extracted player data
print(player_links)

#This code contains two links (profile and market value) 
#we only need the profile one, so Im going to clean the data frame for the links we need

#Remove unnecessary rows (even rows)
player_links <- player_links[c(TRUE, FALSE), , drop = FALSE]

#Fix scraped links to go to their detailed stats page
player_links <- gsub("/profil/", "/leistungsdatendetails/", player_links$Link)
player_links <- as.data.frame(player_links)

#Make full link
base_url <- "https://www.transfermarkt.com"
player_links$player_links <- paste(base_url, player_links$player_links, sep = "")


####################################

# Code for scraping one players stats table

# Define a function to scrape player information 
scrape_player_info <- function(url) {
  # Read the HTML content of the player's page
  page <- read_html(url)
  
  # Extract the player's name from the title of the page
  title <- xml_find_first(page, "//title")
  player_name <- xml_text(title)
  player_name <- strsplit(player_name, " - ")[[1]][1]
  
  # Extract all rows of the player's performance table
  player_table_rows <- xml_find_all(page, "//div[@class='grid-view']/table/tbody/tr")
  
  # Initialize empty lists to store player information
  player_season <- vector("character", length = length(player_table_rows))
  player_competition <- vector("character", length = length(player_table_rows))
  player_appearances <- vector("character", length = length(player_table_rows))
  player_goals <- vector("character", length = length(player_table_rows))
  player_cards <- vector("character", length = length(player_table_rows))
  
  # Loop through each row of the table and extract information
  for (i in seq_along(player_table_rows)) {
    player_season[i] <- xml_text(xml_find_first(player_table_rows[[i]], ".//td[1]"))
    player_competition[i] <- xml_text(xml_find_first(player_table_rows[[i]], ".//td[3]"))
    player_appearances[i] <- xml_text(xml_find_first(player_table_rows[[i]], ".//td[5]"))
    player_goals[i] <- xml_text(xml_find_first(player_table_rows[[i]], ".//td[6]"))
    player_cards[i] <- xml_text(xml_find_first(player_table_rows[[i]], ".//td[7]"))
  }
  
  # Combine the player information into a data frame
  player_info <- data.frame(
    name = player_name,
    season = player_season,
    competition = player_competition,
    appearances = player_appearances,
    goals = player_goals,
    cards = player_cards
  )
  
  # Remove leading and trailing whitespace from all columns
  player_info <- apply(player_info, 2, trimws)
  
  return(player_info)
}

# Example usage
url <- "https://www.transfermarkt.com/matt-turner/leistungsdatendetails/spieler/425306"
player_info <- scrape_player_info(url)
print(head(player_info))  # Display the first few rows of player information













##################################

library(httr)  # For Sys.sleep()

# Define a function to scrape player information
scrape_all_player_stats <- function(player_links_df, delay = 3) {
  # Initialize an empty list to store player information data frames
  all_player_info <- list()
  
  # Loop through each player link in the dataframe
  for (url in player_links_df$player_links) {
    # Read the HTML content of the player's page
    page <- read_html(url)
    
    # Extract the player's name from the title of the page
    title <- xml_find_first(page, "//title")
    player_name <- xml_text(title)
    player_name <- strsplit(player_name, " - ")[[1]][1]
    
    # Extract all rows of the player's performance table
    player_table_rows <- xml_find_all(page, "//div[@class='grid-view']/table/tbody/tr")
    
    # Initialize empty lists to store player information
    player_season <- vector("character", length = length(player_table_rows))
    player_competition <- vector("character", length = length(player_table_rows))
    player_appearances <- vector("character", length = length(player_table_rows))
    player_goals <- vector("character", length = length(player_table_rows))
    player_cards <- vector("character", length = length(player_table_rows))
    
    # Loop through each row of the table and extract information
    for (i in seq_along(player_table_rows)) {
      player_season[i] <- xml_text(xml_find_first(player_table_rows[[i]], "./td[1]"))
      player_competition[i] <- xml_text(xml_find_first(player_table_rows[[i]], "./td[3]"))
      player_appearances[i] <- xml_text(xml_find_first(player_table_rows[[i]], "./td[5]"))
      player_goals[i] <- xml_text(xml_find_first(player_table_rows[[i]], "./td[6]"))
      player_cards[i] <- xml_text(xml_find_first(player_table_rows[[i]], "./td[7]"))
    }
    
    # Combine the player information into a data frame
    player_info <- data.frame(
      name = player_name,
      season = player_season,
      competition = player_competition,
      appearances = player_appearances,
      goals = player_goals,
      cards = player_cards
    )
    
    # Append the player information data frame to the list
    all_player_info <- c(all_player_info, list(player_info))
    
    # Sleep for specified number of seconds
    Sys.sleep(delay)
  }
  
  # Combine all player information data frames into a single data frame
  all_player_info_df <- do.call(rbind, all_player_info)
  return(all_player_info_df)
}

# Example usage with a delay of 3 seconds between requests
all_player_stats <- scrape_all_player_stats(player_links, delay = 3)
print(head(all_player_stats))  # Display the first few rows of player information


