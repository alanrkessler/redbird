library(shiny)
library(shinysky)
library(DT)
library(stringr)
library(dplyr)
library(readr)

# Number of teams in the league
tms <- 18

# Import batters and pitchers auction calculator results
# Create standard naming and switch first and last names
b <- read_csv("./data/batters.csv") %>% 
  select(PlayerName, Team, POS, PA.IP = PA, ADP, Dollars) %>% 
  mutate(PlayerName = paste0(word(PlayerName, -1), 
                             ", ", word(PlayerName, 1, -2)))

p <- read_csv("./data/pitchers.csv") %>% 
  select(PlayerName, Team, POS, PA.IP = IP, ADP, Dollars) %>% 
  mutate(PlayerName = paste0(word(PlayerName, -1),
                             ", ", word(PlayerName, 1, -2)))

# Format Dollars 
allPlayers <- rbind(b, p) %>%  # Combine batters and pitchers data
  mutate(Dollars = as.numeric(gsub(" ", "", chartr('$)(', '  -', Dollars)))) %>%
  arrange(desc(Dollars)) # Sort by dollars descending

# Clean up environment
rm(b, p)

# Create autocomplete list
autocompleteList <- allPlayers %>% select(PlayerName)

# Positions including "All" (for draft sheet filter)
positionsAll <- c("All", "C", "1B", "2B", "3B", "SS", "OF", "SP", "RP", "P")

# List for team drop down
teamsAll <- c("All", as.character(sort(unique(allPlayers$Team))))
