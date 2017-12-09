library(shiny)
library(shinysky)
library(DT)
library(stringr)
library(dplyr)
library(readr)

# Number of teams in the league
tms <- 18
# Number of weeks in the season
wks <- 24

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

# Import Depth Chart Projections
dcb <- read_csv("./data/dc_batters.csv") %>%
  select(Name, Team, PA, AB, H, R, SB, RBI, HR) %>%
  mutate(PlayerName = paste0(word(Name, -1), 
                      ", ", word(Name, 1, -2)),
         AVG = H / AB) %>%
  select(PlayerName, Team, PA, AB, H, HR, R, RBI, SB, AVG)

dcp <- read_csv("./data/dc_pitchers.csv") %>%
  select(Name, Team, IP, W, SV, SO, ER, BB, H) %>%
  mutate(PlayerName = paste0(word(Name, -1), 
                             ", ", word(Name, 1, -2)),
         WHIP = (H + BB) / IP,
         ERA = ER * 9 / IP,
         BB_H = H + BB) %>%
  select(PlayerName, Team, IP, W, SV, SO, ER, BB_H, WHIP, ERA)
  
# Clean up environment
rm(b, p)

# Create autocomplete list
autocompleteList <- allPlayers %>% select(PlayerName)

# Positions including "All" (for draft sheet filter)
positionsAll <- c("All", "C", "1B", "2B", "3B", "SS", "OF", "SP", "RP", "P")

# List for team drop down
teamsAll <- c("All", as.character(sort(unique(allPlayers$Team))))




