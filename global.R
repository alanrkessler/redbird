library(shiny)
library(shinysky)
library(DT)
#library(ggvis)
library(stringr)

library(dplyr)
library(readr)

# Number of teams in the league
tms <- 18

# Import batters and pitchers
batters <- read_csv("./data/batters.csv")
pitchers <- read_csv("./data/pitchers.csv")

# Select variables using a standard naming and switch first and last names
b <- batters %>% 
  select(PlayerName, Team, POS, PA.IP = PA, ADP, Dollars) %>% 
  mutate(PlayerName = paste0(word(PlayerName, -1), 
                             ", ", word(PlayerName, 1, -2)))

p <- pitchers %>% 
  select(PlayerName, Team, POS, PA.IP = IP, ADP, Dollars) %>% 
  mutate(PlayerName = paste0(word(PlayerName, -1),
                             ", ", word(PlayerName, 1, -2)))

# Format Dollars 
all.players <- rbind(b, p) %>%  # Combine batters and pitchers data
  mutate(Dollars = as.numeric(gsub(" ", "", chartr('$)(', '  -', Dollars)))) %>%
  arrange(desc(Dollars)) %>%  # Sort by dollars descending
  dplyr::mutate(Ovr.Rank = 1:n())  # Create ranking id

# Clean up environment
rm(b, p, batters, pitchers)

# Create lists for selectInputs
# Positions including "All" (for draft sheet filter)
positions.all <- c("All", "C", "1B", "2B", "3B", "SS", "OF", "SP", "RP", "P")

# Create autocomplete list
autocomplete.list <- all.players %>% select(PlayerName)

# List for team drop down
teams.all <- c("All", as.character(sort(unique(all.players$Team))))

