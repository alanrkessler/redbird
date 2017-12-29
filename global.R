library(shiny)
library(shinysky)
library(DT)
library(stringr)
library(dplyr)
library(readr)

# todo: Add ranking

# Players with the same name is an issue not worth fixing for this league
# Types of stats is an issue
# Positions and number of teams is an issue

# Number of teams in the league
tms <- 18
# Number of weeks in the season
wks <- 24
# Random seed
set.seed(9615)
# Number of simulations
n <- 1000
# Min innings pitched in a week
minInnings <- 18

# Positions including "All" (for draft sheet filter)
positionsAll <- c("All", "C", "1B", "2B", "3B", "SS", "OF", "SP", "RP", "P")

# Import data
source("imports.R")

# Create baseline
source("baseline.R")

# Simulate weeks for individual players
source("sims.R")

# Aggregate simulations
source("aggregation.R")