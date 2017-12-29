#####
# Create baseline and bReplacement level placeholders
#####

# Set of batters to extract players from without bReplacement
bnr <- allPlayers[grepl("P", allPlayers$POS) == FALSE, ]

setClass(Class = "Baseline", 
         representation(players = "data.frame", remaining = "data.frame"))

bReplacement1 <- function(pos, num, data){
  # List of players eligible for positions in priority order
  pList <- data[grepl(pos, bnr$POS) == TRUE, ]
  
  # Middle third of draftees for the average
  compRange <- round(quantile(1:tms*num, 0.33)):round(quantile(1:tms*num, 0.67))
  
  # Baseline level player creation
  comp <- dcb[dcb$PlayerName %in% pList[compRange, ]$PlayerName, ] %>%
    summarise_if(is.numeric, mean) %>%
    mutate_at(c("PA", "AB", "H", "HR", "R", "RBI", "SB"), round, 0) %>%
    mutate(PlayerName = paste0("Baseline, ", pos),
           Team = "",
           H = round(AVG * AB, 0)) %>%
    select(PlayerName, Team, PA, AB, H, HR, R, RBI, SB, AVG)
  
  # Remove players already considered in the above position(s)
  return(new("Baseline", players = comp, 
             remaining = data[!(data$PlayerName %in% pList[compRange, ]$PlayerName), ]))
  
}

bBaseline <- bReplacement1("C", 1, bnr)
bCompetitor <- bBaseline@players
bnr <- bBaseline@remaining

bBaseline <- bReplacement1("SS", 1, bnr)
bCompetitor <- rbind(bCompetitor, bBaseline@players)
bnr <- bBaseline@remaining

bBaseline <- bReplacement1("2B", 1, bnr)
bCompetitor <- rbind(bCompetitor, bBaseline@players)
bnr <- bBaseline@remaining

bBaseline <- bReplacement1("3B", 1, bnr)
bCompetitor <- rbind(bCompetitor, bBaseline@players)
bnr <- bBaseline@remaining

bBaseline <- bReplacement1("OF", 3, bnr)
bCompetitor <- rbind(bCompetitor, bBaseline@players)
bnr <- bBaseline@remaining

bBaseline <- bReplacement1("1B", 1, bnr)
bCompetitor <- rbind(bCompetitor, bBaseline@players)
bnr <- bBaseline@remaining

bReplacement2 <- function(pos, num, data){
  # List of players eligible for positions in priority order
  pList <- data[grepl(pos, bnr$POS) == TRUE, ]
  
  # Back third of draftees for the average
  compRange <- round(quantile(1:tms*num, 0.67)):round(quantile(1:tms*num, 0.99))
  
  # Replacement level player creation
  repl <- dcb[dcb$PlayerName %in% pList[compRange, ]$PlayerName, ] %>%
    summarise_if(is.numeric, mean) %>%
    mutate_at(c("PA", "AB", "H", "HR", "R", "RBI", "SB"), round, 0) %>%
    mutate(PlayerName = paste0("Replacement, ", pos),
           Team = "",
           H = round(AVG * AB, 0)) %>%
    select(PlayerName, Team, PA, AB, H, HR, R, RBI, SB, AVG)
  
  # Remove players already considered in the above position(s)
  return(new("Baseline", players = repl, 
             remaining = data[!(data$PlayerName %in% pList$PlayerName), ]))
  
}

bBaseline <- bReplacement2("C", 1, bnr)
bCompetitor <- rbind(bCompetitor, bBaseline@players)

bBaseline <- bReplacement2("SS", 1, bnr)
bCompetitor <- rbind(bCompetitor, bBaseline@players)

bBaseline <- bReplacement2("2B", 1, bnr)
bCompetitor <- rbind(bCompetitor, bBaseline@players)

bBaseline <- bReplacement2("3B", 1, bnr)
bCompetitor <- rbind(bCompetitor, bBaseline@players)

bBaseline <- bReplacement2("OF", 3, bnr)
bCompetitor <- rbind(bCompetitor, bBaseline@players)

bBaseline <- bReplacement2("1B", 1, bnr)
bCompetitor <- rbind(bCompetitor, bBaseline@players)

# Append to projections
dcb <- rbind(dcb, bCompetitor)

# Set of pitchers to extract players from without bReplacement
pnr <- allPlayers[grepl("P", allPlayers$POS) == TRUE, ]

pReplacement1 <- function(pos, num, data){
  # List of players eligible for positions in priority order
  pList <- data[grepl(pos, pnr$POS) == TRUE, ]
  
  # Middle third of draftees for the average
  compRange <- round(quantile(1:tms*num, 0.33)):round(quantile(1:tms*num, 0.67))
  
  # Baseline level player creation
  comp <- dcp[dcp$PlayerName %in% pList[compRange, ]$PlayerName, ] %>%
    summarise_if(is.numeric, mean) %>%
    mutate_at(c("G", "IP", "W", "SV", "SO", "ER", "BB_H"), round, 0) %>%
    mutate(PlayerName = paste0("Baseline, ", pos),
           Team = "",
           BB_H = round(WHIP * IP, 0),
           ER = ERA * IP / 9) %>%
    select(PlayerName, Team, G, IP, W, SV, SO, ER, BB_H, WHIP, ERA)
  
  # Remove players already considered in the above position(s)
  return(new("Baseline", players = comp, 
             remaining = data[!(data$PlayerName %in% pList[compRange, ]$PlayerName), ]))
  
}

pBaseline <- pReplacement1("SP", 4, pnr)
pCompetitor <- pBaseline@players
pnr <- pBaseline@remaining

pBaseline <- pReplacement1("RP", 1, pnr)
pCompetitor <- rbind(pCompetitor, pBaseline@players)
pnr <- pBaseline@remaining

pBaseline <- pReplacement1("P", 2, pnr)
pCompetitor <- rbind(pCompetitor, pBaseline@players)
pnr <- pBaseline@remaining

pReplacement2 <- function(pos, num, data){
  # List of players eligible for positions in priority order
  pList <- data[grepl(pos, pnr$POS) == TRUE, ]
  
  # Back third of draftees for the average
  compRange <- round(quantile(1:tms*num, 0.67)):round(quantile(1:tms*num, 0.99))
  
  # Baseline level player creation
  repl <- dcp[dcp$PlayerName %in% pList[compRange, ]$PlayerName, ] %>%
    summarise_if(is.numeric, mean) %>%
    mutate_at(c("G", "IP", "W", "SV", "SO", "ER", "BB_H"), round, 0) %>%
    mutate(PlayerName = paste0("Replacement, ", pos),
           Team = "",
           BB_H = round(WHIP * IP, 0),
           ER = ERA * IP / 9) %>%
    select(PlayerName, Team, G, IP, W, SV, SO, ER, BB_H, WHIP, ERA)
  
  # Remove players already considered in the above position(s)
  return(new("Baseline", players = repl, 
             remaining = data[!(data$PlayerName %in% pList$PlayerName), ]))
  
}

pBaseline <- pReplacement2("SP", 4, pnr)
pCompetitor <- rbind(pCompetitor, pBaseline@players)

pBaseline <- pReplacement2("RP", 1, pnr)
pCompetitor <- rbind(pCompetitor, pBaseline@players)

pBaseline <- pReplacement2("P", 2, pnr)
pCompetitor <- rbind(pCompetitor, pBaseline@players)

# Append to projections
dcp <- rbind(dcp, pCompetitor)

# Clean up workspace
rm(bCompetitor, pCompetitor, bReplacement1, bReplacement2, pReplacement1, 
   pReplacement2, bBaseline, pBaseline, pnr, bnr)