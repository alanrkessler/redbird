#####
# Create baseline and bReplacement level placeholders
#####

# Players with the same name are an outstanding issue

# Set of batters to extract players from without bReplacement
bnr <- allPlayers[grepl("P", allPlayers$POS) == FALSE, ]

# C

# List of players eligible for positions in priority order
pList <- bnr[grepl("C", bnr$POS) == TRUE, ]

# Replacement level player creation
# Replace average hits with the hits based on batting average
bReplacement <- dcb[dcb$PlayerName %in% pList[19:24, ]$PlayerName, ] %>%
  summarise_if(is.numeric, mean) %>%
  mutate_at(c("PA", "AB", "H", "HR", "R", "RBI", "SB"), round, 0) %>%
  mutate(PlayerName = "Replacement, C",
         Team = "",
         H = round(AVG * AB, 0)) %>%
  select(PlayerName, Team, PA, AB, H, HR, R, RBI, SB, AVG)
  
# Baseline level player creation
# Replace average hits with the hits based on batting average 
bCompetitor <- dcb[dcb$PlayerName %in% pList[7:12, ]$PlayerName, ] %>%
  summarise_if(is.numeric, mean) %>%
  mutate_at(c("PA", "AB", "H", "HR", "R", "RBI", "SB"), round, 0) %>%
  mutate(PlayerName = "Baseline, C",
         Team = "",
         H = round(AVG * AB, 0)) %>%
  select(PlayerName, Team, PA, AB, H, HR, R, RBI, SB, AVG)

# Remove players already considered in the above position(s)
bnr <- bnr[!(bnr$PlayerName %in% pList$PlayerName), ]

# SS

# List of players eligible for positions in priority order
pList <- bnr[grepl("SS", bnr$POS) == TRUE, ]

# bReplacement level player creation
# Replace average hits with the hits based on batting average
rep <- dcb[dcb$PlayerName %in% pList[19:24, ]$PlayerName, ] %>%
  summarise_if(is.numeric, mean) %>%
  mutate_at(c("PA", "AB", "H", "HR", "R", "RBI", "SB"), round, 0) %>%
  mutate(PlayerName = "Replacement, SS",
         Team = "",
         H = round(AVG * AB, 0)) %>%
  select(PlayerName, Team, PA, AB, H, HR, R, RBI, SB, AVG)

bReplacement <- rbind(bReplacement, rep)

# Baseline level player creation
# Replace average hits with the hits based on batting average 
comp <- dcb[dcb$PlayerName %in% pList[7:12, ]$PlayerName, ] %>%
  summarise_if(is.numeric, mean) %>%
  mutate_at(c("PA", "AB", "H", "HR", "R", "RBI", "SB"), round, 0) %>%
  mutate(PlayerName = "Baseline, SS",
         Team = "",
         H = round(AVG * AB, 0)) %>%
  select(PlayerName, Team, PA, AB, H, HR, R, RBI, SB, AVG)

bCompetitor <- rbind(bCompetitor, comp)

# Remove players already considered in the above position(s)
bnr <- bnr[!(bnr$PlayerName %in% pList$PlayerName), ]

# 2B

# List of players eligible for positions in priority order
pList <- bnr[grepl("2B", bnr$POS) == TRUE, ]

# Replacement level player creation
# Replace average hits with the hits based on batting average
rep <- dcb[dcb$PlayerName %in% pList[19:24, ]$PlayerName, ] %>%
  summarise_if(is.numeric, mean) %>%
  mutate_at(c("PA", "AB", "H", "HR", "R", "RBI", "SB"), round, 0) %>%
  mutate(PlayerName = "Replacement, 2B",
         Team = "",
         H = round(AVG * AB, 0)) %>%
  select(PlayerName, Team, PA, AB, H, HR, R, RBI, SB, AVG)

bReplacement <- rbind(bReplacement, rep)

# Baseline level player creation
# Replace average hits with the hits based on batting average 
comp <- dcb[dcb$PlayerName %in% pList[7:12, ]$PlayerName, ] %>%
  summarise_if(is.numeric, mean) %>%
  mutate_at(c("PA", "AB", "H", "HR", "R", "RBI", "SB"), round, 0) %>%
  mutate(PlayerName = "Baseline, 2B",
         Team = "",
         H = round(AVG * AB, 0)) %>%
  select(PlayerName, Team, PA, AB, H, HR, R, RBI, SB, AVG)

bCompetitor <- rbind(bCompetitor, comp)

# Remove players already considered in the above position(s)
bnr <- bnr[!(bnr$PlayerName %in% pList$PlayerName), ]

# 3B

# List of players eligible for positions in priority order
pList <- bnr[grepl("3B", bnr$POS) == TRUE, ]

# Replacement level player creation
# Replace average hits with the hits based on batting average
rep <- dcb[dcb$PlayerName %in% pList[19:24, ]$PlayerName, ] %>%
  summarise_if(is.numeric, mean) %>%
  mutate_at(c("PA", "AB", "H", "HR", "R", "RBI", "SB"), round, 0) %>%
  mutate(PlayerName = "Replacement, 3B",
         Team = "",
         H = round(AVG * AB, 0)) %>%
  select(PlayerName, Team, PA, AB, H, HR, R, RBI, SB, AVG)

bReplacement <- rbind(bReplacement, rep)

# Baseline level player creation
# Replace average hits with the hits based on batting average 
comp <- dcb[dcb$PlayerName %in% pList[7:12, ]$PlayerName, ] %>%
  summarise_if(is.numeric, mean) %>%
  mutate_at(c("PA", "AB", "H", "HR", "R", "RBI", "SB"), round, 0) %>%
  mutate(PlayerName = "Baseline, 3B",
         Team = "",
         H = round(AVG * AB, 0)) %>%
  select(PlayerName, Team, PA, AB, H, HR, R, RBI, SB, AVG)

bCompetitor <- rbind(bCompetitor, comp)

# Remove players already considered in the above position(s)
bnr <- bnr[!(bnr$PlayerName %in% pList$PlayerName), ]

# OF

# List of players eligible for positions in priority order
pList <- bnr[grepl("OF", bnr$POS) == TRUE, ]

# Replacement level player creation
# Replace average hits with the hits based on batting average
rep <- dcb[dcb$PlayerName %in% pList[55:60, ]$PlayerName, ] %>%
  summarise_if(is.numeric, mean) %>%
  mutate_at(c("PA", "AB", "H", "HR", "R", "RBI", "SB"), round, 0) %>%
  mutate(PlayerName = "Replacement, OF",
         Team = "",
         H = round(AVG * AB, 0)) %>%
  select(PlayerName, Team, PA, AB, H, HR, R, RBI, SB, AVG)

bReplacement <- rbind(bReplacement, rep)

# Baseline level player creation
# Replace average hits with the hits based on batting average 
comp <- dcb[dcb$PlayerName %in% pList[18:37, ]$PlayerName, ] %>%
  summarise_if(is.numeric, mean) %>%
  mutate_at(c("PA", "AB", "H", "HR", "R", "RBI", "SB"), round, 0) %>%
  mutate(PlayerName = "Baseline, OF",
         Team = "",
         H = round(AVG * AB, 0)) %>%
  select(PlayerName, Team, PA, AB, H, HR, R, RBI, SB, AVG)

bCompetitor <- rbind(bCompetitor, comp)

# Remove players already considered in the above position(s)
bnr <- bnr[!(bnr$PlayerName %in% pList$PlayerName), ]

# 1B

# List of players eligible for positions in priority order
pList <- bnr[grepl("1B", bnr$POS) == TRUE, ]

# Replacement level player creation
# Replace average hits with the hits based on batting average
rep <- dcb[dcb$PlayerName %in% pList[19:24, ]$PlayerName, ] %>%
  summarise_if(is.numeric, mean) %>%
  mutate_at(c("PA", "AB", "H", "HR", "R", "RBI", "SB"), round, 0) %>%
  mutate(PlayerName = "Replacement, 1B",
         Team = "",
         H = round(AVG * AB, 0)) %>%
  select(PlayerName, Team, PA, AB, H, HR, R, RBI, SB, AVG)

bReplacement <- rbind(bReplacement, rep)

# Baseline level player creation
# Replace average hits with the hits based on batting average 
comp <- dcb[dcb$PlayerName %in% pList[7:12, ]$PlayerName, ] %>%
  summarise_if(is.numeric, mean) %>%
  mutate_at(c("PA", "AB", "H", "HR", "R", "RBI", "SB"), round, 0) %>%
  mutate(PlayerName = "Baseline, 1B",
         Team = "",
         H = round(AVG * AB, 0)) %>%
  select(PlayerName, Team, PA, AB, H, HR, R, RBI, SB, AVG)

bCompetitor <- rbind(bCompetitor, comp)

# Set of pitchers to extract players from without bReplacement
pnr <- allPlayers[grepl("P", allPlayers$POS) == TRUE, ]

# Append to projections
dcp <- rbind(dcp, pCompetitor, pReplacement)

# SP

# List of playerss eligible for positions in priority order
pList <- pnr[grepl("SP", pnr$POS) == TRUE, ]

# Replacement level player creation
# Replace average hits with the hits based on batting average
pReplacement <- dcp[dcp$PlayerName %in% pList[73:78, ]$PlayerName, ] %>%
  summarise_if(is.numeric, mean) %>%
  mutate_at(c("IP", "W", "SV", "SO", "ER", "BB_H"), round, 0) %>%
  mutate(PlayerName = "Replacement, SP",
         Team = "",
         BB_H = round(WHIP * IP, 0),
         ER = ERA * IP / 9) %>%
  select(PlayerName, Team, IP, W, SV, SO, ER, BB_H, WHIP, ERA)

# Baseline level player creation
# Replace average hits with the hits based on batting average 
pCompetitor <- dcp[dcp$PlayerName %in% pList[24:49, ]$PlayerName, ] %>%
  summarise_if(is.numeric, mean) %>%
  mutate_at(c("IP", "W", "SV", "SO", "ER", "BB_H"), round, 0) %>%
  mutate(PlayerName = "Baseline, SP",
         Team = "",
         BB_H = round(WHIP * IP, 0),
         ER = ERA * IP / 9) %>%
  select(PlayerName, Team, IP, W, SV, SO, ER, BB_H, WHIP, ERA)

# Remove players already considered in the above position(s)
pnr <- pnr[!(pnr$PlayerName %in% pList[1:72, ]$PlayerName), ]

# RP

# List of playerss eligible for positions in priority order
pList <- pnr[grepl("RP", pnr$POS) == TRUE, ]

# Replacement level player creation
# Replace average hits with the hits based on batting average
rep <- dcp[dcp$PlayerName %in% pList[19:24, ]$PlayerName, ] %>%
  summarise_if(is.numeric, mean) %>%
  mutate_at(c("IP", "W", "SV", "SO", "ER", "BB_H"), round, 0) %>%
  mutate(PlayerName = "Replacement, RP",
         Team = "",
         BB_H = round(WHIP * IP, 0),
         ER = ERA * IP / 9) %>%
  select(PlayerName, Team, IP, W, SV, SO, ER, BB_H, WHIP, ERA)

pReplacement <- rbind(pReplacement, rep)

# Baseline level player creation
# Replace average hits with the hits based on batting average 
comp <- dcp[dcp$PlayerName %in% pList[7:12, ]$PlayerName, ] %>%
  summarise_if(is.numeric, mean) %>%
  mutate_at(c("IP", "W", "SV", "SO", "ER", "BB_H"), round, 0) %>%
  mutate(PlayerName = "Baseline, RP",
         Team = "",
         BB_H = round(WHIP * IP, 0),
         ER = ERA * IP / 9) %>%
  select(PlayerName, Team, IP, W, SV, SO, ER, BB_H, WHIP, ERA)

pCompetitor <- rbind(pCompetitor, comp)

# Remove players already considered in the above position(s)
pnr <- pnr[!(pnr$PlayerName %in% pList[1:18, ]$PlayerName), ]

# P

# List of playerss eligible for positions in priority order
pList <- pnr[grepl("P", pnr$POS) == TRUE, ]

# Replacement level player creation
# Replace average hits with the hits based on batting average
rep <- dcp[dcp$PlayerName %in% pList[37:42, ]$PlayerName, ] %>%
  summarise_if(is.numeric, mean) %>%
  mutate_at(c("IP", "W", "SV", "SO", "ER", "BB_H"), round, 0) %>%
  mutate(PlayerName = "Replacement, P",
         Team = "",
         BB_H = round(WHIP * IP, 0),
         ER = ERA * IP / 9) %>%
  select(PlayerName, Team, IP, W, SV, SO, ER, BB_H, WHIP, ERA)

pReplacement <- rbind(pReplacement, rep)

# Baseline level player creation
# Replace average hits with the hits based on batting average 
comp <- dcp[dcp$PlayerName %in% pList[13:24, ]$PlayerName, ] %>%
  summarise_if(is.numeric, mean) %>%
  mutate_at(c("IP", "W", "SV", "SO", "ER", "BB_H"), round, 0) %>%
  mutate(PlayerName = "Baseline, P",
         Team = "",
         BB_H = round(WHIP * IP, 0),
         ER = ERA * IP / 9) %>%
  select(PlayerName, Team, IP, W, SV, SO, ER, BB_H, WHIP, ERA)

pCompetitor <- rbind(pCompetitor, comp)

# Append to projections
dcp <- rbind(dcp, pCompetitor, pReplacement)