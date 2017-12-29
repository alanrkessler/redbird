## Baseline Batters

# Create randomly sorted values for the outfielders since there are multiple
bbOF1 <- bSims %>%
  filter(grepl("Baseline, OF", PlayerName) == TRUE) %>%
  mutate(sortorder = runif(length(PlayerName))) %>%
  arrange(sortorder) %>%
  select(-sortorder) %>%
  mutate(simNum = 1:n())

bbOF2 <- bSims %>%
  filter(grepl("Baseline, OF", PlayerName) == TRUE) %>%
  mutate(sortorder = runif(length(PlayerName))) %>%
  arrange(sortorder) %>%
  select(-sortorder) %>%
  mutate(simNum = 1:n())

bbOF3 <- bSims %>%
  filter(grepl("Baseline, OF", PlayerName) == TRUE) %>%
  mutate(sortorder = runif(length(PlayerName))) %>%
  arrange(sortorder) %>%
  select(-sortorder) %>%
  mutate(simNum = 1:n())

# Aggregate results for baseline
bbStats <- bSims %>%
  filter(grepl("Baseline", PlayerName) == TRUE,
         grepl("Baseline, OF", PlayerName) == FALSE)

bbStats <- rbind(bbStats, bbOF1, bbOF2, bbOF3) %>% 
  select(-PlayerName, -Team, -AVG, -PA, -AB, -H, 
         -HR, -R, -RBI, -SB, -paSim, -noabSim) %>%
  group_by(simNum) %>%
  summarise_all(sum) %>%
  mutate(avgSim = hSim / abSim) %>%
  select(-abSim, -hSim)
  
## Initial Set Batters

# Create randomly sorted values for the outfielders since there are multiple
brOF1 <- bSims %>%
  filter(grepl("Replacement, OF", PlayerName) == TRUE) %>%
  mutate(sortorder = runif(length(PlayerName))) %>%
  arrange(sortorder) %>%
  select(-sortorder) %>%
  mutate(simNum = 1:n(),
         PlayerName = "Replacement OF 1")

brOF2 <- bSims %>%
  filter(grepl("Replacement, OF", PlayerName) == TRUE) %>%
  mutate(sortorder = runif(length(PlayerName))) %>%
  arrange(sortorder) %>%
  select(-sortorder) %>%
  mutate(simNum = 1:n(),
         PlayerName = "Replacement OF 2")

brOF3 <- bSims %>%
  filter(grepl("Replacement, OF", PlayerName) == TRUE) %>%
  mutate(sortorder = runif(length(PlayerName))) %>%
  arrange(sortorder) %>%
  select(-sortorder) %>%
  mutate(simNum = 1:n(),
         PlayerName = "Replacement OF 3")

brStats <- bSims %>%
  filter(grepl("Baseline", PlayerName) == FALSE,
         grepl("Replacement, OF", PlayerName) == FALSE) 

brStats <- rbind(brStats, brOF1, brOF2, brOF3) %>%
  select(-AVG, -AB, -H, -HR, -R, -RBI, -SB, -paSim, -noabSim)
  
## Baseline Pitchers

# Create randomly sorted values for the SPs since there are multiple
pbSP1 <- pSims %>%
  filter(grepl("Baseline, SP", PlayerName) == TRUE) %>%
  mutate(sortorder = runif(length(PlayerName))) %>%
  arrange(sortorder) %>%
  select(-sortorder) %>%
  mutate(simNum = 1:n())

pbSP2 <- pSims %>%
  filter(grepl("Baseline, SP", PlayerName) == TRUE) %>%
  mutate(sortorder = runif(length(PlayerName))) %>%
  arrange(sortorder) %>%
  select(-sortorder) %>%
  mutate(simNum = 1:n())

pbSP3 <- pSims %>%
  filter(grepl("Baseline, SP", PlayerName) == TRUE) %>%
  mutate(sortorder = runif(length(PlayerName))) %>%
  arrange(sortorder) %>%
  select(-sortorder) %>%
  mutate(simNum = 1:n())

pbSP4 <- pSims %>%
  filter(grepl("Baseline, SP", PlayerName) == TRUE) %>%
  mutate(sortorder = runif(length(PlayerName))) %>%
  arrange(sortorder) %>%
  select(-sortorder) %>%
  mutate(simNum = 1:n())

pbP1 <- pSims %>%
  filter(grepl("Baseline, P", PlayerName) == TRUE) %>%
  mutate(sortorder = runif(length(PlayerName))) %>%
  arrange(sortorder) %>%
  select(-sortorder) %>%
  mutate(simNum = 1:n())

pbP2 <- pSims %>%
  filter(grepl("Baseline, P", PlayerName) == TRUE) %>%
  mutate(sortorder = runif(length(PlayerName))) %>%
  arrange(sortorder) %>%
  select(-sortorder) %>%
  mutate(simNum = 1:n())

# Aggregate results for baseline
pbStats <- pSims %>%
  filter(grepl("Baseline, RP", PlayerName) == TRUE)

pbStats <- rbind(pbStats, pbSP1, pbSP2, pbSP3, pbSP4, pbP1, pbP2) %>% 
  select(-PlayerName, -Team, -G, -IP, -W, -SV, -SO, -ER, -BB_H, 
         -WHIP, -ERA, -gSim) %>%
  group_by(simNum) %>%
  summarise_all(sum) %>%
  mutate(whipSim = bb_hSim / ipSim,
         eraSim = 9 * erSim / ipSim) %>%
  select(-erSim, -bb_hSim)

## Initial Set Pitchers

# Create randomly sorted values for the SPs since there are multiple
prSP1 <- pSims %>%
  filter(grepl("Replacement, SP", PlayerName) == TRUE) %>%
  mutate(sortorder = runif(length(PlayerName))) %>%
  arrange(sortorder) %>%
  select(-sortorder) %>%
  mutate(simNum = 1:n(),
         PlayerName = "Replacement, SP 1")

prSP2 <- pSims %>%
  filter(grepl("Replacement, SP", PlayerName) == TRUE) %>%
  mutate(sortorder = runif(length(PlayerName))) %>%
  arrange(sortorder) %>%
  select(-sortorder) %>%
  mutate(simNum = 1:n(),
         PlayerName = "Replacement, SP 2")

prSP3 <- pSims %>%
  filter(grepl("Replacement, SP", PlayerName) == TRUE) %>%
  mutate(sortorder = runif(length(PlayerName))) %>%
  arrange(sortorder) %>%
  select(-sortorder) %>%
  mutate(simNum = 1:n(),
         PlayerName = "Replacement, SP 3")

prSP4 <- pSims %>%
  filter(grepl("Replacement, SP", PlayerName) == TRUE) %>%
  mutate(sortorder = runif(length(PlayerName))) %>%
  arrange(sortorder) %>%
  select(-sortorder) %>%
  mutate(simNum = 1:n(),
         PlayerName = "Replacement, SP 4")

prP1 <- pSims %>%
  filter(grepl("Replacement, P", PlayerName) == TRUE) %>%
  mutate(sortorder = runif(length(PlayerName))) %>%
  arrange(sortorder) %>%
  select(-sortorder) %>%
  mutate(simNum = 1:n(),
         PlayerName = "Replacement, P 1")

prP2 <- pSims %>%
  filter(grepl("Replacement, P", PlayerName) == TRUE) %>%
  mutate(sortorder = runif(length(PlayerName))) %>%
  arrange(sortorder) %>%
  select(-sortorder) %>%
  mutate(simNum = 1:n(),
         PlayerName = "Replacement, P 2")

prStats <- pSims %>%
  filter(grepl("Baseline", PlayerName) == FALSE,
         grepl("Replacement, SP", PlayerName) == FALSE,
         grepl("Replacement, P", PlayerName) == FALSE)

prStats <- rbind(prStats, prSP1, prSP2, prSP3, prSP4, prP1, prP2) %>%
  select(-G, -IP, -W, -SV, -SO, -ER, -BB_H, -WHIP, -ERA, -gSim)

rm(bbOF1, bbOF2, bbOF3, brOF1, brOF2, brOF3, 
   pbSP1, pbSP2, pbSP3, pbSP4, pbP1, pbP2, 
   prSP1, prSP2, prSP3, prSP4, prP1, prP2, pSims, bSims)