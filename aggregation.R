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
  
  