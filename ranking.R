# Testing function to create ranking on allPlayers

# Step 1 - Start with a specific position 
x <- "Posey, Buster"



c1 <- brStats[brStats$PlayerName %in% c(x, input$p3.input,
                                        input$p4.input, input$p5.input,
                                        input$p6.input, input$p7.input,
                                        input$p8.input, input$p9.input), ]

c2 <- prStats[prStats$PlayerName %in% c(input$p1.input1, input$p1.input2,
                                        input$p1.input3, input$p1.input4,
                                        input$pr.input, input$p.input1,
                                        input$p.input2), ]

e <- c2 %>%
  select(-PlayerName, -Team) %>%
  group_by(simNum) %>%
  summarise_all(sum) %>%
  mutate(whipSim = bb_hSim / ipSim,
         eraSim = 9 * erSim / ipSim) %>%
  na.omit() %>%
  select(-ipSim, -bb_hSim, -erSim) %>%
  left_join(., pbStats, by = c("simNum")) %>%
  mutate(W = wSim.x > wSim.y,
         SV = svSim.x > svSim.y,
         ERA = eraSim.x < eraSim.y,
         WHIP = whipSim.x < whipSim.y,
         SO = soSim.x > soSim.y) %>%
  ungroup() %>%
  select(simNum, W, SV, ERA, WHIP, SO)
d <- c1 %>%
  select(-PlayerName, -Team, -PA) %>%
  group_by(simNum) %>%
  summarise_all(sum) %>%
  mutate(avgSim = hSim / abSim) %>%
  select(-abSim, -hSim) %>%
  left_join(., bbStats, by = c("simNum")) %>%
  mutate(AVG = avgSim.x > avgSim.y,
         RBI = rbiSim.x > rbiSim.y,
         R = rSim.x > rSim.y,
         SB = sbSim.x > sbSim.y,
         HR = hrSim.x > hrSim.y) %>%
  ungroup() %>%
  select(simNum, AVG, RBI, R, SB, HR)
inner_join(e, d, by = "simNum") %>%
  mutate(`Win %` = (AVG + RBI + R + SB + HR + W + SV + ERA + WHIP + SO) > 5) %>%
  select(`Win %`) %>% 
  summarise_all(mean)