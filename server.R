shinyServer(function(input, output, session) {
     
     # tabPanel 1 - Draft Board
     
     # Add deleted players to a list
     myValues <- reactiveValues(p2List = "Replacement, C",
                                p2Sel = "Replacement, C",
                                p3List = "Replacement, 1B",
                                p3Sel = "Replacement, 1B",
                                p4List = "Replacement, 2B",
                                p4Sel = "Replacement, 2B",
                                p5List = "Replacement, 3B",
                                p5Sel = "Replacement, 3B",
                                p6List = "Replacement, SS",
                                p6Sel = "Replacement, SS",
                                poList = c("Replacement, OF 1", 
                                           "Replacement, OF 2", 
                                           "Replacement, OF 3"),
                                p7Sel = "Replacement, OF 1",
                                p8Sel = "Replacement, OF 2",
                                p9Sel = "Replacement, OF 3",
                                p1List = c("Replacement, SP 1", 
                                           "Replacement, SP 2", 
                                           "Replacement, SP 3",
                                           "Replacement, SP 4"),
                                p1Sel1 = "Replacement, SP 1",
                                p1Sel2 = "Replacement, SP 2",
                                p1Sel3 = "Replacement, SP 3",
                                p1Sel4 = "Replacement, SP 4",
                                prList = "Replacement, RP",
                                prSel = "Replacement, RP",
                                pList = c("Replacement, P 1", 
                                          "Replacement, P 2"),
                                pSel1 = "Replacement, P 1",
                                pSel2 = "Replacement, P 2")
     observe({
          if(input$delete.button == 0)
              return()
          isolate({
              myValues$dList <- c(isolate(myValues$dList), input$text)
          })
          
     })
     
     # Add drafted players to two lists
     observe({
       if(input$draft.button == 0)
         return()
       isolate({
         myValues$dList <- c(isolate(myValues$dList), input$text)
         myValues$teamList <- c(isolate(myValues$teamList), input$text)
       })
       
     })
     
     # Save version of the draft sheet without deleted players
     filter <- reactive({
          allPlayers[!(allPlayers$PlayerName %in% myValues$dList), ]
     })
     
     # Display the top 25 entries of the draft sheet
     output$sheet <- DT::renderDataTable({
          if (input$pos.input != "All" & input$team.input == "All") {
               DT::datatable(filter()[grepl(input$pos.input, 
                                            filter()$POS) == TRUE,], 
                             options = list(pageLength = 25)) 
          }
          else if (input$pos.input == "All" & input$team.input != "All"){
               DT::datatable(filter()[grepl(input$team.input, 
                                            filter()$Team) == TRUE,], 
                             options = list(pageLength = 25)) 
          }
          else if (input$pos.input != "All" & input$team.input != "All"){
               DT::datatable(filter()[grepl(input$team.input, 
                                            filter()$Team) == TRUE 
                                      & grepl(input$pos.input,
                                              filter()$POS) == TRUE,], 
                             options = list(pageLength = 25)) 
          }
          else {
               DT::datatable(filter(), 
                             options = list(pageLength = 25)) 
          }
     })
     
     # tab 2
     
     # Test drop down for positions
     observe({
       if(input$draft.button == 0)
         return()
       myValues$teamList
       if (!any(allPlayers$PlayerName == tail(myValues$teamList, 1))){
         return()
       }
       else if (grepl("C", allPlayers[allPlayers$PlayerName == tail(myValues$teamList, 1), ]$POS) == TRUE) {
         myValues$p2List <- c(isolate(myValues$p2List), tail(myValues$teamList, 1))
         if (isolate(myValues$p2Sel) == "Replacement, C") {
           myValues$p2Sel <- tail(myValues$teamList, 1)
         }
         updateSelectInput(session = session, inputId = "p2.input", 
                           choices = myValues$p2List,
                           selected = myValues$p2Sel)
       }
       else if (grepl("SS", allPlayers[allPlayers$PlayerName == tail(myValues$teamList, 1), ]$POS) == TRUE) {
         myValues$p6List <- c(isolate(myValues$p6List), tail(myValues$teamList, 1))
         if (isolate(myValues$p6Sel) == "Replacement, SS") {
           myValues$p6Sel <- tail(myValues$teamList, 1)
         }
         updateSelectInput(session = session, inputId = "p6.input", 
                           choices = myValues$p6List,
                           selected = myValues$p6Sel)
       }
       else if (grepl("2B", allPlayers[allPlayers$PlayerName == tail(myValues$teamList, 1), ]$POS) == TRUE) {
         myValues$p4List <- c(isolate(myValues$p4List), tail(myValues$teamList, 1))
         if (isolate(myValues$p4Sel) == "Replacement, 2B") {
           myValues$p4Sel <- tail(myValues$teamList, 1)
         }
         updateSelectInput(session = session, inputId = "p4.input", 
                           choices = myValues$p4List,
                           selected = myValues$p4Sel)
       }
       else if (grepl("3B", allPlayers[allPlayers$PlayerName == tail(myValues$teamList, 1), ]$POS) == TRUE) {
         myValues$p5List <- c(isolate(myValues$p5List), tail(myValues$teamList, 1))
         if (isolate(myValues$p5Sel) == "Replacement, 3B") {
           myValues$p5Sel <- tail(myValues$teamList, 1)
         }
         updateSelectInput(session = session, inputId = "p5.input", 
                           choices = myValues$p5List,
                           selected = myValues$p5Sel)
       }
       else if (grepl("OF", allPlayers[allPlayers$PlayerName == tail(myValues$teamList, 1), ]$POS) == TRUE) {
         myValues$poList <- c(isolate(myValues$poList), tail(myValues$teamList, 1))
         if (isolate(myValues$p7Sel) == "Replacement, OF 1") {
           myValues$p7Sel <- tail(myValues$teamList, 1)
         }
         else if (isolate(myValues$p8Sel) == "Replacement, OF 2") {
           myValues$p8Sel <- tail(myValues$teamList, 1)
         }
         else if (isolate(myValues$p9Sel) == "Replacement, OF 3") {
           myValues$p9Sel <- tail(myValues$teamList, 1)
         }
         updateSelectInput(session = session, inputId = "p7.input", 
                           choices = myValues$poList,
                           selected = myValues$p7Sel)
         updateSelectInput(session = session, inputId = "p8.input", 
                           choices = myValues$poList,
                           selected = myValues$p8Sel)
         updateSelectInput(session = session, inputId = "p9.input", 
                           choices = myValues$poList,
                           selected = myValues$p9Sel)
       }
       else if (grepl("1B", allPlayers[allPlayers$PlayerName == tail(myValues$teamList, 1), ]$POS) == TRUE) {
         myValues$p3List <- c(isolate(myValues$p3List), tail(myValues$teamList, 1))
         if (isolate(myValues$p3Sel) == "Replacement, 1B") {
           myValues$p3Sel <- tail(myValues$teamList, 1)
         }
         updateSelectInput(session = session, inputId = "p3.input", 
                           choices = myValues$p3List,
                           selected = myValues$p3Sel)
       }
       else if (grepl("SP", allPlayers[allPlayers$PlayerName == tail(myValues$teamList, 1), ]$POS) == TRUE) {
         myValues$p1List <- c(isolate(myValues$p1List), tail(myValues$teamList, 1))
         myValues$pList <- c(isolate(myValues$pList), tail(myValues$teamList, 1))
         if (isolate(myValues$p1Sel1) == "Replacement, SP 1") {
           myValues$p1Sel1 <- tail(myValues$teamList, 1)
         }
         else if (isolate(myValues$p1Sel2) == "Replacement, SP 2") {
           myValues$p1Sel2 <- tail(myValues$teamList, 1)
         }
         else if (isolate(myValues$p1Sel3) == "Replacement, SP 3") {
           myValues$p1Sel3 <- tail(myValues$teamList, 1)
         }
         else if (isolate(myValues$p1Sel4) == "Replacement, SP 4") {
           myValues$p1Sel4 <- tail(myValues$teamList, 1)
         }
         else if (isolate(myValues$pSel1) == "Replacement, P 1") {
           myValues$pSel1 <- tail(myValues$teamList, 1)
         }
         else if (isolate(myValues$pSel2) == "Replacement, P 2") {
           myValues$pSel2 <- tail(myValues$teamList, 1)
         }
         updateSelectInput(session = session, inputId = "p1.input1", 
                           choices = myValues$p1List,
                           selected = myValues$p1Sel1)
         updateSelectInput(session = session, inputId = "p1.input2", 
                           choices = myValues$p1List,
                           selected = myValues$p1Sel2)
         updateSelectInput(session = session, inputId = "p1.input3", 
                           choices = myValues$p1List,
                           selected = myValues$p1Sel3)
         updateSelectInput(session = session, inputId = "p1.input4", 
                           choices = myValues$p1List,
                           selected = myValues$p1Sel4)
         updateSelectInput(session = session, inputId = "p.input1", 
                           choices = myValues$pList,
                           selected = myValues$pSel1)
         updateSelectInput(session = session, inputId = "p.input2", 
                           choices = myValues$pList,
                           selected = myValues$pSel2)
       }
       else if (grepl("RP", allPlayers[allPlayers$PlayerName == tail(myValues$teamList, 1), ]$POS) == TRUE) {
         myValues$prList <- c(isolate(myValues$prList), tail(myValues$teamList, 1))
         myValues$pList <- c(isolate(myValues$pList), tail(myValues$teamList, 1))
         if (isolate(myValues$prSel) == "Replacement, RP") {
           myValues$prSel <- tail(myValues$teamList, 1)
         }
         else if (isolate(myValues$pSel1) == "Replacement, P 1") {
           myValues$pSel1 <- tail(myValues$teamList, 1)
         }
         else if (isolate(myValues$pSel2) == "Replacement, P 2") {
           myValues$pSel2 <- tail(myValues$teamList, 1)
         }
         updateSelectInput(session = session, inputId = "pr.input", 
                           choices = myValues$prList,
                           selected = myValues$prSel)
         updateSelectInput(session = session, inputId = "p.input1", 
                           choices = myValues$pList,
                           selected = myValues$pSel1)
         updateSelectInput(session = session, inputId = "p.input2", 
                           choices = myValues$pList,
                           selected = myValues$pSel2)
       }
       else if (grepl("P", allPlayers[allPlayers$PlayerName == tail(myValues$teamList, 1), ]$POS) == TRUE) {
         myValues$pList <- c(isolate(myValues$pList), tail(myValues$teamList, 1))
         if (isolate(myValues$pSel1) == "Replacement, P 1") {
           myValues$pSel1 <- tail(myValues$teamList, 1)
         }
         else if (isolate(myValues$pSel2) == "Replacement, P 2") {
           myValues$pSel2 <- tail(myValues$teamList, 1)
         }
         updateSelectInput(session = session, inputId = "p.input1", 
                           choices = myValues$pList,
                           selected = myValues$pSel1)
         updateSelectInput(session = session, inputId = "p.input2", 
                           choices = myValues$pList,
                           selected = myValues$pSel2)
       }
     })
     
     # Current simulated results - batting
     cbTeamSims <- reactive({
       brStats[brStats$PlayerName %in% c(input$p2.input, input$p3.input,
                                         input$p4.input, input$p5.input,
                                         input$p6.input, input$p7.input,
                                         input$p8.input, input$p9.input), ]
     })
     
     # Current simulated results - pitching
     cpTeamSims <- reactive({
       prStats[prStats$PlayerName %in% c(input$p1.input1, input$p1.input2,
                                         input$p1.input3, input$p1.input4,
                                         input$pr.input, input$p.input1,
                                         input$p.input2), ]
     })
     
     # Current batter probabilities
     cbProbs <- reactive({
       cbTeamSims() %>%
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
         select(AVG, RBI, R, SB, HR) %>%
         summarise_all(mean)
     })
     
     # Current pitcher probabilities
     cpProbs <- reactive({
       cpTeamSims() %>%
         select(-PlayerName, -Team) %>%
         group_by(simNum) %>%
         summarise_all(sum) %>%
         mutate(whipSim = bb_hSim / ipSim,
                eraSim = 9 * erSim / ipSim) %>%
         na.omit() %>%
         select(-bb_hSim, -erSim) %>%
         left_join(., pbStats, by = c("simNum")) %>%
         ungroup() %>%
         mutate(W = ifelse(ipSim.x < minInnings, FALSE, wSim.x > wSim.y),
                SV = ifelse(ipSim.x < minInnings, FALSE, svSim.x > svSim.y),
                ERA = ifelse(ipSim.x < minInnings, FALSE, eraSim.x < eraSim.y),
                WHIP = ifelse(ipSim.x < minInnings, FALSE, whipSim.x < whipSim.y),
                SO = ifelse(ipSim.x < minInnings, FALSE, soSim.x > soSim.y)) %>%
         select(W, SV, ERA, WHIP, SO) %>%
         summarise_all(mean)
     })
     
     cProbs <- reactive({
       a <- cpTeamSims() %>%
         select(-PlayerName, -Team) %>%
         group_by(simNum) %>%
         summarise_all(sum) %>%
         mutate(whipSim = bb_hSim / ipSim,
                eraSim = 9 * erSim / ipSim) %>%
         na.omit() %>%
         select(-bb_hSim, -erSim) %>%
         left_join(., pbStats, by = c("simNum")) %>%
         mutate(W = ifelse(ipSim.x < minInnings, FALSE, wSim.x > wSim.y),
                SV = ifelse(ipSim.x < minInnings, FALSE, svSim.x > svSim.y),
                ERA = ifelse(ipSim.x < minInnings, FALSE, eraSim.x < eraSim.y),
                WHIP = ifelse(ipSim.x < minInnings, FALSE, whipSim.x < whipSim.y),
                SO = ifelse(ipSim.x < minInnings, FALSE, soSim.x > soSim.y)) %>%
         ungroup() %>%
         select(simNum, W, SV, ERA, WHIP, SO)
       b <- cbTeamSims() %>%
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
       inner_join(a, b, by = "simNum") %>%
         mutate(`Win %` = (AVG + RBI + R + SB + HR + W + SV + ERA + WHIP + SO) > 5) %>%
         select(`Win %`) %>% 
         summarise_all(mean)
     })
     
     # Display current team
     output$cbProbsDT <- DT::renderDataTable({
       DT::datatable(cbProbs(), options = list(searching = FALSE, paging = FALSE), rownames = FALSE) %>%
         formatPercentage(c('AVG', 'RBI', 'R', 'SB', 'HR'), 2)
     })
     
     output$cpProbsDT <- DT::renderDataTable({
       DT::datatable(cpProbs(), options = list(searching = FALSE, paging = FALSE), rownames = FALSE) %>%
         formatPercentage(c('W', 'SV', 'ERA', 'WHIP', 'SO'), 2)
       
     })
     
     output$cProbsDT <- DT::renderDataTable({
       DT::datatable(cProbs(), options = list(searching = FALSE, paging = FALSE), rownames = FALSE) %>%
         formatPercentage(c('Win %'), 2)
       
     })
     
     
})