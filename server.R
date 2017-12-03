shinyServer(function(input, output, session) {
     
     # tabPanel 1 - Draft Board
     
     # Add deleted players to a list
     myValues <- reactiveValues(p2List = "Baseline",
                                p2Sel = "Baseline",
                                p3List = "Baseline",
                                p3Sel = "Baseline",
                                p4List = "Baseline",
                                p4Sel = "Baseline",
                                p5List = "Baseline",
                                p5Sel = "Baseline",
                                p6List = "Baseline",
                                p6Sel = "Baseline",
                                poList = "Baseline",
                                p7Sel = "Baseline",
                                p8Sel = "Baseline",
                                p9Sel = "Baseline",
                                p1List = "Baseline",
                                p1Sel1 = "Baseline",
                                p1Sel2 = "Baseline",
                                p1Sel3 = "Baseline",
                                p1Sel4 = "Baseline",
                                prList = "Baseline",
                                prSel = "Baseline",
                                pList = "Baseline",
                                pSel1 = "Baseline",
                                pSel2 = "Baseline")
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
     
     # Save dataframe of players drafted 
     cteam <- reactive({
       allPlayers[(allPlayers$PlayerName %in% myValues$teamList), ]
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
     
     
     # Display current team
     output$cteam <- DT::renderDataTable({
       DT::datatable(cteam())
     })
     
     # tab 2
     
     # Test drop down for positions
     observe({
       if(input$draft.button == 0)
         return()
       myValues$teamList
       if (grepl("C", allPlayers[allPlayers$PlayerName == tail(myValues$teamList, 1), ]$POS) == TRUE) {
         myValues$p2List <- c(isolate(myValues$p2List), tail(myValues$teamList, 1))
         if (isolate(myValues$p2Sel) == "Baseline") {
           myValues$p2Sel <- tail(myValues$teamList, 1)
         }
         updateSelectInput(session = session, inputId = "p2.input", 
                           choices = myValues$p2List,
                           selected = myValues$p2Sel)
       }
       else if (grepl("SS", allPlayers[allPlayers$PlayerName == tail(myValues$teamList, 1), ]$POS) == TRUE) {
         myValues$p6List <- c(isolate(myValues$p6List), tail(myValues$teamList, 1))
         if (isolate(myValues$p6Sel) == "Baseline") {
           myValues$p6Sel <- tail(myValues$teamList, 1)
         }
         updateSelectInput(session = session, inputId = "p6.input", 
                           choices = myValues$p6List,
                           selected = myValues$p6Sel)
       }
       else if (grepl("2B", allPlayers[allPlayers$PlayerName == tail(myValues$teamList, 1), ]$POS) == TRUE) {
         myValues$p4List <- c(isolate(myValues$p4List), tail(myValues$teamList, 1))
         if (isolate(myValues$p4Sel) == "Baseline") {
           myValues$p4Sel <- tail(myValues$teamList, 1)
         }
         updateSelectInput(session = session, inputId = "p4.input", 
                           choices = myValues$p4List,
                           selected = myValues$p4Sel)
       }
       else if (grepl("3B", allPlayers[allPlayers$PlayerName == tail(myValues$teamList, 1), ]$POS) == TRUE) {
         myValues$p5List <- c(isolate(myValues$p5List), tail(myValues$teamList, 1))
         if (isolate(myValues$p5Sel) == "Baseline") {
           myValues$p5Sel <- tail(myValues$teamList, 1)
         }
         updateSelectInput(session = session, inputId = "p5.input", 
                           choices = myValues$p5List,
                           selected = myValues$p5Sel)
       }
       else if (grepl("OF", allPlayers[allPlayers$PlayerName == tail(myValues$teamList, 1), ]$POS) == TRUE) {
         myValues$poList <- c(isolate(myValues$poList), tail(myValues$teamList, 1))
         if (isolate(myValues$p7Sel) == "Baseline") {
           myValues$p7Sel <- tail(myValues$teamList, 1)
         }
         else if (isolate(myValues$p8Sel) == "Baseline") {
           myValues$p8Sel <- tail(myValues$teamList, 1)
         }
         else if (isolate(myValues$p9Sel) == "Baseline") {
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
         if (isolate(myValues$p3Sel) == "Baseline") {
           myValues$p3Sel <- tail(myValues$teamList, 1)
         }
         updateSelectInput(session = session, inputId = "p3.input", 
                           choices = myValues$p3List,
                           selected = myValues$p3Sel)
       }
       else if (grepl("SP", allPlayers[allPlayers$PlayerName == tail(myValues$teamList, 1), ]$POS) == TRUE) {
         myValues$p1List <- c(isolate(myValues$p1List), tail(myValues$teamList, 1))
         myValues$pList <- c(isolate(myValues$pList), tail(myValues$teamList, 1))
         if (isolate(myValues$p1Sel1) == "Baseline") {
           myValues$p1Sel1 <- tail(myValues$teamList, 1)
         }
         else if (isolate(myValues$p1Sel2) == "Baseline") {
           myValues$p1Sel2 <- tail(myValues$teamList, 1)
         }
         else if (isolate(myValues$p1Sel3) == "Baseline") {
           myValues$p1Sel3 <- tail(myValues$teamList, 1)
         }
         else if (isolate(myValues$p1Sel4) == "Baseline") {
           myValues$p1Sel4 <- tail(myValues$teamList, 1)
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
         if (isolate(myValues$prSel) == "Baseline") {
           myValues$prSel <- tail(myValues$teamList, 1)
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
         if (isolate(myValues$pSel1) == "Baseline") {
           myValues$pSel1 <- tail(myValues$teamList, 1)
         }
         else if (isolate(myValues$pSel2) == "Baseline") {
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
     
})