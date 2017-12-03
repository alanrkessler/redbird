shinyServer(function(input, output, session) {
     
     # tabPanel 1 - Draft Board
     
     # Add deleted players to a list
     myValues <- reactiveValues(p2List = "Baseline",
                                p2Sel = "Baseline",
                                p1List = "Baseline",
                                p1Sel = "Baseline")
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
       else if (grepl("P", allPlayers[allPlayers$PlayerName == tail(myValues$teamList, 1), ]$POS) == TRUE) {
         myValues$p1List <- c(isolate(myValues$p1List), tail(myValues$teamList, 1))
         if (isolate(myValues$p1Sel) == "Baseline") {
           myValues$p1Sel <- tail(myValues$teamList, 1)
         }
         updateSelectInput(session = session, inputId = "p1.input", 
                           choices = myValues$p1List,
                           selected = myValues$p1Sel)
       }
     })
     
})