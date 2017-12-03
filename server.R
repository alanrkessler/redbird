shinyServer(function(input, output, session) {
     
     # tabPanel 1 - Draft Board
     
     # Add deleted players to a list
     myValues <- reactiveValues()
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
          allPlayers[!(allPlayers$PlayerName %in% myValues$dList), -7]
     })
     
     # Save dataframe of players drafted 
     cteam <- reactive({
       allPlayers[(allPlayers$PlayerName %in% myValues$teamList), -7]
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
       myValues$teamList
       updateSelectInput(session = session, inputId = "test1", 
                         choices = c("Baseline", myValues$teamList),
                         selected = myValues$teamList[length(myValues$teamList)])
     })
     
})