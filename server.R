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
     
     # Save version of the draft sheet without deleted players
     filter <- reactive({
          allPlayers[!(allPlayers$PlayerName %in% myValues$dList), ] %>%
            mutate(Adj = ifelse(grepl('P', POS),
                                input$adjustment * Dollars,
                                Dollars)) %>%
            arrange(desc(Adj))
     })
     
     # Display the top 25 entries of the draft sheet
     # Options to filter by team or position
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
     
})