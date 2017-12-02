shinyServer(function(input, output) {
     
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
     
     # Save version of the draft sheet without drafted/deleted players
     filter <- reactive({
          all.players[!(all.players$PlayerName %in% myValues$dList), -7]
     })
     
     cteam <- reactive({
       all.players[(all.players$PlayerName %in% myValues$teamList), -7]
     })
     
     # Display the top 25 entries of the draft sheet
     output$sheet <- DT::renderDataTable({
          if (input$posa != "All" & input$teama == "All") {
               DT::datatable(filter()[grepl(input$posa,filter()$POS) == TRUE,], options = list(pageLength = 25)) 
          }
          else if (input$posa == "All" & input$teama != "All"){
               DT::datatable(filter()[grepl(input$teama,filter()$Team) == TRUE,], options = list(pageLength = 25)) 
          }
          else if (input$posa != "All" & input$teama != "All"){
               DT::datatable(filter()[grepl(input$teama,filter()$Team) == TRUE & grepl(input$posa,filter()$POS) == TRUE,], options = list(pageLength = 25)) 
          }
          else {
               DT::datatable(filter(), options = list(pageLength = 25)) 
          }
     })
     
     
     # Display current team
     output$cteam <- DT::renderDataTable({
       DT::datatable(cteam())
     })
})