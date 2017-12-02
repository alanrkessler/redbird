shinyServer(function(input, output) {
     
     # tabPanel 1 - Draft Board
     
     # Add drafted (deleted) players to a list and add a pick counter
     myValues <- reactiveValues()
     observe({
          myValues$counter <- 1
          if(input$delete.button == 0)
              return()
          isolate({
              myValues$dList <- c(isolate(myValues$dList), input$text)
              myValues$counter <- myValues$counter + 1
          })
          
     })
     
     # Save version of the draft sheet without drafted players
     # Add dynamic probability of pick
     filter <- reactive({
          all.players[!(all.players$PlayerName %in% myValues$dList), -7]
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
})