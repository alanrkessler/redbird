shinyUI(navbarPage("Fantasy Baseball Draft Helper App",
     tabPanel("Draft Board",
          fluidPage(
               fluidRow(align="left",
                        
                    # Text input to remove players when they are drafted
                    column(4, style='padding:0px;', strong('Player'), 
                           div(style = "height:6px;background-color: white;"),
                           textInput.typeahead(id = "text",
                                               placeholder = "", 
                                               local = autocompleteList,
                                               valueKey = "PlayerName",
                                               tokens = c(1:dim(autocompleteList)[1]),
                                               template="<p class='repo-language'>{{info}}</p> <p class='repo-name'>{{PlayerName}}</p>"),
                           div(style = "height:6px;background-color: white;"),
                           # Delete (Someone else drafted this player)
                           shiny::actionButton(inputId = "delete.button", 
                                               label = "Remove", width = "100px", 
                                               icon("remove"), 
                                               style="color: #fff; background-color: #FB6A4A; border-color: #FB6A4A"),
                           numericInput('adjustment', 'Pitcher Mult', 1,
                                        min = 0, max = 10, step = 0.1, 
                                        width = '40%')),
                    
                    # Selection list for position
                    column(4, offset = 0, 
                           selectInput('pos.input', 'Position', 
                                       positionsAll, width = '50%'), 
                           selectInput('team.input', 'Team', 
                                       teamsAll, width = '50%'))
               ),
               fluidRow(align = "center",
                    # Draft sheet
                    DT::dataTableOutput("sheet")
               )
          )
     )
     ))