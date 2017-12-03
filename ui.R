shinyUI(navbarPage("Fantasy Baseball Draft App",
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
                           # Draft
                           shiny::actionButton(inputId = "draft.button", 
                                               label = "Draft", width = "75px", 
                                               icon("check"), 
                                               style="color: #fff; background-color: #74C476; border-color: #74C476"),
                           # Delete (Someone else drafted this player)
                           shiny::actionButton(inputId = "delete.button", 
                                               label = "Delete", width = "75px", 
                                               icon("remove"), 
                                               style="color: #fff; background-color: #FB6A4A; border-color: #FB6A4A")),
                    
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
     ),
     tabPanel("My Team",
              fluidPage(
                   fluidRow(align = "left",
                        # Display tiers after removing drafted players
                        h4("My Players"),
                        selectInput('test1', 'Testing', c("Baseline")),
                        DT::dataTableOutput("cteam")
                   ),
                   fluidRow(align = "center",
                        # Display tiers at the start of the draft
                        h4("Draft Log")
                   )
              )
     ),
     tabPanel("Help",
              fluidPage(
                   includeMarkdown("help.md")     
              )
     )
     ))