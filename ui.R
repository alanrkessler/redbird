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
                        column(4, 
                               selectInput('p2.input', 'Catcher', 
                                           c("Baseline"), width = '60%'),
                               selectInput('p3.input', '1B', 
                                           c("Baseline"), width = '60%'),
                               selectInput('p4.input', '2B', 
                                           c("Baseline"), width = '60%'),
                               selectInput('p5.input', '3B', 
                                           c("Baseline"), width = '60%'),
                               selectInput('p6.input', 'SS', 
                                           c("Baseline"), width = '60%'),
                               selectInput('p7.input', 'OF', 
                                           c("Baseline"), width = '60%'),
                               selectInput('p8.input', 'OF', 
                                           c("Baseline"), width = '60%'),
                               selectInput('p9.input', 'OF', 
                                           c("Baseline"), width = '60%')),
                        column(4,
                               selectInput('p1.input1', 'Starting Pitcher', 
                                           c("Baseline"), width = '60%'),
                               selectInput('p1.input2', 'Starting Pitcher', 
                                           c("Baseline"), width = '60%'),
                               selectInput('p1.input3', 'Starting Pitcher', 
                                           c("Baseline"), width = '60%'),
                               selectInput('p1.input4', 'Starting Pitcher', 
                                           c("Baseline"), width = '60%'),
                               selectInput('pr.input', 'Relief Pitcher', 
                                           c("Baseline"), width = '60%'),
                               selectInput('p.input1', 'Pitcher', 
                                           c("Baseline"), width = '60%'),
                               selectInput('p.input2', 'Pitcher', 
                                           c("Baseline"), width = '60%')),
                        
                        DT::dataTableOutput("teamProjB")
                   )
              )
     ),
     tabPanel("Help",
              fluidPage(
                   includeMarkdown("help.md")     
              )
     )
     ))