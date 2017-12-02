shinyUI(navbarPage("Fantasy Baseball Draft App",
     tabPanel("Draft Board",
          fluidPage(
               fluidRow(align="left",
                        
                    # Text input to remove players when they are drafted
                    column(4, style='padding:0px;', strong('Player'), div(style = "height:6px;background-color: white;"),
                           textInput.typeahead(id = "text",
                                               placeholder = "", 
                                               local = autocomplete.list,
                                               valueKey = "PlayerName",
                                               tokens = c(1:dim(autocomplete.list)[1]),
                                               template="<p class='repo-language'>{{info}}</p> <p class='repo-name'>{{PlayerName}}</p>"),
                           div(style = "height:6px;background-color: white;"),
                           # Delete
                           shiny::actionButton(inputId = "delete.button", label = "Delete", icon("paper-plane"), 
                                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                           # Draft
                           shiny::actionButton(inputId = "delete.button", label = "Delete", icon("paper-plane"), 
                                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                    
                    # Selection list for position
                    column(4, offset = 0, selectInput('posa', 'Position', positions.all, width = '50%'), selectInput('teama', 'Team', teams.all, width = '50%')),
                    
                    # Delete button 
                    #column(4, div(style = "height:6px;background-color: white;"),
                    #       actionButton(inputId = "delete.button", label = "Delete")),
                    shiny::actionButton(inputId = "delete.button", label = "Delete", icon("paper-plane"), 
                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                    shiny::actionButton(inputId = "delete.button", label = "Delete", icon("paper-plane"), 
                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
               ),
               fluidRow(align = "center",
                    # Draft sheet
                    DT::dataTableOutput("sheet")
               )
          )
     ),
     tabPanel("Player Tiers",
              fluidPage(
                   fluidRow(align = "center",
                        # Display tiers after removing drafted players
                        h4("Current Tiers")
                   ),
                   fluidRow(align = "center",
                        # Display tiers at the start of the draft
                        h4("Starting Tiers")
                   )
              )
     ),
     tabPanel("Help",
              fluidPage(
                   includeMarkdown("help.md")     
              )
     )
     ))