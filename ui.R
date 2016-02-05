shinyUI(navbarPage("Fantasy Baseball Draft App",
     tabPanel("Draft Board",
          fluidPage(
               fluidRow(align="left",
                    #Selection list for position
                    column(4,selectInput('posa', 'Position', positions_all)),
                    #Text input to remove players when they are drafted
                    column(4,textInput("text", label = "Player", value = ""),
                    #Delete button 
                    actionButton(inputId = "delete.button", label = "Delete", icon = icon("minus")))
               ),
               fluidRow(align="center",
                    #Draft sheet
                    DT::dataTableOutput("sheet")
               )
          )
     ),
     tabPanel("Player Tiers",
              fluidPage(
                   fluidRow(align="center",
                        #Display tiers after removing drafted players
                        h4("Current Tiers"),
                        DT::dataTableOutput("tierc")
                   ),
                   fluidRow(align="center",
                        #Display tiers at the start of the draft
                        h4("Starting Tiers"),
                        DT::dataTableOutput("tiers")
                   )
              )
     ),
     tabPanel("Plots",
              fluidPage(
                   fluidRow(align="left",
                        #Selection list for position
                        column(4,selectInput('pos', 'Position', positions)),
                        #Choose plotting by overall rank or position rank
                        column(4,selectInput('xvar', 'Rank', xaxis)),
                        column(4,selectInput('drft', 'Show Drafted', c("Yes","No")))
                   ),
                   fluidRow(align="center",
                        h4("Talent by Position Rank"),
                        ggvisOutput("plot1")
                   ),
                   fluidRow(align="center",
                        uiOutput("ui")
                            #sliderInput("range","",
                                    #min = 1, max = 500, value = c(1,50),width=800)
                   )
              )
     ),
     tabPanel("Help",
              fluidPage(
                   includeMarkdown("README.md")     
              )
     )
     ))