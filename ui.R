library(shiny)
library(DT)

# Define the overall UI
shinyUI(fluidPage(
          titlePanel("Eckstein123"),
          sidebarLayout(
               sidebarPanel(
                    #Text box for players to delete
                    textInput("text", label = "Player", value = ""),
                    #Delete button 
                    actionButton(inputId = "delete.button", label = "Delete", icon = icon("minus"))
               ),
               mainPanel(
                    DT::dataTableOutput("table")
               )
          )
))