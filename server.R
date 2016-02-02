library(shiny)
library(DT)
library(dplyr)

setwd("~/Documents/Data Science/Sabremetrics/Fantasy Draft/data")

#Import batters (b) and pitchers (p)
b<-read.csv("batters.csv")
p<-read.csv("pitchers.csv")

#Select variables
b<-b %>% select(PlayerName,Team,POS,PA,Dollars)
p<-p %>% select(PlayerName,Team,POS,IP,Dollars)

#Create a standard variable for innings and plate appearaces
colnames(b)[4]<-"PA.IP"
colnames(p)[4]<-"PA.IP"

#Merge b & p
a<-rbind(b,p)
rm(b,p)

#Convert dollars to numeric
#Remove dollar signs
a$Dollars<-(sub('$','',as.character(a$Dollars),fixed=TRUE))
#Remove negatives
a$Dollars<-(sub(')','',as.character(a$Dollars),fixed=TRUE))
#Convert to numeric
a$Dollars<-as.numeric(sub('(','-',as.character(a$Dollars),fixed=TRUE))

#Sort by dollars
a<-a[order(-a$Dollars),]

#Reset the rownames
rownames(a) <- seq(length=nrow(a))

setwd("~/Documents/Data Science/Sabremetrics/Fantasy Draft/akdraft")

shinyServer(function(input, output) {
     
     #Add deleted players to a list
     myValues <- reactiveValues()
     observe({
          if(input$delete.button > 0){
               myValues$dList <- c(isolate(myValues$dList), input$text)
          }
     })
     
     #Save version with removed players
     filter <- reactive({
          a[!(a$PlayerName %in% myValues$dList),]
     })
     
     ##Display a table with the default of 25 entries
     output$table <- DT::renderDataTable({
          DT::datatable(filter(),options = list(pageLength = 25))
     })
     
})