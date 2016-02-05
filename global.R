library(shiny)
library(DT)
library(dplyr)
library(ggvis)

#Number of teams in the league
tms<-20

#Import batters (b) and pitchers (p)
b<-read.csv("./data/batters.csv")
p<-read.csv("./data/pitchers.csv")

#Select variables
b<-b %>% select(PlayerName,Team,POS,PA,Dollars)
p<-p %>% select(PlayerName,Team,POS,IP,Dollars)

#Create a standard variable for innings and plate appearaces
colnames(b)[4]<-"PA.IP"
colnames(p)[4]<-"PA.IP"

#Merge b & p
all_players<-rbind(b,p)
rm(b,p)

#Convert dollars to numeric
#Remove dollar signs
all_players$Dollars<-(sub('$','',as.character(all_players$Dollars),fixed=TRUE))
#Remove negatives
all_players$Dollars<-(sub(')','',as.character(all_players$Dollars),fixed=TRUE))
#Convert to numeric
all_players$Dollars<-as.numeric(sub('(','-',as.character(all_players$Dollars),fixed=TRUE))

#Sort by dollars descending
all_players<-all_players[order(-all_players$Dollars),]

#Reset the row names
rownames(all_players) <- seq(length=nrow(all_players))

#Provide an ID to each value
all_players$Ovr.Rank<-1:nrow(all_players)

#Function that categorizes players into 1 of 5 possible tiers for each position
#Tier 5 are those with fewer PA/IP than the set threshold
#Tier 4 are those with a rank worse than the number of starting spots on all teams
#Tiers 1-3 are set based on k-means clustering using the dollar amount 
#Select a position, spots for that position in the league, and minimum PA/IP
player_tier<-function(pos="C",spts=1,flr=200){
     #Copy data
     fil<-all_players
     #Filter for the selected position
     fil<-fil[grepl(pos,fil$POS)==TRUE,]
     #Rank by dollars
     fil$rank<-rank(-fil$Dollars)
     
     #If less than the min PA.IP go to tier 5, 
     #If worse than number of spots go to tier 4,
     #Otherwise make temporary tier 1
     fil$Tier<-ifelse(fil$PA.IP<flr,5,ifelse(fil$rank>(tms*spts),4,1))
     
     #Create separate data frames for top tiers and bottom tiers
     fil1<-fil[fil$Tier==1,]
     fil2<-fil[fil$Tier!=1,]
     
     #Cluster top tiers
     filk<-kmeans(fil1$Dollars,3,iter.max=1000,nstart=10)
     
     #Create a table of assignments to clusters
     filk_assign<-data.frame(filk$cluster,1:length(filk$cluster))
     names(filk_assign)<-c("c","order")
     
     #Create custom cluster number scheme
     filk_clusname<-data.frame(1:3,rank(-filk$centers))
     names(filk_clusname)<-c("c","cluster")
     
     #Merge assignments and custom names and order
     fil_merge<-merge(filk_assign,filk_clusname)
     fil_merge<-fil_merge[order(fil_merge$order),3]
     
     #Bind tier assignments
     fil1<-cbind(fil1,fil_merge)
     
     #Overwrite tier with cluster
     fil1$Tier<-fil1$fil_merge
     
     #Remove temporary variables
     fil1$fil_merge<-NULL
     fil1$rank<-NULL
     fil2$rank<-NULL
     
     #Recombine top and bottom tier groups
     fil_fin<-rbind(fil1,fil2)
     
     #Rename tier to show position
     colnames(fil_fin)[7]<-paste("Tier",pos,sep=".")
     
     #Return tiers for the selected position
     fil_fin
}

#Create a copy before joining tier assignments
a_start<-all_players
#Join tier assignments
a_start<-merge(a_start,player_tier("C",1,200),all=TRUE)
a_start<-merge(a_start,player_tier("1B",1,200),all=TRUE)
a_start<-merge(a_start,player_tier("2B",1,200),all=TRUE)
a_start<-merge(a_start,player_tier("3B",1,200),all=TRUE)
a_start<-merge(a_start,player_tier("SS",1,200),all=TRUE)
a_start<-merge(a_start,player_tier("OF",3,200),all=TRUE)
a_start<-merge(a_start,player_tier("SP",4,50),all=TRUE)
a_start<-merge(a_start,player_tier("RP",1,20),all=TRUE)
a_start<-merge(a_start,player_tier("P",2,20),all=TRUE)

#Sort by Dollars
tier_data<-a_start[order(-a_start$Dollars),]
rm(a_start)

#Create lists for selectInputs
#Positions including "All" (for draft sheet filter)
positions_all<-c("All","C","1B","2B","3B","SS","OF","SP","RP","P")
#Positions exlcuding "All" (for plot filter)
positions<-c("C","1B","2B","3B","SS","OF","SP","RP","P")
#X-axis options (for plot)
xaxis<-c("Pos.Rank","Ovr.Rank")
names(xaxis)<-c("Position Rank","Overall Rank")

#Generate tier table before the draft
tier_start<-cbind(1:5,as.data.frame(lapply(tier_data[,7:15],table))[,seq(2, ncol(tier_data[,7:15])*2, by = 2)])
names(tier_start)<-c("Tier","Pos.C","Pos.1B","Pos.2B","Pos.3B","Pos.SS","Pos.OF","Pos.SP","Pos.RP","Pos.P")
