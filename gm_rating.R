#IE332 Group 17
#Max Kwan
#gm_rating
#Calculates a grade for every GM

#Libraries
library(rvest)
library(dplyr)
library(stringr)
library(matrixStats)
library(keras)
library(tidyverse)
library(rsample)
library(tfdatasets)

#Store user inputs and collects data
draft <- get_draft() #Collect draft data from txt file
current_date <- as.Date("2021-10-20") 
current_season <- get_stats(as.numeric(format(current_date, "%Y")))
current_season <- current_season[order(-current_season$Points),] #Sort by points
current_season_raw <- get_stats(as.numeric(format(current_date, "%Y")))
current_season_raw <- current_season_raw[order(-current_season_raw$Points),]

#Store important data
team_names <- unique(draft$Team)
num_teams <- length(team_names)-0
num_rounds <- length(draft$Draft.Position)/num_teams
pick_sig <- 100/num_rounds #Can be turned into a vector that changes by round

#Creates the running draft
running_draft <- merge(draft, current_season, by ='Player', all.x = TRUE, sort = FALSE) #merge draft and current season 

#Initializing Vectors
pick_rating <- (1:length(draft$Draft.Position)) - 0
optimal_pick <-rep("",length(draft$Draft.Position))
gm_report <- data.frame("Name" = team_names, "Grade" = rep(0,num_teams),"playersMissing" = rep(0,num_teams))

#Iterate over draft, calculate pick rating, store in gm_report
for (i in 1:length(draft$Draft.Position)) {
  if(grepl(draft$Player[i], filter(current_season, Pos == running_draft$Pos[i])$Player) %>% sum() > 0) { #If the player is found in both the draft and current season
    #optimal pick and pick_rating calculation
    bestPts <- filter(current_season, Pos == running_draft$Pos[i])$Points[1]
    optimal_pick[i] <- filter(current_season, Pos == running_draft$Pos[i])$Player[1]
    pick_rating[i] <- 1 - (bestPts - running_draft$Points[i])/ bestPts
    
    #Remove from available players (current season)
    current_season <- current_season[-grep(draft$Player[i], current_season$Player),]
    
    #Store in gm_report with adjusted significance
    team_location <- grep(running_draft$Team[i], gm_report$Name)
    gm_report$Grade[team_location] <- gm_report$Grade[team_location] + round(pick_rating[i] * pick_sig)
  } else {
    #For Player Missing Data
    pick_rating[i] <- 0
    team_location <- grep(draft$Team[i], gm_report$Name)
    gm_report$playersMissing[team_location] <- gm_report$playersMissing[team_location] + 1
  }
}

#Stores final results
draft <- cbind(draft, 'Pick Rating' =pick_rating, 'Optimal Pick' =optimal_pick)
print(gm_report)
print(draft)

#Errors 10/26
#Missing Goalies and some players
#2022: #7 David Pastrnak is not optimal but given a 1.00 pick rating
#2022

#Errors:
#Missing players from current season raw
#For 2021, 44 players missing. Positions include goalie, RW (weird bc it's accounted for in NHL_scrape)
#get_stats does not pull the same list as current season, potential issues with it

#Old Code
#pick_rating[i] <- grep(draft$Player[i], filter(current_season, Pos == running_draft$Pos[i])$Player)                                   #Finds the index of the pick from the running list
#current_season <- current_season[-grep(draft$Player[i], current_season$Player),]                                                      #Removes player from running list
#gm_report$Grade[grep(running_draft$Team[i], gm_report$Name)] <- gm_report$Grade[grep(running_draft$Team[i], gm_report$Name)] + pick_rating[i] #Adds pick rating to cumulative gm grade