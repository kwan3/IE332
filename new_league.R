#IE332 - Group 17
#New League Script

#==============================================================================
#Script Preparation 
#==============================================================================
#Libraries
library(rvest)
library(stringi)
library(dplyr)
library(plyr)
library(stringr)
library(matrixStats)
library(keras)
library(tidyverse)
library(rsample)
library(tfdatasets)
library(RMySQL)
library(ggplot2)

#Sourcing all required functions
setwd("/Users/maxkwan/Documents/Everything/College/Senior/IE332/Project/") 
source('get_draft.R') #Scrapes user inputted data from txt file
source('NHL Scrape.R') #Scrapes player stats from websites
source('Scores.R') #Calculates a score for players from inputted metrics 
source('kill_db_connections.R') #Used to kill all connections

killDbConnections()

#==============================================================================
#Read, Format, and Validate Inputs
#==============================================================================
#Save connection to MySQL database
db <- dbConnect(MySQL(), 
  user = "g1117498",
  password = "332group17", 
  dbname = "g1117498", 
  host = "mydb.itap.purdue.edu")

#Generate league_id and process .txt file
league_id <- paste('l',length(dbReadTable(db, "leagues")[,1]) + 1,sep="_")
draft <- get_draft(league_id_input = league_id)

#***********USER INPUTS FOR DREW*******************
current_date <- as.Date("2021-10-20") #REMOVE AFTER TESTING
gm_ids_raw <- c("1,2,8,23,24,25,26,27,28,29,30,31,32,33") #REMOVE AFTER TESTING
league_KPIs_raw <- ('G,A,GS,SV') #REMOVE AFTER TESTING

#Format Inputs and Initialize Variables
KPIs <- c("G", "A", "PPG", "PPA", "SHG","SHA", "PIM", "S", "HIT", "BLK", "GP", "GS", "W", "SV", "SVP", "SO")
official_skater_KPIs <- c("G", "A", "PPG", "PPA", "SHG","SHA", "PIM", "S", "HIT", "BLK")
team_names <- unique(draft$Team)
num_teams <- length(team_names)-0
num_rounds <- length(draft$Draft.Position)/num_teams
gm_ids <- unlist(strsplit(gm_ids_raw,","))                         
league_KPIs <- (unlist(strsplit(league_KPIs_raw,","))) 
db_gm_ids <- dbReadTable(db,'userProfiles')$user_id #Query for user ids

#Check for Valid Inputs
valid = FALSE
reason = ""
if (current_date > Sys.Date()) {
  reason = "Invalid Date"
} else if (length(gm_ids) != num_teams) {
  reason = "Incompatible draft txt file and gm list"
} else if (!all(league_KPIs %in% KPIs)) {
  reason = "Inputted KPIs Do Not Exist"
} else if (!all(gm_ids %in% db_gm_ids)) {
  reason = "One or more GM IDs were not found"
} else {
  valid = TRUE
}

#Stops script from executing and returns error to website
if (!valid) {
  print("Invalid League Upload") 
  print(reason)
  #return...
}

#Check Analysis Types and Prepare Proper Data 
if (current_date > as.Date("2021-10-12")) { 
  anal_type <- "Retrospective Analysis"
  db_stats <- dbReadTable(db, 'stats')[,-1]  
  db_players <- dbReadTable(db, 'players')
  db_stats <- merge(db_stats,db_players,by = "Id", all.y = TRUE)
  db_stats[is.na(db_stats)] = 0
} else if (current_date > as.Date("2021-7-7")){
  anal_type <- "Predictive Analysis"
  db_stats <- dbReadTable(db, 'predictions')
  db_players <- dbReadTable(db, 'players')
  db_stats <- merge(db_stats,db_players,by = "Id", all.y = TRUE)
  db_stats[is.na(db_stats)] = 0
} else { 
  anal_type <- "Past Season Analysis"
  db_stats <- get_stats(as.numeric(format(current_date, "%Y")))[,-1]
  db_players <- dbReadTable(db, 'players')
  db_stats <- merge(db_stats,db_players,by = "Id", all.y = TRUE)
  db_stats[is.na(db_stats)] = 0
  #Check if retired players exist in data base 
}

#Separate user inputted KPIs into skater and goalie KPIs
goalie_KPIs <- c()
skater_KPIs <- c()
for (i in 1:length(league_KPIs)) {
  if (all(league_KPIs[i] %in% official_skater_KPIs)) {
    skater_KPIs <- c(skater_KPIs, toString((league_KPIs[i])))
  } else {
    goalie_KPIs <- c(goalie_KPIs, as.character(league_KPIs[i]))
  }
}

#Calculate Scores with user inputted KPIs (Goalie and Skaters separated and remerged)
goalie_score_input <- db_stats[(db_stats$pos == "G"),c(1:7)]
goalie_league_scores <- get_goalie_score(goalie_score_input, goalie_KPIs) 
skater_score_input <- db_stats[(db_stats$pos != "G"),c(1,20,8:17)]
skater_league_scores <- get_skater_score(skater_score_input, skater_KPIs) 
player_league_scores <- rbind.fill(skater_league_scores, goalie_league_scores) 

#Merge stats 
league_stats <- merge(db_stats, player_league_scores[,c('scores', 'Id')], by ='Id', sort = FALSE)
league_stats <- league_stats[order(-league_stats$scores),]
league_stats_raw <- league_stats

#Change Player Positions to F/D/G for draft quality
FDG <- c()
pos <- c()
for (row in 1:nrow(league_stats)){
  if (league_stats[row,'pos'] == 'RW' ||league_stats[row,'pos'] == 'LW' ||league_stats[row,'pos'] == 'C'){
    FDG[row] <- 'F'
    pos[row] <- league_stats[row,'pos']
  }
  else{
    FDG[row] <- league_stats[row,'pos']
    pos[row] <- league_stats[row,'pos']
  }
}
league_stats$pos <- FDG

#Make FLP (first/last/position) id for db_players and draft
FLP <- paste("", 1:length(draft$Team))
for (i in 1:length(draft$Team)) {
  FLP[i] <- toupper(paste(draft$First[i], draft$Last[i], draft$pos[i]))
}
draft <- cbind(draft,FLP)
FLP <- paste("", 1:length(league_stats$Id))
for (i in 1:length(league_stats$Id)) {
  FLP[i] <- toupper(paste(league_stats$first[i], league_stats$last[i], league_stats$pos[i]))
}
FLP <- stringi::stri_trans_general(FLP, "Latin-ASCII")
league_stats <- cbind(league_stats,FLP)

#Merge database with draft on FLP id
merge_stats_raw <- merge(draft, league_stats, by = 'FLP', all.x = TRUE, sort = FALSE)

#==============================================================================
#Process Data to Evaluate the Draft Quality
#==============================================================================
#Pick Significance Calculation
pick_sig <- 100/num_rounds 
coeff_var <- sd(league_stats$scores[1:(length(draft))])/mean(league_stats$scores[1:(length(draft))]) #Andres Points?
full_pick_sig <- c(1)
for (i in 1:num_rounds) {
  pick_sig[i] = (coeff_var )* (num_rounds/2 - i) + 100/num_rounds #/ (num_teams / 2 )
  full_pick_sig <- c(full_pick_sig, rep(pick_sig[i],num_teams))
}
full_pick_sig <- full_pick_sig[-1]

#Initializing Variables for Processing
pick_rating <- (1:length(draft$Draft.Position)) - 0 
optimal_pick <- rep("",length(draft$Draft.Position))
optimal_pick_flp <- rep("",length(draft$Draft.Position))
gm_report <- data.frame(
  "Name" = team_names, 
  "Grade" = rep(0,num_teams),
  "playersMissing" = rep(0,num_teams)
)
merge_stats_clean <- data.frame(
  "Id" = merge_stats_raw$Id, 
  "Player" = merge_stats_raw$Player, 
  "FLP" = merge_stats_raw$FLP, 
  "Team" = merge_stats_raw$Team, 
  "Team_id"= merge_stats_raw$Team_ID, 
  "Draft Position" = merge_stats_raw$Draft.Position, 
  "scores" = merge_stats_raw$scores, 
  "pos" = merge_stats_raw$pos.y
)
available_player_stats <- league_stats

#Determine the optimal pick and best points and calculate the pick rating for all drafted picks
#Store these scores in the report for each team
for (i in 1:length(merge_stats_clean$FLP)) {
  if(grepl(merge_stats_clean$FLP[i], filter(available_player_stats, pos == merge_stats_clean$pos[i])$FLP) %>% sum() > 0) { #If the player is found in both the draft and current season
    #Determine the optimal pick, best points, and pick rating for a draft position
    bestPts <- filter(available_player_stats, pos == merge_stats_clean$pos[i])$scores[1]
    optimal_pick[i] <- filter(available_player_stats, pos == merge_stats_clean$pos[i])$Id[1]
    optimal_pick_flp[i] <- filter(available_player_stats, pos == merge_stats_clean$pos[i])$FLP[1]
    pick_rating[i] <- 1 - (bestPts - merge_stats_clean$scores[i]) / bestPts
    
    #Remove the selected player from the draftable player list
    available_player_stats <- available_player_stats[-grep(merge_stats_clean$FLP[i], available_player_stats$FLP),]
    
    #Store values in gm_report with adjusted significance
    team_location <- grep(merge_stats_clean$Team[i], gm_report$Name)[1]
    gm_report$Grade[team_location] <- gm_report$Grade[team_location] + (pick_rating[i] * full_pick_sig[i])
  } else {
    #Track the number of players that did not match with a player in our database
    pick_rating[i] <- 0
    team_location <- grep(merge_stats_clean$Team[i], gm_report$Name)
    gm_report$playersMissing[team_location] <- gm_report$playersMissing[team_location] + 1
  }
}

#Calculate a penalty for teams that did not draft a good ratio of forwards, defense, and goalies
ideal_FDG_distribution <- c(14/22,6/22,2/22)
for (i in 1:num_teams) {
  team_pos <- merge_stats_clean$pos[merge_stats_clean$Team == team_names[i]]
  team_FDG_distribution <- c((
    abs((team_pos[team_pos == "F"] %>% length())/num_rounds)-ideal_FDG_distribution[1]),
    abs((team_pos[team_pos == "D"] %>% length())/num_rounds-ideal_FDG_distribution[2]),
    abs((team_pos[team_pos == "G"] %>% length())/num_rounds-ideal_FDG_distribution[3])
    ) %>% sum()
  gm_report$Grade[i] <- gm_report$Grade[i] * (1-(team_FDG_distribution))
}

#Generate report messages that give highlights for each team
#Prepare data required for messages
draft_report <- data.frame(
  "Draft.Position" = merge_stats_clean$Draft.Position, 
  "Team" = merge_stats_clean$Team, 
  "Player" = merge_stats_clean$Player, 
  "Pick Rating" = pick_rating, 
  "Score" = merge_stats_clean$scores)
groupteams <- group_by(draft_report, draft_report$Team)
draft_report_metrics <- data.frame(isGoalie = grepl('G', merge_stats_raw$pos.x), Team = merge_stats_raw$Team, Player = merge_stats_raw$Player, merge_stats_raw[,11:26]) #CHANGE TO TEAM ID

#Group players into best/worst picks for each team based on pick rating
worst_players <- filter(groupteams, Pick.Rating %in% range(Pick.Rating)[1])
best_players <- filter(groupteams, Pick.Rating %in% range(Pick.Rating)[2])
worst_players <- worst_players[!duplicated(worst_players$Team), ] 
best_players <- best_players[!duplicated(best_players$Team), ]

#Generate appropriate messages
best_player_message <- best_players %>% left_join(draft_report_metrics) %>% 
  mutate(Message = case_when (
    isGoalie ~ paste(Player,'was the best pick for',Team,'they averaged the following stats:',GS,'games started,',SV,'saves,',SVP,'save percentage,',SO,'shutouts and',W,'wins.'),
    !isGoalie ~ paste(Player,'was the best pick for',Team,'they averaged the following stats',G,'goals,',A,'assists,',PPG,'power play goals,',PPA,'power play assists,',SHG,'short handed goals,',SHA,'short handed assists,',PIM,'penalty infraction minutes,',S,'shots,',HIT,'hits and',BLK,'blocks.')
    )
  )
worst_player_message <- worst_players %>% left_join(draft_report_metrics) %>% 
  mutate(Message = case_when (
    isGoalie ~ paste(Player,'was the worst pick for',Team,'they averaged the following stats:',GS,'games started,',SV,'saves,',SVP,'save percentage,',SO,'shutouts and',W,'wins.'),
    !isGoalie ~ paste(Player,'was the worst pick for',Team,'they averaged the following stats',G,'goals,',A,'assists,',PPG,'power play goals,',PPA,'power play assists,',SHG,'short handed goals,',SHA,'short handed assists,',PIM,'penalty infraction minutes,',S,'shots,',HIT,'hits and',BLK,'blocks.')
    )
  )

#Store messages
best_player_message <- data.frame('Name' = best_player_message$Team, 'Message' = best_player_message$Message)
worst_player_message <- data.frame('Name' = worst_player_message$Team, 'Message' = worst_player_message$Message)
gm_report <- merge(gm_report,best_player_message, by = 'Name')
gm_report <- merge(gm_report,worst_player_message, by = 'Name')

#Format order for possible draft types (8/10/12/14)
if (num_teams == 12) {
  temp <- gm_report[2:4,]
  gm_report <- gm_report[-c(2:4),]
  gm_report <- rbind(gm_report,temp)
} else if (num_teams == 10) {
  temp <- gm_report[2,]
  gm_report <- gm_report[-2,]
  gm_report <-rbind(gm_report,temp)
} else if (num_teams == 14) {
  temp <- gm_report[2:6,]
  gm_report <- gm_report[-c(2:6),]
  gm_report <- rbind(gm_report,temp)
}
draft <- cbind(draft, 'Pick Rating' = pick_rating, 'Optimal Pick' = optimal_pick)

#*************FOR DREW************************
ranked <- gm_report[order(gm_report$Grade),c(1:2)]
barplot(ranked$Grade,names.arg = ranked$Name, xlab="GMs",ylab="Grade",col="cornflower blue",main="GM Grades", ylim = c(0,100))
df <- data.frame("Draft Position" = merge_stats_clean$Draft.Position, "Score" = merge_stats_clean$scores, "Position" = merge_stats_clean$pos)
ggplot(df,aes(Draft.Position, Score, color = Position),xlab("Draft Position"))+geom_point()

#*************FOR DREW************************

#==============================================================================
#Upload Draft Information to MySQL
#==============================================================================
#Format league Data 
league <- data.frame("league_id" = league_id, 
  "date_of_analysis" = current_date, 
  "analysis_type" = anal_type, 
  "point_categories" = league_KPIs_raw, 
  "number_of_rounds"= num_rounds, 
  "number_of_teams" = num_teams
)
league[is.na(league)] = '0'

#Format teams Data 
league_ids <- rep(league_id, num_teams)
teams <- data.frame( 
  "league_id" = league_ids, 
  "user_id" = gm_ids,
  "team_name" = gm_report$Name, 
  "grade" = round(gm_report$Grade),
  "best_pick_report" = gm_report$Message.x,
  "worst_pick_report" = gm_report$Message.y
)
teams[is.na(teams)] = '0'

#Format has_players Data 
has_players_user_id <- paste("NA",length(merge_stats_clean$Id))
league_ids <- rep(league_id, length(merge_stats_clean$Id))
for (i in 1:length(merge_stats_clean$Id)) {
  for (j in 1:(length(team_names))) {
    if (merge_stats_clean$Team[i] == team_names[j]) {
      has_players_user_id[i] = gm_ids[j]
    }
  }
}
has_players <- data.frame(
  "league_id" = league_ids,
  "user_id" = has_players_user_id,
  "Id" = merge_stats_clean$Id,
  "score" = merge_stats_clean$scores,
  "optimal_pick" = optimal_pick
)
has_players[is.na(has_players)] = '0'

#Write leagues, teams, and has_players tables to MySQL
dbWriteTable(db, value = league, name = "leagues", append = TRUE, row.names = FALSE)

dbWriteTable(
  db,"teams",teams,
  field.types = NULL,
  row.names = FALSE,
  append = TRUE,
  allow.keywords = FALSE
)

dbWriteTable(
  db,"has_players",has_players,
  field.types = NULL,
  row.names = FALSE,
  append = TRUE,
)
