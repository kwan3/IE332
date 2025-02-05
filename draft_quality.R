#IE332 Group 17
#Max Kwan
#draft_quality
#Generate Teams, Leagues, and Has_players tables

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

#Functions
setwd("/Users/maxkwan/Documents/Everything/College/Senior/IE332/Project/") 
source('get_draft.R')
source('NHL Scrape.R')
source('Scores.R')

#==============================================================================
#Inputs
#==============================================================================
db <- dbConnect(MySQL(), user = "g1117498", password = "332group17", dbname = "g1117498", host = "mydb.itap.purdue.edu")
#league_inputs_raw <- dbReadTable(db, 'draft') 
#draft <- get_draft(league_inputs_raw[1]) #Replace with get_draft(file location) where file location gets the .txt file
#current_date <- league_inputs_raw[2] #Ensure columns match up
#gm_ids_raw <- league_inputs_raw[3]
#league_KPIs_raw <- league_inputs_raw[4]

league_id <- paste('l',length(dbReadTable(db, "leagues")[,1]) + 1,sep="_") #KNOWN WARNING, SKIP
draft <- get_draft(league_id_input = league_id) #REMOVE AFTER TESTING
current_date <- as.Date("2021-10-20") #REMOVE AFTER TESTING
gm_ids_raw <- c("1,2,8,23,24,25,26,27,28,29,30,31") #REMOVE AFTER TESTING
league_KPIs_raw <- ('G,A,GS,SV') #REMOVE AFTER TESTING

KPIs <- c("G", "A", "PPG", "PPA", "SHG","SHA", "PIM", "S", "HIT", "BLK", "GP", "GS", "W", "SV", "SVP", "SO")
official_skater_KPIs <- c("G", "A", "PPG", "PPA", "SHG","SHA", "PIM", "S", "HIT", "BLK")

#Format Inputs
team_names <- unique(draft$Team)
num_teams <- length(team_names)-0
num_rounds <- length(draft$Draft.Position)/num_teams
gm_ids <- unlist(strsplit(gm_ids_raw,","))                         
league_KPIs <- (unlist(strsplit(league_KPIs_raw,","))) 
db_gm_ids <- dbReadTable(db,'userProfiles')$user_id

#Valid Input Check
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

#Stopping
if (!valid) {
  print("Error pls stop:") #Fix these messages and pass back to website
  print(reason)
} else {
  print("valid input") #Fix these messages
}

#Check Analysis Types and Query Database Inputs (UNTESTED)
if (current_date > as.Date("2021-10-12")) { 
  anal_type <- "Retrospective Analysis"
  db_stats <- dbReadTable(db, 'stats')[,-1]  
  db_players <- dbReadTable(db, 'players')
  db_stats <- merge(db_stats,db_players,by = "Id", all.y = TRUE)
  db_stats[is.na(db_stats)] = 0
} else if (current_date > as.Date("2021-7-7")){
  anal_type <- "Predictive Analysis"
  db_stats <- dbReadTable(db, 'predictions')[,-1]
} else { 
  anal_type <- "Past Season Analysis"
  db_stats <- get_stats(as.numeric(format(current_date, "%Y")))[,-1]
}

print(anal_type) #REMOVE AFTER TESTING
print(db_stats[1:5,]) #REMOVE AFTER TESTING

#Prepare Score Scraping
goalie_KPIs <- c()
skater_KPIs <- c()
for (i in 1:length(league_KPIs)) {
  if (all(league_KPIs[i] %in% official_skater_KPIs)) {
    skater_KPIs <- c(skater_KPIs, toString((league_KPIs[i])))
  } else {
    goalie_KPIs <- c(goalie_KPIs, as.character(league_KPIs[i]))
  }
}

goalie_score_input <- db_stats[(db_stats$pos == "G"),c(1:7)]
goalie_league_scores <- get_goalie_score(goalie_score_input, goalie_KPIs) 

skater_score_input <- db_stats[(db_stats$pos != "G"),c(1,20,8:17)]
skater_league_scores <- get_skater_score(skater_score_input, skater_KPIs) 
player_league_scores <- rbind.fill(skater_league_scores, goalie_league_scores) 

league_stats <- merge(db_stats, player_league_scores[,c('scores', 'Id')], by ='Id', sort = FALSE)
league_stats <- league_stats[order(-league_stats$scores),]
league_stats_raw <- league_stats

#Changing positions to F/D/G
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

#Make a new ids to merge on with first/last/position
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

merge_stats_raw <- merge(draft, league_stats, by = 'FLP', all.x = TRUE, sort = FALSE)

league_stats[1:5,]
#==============================================================================
#Draft Quality
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

pick_rating <- (1:length(draft$Draft.Position)) - 0 #Penalizing for bad point contribution
optimal_pick <- rep("",length(draft$Draft.Position))
optimal_pick_flp <- rep("",length(draft$Draft.Position))
gm_report <- data.frame("Name" = team_names, "Grade" = rep(0,num_teams),"playersMissing" = rep(0,num_teams)) #Remove Players Missing after testing

#merge_stats <- merge(draft, league_stats, by ='Id', all.x = TRUE, sort = FALSE) #Merge by First/Last/Team?
merge_stats_clean <- data.frame("Id" = merge_stats_raw$Id, "Player" = merge_stats_raw$Player, "FLP" = merge_stats_raw$FLP, "Team" = merge_stats_raw$Team, "Team_id"= merge_stats_raw$Team_ID, "Draft Position" = merge_stats_raw$Draft.Position, "scores" = merge_stats_raw$scores, "pos" = merge_stats_raw$pos.y) #CHANGE TO TEAM ID ONCE FIXED
available_player_stats <- league_stats

#Calculate Draft Quality (Player performance, gm grade, gm report, and team specific report)
#Iterate over draft, calculate pick rating, store in gm_report
for (i in 1:length(merge_stats_clean$FLP)) {
  if(grepl(merge_stats_clean$FLP[i], filter(available_player_stats, pos == merge_stats_clean$pos[i])$FLP) %>% sum() > 0) { #If the player is found in both the draft and current season
    #optimal pick and pick_rating calculation
    bestPts <- filter(available_player_stats, pos == merge_stats_clean$pos[i])$scores[1]
    optimal_pick[i] <- filter(available_player_stats, pos == merge_stats_clean$pos[i])$Id[1]
    optimal_pick_flp[i] <- filter(available_player_stats, pos == merge_stats_clean$pos[i])$FLP[1]
    pick_rating[i] <- 1 - (bestPts - merge_stats_clean$scores[i]) / bestPts
    
    #Remove from available players (current season)
    available_player_stats <- available_player_stats[-grep(merge_stats_clean$FLP[i], available_player_stats$FLP),]
    
    #Store in gm_report with adjusted significance
    team_location <- grep(merge_stats_clean$Team[i], gm_report$Name)[1]
    gm_report$Grade[team_location] <- gm_report$Grade[team_location] + (pick_rating[i] * full_pick_sig[i])
  } else {
    #For Players Missing Data
    pick_rating[i] <- 0
    team_location <- grep(merge_stats_clean$Team[i], gm_report$Name)
    gm_report$playersMissing[team_location] <- gm_report$playersMissing[team_location] + 1
  }
}

#Penalty
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

#GM REPORT
draft_report <- data.frame("Draft.Position" = merge_stats_clean$Draft.Position, "Team" = merge_stats_clean$Team, "Player" = merge_stats_clean$Player, "Pick Rating" = pick_rating, "Score" = merge_stats_clean$scores)

groupteams <- group_by(draft_report, draft_report$Team)

draft_report_metrics <- data.frame(isGoalie = grepl('G', merge_stats_raw$pos.x), Team = merge_stats_raw$Team, Player = merge_stats_raw$Player, merge_stats_raw[,11:26]) #CHANGE TO TEAM ID

df_1 <- groupteams %>% 
  filter(Pick.Rating %in% range(Pick.Rating)) %>% 
  mutate(Best = ifelse(Pick.Rating==max(Pick.Rating),1,0)) %>% 
  left_join(draft_report_metrics) %>% 
  arrange(desc(Pick.Rating))

worst_players <- filter(groupteams, Pick.Rating %in% range(Pick.Rating)[1])
best_players <- filter(groupteams, Pick.Rating %in% range(Pick.Rating)[2])
worst_players <- worst_players[!duplicated(worst_players$Team), ] #CHANGE TO TEAM ID
best_players <- best_players[!duplicated(best_players$Team), ]#CHANGE TO TEAM ID

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
best_player_message <- data.frame('Name' = best_player_message$Team, 'Message' = best_player_message$Message)
worst_player_message <- data.frame('Name' = worst_player_message$Team, 'Message' = worst_player_message$Message)
gm_report <- merge(gm_report,best_player_message, by = 'Name')
gm_report <- merge(gm_report,worst_player_message, by = 'Name')

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

#==============================================================================
#Create Tables
#==============================================================================
#League
#Create 1 new row 
#LeagueID, num_teams, num_rounds, season, point type, draft type, 
#maybe: validity
league <- data.frame("league_id" = league_id, 
  "date_of_analysis" = current_date, 
  "analysis_type" = anal_type, 
  "point_categories" = league_KPIs_raw, 
  "number_of_rounds"= num_rounds, 
  "number_of_teams" = num_teams
)
#Write Table vs Insert
dbWriteTable(db, value = league, name = "leagues", append = TRUE, row.names = FALSE)

#Teams
#Create N new rows (N = num_teams)
#LeagueID, TeamID, TeamName, grade, report
#GM_ID, how should this be created

league_ids <- rep(league_id, num_teams)
#team_ids <- paste(league_ids, gm_ids,sep = "") #Will need to update
teams <- data.frame( 
  "league_id" = league_ids, 
  "user_id" = gm_ids,
  "team_name" = gm_report$Name, 
  "grade" = round(gm_report$Grade),
  "best_pick_report" = gm_report$Message.x,
  "worst_pick_report" = gm_report$Message.y
)
dbWriteTable(db,"teams",teams,
  field.types = NULL, #Column Headers character string (Not sure what this does)
  row.names = FALSE, 
  append = TRUE, #CHANGE OVERWRITE AFTER TESTING
  allow.keywords = FALSE
)

#has_players
#Create N new rows (N = num_drafted_players)
#TeamID, PlayerID or PlayerName, points, 3 ranked scores, and optimal pick
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
  "optimal_pick" = paste("Pick", optimal_pick)) #THIS WILL BE A FK
dbWriteTable(db,"has_players",has_players,
  field.types = NULL, #Column Headers character string
  row.names = FALSE, 
  append = TRUE, 
)
# create table MailSent(
#   
#   Id int primary key,
#   
#   profil_sender_id int,
#   
#   profil_receiver_id int,
#   
#   FOREIGN KEY (profil_sender_id) REFERENCES profil(id),
#   
#   FOREIGN KEY (profil_receiver_id) REFERENCES profil(id)
#   
# )
dbReadTable(db, 'leagues')
dbReadTable(db, 'teams')
dbReadTable(db, 'has_players')
#==============================================================================
#Testing SQL Outputs
#==============================================================================
# test_league_sql <- data.frame("League ID" = 'TEST_LEAGUE_ID_1', "Season" = as.numeric(format(current_date, "%Y")), "Analysis Type" = 'Retrospective Analysis', "Point Categories" = ('G,A,GS'), "Number of Rounds"= '12', "Number of Teams" = '12')
# dbWriteTable(db, value = test_league_sql, name = "league",overwrite = TRUE,)
# 
# test_teams_sql <- data.frame("Team ID" = c("team3","team4"), "GM ID" = c('id1','id2'), "League ID" = 'TEST_LEAGUE_ID_2', "Team Name" = c("Joe1","Joe2"), "Grade" = c(95,87), "Report" = c("The Best Player on team1 was...","The Best Player on team2 was..."))
# dbWriteTable(db, value = test_teams_sql, name = "teams",overwrite = TRUE)
# 
# test_has_player_sql <- data.frame("TeamID" = c("team1","team1","team1"), "PlayerID" = c("Joe1_id","Joe2_id","Joe3_id"), "PlayerName" = c("Joe1","Joe2","Joe3"), "points" = c(42,54,10))
# dbWriteTable(db, value = test_has_player_sql, name = "has_players",overwrite = TRUE)
# 
# dbReadTable(db, 'league')
# dbReadTable(db, 'teams')
# dbReadTable(db, 'has_player')
# 
# # Run query to get results as dataframe
# dbGetQuery(con, "SELECT * FROM arrests limit 3")
# 
# # Send query to pull requests in batches
# res <- dbSendQuery(con, "SELECT * FROM arrests")
# data <- dbFetch(res, n = 2)
# data
# dbHasCompleted(res)
# 
# dbListResults(con)
# dbClearResult(res)
# dbRemoveTable(con, "arrests")
# dbDisconnect(con)
