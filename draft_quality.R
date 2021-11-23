#IE332 Group 17
#Max Kwan
#draft_quality
#Generate Teams, Leagues, and Has_players tables

#Libraries
library(rvest)
library(dplyr)
library(stringr)
library(matrixStats)
library(keras)
library(tidyverse)
library(rsample)
library(tfdatasets)
library(RMySQL)

#Functions
#source("get_draft.R")
#source("NHL_Scrape.R")
#source("get_score.R")

#Query User Inputs (.txt of draft,Date of date, varchar of gm_id, varchar of stats) 
mydb <- dbConnect(MySQL(), user = "g1117498", password = "332group17", dbname = "g1117498", host = "mydb.itap.purdue.edu")
#league_inputs_raw <- dbReadTable(mydb, draft) #ERROR: How to determine which row we are processing? #Fix by deleting after it is created
#draft <- get_draft(league_inputs_raw[1]) #ERROR: passing a file when location is required
#current_date <- league_inputs_raw[2]
#gm_ids_raw <- league_inputs_raw[3]
#league_KPIs_raw <- league_inputs_raw[4]

draft <- get_draft()                                           #placeholders VVV
current_date <- as.Date("2021-10-20") 
gm_ids_raw <- c("Joe1,Joe2,Joe3,Joe4,Joe5,Joe6,Joe7,Joe8,Joe9,Joe10,Joe11,Joe12") 
league_KPIs_raw <- ("G,A,GS")

KPIs <- c("G","A", "GS", "HIT") #List of all possible KPIs

#Format Inputs
team_names <- unique(draft$Team)
num_teams <- length(team_names)-0
num_rounds <- length(draft$Draft.Position)/num_teams
gm_ids <- unlist(strsplit(gm_ids_raw,","))                         
league_KPIs <- unlist(strsplit(league_KPIs_raw,",")) #May have to remove spaces before and after

#Check Validity (UNTESTED)
valid = FALSE
reason = ""
if (current_date > Sys.Date()) {
  reason = "Invalid Date"
} else if (length(gm_ids) != num_teams) {
  reason = "Incompatible draft txt file and gm list"
} else if (!all(league_KPIs %in% KPIs)) {
  reason = "Inputted KPIs Do Not Exist"
} else {
  valid = TRUE
}
if (!valid) {
  #return error and reason
  print("Error pls stop:")
  print(reason)
} else {
  print("valid input")
}

#Check Analysis Types and Query Database Inputs (UNTESTED)
if (current_date > as.Date("2021-10-12")) { #Current Season Retrospective
  anal_type <- "Retrospective Analysis"
  db_stats <- dbReadTable(mydb, 'stats')   #Check Name of table
} else if (current_date > as.Date("2021-7-7")){ #Current Season Predictive
  anal_type <- "Predictive Analysis"
  db_stats <- dbReadTable(mydb, 'predictions')
} else { #Past Season
  anal_type <- "Past Season Analysis"
  db_stats <- get_stats(as.numeric(format(current_date, "%Y")))
}

#For Testing
print(anal_type)
print(db_stats[1:5,])

#player_league_scores <- get_score(league_KPIs) #Double check inputs w/ Andres
#league_stats <- merge(db_stats, player_league_scores, by ='Player', all.x = TRUE, sort = FALSE) #MUST CHANGE merge by 'Player'
#league_stats <- league_stats[order(-league_stats$Points),]  #MUST CHANGE $Points -> $Andres_Points
#league_stats_raw <- league_stats

#Initialize Required Variables

#Pick Significance Calculation
#pick_sig <- 100/num_rounds for Uniform #Placeholder
coeff_var <- sd(league_stats$Points[1:(length(draft))])/mean(league_stats$Points[1:(length(draft))]) #Andres Points?
full_pick_sig <- c(1)
for (i in 1:num_rounds) {
  pick_sig[i] = (coeff_var )* (num_rounds/2 - i) + 100/num_rounds #/ (num_teams / 2 )
  full_pick_sig <- c(full_pick_sig, rep(pick_sig[i],num_teams))
}
full_pick_sig <- full_pick_sig[-1]

pick_rating <- (1:length(draft$Draft.Position)) - 0 #Penalizing for bad point contribution
optimal_pick <-rep("",length(draft$Draft.Position))
gm_report <- data.frame("Name" = team_names, "Grade" = rep(0,num_teams),"playersMissing" = rep(0,num_teams)) #Remove Players Missing after testing

merge_stats <- merge(draft, league_stats, by ='Player', all.x = TRUE, sort = FALSE) #Merge by id
available_player_stats <- league_stats

#Calculate Draft Quality (Player performance, gm grade, gm report, and team specific report)
#Iterate over draft, calculate pick rating, store in gm_report
for (i in 1:length(draft$Draft.Position)) {
  if(grepl(draft$Player[i], filter(available_player_stats, Pos == merge_stats$Pos[i])$Player) %>% sum() > 0) { #If the player is found in both the draft and current season
    #optimal pick and pick_rating calculation
    bestPts <- filter(available_player_stats, Pos == merge_stats$Pos[i])$Points[1]
    optimal_pick[i] <- filter(available_player_stats, Pos == merge_stats$Pos[i])$Player[1]
    pick_rating[i] <- 1 - (bestPts - merge_stats$Points[i])/ bestPts
    
    #Remove from available players (current season)
    available_player_stats <- available_player_stats[-grep(draft$Player[i], available_player_stats$Player),]
    
    #Store in gm_report with adjusted significance
    team_location <- grep(merge_stats$Team[i], gm_report$Name)
    gm_report$Grade[team_location] <- gm_report$Grade[team_location] + round(pick_rating[i] * full_pick_sig[i])
  } else {
    #For Players Missing Data
    pick_rating[i] <- 0
    team_location <- grep(draft$Team[i], gm_report$Name)
    gm_report$playersMissing[team_location] <- gm_report$playersMissing[team_location] + 1
  }
}
#gm_report_text <- get_gm_report() from Jose

#Save Values Locally
draft <- cbind(draft, 'Pick Rating' = pick_rating, 'Optimal Pick' = optimal_pick)

#Prepare for Export

#Create Tables

#League
#Create 1 new row 
#LeagueID, num_teams, num_rounds, season, point type, draft type, 
#maybe: date, validity
league_id <- length(dbReadTable(mydb, league)) + 1 #Temporary
league <- data.frame("League ID" = league_id, 
                     "Season" = as.numeric(format(current_date, "%Y")), 
                     "Analysis Type" = anal_type, 
                     "Point Categories" = league_KPIs_raw, 
                     "Number of Rounds"= num_rounds, 
                     "Number of Teams" = num_teams)
dbWriteTable(mydb, value = league, name = "league",append = TRUE)

#Teams
#Create N new rows (N = num_teams)
#LeagueID, TeamID, TeamName, grade, report
#GM_ID, how should this be created

league_ids <- rep(league_id, num_teams)
team_ids <- league_ids + gm_ids #Will need to update
teams <- data.frame("Team ID" = team_ids, "GM ID" = gm_ids, "League ID" = league_ids, "Team Name" = gm_report$Name, "Grade" = gm_report$Grade, "Report" = gm_report_text)

dbWriteTable(mydb,"league",league,
  field.types = NULL, #Column Headers character string
  row.names = TRUE, 
  overwrite = FALSE,
  append = TRUE,
  allow.keywords = FALSE
)

#has_players
#Create N new rows (N = num_drafted_players)
#TeamID, PlayerID or PlayerName, points
has_players <- merge_stats[,c('Team_ID','Player_ID','Points','Rank1','Rank2','Rank3')]  #Dataframe version

#=====================================================================================
#TEST LEAGUE
test_league_sql <- data.frame("League ID" = 'TEST_LEAGUE_ID_1', "Season" = as.numeric(format(current_date, "%Y")), "Analysis Type" = 'Retrospective Analysis', "Point Categories" = ('G,A,GS'), "Number of Rounds"= '12', "Number of Teams" = '12')
dbWriteTable(mydb, value = test_league_sql, name = "league",append = TRUE,)

test_teams_sql <- data.frame("Team ID" = c("team3","team4"), "GM ID" = c('id1','id2'), "League ID" = 'TEST_LEAGUE_ID_2', "Team Name" = c("Joe1","Joe2"), "Grade" = c(95,87), "Report" = c("The Best Player on team1 was...","The Best Player on team2 was..."))
dbWriteTable(mydb, value = test_teams_sql, name = "teams",append = TRUE)


test_has_player_sql <- data.frame("TeamID" = c("team1","team1","team1"), "PlayerID" = c("Joe1_id","Joe2_id","Joe3_id"), "PlayerName" = c("Joe1","Joe2","Joe3"), "points" = c(42,54,10))
dbWriteTable(mydb, value = test_has_player_sql, name = "has_player",append = TRUE)

dbReadTable(mydb, 'league')
dbReadTable(mydb, 'teams')
dbReadTable(mydb, 'has_player')


# Run query to get results as dataframe
dbGetQuery(con, "SELECT * FROM arrests limit 3")

# Send query to pull requests in batches
res <- dbSendQuery(con, "SELECT * FROM arrests")
data <- dbFetch(res, n = 2)
data
dbHasCompleted(res)

dbListResults(con)
dbClearResult(res)
dbRemoveTable(con, "arrests")
dbDisconnect(con)
