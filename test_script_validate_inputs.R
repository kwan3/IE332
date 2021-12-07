#User Input Validation Test Script

#Creates function for the input validation (excluding .txt scraping)
#The contents of this function are in the new_league script, they are pulled here for testing
setwd("/Users/maxkwan/Documents/Everything/College/Senior/IE332/Project/") 
source('kill_db_connections.R')
killDbConnections()

input_validation <- function(current_date, gm_ids_raw, league_KPIs_raw) {
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
  db <- dbConnect(MySQL(), user = "g1117498", password = "332group17", dbname = "g1117498", host = "mydb.itap.purdue.edu")
  setwd("/Users/maxkwan/Documents/Everything/College/Senior/IE332/Project/") 
  source('get_draft.R')
  league_id <- 1 #Set as 1 for isolated testing purposes
  draft <- get_draft(league_id_input = league_id) 
  
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
    #print("Error pls stop:") #Fix these messages and pass back to website
    #print(reason)
  } else {
    #print("valid input") #Fix these messages
  }
  
  #Check Analysis Types and Query Database Inputs (UNTESTED)
  if (current_date > as.Date("2021-10-12")) { 
    anal_type <- "Retrospective Analysis"
    #db_stats <- dbReadTable(db, 'stats')[,-1]  
    #db_players <- dbReadTable(db, 'players')
    #db_stats <- merge(db_stats,db_players,by = "Id", all.y = TRUE)
    #db_stats[is.na(db_stats)] = 0
  } else if (current_date > as.Date("2021-7-7")){
    anal_type <- "Predictive Analysis"
    #db_stats <- dbReadTable(db, 'predictions')[,-1]
  } else { 
    anal_type <- "Past Season Analysis"
    #db_stats <- get_stats(as.numeric(format(current_date, "%Y")))[,-1]
  }
  return(c(anal_type, valid))
}

#Initialize Test Inputs
case_name <- c('Retrospective Analysis', 'Predictive Analysis', 'Past Season Analysis', 'Nonexistent KPIs' , 'Nonexistent gm_id')
current_date <- c(Sys.Date(),"2021-8-1",'2019-08-1', Sys.Date(),Sys.Date())
gm_ids_raw1 <- c("1,2,8,23,24,25,26,27,28,29,30,31")
gm_ids_raw2 <- c("100,2,8,23,24,25,26,27,28,29,30,31")
league_KPIs_raw1 <- c('G,A,GS,SV')
league_KPIs_raw2 <- c('L,A,GS,SV')

#Expected Outputs
case_output1 <- c("Retrospective Analysis", TRUE)
case_output2 <- c("Predictive Analysis", TRUE)
case_output3<- c("Past Season Analysis", TRUE)
case_output4<- c("Retrospective Analysis", FALSE)
case_output5<- c("Retrospective Analysis", FALSE)

#Conduct Tests
test_output1 <- input_validation(current_date[1],gm_ids_raw1,league_KPIs_raw1)
test_output2 <- input_validation(current_date[2],gm_ids_raw1,league_KPIs_raw1)
test_output3 <- input_validation(current_date[3],gm_ids_raw1,league_KPIs_raw1)
test_output4 <- input_validation(current_date[4],gm_ids_raw1,league_KPIs_raw2)
test_output5 <- input_validation(current_date[5],gm_ids_raw2,league_KPIs_raw1)

test_result <- c()
if (test_output1[1] == case_output1[1] && test_output1[2] == case_output1[2]) {
  test_result[1] = paste(case_name[1], 'passed')
} else {
  test_result[1] = paste(case_name[1], 'failed')
}
if (test_output2[1] == case_output2[1] && test_output2[2] == case_output2[2]) {
  test_result[2] = paste(case_name[2], 'passed')
} else {
  test_result[2] = paste(case_name[2], 'failed')
}
if (test_output3[1] == case_output3[1] && test_output3[2] == case_output3[2]) {
  test_result[3] = paste(case_name[3], 'passed')
} else {
  test_result[3] = paste(case_name[3], 'failed')
}
if (test_output4[1] == case_output4[1] && test_output4[2] == case_output4[2]) {
  test_result[4] = paste(case_name[4], 'passed')
} else {
  test_result[4] = paste(case_name[4], 'failed')
}
if (test_output5[1] == case_output5[1] && test_output5[2] == case_output5[2]) {
  test_result[5] = paste(case_name[5], 'passed')
} else {
  test_result[5] = paste(case_name[5], 'failed')
}

#Results
print(test_result)