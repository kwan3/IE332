#IE332 Project
#New League Test Script
#To use properly you'll need to follow the comments
#Make sure to save all the required files for full testing (functions and mock.txt)

#==============================================================================
#Script Preparation
#==============================================================================
library(rvest) #Install these packages
library(dplyr)
library(stringr)
library(matrixStats)
library(keras)
library(tidyverse)
library(rsample)
library(tfdatasets)
library(RMySQL)

#setwd("/Users/maxkwan/Documents/Everything/College/Senior/IE332/Project/") #Set to your working directory
#source('get_draft.R') #Download these files from drew and max's githubs
#source("NHL_Scrape.R") 
#source("scores.R")

#==============================================================================
#Inputs
#==============================================================================
db <- dbConnect(MySQL(), user = "g1117498", password = "332group17", dbname = "g1117498", host = "mydb.itap.purdue.edu")

league_inputs_raw <- dbReadTable(db, 'draft') #If testing with data in the db
league_id <- length(dbReadTable(db, "league")[,1]) + 1 #Queries for number of leagues and makes it the id
draft <- get_draft('file_path', league_id_input = league_id) #Will have to put your file path in

#==============================================================================
#Processes
#==============================================================================
league_id <- length(dbReadTable(db, "league")[,1]) + 1 #This section is used for testing db reading
current_date <- league_inputs_raw[2]
gm_ids_raw <- league_inputs_raw[3]
league_KPIs_raw <- league_inputs_raw[4]

#league_id <- 1 #This block of code is used for manual testing
#current_date <- Sys.Date()
#gm_ids_raw <- c("Joe1,Joe2,Joe3,Joe4,Joe5,Joe6,Joe7,Joe8,Joe9,Joe10,Joe11,Joe12") 
#league_KPIs_raw <- ("G,A,GS")

if (current_date > as.Date("2021-10-12")) { #Current Season Retrospective
  anal_type <- "Retrospective Analysis"
  db_stats <- dbReadTable(db, 'stats')   #Check Name of table
  db_players <- dbReadTable(db, 'players')
  db_stats <- merge(db_stats,db_players,by = "Id")
} else if (current_date > as.Date("2021-7-7")){ #Current Season Predictive
  anal_type <- "Predictive Analysis"
  db_stats <- dbReadTable(db, 'predictions')
} else { #Past Season
  anal_type <- "Past Season Analysis"
  db_stats <- get_stats(as.numeric(format(current_date, "%Y")))
}

#==============================================================================
#Outputs
#==============================================================================
league <- data.frame("League ID" = league_id, 
  "Season" = as.numeric(format(current_date, "%Y")), 
  "Analysis Type" = anal_type, 
  "Point Categories" = league_KPIs_raw, 
  "Number of Rounds"= num_rounds, 
  "Number of Teams" = num_teams
)
dbWriteTable(db, value = league, name = "league",append = TRUE)

print(db_stats[1:5,])
#==============================================================================
#Test Set up - Use these to set the database up as needed
#==============================================================================
#test_league_sql <- data.frame(
#   "League ID" = 'TEST_LEAGUE_ID_1', 
#   "Season" = as.numeric(format(current_date, "%Y")), 
#   "Analysis Type" = 'Retrospective Analysis', 
#   "Point Categories" = ('G,A,GS'), 
#   "Number of Rounds"= '12', 
#   "Number of Teams" = '12'
# )
#dbWriteTable(db, value = test_league_sql, name = "league",append = FALSE,)