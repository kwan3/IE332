#IE332 Group 17
#Max Kwan
#gm_rating
#Calculates a grade for every GM

#Store user inputs and collects data
draft <- get_draft() #Get's the user inputted draft data
current_date <- as.Date("2020-10-20") #User Input String
current_season <- get_stats(as.numeric(format(current_date, "%Y"))) #Scrape current season stats
current_season_raw <- get_stats(as.numeric(format(current_date, "%Y")))
past_season_stats <- get_stats(as.numeric(format(current_date, "%Y")) - 1) #Scrape past season stats

#Store important data
team_names <- unique(draft$Team)
num_teams <- length(team_names)-0

#Creates the running draft
running_draft <- merge(draft, current_season, by ='Player', all.x = TRUE) #merge draft and current season stats, not sure if I should

#Ranked lists for each position
#draft <- data.frame("Draft Position" = (1:6), "Team Name" = paste("Team", c(1:6)), "Player Name" = c("Adam","Bob", "Carl", "Dave", "Edward", "Frank"))

#Initializing Vectors
pick_rating <- (1:length(draft$Draft.Position)) - 0
gm_report <- data.frame("Name" = team_names, "Grade" = rep(0,num_teams))

#Iterate over draft and calculate pick rating for each drafted player
for (i in 1:length(draft)) {
  pick_rating[i] <- grep(running_draft$Player[i], filter(current_season, Pos == running_draft$Pos[i])$Player) #Finds the index of the pick from the running list
  current_season <- current_season[-grep(running_draft$Player[i], current_season$Player),] #Removes player from running list
  gm_report$Grade[grep(running_draft$Team[i], gm_report$Name)] <- gm_report$Grade[grep(running_draft$Team[i], gm_report$Name)] + pick_rating[i] #Adds pick rating to cumulative gm grade
}
#Having issues with merging probably, the draft positions are not matching up with the player


#Stores final results
draft <- cbind(draft, pick_rating)


