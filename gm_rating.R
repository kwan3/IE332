#Performance Grade

#Store User Input: Draft Data
c(draft,teams) <- read_draft_input() #User Input txt file
current_date <- as.Date("2020-10-20") #User Input String
current_season_stats <- get_stats(as.numeric(format(current_date, "%Y"))) #Scrape current season stats
past_season_stats <- get_stats(as.numeric(format(current_date, "%Y")) - 1) #Scrape past season stats
running_draft <- merge(draft, current_season_stats, by ='Player', all = TRUE) #merge draft and current season stats, not sure if I should

#Ranked lists for each position
"
RW_ranked <- draft
For (i in 1:length(draft)) {
  if(draft$Pos[i] == 'RW') {
    RW_ranked = 
  }
}
"
#team_report <- names(draft)
pick_rating <- zeroes(length(draft)) #Initialize in case im dumb

for (i in 1:length(draft)) {
  pick_rating[i] = grep(draft$Player[i], filter(running_draft, Pos == draft$Pos[i])$Player) #Finds the index of the pick from the running list
  running_draft[-grep(draft$Player[i],running_draft$Player)] #Removes player from running list
}

cbind(draft, pick_rating) #combining into draft

gm_rating <- draft.groupby(['Team'])$pick_rating.sum() #Save pick ratings into report

#Functions
read_draft_input <- function(location = "/Users/maxkwan/Desktop/Everything/College/Senior/IE332/Project/mock1.txt" ) {
  #https://cran.r-project.org/doc/manuals/R-data.html#Spreadsheet_002dlike-data
  
  readLines("/Users/maxkwan/Desktop/Everything/College/Senior/IE332/Project/mock1.txt")
  #Not sure quite how to format this without using grep or splitting an absurd # of times
  #Remove rounds and set draft as absolute draft position
  #Assume output is a beautiful table of the current season draft 
  #Columns: Draft Position, Team, Player, Hometown
  return c(table, c(team_names))
}


pickRating #No need to turn it into a function
Equate to difference in rank position of current season to draft position
Positive indicates good picks, negative indicates poor picks 

draft position = string[,2]
name = grep('-', string, )
