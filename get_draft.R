#IE332 Group 17
#Max Kwan
#get_draft
#Scrapes draft data from the user inputed txt file

get_draft <- function(location = "/Users/maxkwan/Documents/Everything/College/Senior/IE332/Project/mock1.txt", league_id_input) {
  #location = "/Users/maxkwan/Desktop/Everything/College/Senior/IE332/Project/mock1.txt"
  
  #Reads in txt file and formats character array
  raw_draft <- readLines(location)
  raw_draft <- raw_draft[-which(raw_draft == "")]
  num_rounds <- length(grep("Round", raw_draft, ignore.case = TRUE)) - 0
  num_teams <- grep("Round 2", raw_draft, ignore.case = TRUE) -2
  raw_draft <- raw_draft[-grep("Round", raw_draft, ignore.case = TRUE)]
  
  #Initializing Variables
  Draft_pos <- 1:length(raw_draft)
  Team <- paste("", 1:length(raw_draft))
  Player <- paste("", 1:length(raw_draft))
  First <- paste("", 1:length(raw_draft))
  Last <- paste("", 1:length(raw_draft))
  
  #Iterates over raw_draft pulling required data
  for (i in 1:length(raw_draft)) { 
    Draft_pos[i] <- i
    #Round[i] <- floor(i/num_rounds) + 1
    Team[i] <- trimws(substring(raw_draft[i], 5, regexpr("-",raw_draft[i]) - 2), which = c("both", "left", "right") )
    name <- strsplit(strsplit(raw_draft[i], split = "\\(")[[1]][2], split="-")[[1]][2] %>% trimws(which = c("both", "left", "right"))
    Player[i] <- name
    First[i] <- strsplit(name, split = " ")[[1]][1]
    if(length(strsplit(Player[i], split = " ")[[1]]) > 1) {
      Last[i] <- strsplit(Player[i], split = " ")[[1]][2]
    } else {
      Last[i] <- ""
    }
  }
  
  team_id <- paste(league_id, "_", 1:length(Team), sep = "")
  output <- data.frame("Draft Position" = Draft_pos, "Team" = Team, "Team_ID" = team_id, "Player" = Player, "First" = First, "Last" = Last)
  return(output)
  
  if ((num_rounds != c(6, 8, 10, 12)) && length(unique(Team)) > num_teams) {
    validity = FALSE
  } else {
    validity = TRUE
  }
  print("Draft is Valid")
}

#Input Validity
#File Type, order of information, number of rounds

#Draft Validity
#Number of rounds, team names

