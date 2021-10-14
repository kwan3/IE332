#IE332 Group 17
#Max Kwan
#get_draft
#Scrapes draft data from the user inputed txt file

get_draft <- function(location = "/Users/maxkwan/Desktop/Everything/College/Senior/IE332/Project/mock1.txt") {
  #location = "/Users/maxkwan/Desktop/Everything/College/Senior/IE332/Project/mock1.txt"
  
  #Reads in txt file and formats character array
  raw_draft <- readLines(location)
  raw_draft <- raw_draft[-which(raw_draft == "")]
  num_rounds <- length(grep("Round", raw_draft, ignore.case = TRUE)) - 0
  num_teams <- grep("Round 2", raw_draft, ignore.case = TRUE) -2
  raw_draft <- raw_draft[-grep("Round", raw_draft, ignore.case = TRUE)]
  
  #Initializing Variables
  Draft_pos <- 1:length(raw_draft)
  Round <- 1:length(raw_draft)
  Team <- paste("", 1:length(raw_draft))
  Player <- paste("", 1:length(raw_draft))
  
  #Iterates over raw_draft pulling required data
  for (i in 1:length(raw_draft)) { 
    Draft_pos[i] <- i
    Round[i] <- floor(i/num_rounds) + 1
    Team[i] <- trimws(substring(raw_draft[i], 5, regexpr("-",raw_draft[i]) - 2), which = c("both", "left", "right") )
    ugly_name <-strsplit(strsplit(raw_draft[i], split = "\\(")[[1]][2], split="-")[[1]][2]
    Player[i] <- trimws(ugly_name, which = c("both", "left", "right"))
  }
  
  output <- data.frame("Draft Position" = Draft_pos, "Round" = Round, "Team" = Team, "Player" = Player)
  return(output)
}


