#Pick Significance Test Script

#Pick Significance Function Form 
pick_sig <- function(num_rounds, league_stats, draft) {
  pick_sig <- 100/num_rounds 
  coeff_var <- sd(league_stats$scores[1:(length(draft))])/mean(league_stats$scores[1:(length(draft))]) #Andres Points?
  full_pick_sig <- c(1)
  for (i in 1:num_rounds) {
    pick_sig[i] = (coeff_var )* (num_rounds/2 - i) + 100/num_rounds #/ (num_teams / 2 )
    full_pick_sig <- c(full_pick_sig, rep(pick_sig[i],num_teams))
  }
  full_pick_sig <- full_pick_sig[-1]
  return(pick_sig)
}

#Full Pick Significance Function
full_pick_sig <- function(num_rounds, league_stats, draft) {
  pick_sig <- 100/num_rounds 
  coeff_var <- sd(league_stats$scores[1:(length(draft))])/mean(league_stats$scores[1:(length(draft))]) #Andres Points?
  full_pick_sig <- c(1)
  for (i in 1:num_rounds) {
    pick_sig[i] = (coeff_var )* (num_rounds/2 - i) + 100/num_rounds #/ (num_teams / 2 )
    full_pick_sig <- c(full_pick_sig, rep(pick_sig[i],num_teams))
  }
  full_pick_sig <- full_pick_sig[-1]
  return(full_pick_sig)
}

#Initialize Test Inputs
case_name <- c("10 Rounds/10 Teams, Uniform Scores: Pick Sig Test","10 Rounds/10 Teams, Uniform Scores: Full Pick Sig Test","18 Rounds/8 Teams, Uniform Scores: pick sig test","18 Rounds/8 Teams, Uniform Scores: full pick sig test", "Normal Distribution creates accurate value", "Normal Distribution creates unique values", "10 High - 90 Uniform: Full Test","10 High - 90 Uniform: Sum Test")
case_number <- c(1:8)

case_output1 <- rep(100/10,10)
case_output2 <- rep(100/10,10*10)
case_output3 <- rep(100/18,18)
case_output4 <- rep(100/18,18*8)
set.seed(1)
ndist <- abs(rnorm(100,300,100))
case_output5 <- sd(ndist)/mean(ndist) * (10/2-1) + 100/10
case_output6 <- 10
case_output7 <- c(20, rep(20/9,9))

# test_cases <- data.frame(case_name,case_data,case_output)
test_results <- paste(FALSE,8)

#Test Case 1
num_rounds = 10
num_teams = 10
league_stats <- data.frame("scores" = rep(300,num_rounds*num_teams))
draft <- rep("1",num_rounds*num_teams)
test_output1 <- pick_sig(num_rounds,league_stats,draft)
test_results[1] <- case_output1 == test_output1

#Test Case 2
test_output2 <- full_pick_sig(num_rounds,league_stats,draft)
test_results[2] <- case_output2 == test_output2

#Test Case 3
num_rounds = 18
num_teams = 8
league_stats <- data.frame("scores" = rep(300,num_rounds*num_teams))
draft <- rep("1",num_rounds*num_teams)
test_output3 <- pick_sig(num_rounds,league_stats,draft)
test_results[3] <- case_output3 == test_output3


#Test Case 4
test_output4 <- full_pick_sig(num_rounds,league_stats,draft)
test_results[4] <- case_output4 == test_output4

#Test Case 5
num_rounds = 10
num_teams = 10
set.seed(1)
league_stats <- data.frame("scores" = abs(rnorm(num_rounds*num_teams,300,100)))
draft <- rep("1",num_rounds*num_teams)
test_output5 <- pick_sig(num_rounds,league_stats,draft)[1] #Only tests for first position for ease of calculation
test_results[5] <- case_output5 == test_output5

#Test Case 6
test_output6 <- unique(full_pick_sig(num_rounds,league_stats,draft))
test_results[6] <- case_output6 == case_output6

#Test Case 7
num_rounds = 10
num_teams = 10
league_stats <- data.frame("scores" = c(rep(1000,10), rep(500,90)))
draft <- rep("1",num_rounds*num_teams)
test_output7 <- pick_sig(num_rounds,league_stats,draft)[1] #Only tests for first position for ease of calculation
test_results[7] <- case_output7 == test_output7

#Test Case 8
test_results[8] <- ceiling(sum(pick_sig(num_rounds,league_stats,draft))) == 100

#Results
results <- cbind(case_number,case_name,test_results)
print(results)
