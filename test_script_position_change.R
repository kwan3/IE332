#Change PositionTest Script
change_position <- function(output) {
  FDG <- c()
  pos <- c()
  for (row in 1:nrow(output)){
    if (output[row,'Position'] == 'RW' || output[row,'Position'] == 'LW' || output[row,'Position'] == 'C'){
      FDG[row] <- 'F'
      pos[row] <- output[row,'Position']
    }
    else{
      FDG[row] <- output[row,'Position']
      pos[row] <- output[row,'Position']
    }
  }
  output$pos <- FDG
  return(output$pos)
}
#Initialize Test Information
case_name<- c('RW', 'LW', 'C', 'G', 'D')
case_input <- data.frame("Position" = c('RW', 'LW', 'C', 'G', 'D'))
case_output <- data.frame('F', 'F', 'F', 'G', 'D')
test_results <- paste("",length(case_name))

#Conduct Test
test_output <- change_position(case_input)
for (i in 1:length(test_output)) {
  if (case_output[i] == test_output[i]) {
    test_results[i] = TRUE
  } else {
    test_results[i] = FALSE
  }
}

#Results
results <- cbind(case_name,test_results)
print(results)
