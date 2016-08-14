best <- function(state, outcome){
  # Read outcome data
  all.data <- read.csv("outcome-of-care-measures.csv",
                       na.strings="Not Available",
                       stringsAsFactors = FALSE)

  # Create a subset
  my.data <- data.frame(all.data[, 2],   # hospital
                       all.data[, 7],    # state
                       all.data[, 11],   # heart attack
                       all.data[, 17],   # heart failure
                       all.data[, 23],   # pneumonia
                       stringsAsFactors = FALSE)   

  colnames(my.data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia") 
  
  # Validate input state
  valid.state <- unique(my.data[, "state"])
  if(!(state %in% valid.state)){
    stop("invalid state")}
  
  # Validate input outcome
  valid.outcome <- c("heart attack", "heart failure", "pneumonia")
  if(!(outcome %in% valid.outcome)){
    stop( "invalid outcome")}

  # Filter out records of a certain state
  idx.state <- which(my.data$state == state)
  my.data.state <- my.data[idx.state, ]
  
  # Find min value of a certain outcome
  min <- min(my.data.state[, outcome], na.rm = TRUE)
  
  # Filter out records with min value, and subsetted with hospital
  idx.outcome <- which(my.data.state[, outcome] == min)
  my.data.state.outcome <- my.data.state[idx.outcome, ][, c("hospital")]
  #
  # Below is also good, but fail to handle ties
  # idx.outcome <- which.min(my.data.state[, outcome])
  # my.data.state.outcome <- my.data.state[idx.outcome, ]
  
  # Reorder results
  my.data.state.outcome.ordered <- my.data.state.outcome[order(my.data.state.outcome)]
  print(my.data.state.outcome.ordered)

  
}