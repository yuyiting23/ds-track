rankhospital<- function(state, outcome, num = "best"){
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
      stop("invalid outcome")}

  # Filter out records of a certain state
  idx.state <- which(my.data$state == state)
  my.data.state <- my.data[idx.state, ]
  
  # Remove the entire row if that outcmoe is NA
  idx.na <- is.na(my.data.state[, outcome])
  my.data.state <- my.data.state[!idx.na, ]
  
  # Validate input num
  if(num == "best"){
      num = 1}
  if(num == "worst"){
      num = nrow(my.data.state)}
  
  # Sort the result by outcome(heart attack, heart failure or pneumonia)
  result <- sort(my.data.state[, outcome], index.return = TRUE)
  my.data.state.outcome <- my.data.state[result$ix, ]
  
  # Handle ties
  # Intend to list hospital names alphabetically if the outcomes are the same
  # Solution: order(), which returns indices of x, sorted first by outcome
  #           and secondly by hospital name
  idx.order.outcome.hospitcal <- order(my.data.state.outcome[, outcome], my.data.state.outcome[, "hospital"])
  my.data.state.outcome.ordered <- my.data.state.outcome[idx.order.outcome.hospitcal, ]

  # Print the record of the corresponding num index  
  print(my.data.state.outcome.ordered[num, c("hospital")])
    
}