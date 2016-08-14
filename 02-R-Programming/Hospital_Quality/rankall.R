rankall<- function(outcome, num = "best"){
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
  
  # Validate input outcome
  valid.outcome <- c("heart attack", "heart failure", "pneumonia")
  if(!(outcome %in% valid.outcome)){
      stop("invalid outcome")}
  
  # Remove the entire row if the specific outcome is NA
  idx.na <- is.na(my.data[, outcome])
  my.data <- my.data[!idx.na, ]
  
  # Assign value to num
  
  
  # Order by 1st: state, 2nd: outcome, 3rd: hospital name 
  idx.order.state.outcome.hospitcal <- order(my.data[ , "state"], my.data[ , outcome], my.data[ ,"hospital"]) 
  my.data.ordered <- my.data[idx.order.state.outcome.hospitcal, ]
  
  # Split data.frame by state
  state.list <- split(my.data.ordered, my.data.ordered$state)
  
  # Create result from final output
  result <- data.frame()
  
  lapply(state.list, 
         function(x) { 
                # Assign value to num
                if(num == "best"){
                   num = 1}
                if(num == "worst"){
                   num = nrow(x)}
                
                # For each data.frame in the list,
                # extract specified row, and insert into result data.frame 
                result <<- rbind(result, x[num, c("hospital","state")]) 
            }
  )
  
  # Deal with <NA> state name
  valid.state <- sort(unique(my.data[, "state"]), decreasing =FALSE)
  result[, "state"] <- as.data.frame(valid.state)
  
  result
}