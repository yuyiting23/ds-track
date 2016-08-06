corr <- function(directory, threshold = 0) {
  
  # List all csv in the directory
  filenames <- list.files(path = directory, pattern = "*.csv", full.names = TRUE)
  
  # Ceeate an empty vector to store correalions 
  correlation.vec <- vector()
  
  lapply(filenames, 
         function(x){
              # Read each csv and generate data.frame
              each.df <- read.csv(x)
              
              # Create a vector to save whether it is a complete case or not
              complete.index <- complete.cases(each.df)
              
              # Calcualte the number of complete case in each data.frame
              nobs <- sum(complete.index) 
              
              # Compare it with threshold
              if (nobs > threshold) {
                  
                  # Remove imcomplete cases and calculate correlations
                  each.df <- each.df[complete.index, ]
                  correlation.vec <<- c(correlation.vec, cor(each.df$sulfate, each.df$nitrate))
              }
              
         })

  correlation.vec

  
}