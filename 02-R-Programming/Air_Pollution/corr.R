corr <- function(directory, threshold = 0) {
  
  # List all csv in the directory
  filenames <- list.files(path = directory, pattern = "*.csv", full.names = TRUE)
  
  # Ceeate an empty vector to store correlations 
  cor.vec <- vector(mode = "numeric")
  
  # Loop through each file
  for (i in 1:length(filenames)) {
     
    # Read each csv 
    each.df <- read.csv(filenames[i])
    
    # Calculate number of complete cases
    nobs <- sum(complete.cases(each.df))
    
    # If nobs > threshold, calculate correlatin of each df
    if (nobs > threshold) {
          each.df <- each.df[complete.cases(each.df), ]
          cor.vec <- c(cor.vec, cor(each.df$sulfate, each.df$nitrate))
          }
    }

  cor.vec
}