complete <- function(directory, id = 1:332) {
  
  # List all csv in the directory
  filenames <- list.files(path = directory, pattern = "*.csv", full.names = TRUE)
  
  # Subsetting by id
  filenames <- filenames[id]
  
  # With each filenames 
  # (1) Read the content of each csv
  # (2) Evaluate complete case of each observation
  # (3) Count the number of complete case
  # (4) Return nobs as a list
  nobs.list <- lapply(filenames, 
                      function(x){
                            temp <- read.csv(x)
                            nobs <- sum(complete.cases(temp))
                            nobs
                      })
  
  # Construct a data.frame with id and nobs  
  print(data.frame(id = id, nobs = unlist(nobs.list)))
  
}