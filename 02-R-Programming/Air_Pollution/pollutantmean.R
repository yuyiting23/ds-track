# library(plyr)
library(data.table)

pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  # List all csv in the directory
  filenames <- list.files(path = directory, pattern = "*.csv", full.names = TRUE)      
  
  # Subsetting by id
  filenames <- filenames[id]
    
  # With filenames, read the content of each csv
  datalist <- lapply(filenames, read.csv)
  
  # Merge content in the list into a master data.frame
  # Option 1 : rbindlist (Much more efficint way)
  # doesn't do this kind of checking, and will join by position
  data <- rbindlist(datalist)
  data <- as.data.frame(data)
  #
  # Option 2 : ldply (but less efficient) 
  # ldply takes a list and return a data.frame
  # for each element of a list, apply function then combine results into a data frame.
  # data <- ldply(datalist, rbind)))
  #
  # Option 3 : rbind (applicable but very inefficient)
  if(FALSE){
      data <- data.frame()
      
      for (i in id) {
        data <- rbind(data, read.csv(filenames[i]))
      }
  }
  
  # Calculate mean of a certain pollutant
  print(mean(data[, pollutant], na.rm = TRUE))
  
  
}