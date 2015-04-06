pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)(
  
  file.list <- list.files(directory)
  pollutant.data <- data.frame(matrix(ncol = 4, nrow = 0))
  names(pollutant.data) <- c("Date", "sulfate", "nitrate", "ID")
                               
  for(monitor in id){
    current.file <- read.csv(file.path(directory, file.list[monitor]))
    is.sulfate.na <- is.na(current.file$sulfate)
    is.nitrate.na <- is.na(current.file$nitrate)
    
    # Other functions will need complete data, but we need readings with
    # a sulfate OR nitrate level for the mean
    pollutant.data <- rbind(pollutant.data, current.file[(!is.sulfate.na | !is.nitrate.na),])    
  }
  
  # summary(pollutant.data)
  round(mean(pollutant.data[,pollutant], na.rm = TRUE), digits = 3)
}

