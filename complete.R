complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases

  file.list <- list.files(directory)
  complete.obs <- data.frame(matrix(ncol = 2, nrow = 0))

  for(monitor in id){
    current.file <- read.csv(file.path(directory, file.list[monitor]))
    is.sulfate.na <- is.na(current.file$sulfate)
    is.nitrate.na <- is.na(current.file$nitrate)
    
    complete.obs <- rbind(complete.obs, c(monitor, sum(!is.sulfate.na & !is.nitrate.na)))
  }

  names(complete.obs) <- c("id", "nobs")
  complete.obs
}