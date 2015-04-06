corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  file.list <- list.files(directory)
  complete.obs <- complete(directory, 1:332)
  is.above.threshold <- complete.obs$nobs > threshold
  monitors.above.threshold <- complete.obs[is.above.threshold, "id"]
  monitors <- length(monitors.above.threshold)
  
  correlations <- vector("numeric", monitors)
  
  if (monitors == 0){
    return(correlations)
  }
   
  for(i in 1:monitors){
    current.file <- read.csv(file.path(directory, file.list[monitors.above.threshold[i]]))
    
    correlations[i] <- cor(current.file$sulfate, current.file$nitrate, use = "complete.obs")
 
  }

  correlations

  
}