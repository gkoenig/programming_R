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
  
  df <- data.frame(id=numeric(), nobs=numeric())
  
  for(singleid in id) {
    formattedId <- formatC(singleid, width = 3, format = "d", flag = "0")
    filename <- paste(directory[1],"/",formattedId,".csv",sep="")
    rawdata <- read.csv(filename)
    count <- sum(complete.cases(rawdata))
    df <- rbind(df,c(singleid,count))
  }
  names(df) <- c("id","nobs")
  print(df)
}