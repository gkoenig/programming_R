#Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) 
#across a specified list of monitors. 
#The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. 
#Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data 
#from the directory specified in the 'directory' argument and returns the mean of the pollutant 
#across all of the monitors, ignoring any missing values coded as NA. 


pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  values_overall = c()
  
  if( pollutant != "sulfate" && pollutant != "nitrate") {
    print("Invalid 'pollutant'. Must be either 'sulfate' or 'nitrate'")
    return(-1)
  }
  
  for(singleid in id) {
    formattedId <- formatC(singleid, width = 3, format = "d", flag = "0")
    filename <- paste(directory[1],"/",formattedId,".csv",sep="")
    rawdata <- read.csv(filename)
    values <- na.omit(rawdata[ ,pollutant])
    values_overall <- append(values_overall,values)
  }
  
  print(mean(values_overall))
  
}