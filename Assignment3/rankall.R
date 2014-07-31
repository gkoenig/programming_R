rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",stringsAsFactors = FALSE)
  ## Check that state and outcome are valid
  if(!(outcome %in% c("heart attack","pneumonia","heart failure"))) {
    stop("invalid outcome")
  }
  hospitalnames <- character()
  states <- character()
  
  outcome_column <- 0
  if(outcome =="heart attack") {
    outcome_column <- 11
  }else if(outcome == "heart failure") {
    outcome_column <- 17
  }else {
    outcome_column <- 23
  }
  
  ## loop over all states
  for(state in unique(as.vector(data$State))) {
    data_filtered <- subset(data,State==state, select=c(2,outcome_column))
    names(data_filtered) <- c("hospitalname","outcome")
    data_filtered <- data_filtered[data_filtered$outcome != "Not Available", ]
    data_filtered <- transform(data_filtered, outcome = as.numeric(outcome))
    data_sorted <- data_filtered[with(data_filtered, order(outcome,hospitalname)), ]
    
    if(num=="best") {
      rowno <- 1
    }else if(num=="worst") {
      rowno <- nrow(data_sorted)
    }else{
      rowno <- as.numeric(num)
    }
    hospitalnames <- append(hospitalnames,data_sorted[rowno,1])
    states <- append(states,state)
  }
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  result <- data.frame(hospitalnames,states,stringsAsFactors = FALSE)
  names(result) <- c("hospital","state")
  result <- result[with(result, order(state)), ]
  print(result)
}