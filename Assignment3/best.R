library("data.table")
best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",stringsAsFactors = FALSE)
  ## Check that state and outcome are valid
  if(!(outcome %in% c("heart attack","pneumonia","heart failure"))) {
    stop("invalid outcome")
  }
  if(!(state %in% data[,7])) {
    stop("invalid state")
  }
  ## determine the best hospital in that state for the desired outcome
  outcome_column <- 0
  if(outcome =="heart attack") {
    outcome_column <- 11
  }else if(outcome == "heart failure") {
    outcome_column <- 17
  }else {
    outcome_column <- 23
  }
  data_filtered <- subset(data,State==state, select=c(2,outcome_column))
  names(data_filtered) <- c("hospitalname","outcome")
  data_filtered <- data_filtered[data_filtered$outcome != "Not Available", ]
  data_filtered <- transform(data_filtered, outcome = as.numeric(outcome))
  data_sorted <- data_filtered[with(data_filtered, order(outcome,hospitalname)), ]
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  print(data_sorted[1,1])
}