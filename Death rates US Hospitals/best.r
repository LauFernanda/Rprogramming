best <- function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv",stringsAsFactors=FALSE) ## Read outcome data
  reduced_data<- data.frame() 
  ## Check that state and outcome are valid
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!state %in% data$State) {
    stop("invalid state")
  } else if(!outcome %in% valid_outcomes) {
    stop("invalid outcome")
  }## Return hospital name in that state with lowest 30-day death
 
  else {
  if(outcome== "heart attack"){
    index <-11
  }
  else if(outcome== "heart failure"){
    index <- 17
  }
  else if(outcome== "pneumonia"){
    index <- 23
  }
  reduced_data<- subset(data, data$State== state, select= c(2,7,index)) 
  colnames(reduced_data)<- c("hospital","state","outcome")
  reduced_data[,3]<- as.numeric(reduced_data[,3])
  data_ordered <- reduced_data[order(reduced_data$outcome,reduced_data$hospital),]
  data_ordered[1,1]
}
}

