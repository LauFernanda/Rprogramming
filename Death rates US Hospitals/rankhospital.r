rankhospital <- function(state, outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv",na.strings = "Not Available",stringsAsFactors=FALSE) ## Read outcome data
  reduced_data<- data.frame() 
  
  ## Check that state and outcome are valid
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!state %in% data$State) {
    stop("invalid state")
  } else if(!outcome %in% valid_outcomes) {
    stop("invalid outcome")
  }
  ## Return hospital name in that state with the given rank  
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
    data_ranking <- reduced_data[order(reduced_data$outcome,reduced_data$hospital,na.last=NA),]
    nobs <- nrow(data_ranking)
    
    if (num== "best"){
      num <- 1
    }
    else if (num== "worst"){
      num <- nobs
    }
    else {
    num<- num
  }
    data_ranking[num,1]
  }
}