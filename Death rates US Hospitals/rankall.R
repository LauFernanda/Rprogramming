rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",na.strings = "Not Available",stringsAsFactors=FALSE) ## Read outcome data
  ## Check that outcome are valid
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!outcome %in% valid_outcomes) {
    stop("invalid outcome")
  }
  ## For each state, find the hospital of the given rank
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
    reduced_data<- subset(data, select= c(7,2,index)) 
    colnames(reduced_data)<- c("state","hospital","outcome")
    data_ranking<- reduced_data[order(reduced_data$state,reduced_data$outcome,reduced_data$hospital,na.last=NA),]
    ranking_state<- split(data_ranking, data_ranking$state)
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  requested<- lapply(ranking_state,function(x,num){
    ## gives value to best and worst
    nobs <- nrow(x)
    if (num== "best"){
      num <- 1
    }
    else if (num== "worst"){
      num <- nobs
    }
    else {
      num<- num
    }
    requestedname<-x$hospital[num]
  }
  ,num)
  hospital<- unlist(requested)
  state<- names(requested)
  requesteddf <- data.frame(State=state,Hospital=hospital) 
  requesteddf
}
}