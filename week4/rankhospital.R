rankhospital<-function(state, outcome, num){
  data<-read.csv("outcome-of-care-measures.csv")
  indx<-if(outcome=="heart attack") 11
  else if(outcome=="heart failure")17
  else if(outcome=="pneumonia") 23
  else stop("invalid outcome")
  if(!is.element(state, levels(data[,7]))){
    stop("invalid state")
  }
  stateValid<-data[,7]==state & (data[,indx]!="Not Available")
  stateData<-data[stateValid,c(2, indx)]
  colnames(stateData)<-c("Hospital.name", "Rate")
  stateData$Hospital.name<-as.character(stateData$Hospital.name)
  stateData$Rate<-as.numeric(as.character(stateData$Rate))
  orderedData<-stateData[order(stateData$Rate, stateData$Hospital.name),] 
  hospitalAtRank<-if(num=="best")orderedData[1,1]
                  else if(num=="worst")orderedData[nrow(orderedData),1]
                  else orderedData[num,1]
                    
  
  hospitalAtRank
}
