rankall<-function(outcome, num="best"){
  data<-read.csv("outcome-of-care-measures.csv")
  hospitalsAtRank<-data.frame(hospital = character(length(levels(data[,7]))), state=character(length(levels(data[,7]))), stringsAsFactors = FALSE)
  indx<-if(outcome=="heart attack") 11
  else if(outcome=="heart failure")17
  else if(outcome=="pneumonia") 23
  else stop("invalid outcome")
  for (i in seq(levels(data[,7]))){
    stateValid<-data[,7]==levels(data[,7])[i] & (data[,indx]!="Not Available")
    stateData<-data[stateValid,c(2,7,indx)]
    colnames(stateData)<-c("Hospital.name","State", "Rate")
    stateData$Hospital.name<-as.character(stateData$Hospital.name)
    stateData$Rate<-as.numeric(as.character(stateData$Rate))
    orderedData<-stateData[order(stateData$Rate, stateData$Hospital.name),] 
    hospitalsAtRank$state[i]<-as.character(levels(data[,7])[i])
    hospitalsAtRank$hospital[i]<-if(num=="best") orderedData$Hospital.name[1]
                                else if(num=="worst") orderedData$Hospital.name[nrow(orderedData)]
                                else orderedData$Hospital.name[num]
  }
hospitalsAtRank
}