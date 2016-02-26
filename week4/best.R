best<-function(state, outcome){
  data<-read.csv("outcome-of-care-measures.csv")
  indx<-if(outcome=="heart attack") 11
        else if(outcome=="heart failure")17
        else if(outcome=="pneumonia") 23
        else stop("invalid outcome")
  if(!is.element(state, levels(data[,7]))){
    stop("invalid state")
  }
  stateValid<-data[,7]==state & (data[,indx]!="Not Available")
  stateData<-data[stateValid,]
  bestRecord<-min(as.numeric(as.character(stateData[,indx])), na.rm = TRUE)
  indxBestRecord<-which(as.numeric(as.character(stateData[,indx]))==bestRecord)
  sort(as.character(stateData[indxBestRecord,2]))[1]

  }