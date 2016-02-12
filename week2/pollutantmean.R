author <- "Peter M Szabo <peter.szabo@bms.com>"
company <- "Bristol-Myers Squibb Co."
date <- as.Date("2016-02-11")

#Write a function named 'pollutantmean' that calculates the mean of a pollutant 
#(sulfate or nitrate) across a specified list of monitors. 
#The function 'pollutantmean' takes three arguments: 
#'directory', 'pollutant', and 'id'. 
#Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' 
#particulate matter data from the directory specified in the 'directory' 
#argument and returns the mean of the pollutant across all of the monitors, 
#ignoring any missing values coded as NA. 


pollutantmean<-function(directory, pollutant, id=1:332){
  #the sum of all valid records and the number of them will
  #be used to calculate mean. At the beginning both set to 0.
  sumOfPollutant=0.0
  numOfMeasures=0
  
  #Iterate trough the file ids
  for(i in 1:length(id)){
    #the file is in the user defined "directory" separated by "/"
    #all the files has a 3 digit names (e.g. 001, 002,... ,010, ..., 100 etc.)
    #when the loop iterates by integers we need to create 3 digit numbers
    #from those (e.g. 1 -> 001), sprintf("%03d", id[i]) function does that part
    #and paste() concatenate together the strings.
    filename=paste(directory,"/",sprintf("%03d", id[i]),".csv", sep="")
    #read the file into a data frame
    data<-data.frame(read.csv(file=filename))
    #!is.na(data[,pollutant]) results a vector with the row indexes of not "NA" records
    #in the "pollutant" column 
    #data[!is.na(data[,pollutant]),pollutant] results a vector with the values of not "NA" records
    #in the "pollutant" column 
    #then add the sum of those values to the sumOfPollutant and the 
    #number of them to numOfMeasures
    sumOfPollutant=sumOfPollutant+sum(data[!is.na(data[,pollutant]),pollutant])
    numOfMeasures=numOfMeasures+length((data[!is.na(data[,pollutant]),pollutant]))
  }
  #return mean of all "NA" measurements 
  return(sumOfPollutant/numOfMeasures)
}