author <- "Peter M Szabo <peter.szabo@bms.com>"
company <- "Bristol-Myers Squibb Co."
date <- as.Date("2016-02-11")

#Write a function that takes a directory of data files and a threshold 
#for complete cases and calculates the correlation between sulfate and 
#nitrate for monitor locations where the number of completely observed 
#cases (on all variables) is greater than the threshold. The function 
#should return a vector of correlations for the monitors that meet the 
#threshold requirement. If no monitors meet the threshold requirement, 
#then the function should return a numeric vector of length 0. 
#For this function you will need to use the 'cor' function in R which 
#calculates the correlation between two vectors. Please read the help 
#page for this function via '?cor' and make sure that you know how to use it.

corr <-function(directory, treshold=0){
  #create an empty numeric vector for the correlation coefficients
  result<-numeric()
  #by using complete() function get the number of complete records for each file
  x<-complete(directory)
  #get the names of the files where the number of complete observations is over
  #the treshold
  y<-x[which(as.numeric(x$nobs)>=treshold),1]
  #Iterate trough the file ids where the number of observations was above the treshold
  for (i in seq(y)){
    #the file is in the user defined "directory" separated by "/"
    #all the files has a 3 digit name (e.g. 001, 002,... ,010, ..., 100 etc.)
    #when the loop iterates by integers we need to create 3 digit numbers
    #from those (e.g. 1 -> 001), sprintf("%03d", id[i]) function does that part
    #and paste() concatenate together the strings.
    filename=paste(directory,"/",sprintf("%03d", y[i]),".csv", sep="")
    #read the file data into a data frame
    data<-data.frame(read.csv(file=filename))
    #calculate correlation coeficient between second and third columns, 
    #but only for complete observations (use="pairwise.complete.obs")
    #and store them inh the result vector
    result[i]<-cor(data[,c(2)], data[,c(3)], use="pairwise.complete.obs")
  }
  return (result)
}