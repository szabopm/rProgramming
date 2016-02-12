author <- "Peter M Szabo <peter.szabo@bms.com>"
company <- "Bristol-Myers Squibb Co."
date <- as.Date("2016-02-11")

#Write a function that reads a directory full of files and reports the number of 
#completely observed cases in each data file. The function should return a data 
#frame where the first column is the name of the file and the second column is 
#the number of complete cases. 


complete <-function(directory, id=1:332){
  #create the result data frame with two columns ("id", "nobs") and 
  #the number of data files rows
  x<-data.frame(matrix(ncol = 2, nrow = length(id)))
  #rename columns ("id", "nobs") 
  colnames(x)<-c("id", "nobs")
  #Iterate trough the file ids defined by the user (or default all of them)
  for(i in 1:length(id)){
    #the file is in the user defined "directory" separated by "/"
    #all the files has a 3 digit name (e.g. 001, 002,... ,010, ..., 100 etc.)
    #when the loop iterates by integers we need to create 3 digit numbers
    #from those (e.g. 1 -> 001), sprintf("%03d", id[i]) function does that part
    #and paste() concatenate together the strings.
    filename=paste(directory,"/",sprintf("%03d", id[i]),".csv", sep="")
    #read the file data into a data frame
    data<-data.frame(read.csv(file=filename))
    #write the id of the file in the first ("id") column of the result data frame
    x[i, 1]=id[i]
    #!is.na(data[,2]) is gives a logical vector as a result,
    #the value at a given index is TRUE if the the value is not "NA" in the second column 
    #of the data file and FALSE if it is. !is.na(data[,3]) is the same for column three.
    #By using the logical operand "&" between those two we get a logical vector 
    #with the size of the columns and the values are TRUE if both the second and 
    #third column was not "NA" at the given row in the data file and FALSE for all 
    #other cases (F&F, T&F, F&T). Conventionally if treated as a number 
    #TRUE is 1 and FALSE is 0, so if we sum the values in the last logical vector
    #we got the number of rows with completely observed cases.
    x[i, 2]=(sum(!is.na(data[,2])&!is.na(data[,3])))
  }
  return(x)
}