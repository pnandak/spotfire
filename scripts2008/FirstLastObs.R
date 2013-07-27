
# R Program Selecting Last Obs per Group.

setwd("/myRfolder")
load(file="myWorkspace.RData")
mydata

myBys <- data.frame(mydata$workshop,mydata$gender)
mylastList <- by( mydata,myBys,tail,n=1 )
mylastList

#Back into a data frame:
mylastDF <- do.call(rbind, mylastList) 
mylastDF

# Another way to create the dataframe:
mylastDF <- rbind(mylastList[[1]],
                  mylastList[[2]],
                  mylastList[[3]],
                  mylastList[[4]])
mylastDF

