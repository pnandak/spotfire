
# R Program for Transforming Variables.

setwd("/myRfolder")
load(file="myWorkspace.RData")
mydata

#Transformation in the middle of another function.
summary( log(mydata$q4) )

#Creating meanQ with dollar notation.
mydata$meanQ <- (mydata$q1 + mydata$q2 
               + mydata$q3 + mydata$q4)/4
mydata

# Creating meanQ using attach.
attach(mydata)
mydata$meanQ <- (q1+q2+q3+q4)/4
detach(mydata)
mydata

# Creating meanQ using transform.
mydata <- transform(mydata, 
  meanQ=(q1+q2+q3+q4)/4 )
mydata

# Creating two variables using transform.
mydata <- transform( mydata,
  score1=(q1+q2)/2,
  score2=(q3+q4)/2 )
mydata

# Creating meanQ using index notation on the left.
load(file="myWorkspace.RData")
mydata <- data.frame( cbind( mydata, meanQ=0.) )
mydata[7] <- (mydata$q1 + mydata$q2 + 
              mydata$q3 + mydata$q4)/4
mydata

save.image(file="myWorkspace.RData")

