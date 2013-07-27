
# R Program for calculating N and Mean
# across both variables and observations;

setwd("/myRfolder")
load(file="myWorkspace.RData")
mydata
attach(mydata)

# Create myQmatrix.
myQmatrix <- as.matrix( mydata[ ,3:6] ) 
myQmatrix

# Get mean of whole matrix.
mean( myQmatrix )
mean( myQmatrix,na.rm=TRUE )

# Get mean of matrix columns
apply(myQmatrix,2,mean,na.rm=TRUE)

# Get mean of matrix rows.
apply(myQmatrix,1,mean,na.rm=TRUE) 
rowMeans(myQmatrix,na.rm=TRUE)

# Add row means to mydata.
mydata$meanQ <- rowMeans(myQmatrix,na.rm=TRUE)
mydata$meanQ <- rowMeans(myQmatrix,na.rm=TRUE)
mydata <- transform(mydata, 
  meanQ=rowMeans(myQmatrix, na.rm=TRUE)
)mydata

# Means of data frames & their vectors.
mean(mydata, na.rm=TRUE)
lapply(mydata[ ,3:6], mean, na.rm=TRUE)
sapply(mydata[ ,3:6], mean, na.rm=TRUE)

mean( 
  sapply(mydata[ ,3:6], mean, na.rm=TRUE) 
)

# Length of data frames & their vectors.
length(mydata[ ,"q3"] )
is.na( mydata[ ,"q3"] )
!is.na( mydata[ ,"q3"] )
sum( !is.na( mydata[ ,"q3"] ) )

# Like the SAS/SPSS n from stat procedures.
library("prettyR")
sapply(mydata, valid.n)

# Like the SAS/SPSS n function.
apply(myMatrix, 1, valid.n)
mydata$myQn <- apply(myMatrix, 1, valid.n)
mydata

