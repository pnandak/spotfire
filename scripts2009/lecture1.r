
####################################
##
## Introduction to R Programming
## Lecture 1 R Code
##
## Evan Girvetz
##
####################################

help.search("logarithm")
apropos("log")
help(log)
?log

## Basic computation
2+2
log(42)
log(42, base = 10)
log(42             ## Incomplete command
, base = 10)

## Creating objects:
myObject <- log(42)
ls()        ## list all objects in workspace
rm(myObject)     ## Remove myObject

## Creating vector objects

Year <- c(1800, 1850, 1900, 1950, 2000)  ## Create new vector called Year
Carbon <- c(8, 54, 534, 1630, 6611)  ## Create new vector called Carbon
Carbon  ## show contents of vector

Carbon <- scan()    ##You can also manually input values using scan() -- hit enter twice after last entry
Carbon  ## show contents of vector

Carbon[5]    ## extract the fifth element of vector1

Carbon[c(2,5)]    ## extract the second and fifth element of vector1

Carbon[-c(2,5)]    ## extract all elements except the fifth element of vector1

Carbon[Carbon>1000]     ## extract all elements in vector1 that are greater than 10

## Logical (Boolean) operators
Carbon > 1000   ## logical operator greater than

c(FALSE,FALSE,FALSE,TRUE,TRUE)  ## This is the same as the result from the last line

Carbon[c(F,F,F,T,T)]   ## Same as vector1[vector1>8]

Carbon<5000        ## Logical statement of whether Carbon is less than 5000  
(Carbon>=1000)|(Carbon<5000)
Carbon[(Carbon>=1000)|(Carbon<5000)]

TRUE == T   ## Note that these are the same

TRUE == true   #note that you get an error becuase true is not a recognized object

TRUE == "true"    # Note that this is false because TRUE and "true" are different


### Indexing a vector based on the values of another vector (both vectors must be of the same size)
Carbon        ## Print Carbon Values
Year          ## Print Year values
Carbon[Year >=1900]       ## Extract Carbon values when Year is greater than or equal to 1900

Carbon[c(3,4)]
Carbon[-c(3,4)]

## Patterned data
1:5     ## Sequence from 1 to 5
seq(from = 1, to = 5, by = 1)  ## Sequence from 1 to 5
seq(from = 1800, to = 2000, by = 50)  ## Sequence from 2 to 10 by 2

rep (x = c(1,2,3), times = 2)    ## Repeat 1,2,3 two times

## Adding dimensions to vectors: Matrices 
newData <-  1:20
newData
dim(newData) <- c(5,4)
newData     ## Note that this data was organized by column  
newData1 <- 1:20
matrix(newData, 5,4, byrow = T)
newData1    ## Note that this data was organized by row
newData     ## Note that this data was organized by column

## Adding rows and columns 
newData <- cbind(newData, 21:25)       ## Add column

newData <- rbind(newData, 96:100)       ## Add column


newData[2:4,1:2]
newData[c(1,3,5), c(1:2)]

################
#### Data Frames

## create data frame from scratch
myDataFrame <- data.frame(YearMeasured = Year, CarbonOutput = Carbon)       ## Create a data frame
myDataFrame     ## View data frame

## Add column
myDataFrame$otherValues <- c(2,7,12,18,6)   #Add another column to the data frame
myDataFrame

## Add row
myDataFrame <- rbind(myDataFrame, c(2050, 10000, 27))
myDataFrame
myDataFrame <- myDataFrame[-6,]  #get rid of the new row that was made

## Row and Column names
names(myDataFrame)           ## show column names
colnames(myDataFrame)        ## show column names (same as last line)
rownames(myDataFrame)        ## show row names
names(myDataFrame)[3] <- "new.name"    ## Change name of third column to "new.name"
rownames(myDataFrame)<-c("row1", "row2", "row3", "row4", "row5")   ##change all row names


## Attaching and detaching data frames

myDataFrame$dataValues   ##access a column 
dataValues     #This gives error because dataValues is not an object recognized by R
attach(myDataFrame)      ## attached myDataFrame
dataValues          ## Now dataValues is recognized as an object by R
detach(myDataFrame)   ## Detach myDataFrame
dataValues     #Now again, this gives error because dataValues is not an object recognized by R                         


## Indexing data frames
myDataFrame[1,2]        #returns the value in the first row, second column of myDataFrame
myDataFrame[1:3, 1:2]    #returns the values in the first through third rows, but only those in the first or second column of myDataFrame
myDataFrame[, 1:2]     #returns all the rows, but only for the first or second column of myDataFrame
myDataFrame[,c("YearMeasured", "CarbonOutput")]    #returns all the rows for columns named "YearMeasured", and "CarbonOutput" in myDataFrame
myDataFrame[, -c(2,3)]    #returns all the rows, and all columns except for the second and third column of myDataFrame
myDataFrame$CarbonOutput    #returns all rows in the column CarbonOutput
myDataFrame$CarbonOutput[myDataFrame$YearMeasured >= 1900]   #returns the carbon output values for the years greater than or equal to 1900

## Creating a simple plot
?plot       #look at the help file for this function
help(plot)
args(plot)  #look at the arguments
plot(x = myDataFrame$YearMeasured, y = myDataFrame$CarbonOutput, type = "b")



