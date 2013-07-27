
# R Program for Reading a Fixed-Width Text File, 
# 1 Record per Case.

setwd("/myRfolder")
mydata <- read.fwf(
   file="mydataFWF.txt",
   width=c(2,-1,1,1,1,1,1),  
   col.names=c("id","gender","q1","q2","q3","q4"),
   row.names="id",
   na.strings="",
   fill=TRUE,
   strip.white=TRUE)
mydata

# Now we'll use "macro" substitution to do the same thing.

myfile <- "mydataFWF.txt"
myVariableNames  <- c("id","gender","q1","q2","q3","q4")
myVariableWidths <- c(2,-1,1,1,1,1,1)

mydata <- read.fwf(
   file=myfile,
   width=myVariableWidths,  
   col.names=myVariableNames,
   row.names="id",
   na.strings="",
   fill=TRUE,
   strip.white=TRUE) 
mydata

