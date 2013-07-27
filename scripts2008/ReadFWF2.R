
# R Program for Reading Fixed Width Text Files,
# 2 Records per Case.

setwd("/myRfolder")

# Set all the values to use.
myfile <- "mydataFWF.txt"
myVariableNames  <- c("id","group", "gender",
  "q1","q2","q3","q4",
  "q5","q6","q7","q8")
myRecord1Widths  <- c( 2, 1, 1, 1, 1, 1, 1)
myRecord2Widths  <- c(-2,-1,-1, 1, 1, 1, 1)
myVariableWidths <- list(myRecord1Widths,myRecord2Widths)

#Now plug them in and read the data:
mydata <- read.fwf(
   file=myfile,
   width=myVariableWidths,  
   col.names=myVariableNames,
   row.names="id",
   na.strings="",
   fill=TRUE,
   strip.white=TRUE )
mydata

