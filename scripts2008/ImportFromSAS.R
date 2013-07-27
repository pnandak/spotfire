
# R Program to Read a SAS Export File 
# You do not need SAS on your computer.

setwd("/myRfolder")

library("foreign")
library("Hmisc")

mydata <- sasxport.get("mydata.xpt") 
mydata

