

# R Program to Write a SAS Export File
# and a program to read it into SAS.

setwd("/myRfolder")
library("foreign")

write.foreign(mydata,
  datafile="mydata2.txt",
  codefile="mydata2.sas",
  package="SAS")

# Look at the contents of our new files.
file.show("mydata2.txt")
file.show("mydata2.sas")
