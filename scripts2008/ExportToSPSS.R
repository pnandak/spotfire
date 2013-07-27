
# R Program to Write an SPSS Export File
# and a program to read it into SPSS.

setwd("/myRfolder")
library("foreign")

write.foreign(mydata,
  datafile="mydata2.txt",
  codefile="mydata2.sps",
  package="SPSS")

# Look at the contents of our new files.
file.show("mydata2.txt")
file.show("mydata2.sps")

