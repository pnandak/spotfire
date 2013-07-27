
# R Program to Write a Text File.

setwd("/myRfolder")

write.table(mydata,
  file="mydata2.txt",
  quote=FALSE,
  sep="\t",
  na=" ",
  row.names=TRUE,
  col.names=TRUE)

# Look at the contents of our new file.
file.show("mydata2.txt")

