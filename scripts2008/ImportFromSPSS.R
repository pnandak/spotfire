
# R Program to Import an SPSS Data File.

library("Hmisc")
setwd("/myRfolder")

# Read & print the SPSS file.
mydata <- spss.get("mydata.por",use.value.labels=TRUE)
mydata

# Save the workspace.
save.image(file="myWorkspace.RData")

