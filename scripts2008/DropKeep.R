
* R Program for Keeping and Dropping Variables.

setwd("/myRfolder")
load(file="myWorkspace.RData")

# Using variable selection.
myleft <- mydata[ ,1:4]
myleft

# Using NULL.
myleft <- mydata
myleft$q3 <- myleft$q4 <- NULL
myleft

