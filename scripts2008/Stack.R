
# R Program for Stacking/Concatenating/Adding Data Sets.

setwd("/myRfolder")
load(file="myWorkspace.RData")
mydata
attach(mydata)

# Create female data frame.
females <- mydata[ which(gender=="f"), ]
females

# Create male data frame.
males <- mydata[ which(gender=="m"), ]
males

#Bind their rows together with the rbind function.
both <- rbind(females, males)
both

# Drop q2 to see what happens.
males$q2 <- NULL
males

# See that row bind will not work.
both <- rbind(females, males)

# Use reshape's rbind.fill.
library("reshape")
both <- rbind.fill(females, males)
both

# Add a q2 variable to males.
males <- data.frame( males, q2=NA )
males

# Now rbind can handle it.
both <- rbind(females,males)
both
 
