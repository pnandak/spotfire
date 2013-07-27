# R Program to select both variables and observations.

setwd("/myRfolder")
load(file="myWorkspace.RData")

print( mydata [1:4, 2:6 ])

attach (mydata)
print( mydata [ 
  which(gender=="f"),
  c("gender", "q1", "q2", "q3", "q4")
] )
detach(mydata)

print (
subset(mydata, subset=gender=="f",
select=gender:q4)
)