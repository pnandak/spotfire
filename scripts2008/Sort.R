
# R Program to Sort Data.

setwd("/myRfolder")
load(file="myWorkspace.RData")
mydata

# Show first four observations in order.
mydata[ c(1,2,3,4), ]

# Show them in reverse order.
mydata[ c(4,3,2,1), ]

# Create order variable for workshop.
myW <- order( mydata$workshop )
myW
mydata[ myW, ]

# Create order variable for gender then workshop.
myGW <- order( mydata$gender, mydata$workshop )
myGW
mydata[ myGW, ]

# Create order variable for 
# descending (-) workshop then gender
myWdG <- order( -mydata$workshop, mydata$gender )
myWdG

# Print data in WG order.
mydata[ myWdG, ]

# Save data in WdG order.
mydataSorted <- mydata[ myWdG,  ]
mydataSorted

