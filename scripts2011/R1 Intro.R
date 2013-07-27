
# R1 INTRODUCTION TO R

# Set our Working Directory

setwd("myRworkshop")



# Read & print data

mydata <- read.csv("mydata.csv")

print(mydata)



# Convert workshop to a factor

mydata$workshop <- factor(mydata$workshop)



# Get summary stats

summary(mydata)



# Get a scatterplot

plot( mydata$q1, mydata$q4 )



# Run a linear regression model

myModel <- lm( q4~q1+q2+q3, data=mydata )

summary( myModel ) #Summary now does something different

anova( myModel )

plot( myModel )   #And plot does something different too

