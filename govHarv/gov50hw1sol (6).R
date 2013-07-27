## Problem 5
## Create the data frame
population <- c(512930,135294,908007, 729498 ,7237479, 11410046 , 1119583 , 
     978933, 2974807,3907738, 28674, 37360, 26745)
legislature <- c(47, 27, 52, 55, 123,100, 56, 57, 83, 79, 18, 19, 19)
party <- factor(c("PC", "PC", "PC", "Liberal", "Liberal", "Liberal", 
     "NDP", "NDP", "PC", "Liberal", "Yukon", "None", "None"))
canada <- data.frame(population, legislature, party)
rownames(canada) <- c("Newfoundland", "Prince Edward Island", "Nova Scotia", 
     "New Brunswick", "Quebec", "Ontario", "Manitoba", "Saskatchewan", 
     "Alberta", "British Columbia", "Yukon", "Northwest Territory", "Nunavut")
canada

## Calculate medians
median(canada$population)
median(canada$legislature)

## Pull out the provinces (or you can just look them up in the table)
rownames(canada)[which(canada$population==median(canada$population))]
rownames(canada)[which(canada$legislature==median(canada$legislature))]


## Problem 6
## Load the car library
library(car)
## Load the Florida data
data(Florida)
## Calculate the mean, median, and standard deviation

## The apply() function is a faster way to use the same function 
## on a bunch of variables;
## the 2 indicates that you are applying it to columns.
apply(Florida, 2, mean)
apply(Florida, 2, median)
apply(Florida, 2, sd)

## The rbind() function allows you to join rows together:
rbind(apply(Florida, 2, mean),apply(Florida, 2, median),apply(Florida, 2, sd))

## Problem 7

boxplot(Florida[,1:4], main = "Florida 2000: Votes for leading candidates, by county")

## Problem 8

attach(Florida)
## Create NaderShare variable
NaderShare <- NADER/Total
## Create histogram
hist(NaderShare, main="Nader vote share, by county")

## Calculate mean Nader share
NaderMean <- mean(NaderShare)
## Identify counties with below-mean shares
belowNaderMean <- NaderShare < NaderMean
## Find the proportion of below-mean counties
mean(belowNaderMean)