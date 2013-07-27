
set.seed(1234567)

## The brute force method: run 21 separate commands, store the output
## either in objects (or by hand if necessary), then create a vector 
## with the results.  This was fine for this homework.
try1 <- sample(1:6, 1) + sample(1:6, 1)
try2 <- sample(1:6, 1) + sample(1:6, 1)
try3 <- sample(1:6, 1) + sample(1:6, 1)
try4 <- sample(1:6, 1) + sample(1:6, 1)
try5 <- sample(1:6, 1) + sample(1:6, 1)
try6 <- sample(1:6, 1) + sample(1:6, 1)
try7 <- sample(1:6, 1) + sample(1:6, 1)
try8 <- sample(1:6, 1) + sample(1:6, 1)
try9 <- sample(1:6, 1) + sample(1:6, 1)
try10 <- sample(1:6, 1) + sample(1:6, 1)
try11 <- sample(1:6, 1) + sample(1:6, 1)
try12 <- sample(1:6, 1) + sample(1:6, 1)
try13 <- sample(1:6, 1) + sample(1:6, 1)
try14 <- sample(1:6, 1) + sample(1:6, 1)
try15 <- sample(1:6, 1) + sample(1:6, 1)
try16 <- sample(1:6, 1) + sample(1:6, 1)
try17 <- sample(1:6, 1) + sample(1:6, 1)
try18 <- sample(1:6, 1) + sample(1:6, 1)
try19 <- sample(1:6, 1) + sample(1:6, 1)
try20 <- sample(1:6, 1) + sample(1:6, 1)
try21 <- sample(1:6, 1) + sample(1:6, 1)

method1 <- c(try1, try2, try3, try4,try5, try6,
             try7, try8,try9, try10,try11, try12,
             try13, try14,try15, try16,try17, try18,
             try19, try20,try21)
method1


## Another method: in this approach, you draw the 21 rolls of the 
## first die first, then the 21 rolls of the second die, and then 
## add them together.  Remember to sample with replacement!
roll1 <- sample(1:6, 21, replace=TRUE)
roll2 <- sample(1:6, 21, replace=TRUE)
method2<- roll1 + roll2


## Yet another method: this uses a simple for loop, simulates the
## two rolls using a single sample command, and then adds them 
## using the sum command.
method3 <- rep(0, 21)
for(ii in 1:length(draws)){
method3[ii] <- sum(sample(1:6, 2, replace=TRUE))
}  
method3



## Yet another method: use the replicate function to replace the for loop
## The first argument is the number of replications, the second is the 
## expression to be replicated.
method4 <- replicate(n=21, sample(1:6, 1)+ sample(1:6, 1))
method4

## Plot histogram of draws
hist(method1, main="Sum of roll of two fair dice", xlab="Sum")

## Calculate descriptive statistics
mean(method1)
median(method1)
sd(method1)

## Problem 6
## Create the data frame
population <- c(512930,135294,908007, 729498 ,7237479, 11410046 , 1119583 ,	978933, 2974807,3907738, 28674, 37360, 26745)
legislature <- c(47, 27, 52, 55, 123,100, 56, 57, 83, 79, 18, 19, 19)
party <- factor(c("PC", "PC", "PC", "Liberal", "Liberal", "Liberal", "NDP", "NDP", "PC", "Liberal", "Yukon", "None", "None"))
canada <- data.frame(population, legislature, party)
rownames(canada) <- c("Newfoundland", "Prince Edward Island", "Nova Scotia", "New Brunswick", "Quebec", "Ontario", "Manitoba", "Saskatchewan", "Alberta", "British Columbia", "Yukon", "Northwest Territory", "Nunavut")
canada

## Calculate medians
median(canada$population)
median(canada$legislature)

## Pull out the provinces (or you can just look them up in the table)
rownames(canada)[which(canada$population==median(canada$population))]
rownames(canada)[which(canada$legislature==median(canada$legislature))]


## Problem 7
## Load the car library
library(car)
## Load the Florida data
data(Florida)
## Calculate the mean, median, and standard deviation
summary(Florida)
## This is a faster way to use the same function on a bunch of variables;
## the 2 indicates that you are applying it to columns.
apply(Florida, 2, sd)

## Problem 8

boxplot(Florida[,1:4], main = "Florida 2000: Votes for the four leading candidate, by county")