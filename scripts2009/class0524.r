#####################################################
## ps151b, May 24 2006
## examples from Verzani and Jackman add-ons
#####################################################

## example 3.4 kids weights
library(UsingR)

attach(kid.weights)
plot(height,weight,
     pch=as.character(gender))
	
## fit a linear model
linear <- lm(weight ~ height)

## overlay linear fit
abline(linear)   

## now try a quadratic model
z <- height^2
## fit a MULTIPLE regression with height^2 as
## an additional predictor
quadratic <- lm(weight ~ height + z)

## the linear model nests as a special case
## of the quadratic model, i.e., if z had no
## impact on weight

## overlay the quadratic model on the scatterplot
fakeHeights <- 12:67
b <- coef(quadratic)    ## extract the coefficients
fx <- b[1] + b[2]*fakeHeights + b[3]*fakeHeights^2
lines(fakeHeights,fx) ## superimposes a line 

##########################################################
## regression if you had your own data?
x <- c(1,2,4,3,5,7,4,6,4,8)
y <- c(4,3,7,9,12,4,7,9,4,9)
summary(lm(y~x))

##########################################################
## reading data in Excel's CSV format
## this won't work on your machine...!!
foo <- read.csv(file="/Users/jackman/docs/Projects/mcmc/Wiley/Examples/AustraliaSeatsVotes/house1p.csv",
                header=T)

##########################################################
## example 3.7 in Verzani
## Florida 2000
attach(florida)
plot(BUSH,BUCHANAN)

## run regression without Pal Beach County
result.lm <- lm(BUCHANAN ~ BUSH,
                subset=County!="PALM BEACH")
abline(result.lm)

identify(BUSH,BUCHANAN,County)  ## cool look at data

## lets make prediction for Buchanan votes in PALM BEACH given
## its Bush vote share
yhat <- predict(result.lm,
                newdata=list(BUSH=152846))
surplus <- 3407 - yhat             ## how many extra Buchanan votes?










                


