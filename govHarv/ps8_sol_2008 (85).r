## Gov 2001 Problem Set 8
## Solutions

## Jens Hainmueller and Jenn Larson

################################################################################
## Problem 1: matching
################################################################################

## First we load the data
load("ps8_data.RData")

## Now we implement nearest-neighbor matching on all five covariates

library(MatchIt)

m.out <- matchit(data$D ~ data$X1 + data$X2 + data$X3 + data$X4 +
                   data$X5, data = data, method = "nearest")

## We can evaluate balance with 
summary(m.out)

## To run a linear model on the matched data, we extract our data
m.data <- match.data(m.out)

## And ues lm()
m.ols <- lm(m.data$Y ~ m.data$D + m.data$X1 + m.data$X2 + m.data$X3 +
            m.data$X4 + m.data$X5, data = m.data)
summary(m.ols)

## Now we run a linear model on the unmatched dta
m.ols.unm <- lm(data$Y ~ data$D + data$X1 + data$X2 + data$X3 +
            data$X4 + data$X5, data = data)

## Now we use genetic matching

## We create a matrix of covariates

X <- cbind(data$X1, data$X2, data$X3, data$X4, data$X5)

## Estimate weights for the matching algorithm:

gen.weights <- GenMatch(Tr=data$D, X=X, BalanceMatrix=X, estimand="ATT", M=1,replace=T,
                   pop.size=1000, max.generations=100, wait.generations=10)


## And create a matched dataset using the weights
gen.out <- Match(Y=data$Y, Tr=data$D, X=X, estimand="ATT",
                   Weight.matrix=gen.weights, Z=X, M=1,  BiasAdjust=T,
                   replace=T, sample=T)
summary(gen.out)

## We can compare balance with

mb <- MatchBalance(data$D ~ data$X1 + data$X2 + data$X3 + data$X4 +
                   data$X5, data
        = data, match.out=gen.out, nboots=500, ks=TRUE, mv=F)


## We again extract the dataset
gen.data <- cbind(gen.out$mdata$Y, gen.out$mdata$Tr, gen.out$mdata$X)
colnames(gen.data) <- c("Y", "D", "X1", "X2", "X3", "X4", "X5")
gen.data <- as.data.frame(gen.data)

#And regress the dependent variables on the treatment and the covariates
gen.ols <- lm(gen.data$Y ~ gen.data$D + gen.data$X1 + gen.data$X2 + gen.data$X3 +
            gen.data$X4 + gen.data$X5, data = gen.data)

#And retrieve the coefficient on D
summary(gen.ols)

