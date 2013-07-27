## Gov 2000 Problem Set 8 Solutions

############################################################
## Problem 1
############################################################

## We create a matrix of coefficients

mat <- matrix(c(2,4,-3,24,1,1,0,10,0,11,-2,-2,-1,-3,-5,1), byrow = TRUE, nrow = 4)

## And a column matrix of right hand side values

rhs <- matrix(c(10,-5,0,2), ncol = 1)

## So using the inverse method to solve the system of equations,
## (rounding to the nearest hundredth)

round(solve(mat) %*% rhs, 2)



####################################
## Problem 2
####################################

## Now we load the data

load("ps8_data.RData")

## And replicate table 1 with lm()

lm.table1 <- lm(TI98 ~ commonlaw + britcolony + noncolony + pctprot
  + elf + FMMexports93, data = TI)

summary(lm.table1)

## And table 5

lm.table5 <- lm(TI98 ~ commonlaw + britcolony + noncolony + pctprot
  + elf + FMMexports93 + logGDP90 + federal + longdemoc + impgdp94
  + stateintervention + govwages + turnover, data = TI)

summary(lm.table5)

## And we collect the results in a table

repl.treisman <- cbind("Estimate" = coef(lm.table5), "Standard Error" =  coef(summary(lm.table5))[,"Std. Error"], 
  "Estimate" = c(coef(lm.table1),rep(NA, length(coef(lm.table5))-length(coef(lm.table1)))),
  "Standard Error" =  c(coef(summary(lm.table1))[,"Std. Error"], rep(NA, length(coef(lm.table5))-length(coef(lm.table1))))
  )

## And reorder the columns (using model 5 first in the cbind above recorded 
## all the rownames, as opposed to using model 1 first, which would have
## only recorded the rownames for the variables in model 1

repl.treisman <- cbind(repl.treisman[,c(3,4)], repl.treisman[,c(1,2)])

library(xtable) 
xtable(repl.treisman, round = c(4,4,4,4,4)) #make latex table











## Now we'll write a function which performs the replication using 
## matrix operations.



my.lm <- function(y,X){#y d.v. vector, X i.v. matrix
  
  ## First, listwise delete (which requires cbinding the whole
  ## dataset together)

  data <- na.omit(cbind(y,X))# listwise deletes missing observations
  X <- data[,-1] # X with no missing observations
  y <- data[,1] # y with no missing observations
  
  ## Next, add a column of 1's if there isn't one in X already
  
  if(sum(X[,1] == 1) < nrow(X)){
    X <- cbind(1,X)
  }

  ## Now use matrix operations to perform OLS

  beta.hat <- solve(t(X) %*% X) %*% t(X) %*% y
  y.hat <- X %*% beta.hat
  resid <- y - y.hat
  SE <- sqrt( (t(resid) %*% resid) / (length(y)-ncol(X)))
  vcov.mtx <- as.numeric(SE^2) * solve(t(X) %*% X)
  beta.se <- sqrt(diag(vcov.mtx))
  RegSS <- sum((y.hat - mean(y))^2)
  TSS <- sum((y-mean(y))^2)
  R2 <- RegSS/TSS
  n <- nrow(X)

  reg.out <- list(beta.hat = beta.hat, se.beta.hat = sqrt(diag(vcov.mtx)), vcov.mtx = vcov.mtx, R2=R2, n = n)

  return(reg.out)
}


## So, to rerun model 1 and model 5 of treisman,

y <- TI$TI98
X.mod1 <- cbind(TI$commonlaw, TI$britcolony, TI$noncolony, TI$pctprot, 
  TI$elf, TI$FMMexports93)

X.mod5 <- cbind(TI$commonlaw, TI$britcolony, TI$noncolony, TI$pctprot, 
  TI$elf, TI$FMMexports93, TI$logGDP90, TI$federal, TI$longdemoc, 
  TI$impgdp94, TI$stateintervention, TI$govwages, TI$turnover)

## Model 1:

my.lm(y = y, X = X.mod1)

## Model 5:

my.lm(y = y, X = X.mod5)









    
  