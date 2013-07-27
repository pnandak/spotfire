
#Load Libraries
library(Matching)
library(mgcv)
library(foreign)

#Set Working Directory
setwd()

#Load the Data
visit02 <- read.dta("House2002.1.dta")
visit02 <- na.omit(visit02)
attach(visit02)

#Estimate Propensity Scores
glm <- glm(visit2 ~ open + hotrace + spending + I(spending^2) + urban + black + hispanic + hs + college + income + I(income^2) + presvote + I(presvote^2), data=visit02)

gam <- gam(visit2 ~ open + hotrace + s(spending, bs="cr") + urban + black + hispanic + hs + college + s(income, bs="cr") + s(presvote, bs="cr"), data=visit02)

#Matching
X <- glm$fitted
Y <- visit02$cookdiff
Tr <- visit02$visit2

rr.1  <- Match(Y=Y,Tr=Tr,X=X,M=1);
summary(rr.1)

MatchBalance(visit2 ~ open + hotrace + spending + I(spending^2) + urban + black + hispanic + hs + college + income + I(income^2) + presvote + I(presvote^2), data=visit02, match.out=rr.1, nboots=1000)

#Now with GAM
X  <- gam$fitted

# One-to-one matching with replacement
rr.2  <- Match(Y=Y,Tr=Tr,X=X,M=1);
summary(rr.2)

MatchBalance(visit2 ~ open + hotrace + spending + I(spending^2) + urban + black + hispanic + hs + college + income + I(income^2) + presvote + I(presvote^2), data=visit02, match.out=rr.2, nboots=1000)


