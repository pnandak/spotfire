## Basic Panel Data models in R ##

library(foreign)

BESData <- read.dta("bes.dta",  convert.factors=FALSE)

olsmodel1 <- lm(right ~ manual + age + male + price + tory + labour, data=BESData, na.action=na.omit)
summary(olsmodel1)

install.packages("plm")
library(plm)

pdim(BESData, index=c("serialno", "year"))

## Pooled model (regular OLS)

pmodel1 <- plm(right ~ manual + age + male + price + tory + labour, data=BESData, index=c("serialno", "year"), na.action=na.omit, model="pooling")
summary(pmodel1)

## Fixed effects model

pmodel2 <- plm(right ~ manual + age + male + price + tory + labour, data=BESData, index=c("serialno", "year"), na.action=na.omit, model="within")
summary(pmodel2)

## F test for fixed effects

pFtest(pmodel2,pmodel1)

## Random effects model

pmodel3 <- plm(right ~ manual + age + male + price + tory + labour, data=BESData, index=c("serialno", "year"), na.action=na.omit, model="random")
summary(pmodel3)

## Breusch-Pagan test 

plmtest(pmodel3, effect="individual", type="bp")

## Hausman test

phtest(pmodel2, pmodel3)


#########################
## Example 2
##


BankData <- read.dta("franzese.dta",  convert.factors=FALSE)

olsmodel2 <- lm(inf ~ cbilag1 + glag1 + tlag1 + flag1 + infalag1 + ulag1 + cwb, data=BankData, na.action=na.omit)
summary(olsmodel2)

pdim(BankData, index=c("ctry", "year"))

## Fixed effects model

pmodel4 <- plm(inf ~ cbilag1 + glag1 + tlag1 + flag1 + infalag1 + ulag1 + cwb, data=BankData, index=c("ctry", "year"), na.action=na.omit, model="within")
summary(pmodel4)

## F test for fixed effects

pFtest(pmodel4,olsmodel2)

## Random effects model 

pmodel5 <- plm(inf ~ cbilag1 + glag1 + tlag1 + flag1 + infalag1 + ulag1 + cwb, data=BankData, index=c("ctry", "year"), na.action=na.omit, model="random")

## Crash because of negative estimated variance of individual effects

## Use different method of calculating variance of random effects
## random.method options only work on balanced panels

pmodel5 <- plm(inf ~ cbilag1 + glag1 + tlag1 + flag1 + infalag1 + ulag1 + cwb, data=BankData, index=c("ctry", "year"), na.action=na.omit, model="random", random.method="walhus")
summary(pmodel5)

## Breusch-Pagan test 

plmtest(pmodel5, effect="individual", type="bp")

## Hausman test

phtest(pmodel4, pmodel5)

