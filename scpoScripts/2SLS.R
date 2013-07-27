## 2SLS in R ##

library(foreign)

TSLSData <- read.dta("C:\\courses\\ps207\\newdail2002v2.dta",  convert.factors=FALSE)

## OLS ##

olsmodel <- lm(votes1st ~ incumb + spend_regular + spend_regularXinc, data=TSLSData)
summary(olsmodel)

olsmodel2 <- lm(votes1st ~ incumb + spend_regular + spend_regularXinc + spend_public, data=TSLSData)
summary(olsmodel2)

## 2SLS ##

install.packages("AER")
library(AER)

tslsmodel <- ivreg(votes1st  ~ incumb + spend_regular + spend_regularXinc | incumb + partyquota1997 + electoraK + dublin + senator + councillor, data=TSLSData)
summary(tslsmodel)

tslsmodel2 <- ivreg(votes1st  ~ incumb + spend_regular + spend_regularXinc + spend_public | incumb + partyquota1997 + electoraK + dublin + senator + councillor, data=TSLSData)
summary(tslsmodel2)


###############################
## Some people have said they are having trouble making the AER package work for the homework 1 data 
## Try this package instead

install.packages("sem")
library(sem)

tslsmodel <- tsls(votes1st  ~ incumb + spend_regular + spend_regularXinc, ~ incumb + partyquota1997 + electoraK + dublin + senator + councillor, data=TSLSData)
summary(tslsmodel)

tslsmodel2 <- tsls(votes1st  ~ incumb + spend_regular + spend_regularXinc + spend_public , ~ incumb + partyquota1997 + electoraK + dublin + senator + councillor, data=TSLSData)
summary(tslsmodel2)

