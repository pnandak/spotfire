## TSCS models in R ##

library(foreign)

AGLData <- read.dta("AGL.dta",  convert.factors=FALSE)

## year: The year of the observation.
## growth: GDP growth.
## opengdp: Economic vulnerability of the economy to economic conditions in other countries.
## openex: Economic vulnerability of the economy to exports.
## openimp: Economic vulnerability of the economy to imports.
## leftc: Percentage of cabinet positions held by leftist parties.
## udif: Strength of labor organizations (an index with higher numbers meaning stronger).
## inter: The interaction between leftc and udif.
## ctrynum: Country.

olsmodel1 <- lm(growth ~ opengdp + openex + openimp + leftc + udif + inter, data=AGLData, na.action=na.omit)
summary(olsmodel1)

#install.packages("plm")
library(plm)

## this is pretty close to the Parks estimator ##

xtglsmod1 <- plm(growth ~ opengdp + openex + openimp + leftc + udif + inter, data=AGLData, index=c("ctrynum", "year"), na.action=na.omit, model="random", effect="time")
summary(xtglsmod1)

## Panel corrected standard errors ##

install.packages("pcse")
library(pcse)

pcsemodel1 <- pcse(olsmodel1, AGLData$ctrynum, AGLData$year, pairwise=FALSE)
summary(pcsemodel1)

## Reduced number of time periods ##

AGLData2 <- subset(AGLData, year<1978)

xtglsmod2 <- plm(growth ~ opengdp + openex + openimp + leftc + udif + inter, data=AGLData2, index=c("ctrynum", "year"), na.action=na.omit, model="random", effect="time")
summary(xtglsmod2)

olsmodel2 <- lm(growth ~ opengdp + openex + openimp + leftc + udif + inter, data=AGLData2, na.action=na.omit)
summary(olsmodel2)

pcsemodel2 <- pcse(olsmodel2, AGLData2$ctrynum, AGLData2$year, pairwise=FALSE)
summary(pcsemodel2)

## Lagrange Multiplier test for autocorrelation ##

olsmodel3 <- lm(growth ~ opengdp + openex + openimp + leftc + udif + inter, data=AGLData, na.action=na.omit)
LMresid <- olsmodel3$residuals

AGLData3 <- cbind(AGLData, LMresid)

LMtest1 <- plm(LMresid ~ lag(LMresid) + opengdp + openex + openimp + leftc + udif + inter, data=AGLData3, index=c("ctrynum", "year"), na.action=na.omit, model="pooling")
summary(LMtest1)

## If lag is needed ##

lagmod1 <- plm(growth ~ lag(growth) + opengdp + openex + openimp + leftc + udif + inter, data=AGLData3, index=c("ctrynum", "year"), na.action=na.omit, model="pooling")
summary(lagmod1)

## Test for unit specific effects ##

tscsmod1 <- plm(growth ~ opengdp + openex + openimp + leftc + udif + inter, data=AGLData3, index=c("ctrynum", "year"), na.action=na.omit, model="within")
pFtest(tscsmod1,olsmodel3)






