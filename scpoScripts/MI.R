## multiple imputation code for R ##

## Example 1 ##

library(foreign)
MIData <- read.dta("CCS2010v2.dta", convert.factors=FALSE)

## pick out subset of data for imputation ##
## Note: You don't need to pick out a subset with a dataset this smaill -- I just did it to speed things up ##

MIDatasub <- subset(MIData, select=c(develop1, develop2, incgroup, rent, southcoast, NEP, PID, age))

install.packages("Amelia")
library(Amelia)

## imputation ##

impute.out <- amelia(MIData, m=5, noms=c("develop1","develop2"))

# built-in diagnostics #

summary(impute.out)

plot(impute.out)

overimpute(impute.out, var="age")

## save imputed datasets ##

write.amelia(obj=impute.out, file.stem = "impdata", format = "dta")

impd1 <- subset(impute.out$imputations[[1]])
impd2 <- subset(impute.out$imputations[[2]])
impd3 <- subset(impute.out$imputations[[3]])
impd4 <- subset(impute.out$imputations[[4]])
impd5 <- subset(impute.out$imputations[[5]])


install.packages("Zelig")
library(Zelig)
install.packages("VGAM")
library(VGAM)

regmod1.out <- zelig(as.factor(develop1) ~ incgroup + rent + southcoast + NEP, model="mlogit", data=MIData)
summary(regmod1.out)

impmod1.out <- zelig(as.factor(develop1) ~ incgroup + rent + southcoast + NEP, model="mlogit", data=mi(impd1, impd2, impd3, impd4, impd5))
summary(impmod1.out)


## Example 2 ##

MIData2 <- read.dta("development.dta", convert.factors=FALSE)

MIData2sub <- subset(MIData2, select=c(gxpdhlth, gini, g, glag, dictator, births, infmort, cath, moslem, femsec, fertil,country,year))

impute2.out <- amelia(MIData2sub, m=5, cs="country", ts="year", noms="dictator")

regmod2.out <- zelig(infmort ~ gxpdhlth + glag + dictator + femsec, model="ls", data=MIData2)
summary(regmod2.out)

impd1 <- subset(impute2.out$imputations[[1]])
impd2 <- subset(impute2.out$imputations[[2]])
impd3 <- subset(impute2.out$imputations[[3]])
impd4 <- subset(impute2.out$imputations[[4]])
impd5 <- subset(impute2.out$imputations[[5]])

impmod2.out <- zelig(infmort ~ gxpdhlth + glag + dictator + femsec, model="ls", data=mi(impd1, impd2, impd3, impd4, impd5))
summary(impmod2.out)

impmod2.out <- zelig(g ~ gxpdhlth + glag + dictator + femsec, model="ls", data=mi(impd1, impd2, impd3, impd4, impd5))
summary(impmod2.out)
