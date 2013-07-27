##################################################################
## analyze the road fatality data from Stock and Watson Chapter 8
## Introduction to Econometrics, 2003.
##
## original source is
## Ruhm, Christopher J. (1996)
## "Alcohol policies and highway vehicle fatalities",
## Journal of Health Economics, 15(4): 435-454.
## 
## simon jackman, dept of political science, stanford university
## may 2003
## june 2006
##################################################################
## description of the variabes:
## state     	     State ID (FIPS) Code
## year      	     Year
## spircons  	     Spirits Consumption
## unrate    	     Unemployment Rate
## perinc    	     Per Capita Personal Income
## emppop    	     Employment/Population Ratio
## beertax   	     Tax on Case of Beer
## sobapt    	     % Southern Baptistb
## mormon    	     % Mormon
## mlda      	     Minimum Legal Drinking Age
## dry       	     % Residing in Dry Counties
## yngdrv    	     % of Drivers Aged 15-24
## vmiles    	     Ave. Mile per Driver
## breath    	     Prelim. Breath Test Law
## jaild     	     Mandatory Jail Sentence
## comserd   	     Mandatory Community Service
## allmort   	     ## of Vehicle Fatalities (##VF)
## mrall     	     Vehicle Fatality Rate (VFR) (annual traffic deaths per capita)
## allnite   	     ## of Night-time VF (##NVF)
## mralln    	     Night-time VFR (NFVR)
## allsvn    	     ## of Single VF (##SVN)
## a1517     	     ##VF, 15-17 year olds
## mra1517   	     VFR, 15-17 year olds
## a1517n    	     ##NVF, 15-17 year olds
## mra1517n  	     NVFR, 15-17 year olds
## a1820     	     ##VF, 18-20 year olds
## a1820n    	     ##NVF, 18-20 year olds
## mra1820   	     VFR, 18-20 year olds
## mra1820n  	     NVFR, 18-20 year olds
## a2124     	     ##VF, 21-24 year olds
## mra2124   	     VFR, 21-24 year olds
## a2124n    	     ##NVF, 21-24 year olds
## mra2124n  	     NVFR, 21-24 year olds
## aidall    	     ## of alcohol-involved VF
## mraidall  	     Alcohol-Involved VFR
## pop       	     Population
## pop1517   	     Population, 15-17 year olds
## pop1820   	     Population, 18-20 year olds
## pop2124   	     Population, 21-24 year olds
## miles     	     total vehicle miles (millions
## unus      	     U.S. unemployment rate
## epopus    	     U.S. Emp/Pop Ratio
## gspch     	     GSP Rate of Change

library(foreign)
fatality <- read.dta(file="fatality.dta")
fatality$y <- fatality$mrall*10000
attach(fatality)

boxplot(y~year,
        data=fatality)
boxplot(y~state,
        data=fatality)

## how much variation is between state variation?
simple1 <- lm(y ~ state)           ## about 90% is between variation
simple2 <- lm(y ~ as.factor(year))

statemean <- tapply(y,state,mean)  ## mean by state
yearmean <- tapply(y,year,mean)    ## mean by year

library(nlme)                       ## load the oh-so-helpful nlme library
## some interesting plots
interaction.plot(x.factor=year,     ## useful
                 trace.factor=state,
                 response=y,
                 ylab="Traffic Fatalities per 10,000 population",
                 legend=F)

interaction.plot(x.factor=year,     ## useful
                 trace.factor=state,
                 response=beertax,
                 ylab="Beer Tax",
                 legend=F)

## some more plots
require(lattice)
xyplot(y ~ year | state,
       panel=panel.lines,
       data=fatality)

xyplot(beertax ~ year | state,
       panel=panel.lines,
       data=fatality)


## center by year
y.c.by.year <- unlist(tapply(y,year,function(x)x-mean(x)))
interaction.plot(x.factor=year,     ## useful
                 trace.factor=state,
                 response=y.c.by.year,
                 ylab="Traffic Fatalities per 10,000 population",
                 legend=F)

## center by year
y.c.by.state <- unlist(tapply(y,state,function(x)x-mean(x)))
interaction.plot(x.factor=year,     ## useful
                 trace.factor=state,
                 response=y.c.by.state,
                 ylab="Traffic Fatalities per 10,000 population",
                 legend=F)

y.doublecentered <- unlist(tapply(y.c.by.state,year,function(x)x-mean(x)))
interaction.plot(x.factor=year,     ## useful
                 trace.factor=state,
                 response=y.doublecentered,
                 ylab="Traffic Fatalities per 10,000 population",
                 legend=F)

## year specific analyses
## figure 8.1 in Stock and Watson
plot(y ~ beertax,
     ylim=c(0,4.5),
     xlim=c(0,3),
     subset=year==1982)
lm1 <- lm(y ~ beertax,
          subset=year==1982)
abline(coef(lm1))

plot(y ~ beertax,
     ylim=c(0,4.5),
     xlim=c(0,3),
     subset=year==1988)
lm2 <- lm(y ~ beertax,
          subset=year==1988)
abline(coef(lm2))

## overall analysis, naive OLS
plot(y ~ beertax)
lmols <- lm(y ~ beertax)
abline(coef(lmols))

## look at all the year-by-year regressions
coplot(y ~ beertax | as.factor(year),
       panel=function(x,y,...){
         foo <- lm(y~x)
         points(x,y)
         abline(coef(foo))
       }
       )

## my own customized plot, looking at all the
## state-specific regressions
plot(y ~ beertax,
     type="n")
for(i in 1:48){
  ok <- state == levels(state)[i]
  there <- order(beertax[ok])[1]
  text(beertax[ok][there],y[ok][there],levels(state)[i])
  xseq <- seq(min(beertax[ok]),max(beertax[ok]),length=10)
  reg <- lm(y~beertax,subset=ok)
  lines(xseq,predict(reg,newdata=list(beertax=xseq)))
}

## a nice/similar picture from nlme
## pre-processing/house-keeping, dropping states with virtually no variation in beertax
ok <- state!="wy" & state!="mt" & state !="co"
fatality.New <- groupedData(y ~ beertax | state,
                            data=fatality[ok,])

## run the separate regressions
fatality.lis <- lmList(fatality.New,
                       na.action=na.omit)
fe <- summary(fatality.lis)$coefficients[,1,]
plot(fe)
plot(intervals(fatality.lis))
##################################################################
## how to deal with unmeasured state-level heterogeneity
##################################################################
## first move is to difference the data within units
## consider a two-time point comparison
## differences-in-differences
y.diff <- y[year==1988] - y[year==1982]
x.diff <- beertax[year==1988] - beertax[year==1982]
plot(y.diff ~ x.diff,
     xlab="Change in Beer Tax",ylab="Change in Fatality Rate")
title("Differences in Differences")
lm.diff <- lm(y.diff ~ x.diff)
abline(coef(lm.diff))
abline(h=0,lty=2)
abline(v=0,lty=2)

## LSDV (fixed effects by brute force)
## use all data now, with fixed effects for state
lm.fixed <- lm(y ~ -1 + beertax + state,data=fatality)
## test that fixed effect are jointly non-zero
anova(lmols,lm.fixed)

## add time fixed effects to regression
lm.everything <- lm(y ~ beertax + state + as.factor(year))

## do the other characteristics of states matter???
lm.extra <- lm(y ~ beertax + mlda + jaild + unrate + state)

#####################################################################
## random effects (wrong model for these data)
#####################################################################
## plm to do both fixed and random effects and Hausman test
library(plm)
pdata.frame(fatality,"state","year","fatality.plm")
zz <- plm(y ~ beertax,
          data=fatality.plm)

## contrast estimating random effects with lme
rf1 <- lme(y ~ beertax,
           random = ~ 1 | state,
           data=fatality.New)

## random slope as well
rf2 <- lme(fatality.lis,
           control=lmeControl(maxIter=1000,msMaxIter=1000,niterEM=1000,
             msMaxEval=1000,msVerbose=TRUE))

compFatality <- compareFits(coef(fatality.lis),
                            coef(rf2))
## very nifty plot
plot(compFatality,
     mark=fixef(rf2))

## mixed model for these data
options(contrasts=c("contr.treatment","contr.treatment"))
re3 <- lme(y ~ -1 + beertax + state,
           random = ~ -1 + beertax | state,
           data=fatality.New)

## random effects AND ar(1) errors within each state
ar1 <- lme(y ~ beertax,
           random = ~ 1 | state,
           data=fatality.New,
           correlation=corAR1(form = ~ 1 | state))

