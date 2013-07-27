## Gov 2000 Problem Set 10
## One possible solution

#setwd("C:/Users/Jenn/Documents/Teaching/2000_TF/ps_10")
setwd("C:/Documents and Settings/user/Desktop/2000_TF/PSets/ps10")

## Load the data

library(foreign)
statedata <- read.dta("statedata.dta")

## First let's look at some descriptive statistics

library(car)
scatterplot.matrix(~ murder + popdensity + police + blackpop + unemp, 
  data = statedata)

## Let's fit a model

lm.base <- lm(murder ~ popdensity + police + blackpop + unemp, 
  data = statedata)

summary(lm.base)

## Look at linearity:

cr.plots(lm.base)
## As the scatter plots suggested, popdensity, police and blackpop
## may have extreme points that could be influence points

library(mgcv)

g.base <- gam(murder~s(popdensity)+s(police)+ s(blackpop) + s(unemp),
  data=statedata) 

summary(g.base)

plot.gam(g.base)




## Now create logged popdensity, police and blackpop to deal with
## the possible extreme point problem

statedata$logpopdensity <- log(statedata$popdensity + 1)
statedata$logpolice<- log(statedata$police)
statedata$logblackpop <- log(statedata$blackpop)

lm.log <- lm(murder ~ logpopdensity + logpolice + logblackpop + unemp, 
  data = statedata)

summary(lm.log)

cr.plots(lm.log)

g.log <- gam(murder~s(logpopdensity)+s(logpolice)+ s(logblackpop) + s(unemp),
  data=statedata) 

summary(g.log)

plot.gam(g.log)

## Now linearity looks a little worse.  Let's keep tinkering.  
## Try including polynomials of the variables included in lm.log
## and interactions and examine the linearity assumption

## Square the unemployment variable:

statedata$unempsq <- statedata$unemp^2

lm.logsq <- lm(murder ~ logpopdensity + logpolice + logblackpop + unemp
  + unempsq,   data = statedata)

summary(lm.logsq)

g.logsq <- gam(murder~s(logpopdensity)+s(logpolice)+ s(logblackpop) + s(unemp)
  +s(unempsq),
  data=statedata) 

summary(g.logsq)
plot.gam(g.logsq)



## Now let's try some interactions and see how linearity looks

## ...

## Finally, after much tinkering, here is the model specification I like best

lm.final <- lm(murder~logpopdensity + logpolice + logblackpop + I(logpolice^2)
  + unemp + I(unemp^2) + logblackpop:I(logpolice) + 
  logblackpop:I(logpolice^2), data = statedata)
summary(lm.final)

statedata$logpolsq <- statedata$logpolice^2
statedata$unempsq <- statedata$unemp^2
statedata$logblacklogpol <- statedata$logblackpop*statedata$logpolice
statedata$logblacklogpolsq <- statedata$logblackpop*statedata$logpolice^2

g.final <- gam(murder~logpopdensity + s(logpolice) + logblackpop
  + s(logpolsq)
  + unemp + unempsq + s(logblacklogpol) + 
  s(logblacklogpolsq), data = statedata)

plot.gam(g.final) #looks good

#####################
## Check other model assumptions
#####################

## Heteroskedasticity:

scatter.smooth(fitted(lm.final), sqrt(abs(rstudent(lm.final))), col="red")

## Normality:

qq.plot(lm.final)

## Unusual observations:

pdf(file = "ps10_plot1.pdf")
plot(lm.final, which = 5)
dev.off()

## identifies 3, 8, 11 as potential problems
statedata[c(3,8,11),"state"] #az, de, hi

dim(model.matrix(lm.final)) # so lm.final used 48 observations
hats <- hatvalues(lm.final)
plot(sort(hats))
abline(h = 2*ncol(model.matrix(lm.final))/nrow(model.matrix(lm.final)))

tail(sort(hats))
statedata[c(48,27,8),"state"] #west virginia, ne, de

## Let's check the robustness of our results

statedata.rest <- statedata[-c(3,8,11),]
lm.final.omit <- lm(murder~logpopdensity + logpolice + logblackpop + I(logpolice^2)
  + unemp + I(unemp^2) + logblackpop:I(logpolice) + 
  logblackpop:I(logpolice^2), data = statedata.rest)
summary(lm.final.omit)

#########
## Finding our estimate of the conditional expectation of murder
########

co <- coef(lm.final)

logpol <- log(60000)
logbp <- log(1000)
logpop <- log(1000)
unemp <- 8

est <- co[1]+co[2]*logpop +co[3]*logpol+co[4]*logbp+co[5]*logpol^2+co[6]*unemp+co[7]*unemp^2+co[8]*logbp*logpol + co[9]*logbp*logpol^2


#2.61


















#########################
## Scratch


## Now logblackpop looks less linear.  Let's try keeping blackpop as 
## the original but logging popdensity and police

lm.log2 <- lm(murder ~ logpopdensity + logpolice + blackpop + unemp, 
  data = statedata)
summary(lm.log2)
cr.plots(lm.log2)
g.log2 <- gam(murder~s(logpopdensity)+s(logpolice)+ s(blackpop) + s(unemp),
  data=statedata) 
plot.gam(g.log2)

## let's try adding logpolice^2

statedata$logpolicesq <- statedata$logpolice^2

lm.log2sq <- lm(murder ~ logpopdensity + logpolice +logpolicesq + blackpop
 + unemp, data = statedata)
summary(lm.log2sq)
cr.plots(lm.log2sq)
g.log2sq <- gam(murder~s(logpopdensity)+s(logpolice)+ s(logpolicesq) +
  s(blackpop) + s(unemp), data=statedata) 
plot.gam(g.log2sq)

