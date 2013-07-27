
#Set Working Directory Here
setwd()
#Read the data
library(foreign)
jacob <- read.dta("jacob.dta")
attach(jacob)

mod.loess<-loess(chal.vote~chal.spend+checks.raw, span=.8, degree=1)
#Setting up "newdata" points for fitted values
#in order to make perspective plots
chals2 <- seq(min(chal.spend), max(chal.spend), len=25)
checks2 <- seq(min(checks.raw), max(checks.raw), len=25)
data <- expand.grid(chal.spend=chals2, checks.raw=checks2)

#Fitted values for each model for the newdata
fit.mod<-matrix(predict(mod.loess, data), 25, 25)

#Figure 2.14
persp (chals2, checks2, fit.mod, 
theta=230, xlab='Challenger Spending', ylab='Overdrafts', zlab='Challenger Vote Shate', shade = .15, col = "white")
 
mod.loess2<-loess(chal.vote ~ checks.raw, span=.8, degree=1)
anova(mod.loess, mod.loess2)

