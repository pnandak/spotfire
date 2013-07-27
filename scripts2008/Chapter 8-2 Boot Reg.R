
library(foreign)
library(boot)

#Set Working Directory
setwd()

Election <- read.dta("election.dta")
attach(Election)

ols <- lm(inc ~ rdi + moderate + gdp, data=Election)
summary(ols)

par(mfrow=c(2,2))
plot(ols)
 
#Write Bootstrap Function To Resample Pairs
boot.elec.pairs <- function(data, indices){
             #Selects By Observations For Bootstrap Samples
             data=data[indices,]
             mdl <- lm(inc ~ rdi + moderate + gdp, data=data)
             #Returns coefficients vector 
             coefficients(mdl)
             }
#Get bootstrap estimates
elec.boot <- boot(data=Election, statistic=boot.elec.pairs, 1500)
elec.boot


#Figure 8.2
plot(elec.boot, index=3, main="Policy Moderation")

#CIs
boot.ci(elec.boot, index=1, type=c("norm", "perc", "bca"))
boot.ci(elec.boot, index=2, type=c("norm", "perc", "bca"))
boot.ci(elec.boot, index=3, type=c("norm", "perc", "bca"))
boot.ci(elec.boot, index=4, type=c("norm", "perc", "bca"))


#Now Residuals
elec.ols <- lm(inc ~ rdi + moderate + gdp, data=Election)

boot.elec.resid <- function(data, indices){
                   X <- model.matrix(elec.ols)
                   yhat <- fitted(elec.ols)
                   e <- resid(elec.ols)
                   y.star <- yhat + e[indices]
                   model <- lm(y.star ~ X-1)
                   coefficients(model)
             }
elec.boot <- boot(data=Election, statistic=boot.elec.resid, 1500)
elec.boot
#plot(elec.boot, index=3)

#New CIs
boot.ci(elec.boot, index=1, type=c("norm", "perc", "bca"))
boot.ci(elec.boot, index=2, type=c("norm", "perc", "bca"))
boot.ci(elec.boot, index=3, type=c("norm", "perc", "bca"))
boot.ci(elec.boot, index=4, type=c("norm", "perc", "bca"))
