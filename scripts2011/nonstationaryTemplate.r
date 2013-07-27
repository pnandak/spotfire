# Non-stationary ARIMA & ECM estimation, fiting, and interpretation
# Chris Adolph
rm(list=ls())

# Load libraries
library(Zelig)        # For Zelig simulation procedures
library(tseries)      # For unit root tests
library(lmtest)       # For Breusch-Godfrey LM test of serial correlation
library(urca)         # For estimating cointegration models

# Load data
# US Presidential approval data (Bush, Monthly, 2/2001--6/2006)
# Includes average oil price data ($/barrel?)
#
# Variable names:  month year approve disapprove unsure
#                  sept.oct.2001 iraq.war avg.price

data(approval)
attach(approval)

# Create a random walk
set.seed(1)
phony <- rnorm(length(approve))
for (i in 2:length(phony))
    phony[i] <- phony[i-1] + rnorm(1) 

# Look at the time series
pdf("tsapproval.pdf",width=6,height=3.25)
plot(approve,type="l",ylab="Percent Approving",xlab="Time",
     main = "US Presidential Approval")
lines(x=c(8,8),y=c(-1000,1000),col="red")
lines(x=c(26,26),y=c(-1000,1000),col="blue")
text("9/11",x = 10, y = 40, col="red",cex=0.7)
text("Iraq \n War",x = 28, y = 40, col="blue",cex=0.7) 
dev.off()

pdf("tsprice.pdf",width=6,height=3.25)
plot(avg.price,type="l",ylab="$ per Barrel",xlab="Time",
     main = "Average Price of Oil")
lines(x=c(8,8),y=c(-1000,1000),col="red")
lines(x=c(26,26),y=c(-1000,1000),col="blue")
text("9/11",x = 10, y = 175, col="red",cex=0.7)
text("Iraq \n War",x = 28, y = 175, col="blue",cex=0.7) 
dev.off()

# Look at the ACF
pdf("acfapproval.pdf",width=6,height=3.25)
acf(approve)
dev.off()

pdf("acfprice.pdf",width=6,height=3.25)
acf(avg.price)
dev.off()

# Look at the PACF
pdf("pacfapproval.pdf",width=6,height=3.25)
pacf(approve)
dev.off()

pdf("pacfprice.pdf",width=6,height=3.25)
pacf(avg.price)
dev.off()

# Check for a unit root
PP.test(approve)
adf.test(approve)

PP.test(avg.price)
adf.test(avg.price)

# Consider the first difference of each variable
approveLag <- c(NA, approve[1:(length(approve)-1)])
approveDiff <- approve - approveLag

avg.priceLag <- c(NA, avg.price[1:(length(avg.price)-1)])
avg.priceDiff <- avg.price - avg.priceLag

# Look at the DIFFERENCED time series
pdf("tsapprovalDiff.pdf",width=6,height=3.25)
plot(approveDiff,type="l",ylab="Change in Percent Approving",xlab="Time",
     main = "US Presidential Approval")
lines(x=c(8,8),y=c(-1000,1000),col="red")
lines(x=c(26,26),y=c(-1000,1000),col="blue")
text("9/11",x = 10, y = 15, col="red",cex=0.7)
text("Iraq \n War",x = 28, y = 15, col="blue",cex=0.7) 
dev.off()

pdf("tspriceDiff.pdf",width=6,height=3.25)
plot(avg.priceDiff,type="l",ylab="Change in $ per Barrel",xlab="Time",
     main = "Average Price of Oil")
lines(x=c(8,8),y=c(-1000,1000),col="red")
lines(x=c(26,26),y=c(-1000,1000),col="blue")
text("9/11",x = 10, y = -30, col="red",cex=0.7)
text("Iraq \n War",x = 28, y = -30, col="blue",cex=0.7) 
dev.off()

# Look at the new ACF
pdf("acfapprovalDiff.pdf",width=6,height=3.25)
acf(approveDiff, na.action=na.pass)
dev.off()

pdf("acfpriceDiff.pdf",width=6,height=3.25)
acf(avg.priceDiff, na.action=na.pass)
dev.off()

# Look at the new PACF
pdf("pacfapprovalDiff.pdf",width=6,height=3.25)
pacf(approveDiff, na.action=na.pass)
dev.off()

pdf("pacfpriceDiff.pdf",width=6,height=3.25)
pacf(avg.priceDiff, na.action=na.pass)
dev.off()

# Check for a unit root in differenced time series
PP.test(na.omit(approveDiff))
adf.test(na.omit(approveDiff))

PP.test(na.omit(avg.priceDiff))
adf.test(na.omit(avg.priceDiff))

#################################################################
## Model 1a:  ARIMA(0,1,0) model of approve
##

## Estimate an ARIMA(0,1,0) using arima
xcovariates <- cbind(sept.oct.2001, iraq.war, avg.price)
arima.res1a <- arima(approve, order = c(0,1,0),
                     xreg = xcovariates, include.mean = TRUE
                     )
print(summary(arima.res1a))

# Extract estimation results from arima.res1a
pe.1a <- arima.res1a$coef                    # parameter estimates (betas)
se.1a <- sqrt(diag(arima.res1a$var.coef))    # standard errors
ll.1a <- arima.res1a$loglik                  # log likelihood at its maximum
sigma2hat.1a <- arima.res1a$sigma2           # standard error of the regression
aic.1a <- arima.res1a$aic                    # Akaike Information Criterion
resid.1a <- arima.res1a$resid                # residuals


#################################################################
## Model 1c:  ARIMA(1,1,0) model of approve
##

## Estimate an ARIMA(1,1,0) using arima
xcovariates <- cbind(sept.oct.2001, iraq.war, avg.price)
arima.res1b <- arima(approve, order = c(1,1,0),
                     xreg = xcovariates, include.mean = TRUE
                     )
print(summary(arima.res1b))

# Extract estimation results from arima.res1a
pe.1b <- arima.res1b$coef                    # parameter estimates (betas)
se.1b <- sqrt(diag(arima.res1b$var.coef))    # standard errors
ll.1b <- arima.res1b$loglik                  # log likelihood at its maximum
sigma2hat.1b <- arima.res1b$sigma2           # standard error of the regression
aic.1b <- arima.res1b$aic                    # Akaike Information Criterion
resid.1b <- arima.res1b$resid                # residuals



#################################################################
## Model 1b:  ARIMA(2,1,2) model of approve
##

## Estimate an ARIMA(2,1,2) using arima
xcovariates <- cbind(sept.oct.2001, iraq.war, avg.price)
arima.res1c <- arima(approve, order = c(2,1,2),
                     xreg = xcovariates, include.mean = TRUE
                     )
print(summary(arima.res1c))

# Extract estimation results from arima.res1a
pe.1c <- arima.res1c$coef                    # parameter estimates (betas)
se.1c <- sqrt(diag(arima.res1c$var.coef))    # standard errors
ll.1c <- arima.res1c$loglik                  # log likelihood at its maximum
sigma2hat.1c <- arima.res1c$sigma2           # standard error of the regression
aic.1c <- arima.res1c$aic                    # Akaike Information Criterion
resid.1c <- arima.res1c$resid                # residuals



#################################################################
## Model 1d:  ARIMA(0,1,0) model of approve including a spurrious regressor
##

## Estimate an ARIMA(0,1,0) using arima
xcovariates <- cbind(sept.oct.2001, iraq.war, avg.price, phony)
arima.res1d <- arima(approve, order = c(0,1,0),
                     xreg = xcovariates, include.mean = TRUE
                     )
print(summary(arima.res1d))

# Extract estimation results from arima.res1a
pe.1d <- arima.res1d$coef                    # parameter estimates (betas)
se.1d <- sqrt(diag(arima.res1d$var.coef))    # standard errors
ll.1d <- arima.res1d$loglik                  # log likelihood at its maximum
sigma2hat.1d <- arima.res1d$sigma2           # standard error of the regression
aic.1d <- arima.res1d$aic                    # Akaike Information Criterion
resid.1d <- arima.res1d$resid                # residuals




# Based on ACF, PACF, and AIC, let's select Model 1a to be Model 1
arima.res1 <- arima.res1a


## What would happen if we used linear regression on a single lag of approval?
lm.res1e <- lm(approve ~ approveLag + sept.oct.2001 + iraq.war + avg.price)
print(summary(lm.res1e))

# linear regression with a spurious regressor?
lm.res1f <- lm(approve ~ approveLag + sept.oct.2001 + iraq.war + avg.price + phony)
print(summary(lm.res1f))

# Check LS result for serial correlation in the first or second order
bgtest(lm.res1e,1)
bgtest(lm.res1f,2)


#########################################################
##
## Now that we've selected a model, let's interpret it
## using counterfactuals iterated over time
##

# if we want CIs, use Zelig's ARIMA


# Estimate an ARIMA(0,1,0) with arima() run *through* Zelig
# Can use any additional options from the arima() help page
#
# See help on package Zelig using: help(package="Zelig")

zarima.res1 <- zelig(# Provide a formula, with the response in
                     #    Diff(y,1)
                     #
                     # the number lags of the response is 0
                     #    lag.y(0)
                     #
                     # the number of lags of the error is 0 
                     #    lag.eps(0)
                     #
                     # and any covariates after that                     
                     Diff(approve,1) ~ lag.y(0) + lag.eps(0) + avg.price + sept.oct.2001 + iraq.war,

                     # Provide the data as a dataframe (mandatory)
                     data = approval,

                     # Specify the model type (here, arima)
                     model="arima")

print(summary(zarima.res1))

# Extract estimation results from zarima.res1
pe <- zarima.res1$coef                    # parameter estimates (betas)
se <- sqrt(diag(zarima.res1$var.coef))    # standard errors
ll <- zarima.res1$loglik                  # log likelihood at its maximum
sigma2hat <- zarima.res1$sigma2           # standard error of the regression
aic <- zarima.res1$aic                    # Akaike Information Criterion
resid <- zarima.res1$resid                # residuals


## Now we conduct counterfactual experiments to see how x influences y over time
## within the time range of the observed data

# Counterfactually simulate the real world under a change in a single covariate
# (see Zelig help for setx(), sim())

# The counterfactual (No Iraq war)
nowarx <- iraq.war
nowarx[26:28] <- rep(0,3) 
xpre  <- setx(# Give setx the estimated model
              zarima.res1,
              # Now we list each covariate we want to change from its actual
              # values by name, and define for it a list including the
              # range of periods we want to change it, and the new value for
              # each period
              iraq.war = list(
                         # We want to simulate over periods 26 (start of war)
                         # to 65 (end of data),
                         # to see what would happen to approve if the Iraq
                         # hadn't happened
                         time = seq(from=26, to=65, by=1),
                         # ask setx to set iraq.war to 0 for these periods
                         value = nowarx[26:length(nowarx)]
                         )
              )

# Ask Zelig to simulate the expected and predicted approve under no Iraq war
ypre <- sim(zarima.res1, x = xpre)

# See the simulations
print(summary(ypre))

# Plot the sims:  no war
pdf("WithinNoIraq.pdf",width=6,height=3.25)
plot(ypre, lty.set=2, pred.se=FALSE)
lines(y=approve[26:length(approve)],x=26:length(approve))   # Add real data to plot
dev.off()

# Set up the factual scenario
xpost <- setx(zarima.res1,
              # Now we list each covariate we want to change from its actual
              # values by name, and define for it a list including the
              # range of periods we want to change it, and the new value for
              # each period
              iraq.war = list(
                         # We want to simulate over periods 26 (start of war)
                         # to 65 (end of data),
                         # to see what happens to approve if the Iraq
                         # happens
                         time = seq(from=26, to=65, by=1),
                         # ask setx to set iraq.war to its historical values
                         # for these periods
                         value = iraq.war[26:length(iraq.war)]
                         )
              )


# Ask Zelig to simulate the expected and predicted deaths under yes Iraq war
ypost <- sim(zarima.res1, x = xpost)

# See the simulations
print(summary(ypost))

# Plot the sims:  war
pdf("WithinIraq.pdf",width=6,height=3.25)
plot(ypost, lty.set=2, pred.se=FALSE)
lines(y=approve[26:length(approve)],x=26:length(approve))   # Add real data to plot
dev.off()


# Ask Zelig to simulate the expected change between these two scenarios
yfirstdiff <- sim(zarima.res1, x=xpre, x1 = xpost)
print(summary(yfirstdiff))
pdf("WithinSampleFirstDiffIraq.pdf",width=6,height=7)
plot(yfirstdiff,pred.se=FALSE)
dev.off()



############# OIL simulations


# The counterfactual (Cheap Oil)
cheapoil <- avg.price
cheapoil[26:length(nowarx)] <- rep(161.3,(length(cheapoil)-26+1))
xpre  <- setx(# Give setx the estimated model
              zarima.res1,
              # Now we list each covariate we want to change from its actual
              # values by name, and define for it a list including the
              # range of periods we want to change it, and the new value for
              # each period
              avg.price = list(
                         # We want to simulate over periods 26 (start of war)
                         # to 65 (end of data),
                         # to see what would happen to approve if the Iraq
                         # hadn't happened
                         time = seq(from=26, to=65, by=1),
                         # ask setx to set iraq.war to 0 for these periods
                         value = cheapoil[26:length(nowarx)]
                         )
              )

# Ask Zelig to simulate the expected and predicted approve under cheap oil
ypre <- sim(zarima.res1, x = xpre)

# See the simulations
print(summary(ypre))

# Plot the sims:  cheap oil
pdf("WithinCheapOil.pdf",width=6,height=3.25)
plot(ypre, lty.set=2, pred.se=FALSE)
lines(y=approve[26:length(approve)],x=26:length(approve))   # Add real data to plot
dev.off()

# Set up the factual scenario for actual oil price
xpost <- setx(zarima.res1,
              # Now we list each covariate we want to change from its actual
              # values by name, and define for it a list including the
              # range of periods we want to change it, and the new value for
              # each period
              avg.price = list(
                         # We want to simulate over periods 26 (start of war)
                         # to 65 (end of data),
                         # to see what happens to approve if oil follows
                         # actual price history
                         time = seq(from=26, to=65, by=1),
                         # ask setx to set iraq.war to its historical values
                         # for these periods
                         value = avg.price[26:length(iraq.war)]
                         )
              )


# Ask Zelig to simulate the expected and predicted approve under actual oil prices
ypost <- sim(zarima.res1, x = xpost)

# See the simulations
print(summary(ypost))

# Plot the sims:  actual oil
pdf("WithinExpensiveOil.pdf",width=6,height=3.25)
plot(ypost, lty.set=2, pred.se=FALSE)
lines(y=approve[26:length(approve)],x=26:length(approve))   # Add real data to plot
dev.off()


# Ask Zelig to simulate the expected change between these two scenarios
yfirstdiff <- sim(zarima.res1, x=xpre, x1 = xpost)
print(summary(yfirstdiff))
pdf("WithinSampleFirstDiffOil.pdf",width=6,height=7)
plot(yfirstdiff,pred.se=FALSE)
dev.off()



################################################
# Cointegration analysis
cointvars <- cbind(approve,avg.price)

ecm.test1 <- ca.jo(cointvars,
                   ecdet = "const",
                   type="eigen",
                   K=2,
                   spec="longrun",
                   dumvar=cbind(sept.oct.2001,iraq.war)
                   )

ecm.res1 <- cajorls(ecm.test1,
                    r = 1,
                    reg.number = 1)

summary(ecm.res1$rlm)



########################################################
# Cointegration analysis with a spurious regressor
cointvars <- cbind(approve,avg.price,phony)

ecm.test1 <- ca.jo(cointvars,
                   ecdet = "const",
                   type="eigen",
                   K=2,
                   spec="longrun",
                   dumvar=cbind(sept.oct.2001,iraq.war)
                   )

ecm.res1 <- cajorls(ecm.test1,
                    r = 1,
                    reg.number = 1)

summary(ecm.res1$rlm)


