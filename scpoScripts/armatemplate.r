# ARIMA estimation, fitiing, and interpretation
# Chris Adolph
rm(list=ls())

# Load libraries
library(Zelig)        # For Zelig simulation procedures
library(tseries)      # For unit root tests
library(lmtest)       # For Breusch-Godfrey LM test of serial correlation

# Load data
# Number of deaths and serious injuries in UK road accidents each month.
# Jan 1969 - Dec 1984. Seatbelt law introduced in Feb 1983
# (indicator in second column). Source: Harvey, 1989, p.519ff.
# http://www.staff.city.ac.uk/~sc397/courses/3ts/datasets.html
#
# Variable names:  death law

ukdata <- read.csv("ukdeaths.csv",header=TRUE)
attach(ukdata)

# Look at the time series
pdf("tsdeath.pdf",width=6,height=3.25)
plot(death,type="l",ylab="deaths",xlab="Time",
     main = "Vehicular accident deaths, UK, 1969-1984")
lines(x=c(170,170),y=c(0,5000),col="red")
text("Seat \n belt \n law",x = 180, y = 2300, col="red",cex=0.7) 
dev.off()

# Look at the ACF
pdf("acfdeath.pdf",width=6,height=3.25)
acf(death)
dev.off()

# Look at the PACF
pdf("pacfdeath.pdf",width=6,height=3.25)
pacf(death)
dev.off()

# Check for a unit root
PP.test(death)
adf.test(death)


#################################################################
## Model 1a:  AR(1) model of death as function of law
##

## Estimate an AR(1) using arima
xcovariates <- law
arima.res1a <- arima(death, order = c(1,0,0),
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
## Model 1b:  AR(1) model of death as function of law & q4
##

## Estimate an AR(1) using arima
xcovariates <- cbind(q4,law)
arima.res1b <- arima(death, order = c(1,0,0),
                     xreg = xcovariates, include.mean = TRUE
                     )
print(summary(arima.res1b))

# Extract estimation results from arima.res1b
pe.1b <- arima.res1b$coef                    # parameter estimates (betas)
se.1b <- sqrt(diag(arima.res1b$var.coef))    # standard errors
ll.1b <- arima.res1b$loglik                  # log likelihood at its maximum
sigma2hat.1b <- arima.res1b$sigma2           # standard error of the regression
aic.1b <- arima.res1b$aic                    # Akaike Information Criterion
resid.1b <- arima.res1b$resid                # residuals




#################################################################
## Model 1c:  ARMA(1,1) model of death as function of law & q4
##

## Estimate an ARMA(1,1) using arima
xcovariates <- cbind(q4,law)
arima.res1c <- arima(death, order = c(1,0,1),
                     xreg = xcovariates, include.mean = TRUE
                     )
print(summary(arima.res1c))


# Extract estimation results from arima.res1c
pe.1c <- arima.res1c$coef                    # parameter estimates (betas)
se.1c <- sqrt(diag(arima.res1c$var.coef))    # standard errors
ll.1c <- arima.res1c$loglik                  # log likelihood at its maximum
sigma2hat.1c <- arima.res1c$sigma2           # standard error of the regression
aic.1c <- arima.res1c$aic                    # Akaike Information Criterion
resid.1c <- arima.res1c$resid                # residuals



#################################################################
## Model 1d:  ARMA(1,2) model of death as function of law & q4
##

## Estimate an ARMA(1,2) using arima
xcovariates <- cbind(q4,law)
arima.res1d <- arima(death, order = c(1,0,2),
                     xreg = xcovariates, include.mean = TRUE
                     )
print(summary(arima.res1d))

# Extract estimation results from arima.res1d
pe.1d <- arima.res1d$coef                    # parameter estimates (betas)
se.1d <- sqrt(diag(arima.res1d$var.coef))    # standard errors
ll.1d <- arima.res1d$loglik                  # log likelihood at its maximum
sigma2hat.1d <- arima.res1d$sigma2           # standard error of the regression
aic.1d <- arima.res1d$aic                    # Akaike Information Criterion
resid.1d <- arima.res1d$resid                # residuals




#################################################################
## Model 1e:  ARMA(1,3) model of death as function of law & q4
##

## Estimate an ARMA(1,3) using arima
xcovariates <- cbind(q4,law)
arima.res1e <- arima(death, order = c(1,0,3),
                     xreg = xcovariates, include.mean = TRUE
                     )
print(summary(arima.res1e))

# Extract estimation results from arima.res1e
pe.1e <- arima.res1e$coef                    # parameter estimates (betas)
se.1e <- sqrt(diag(arima.res1e$var.coef))    # standard errors
ll.1e <- arima.res1e$loglik                  # log likelihood at its maximum
sigma2hat.1e <- arima.res1e$sigma2           # standard error of the regression
aic.1e <- arima.res1e$aic                    # Akaike Information Criterion
resid.1e <- arima.res1e$resid                # residuals



#################################################################
## Model 1f:  ARMA(2,1) model of death as function of law & q4
##

## Estimate an ARMA(2,1) using arima
xcovariates <- cbind(q4,law)
arima.res1f <- arima(death, order = c(2,0,1),
                     xreg = xcovariates, include.mean = TRUE
                     )
print(summary(arima.res1f))

# Extract estimation results from arima.res1f
pe.1f <- arima.res1f$coef                    # parameter estimates (betas)
se.1f <- sqrt(diag(arima.res1f$var.coef))    # standard errors
ll.1f <- arima.res1f$loglik                  # log likelihood at its maximum
sigma2hat.1f <- arima.res1f$sigma2           # standard error of the regression
aic.1f <- arima.res1f$aic                    # Akaike Information Criterion
resid.1f <- arima.res1f$resid                # residuals



# Based on ACF, PACF, and AIC, let's select Model 1d to be Model 1
arima.res1 <- arima.res1d


## What would happen if we used linear regression on a single lag of death?
lagdeath <- c(NA,death[1:(length(death)-1)])
lm.res1g <- lm(death ~ lagdeath + q4 + law)
print(summary(lm.res1g))

# Check LS result for serial correlation in the first or second order
bgtest(lm.res1g,1)
bgtest(lm.res1g,2)

# Rerun with two lags
lag2death <- c(NA,NA,death[1:(length(death)-2)])
lm.res1h <- lm(death ~ lagdeath + lag2death + q4 + law)
print(summary(lm.res1h))

# Check LS result for serial correlation in the first or second order
bgtest(lm.res1h,1)
bgtest(lm.res1h,2)

# MA terms in ARMA seem justified; we reject the LS model with lags of the DV


#########################################################
##
## Now that we've selected a model, let's interpret it
## using counterfactuals iterated over time
##


## Predict out five years (60 periods) assuming law is kept

# Make newdata dataframe for prediction
n.ahead <- 60
lawhyp0 <- rep(1,n.ahead)
q4hyp0 <- rep( c( rep(0,9), rep(1,3) ), 5)
newdata0 <- cbind(q4hyp0,lawhyp0)   # Must be in same order as model!
newdata0 <- as.data.frame(newdata0)
names(newdata0) <- c("q4","law")

# Run predict
ypred0 <- predict(arima.res1,
                 n.ahead = n.ahead,
                 newxreg = newdata0)


## Predict out five years (60 periods) assuming law is repealed

# Make newdata dataframe for prediction
n.ahead <- 60
lawhyp <- rep(0,n.ahead)
q4hyp <- rep( c( rep(0,9), rep(1,3) ), 5)
newdata <- cbind(q4hyp,lawhyp)   # Must be in same order as model!
newdata <- as.data.frame(newdata)
names(newdata) <- c("q4","law")

# Run predict
ypred <- predict(arima.res1,
                 n.ahead = n.ahead,
                 newxreg = newdata)

# Make a plot
pdf("prediction1.pdf",width=6,height=3.25)
plot.new()
par(usr = c(0, length(death) + n.ahead, 1000, 3000) )
# make the x-axis
axis(1,
     at = seq(from = 10, to = 252, by = 12),
     labels = 1969:1989
     )
axis(2)

title(xlab = "Time",
      ylab = "Deaths",
      main="Predicted effect of reversing seat belt law")



# Polygon of predictive interval for no law (optional)
x0 <- (length(death)+1):(length(death) + n.ahead)
y0 <- c(ypred$pred - 2*ypred$se, rev(ypred$pred + 2*ypred$se), (ypred$pred - 2*ypred$se)[1] )
polygon(x = c(x0, rev(x0), x0[1]),
        y = y0,
        border=NA,
        col="gray70"
        )


# Plot the actual data
lines(x = 1:length(death),
      y = death
      )

# Add the predictions for no law
lines(x = length(death):(length(death)+n.ahead),
      y = c(death[length(death)],ypred$pred),  # link up the actual data to the prediction
      col = "red"
      )


# Add the lower predictive interval for no law
lines(x = length(death):(length(death) + n.ahead),
      y = c(death[length(death)], ypred$pred - 2*ypred$se),
      col = "red",
      lty="dashed"
      )

# Add the upper predictive interval for no law
lines(x = length(death):(length(death) + n.ahead),
      y = c(death[length(death)], ypred$pred + 2*ypred$se),
      col = "red",
      lty = "dashed"
      )


# Add the predictions for keeping law
lines(x = length(death):(length(death)+n.ahead),
      y = c(death[length(death)],ypred0$pred),  # link up the actual data to the prediction
      col = "blue"
      )

dev.off()


# Note that these are *predictive* intervals for y,
# not confidence intervals
# if we want CIs, use Zelig's ARIMA


# Estimate an ARMA(1,2) with arima() run *through* Zelig
# Can use any additional options from the arima() help page
#
# See help on package Zelig using: help(package="Zelig")

zarima.res1 <- zelig(# Provide a formula, with the response in
                     #    Diff(y,0)
                     #
                     # the number lags of the response in
                     #    lag.y(1)
                     #
                     # the number of lags of the error in
                     #    lag.eps(1)
                     #
                     # and any covariates after that                     
                     Diff(death,0) ~ lag.y(1) + lag.eps(2) + q4 + law,

                     # Provide the data as a dataframe (mandatory)
                     data = ukdata,

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

# The counterfactual (no seat belt law)
xpre  <- setx(# Give setx the estimated model
              zarima.res1,
              # Now we list each covariate we want to change from its actual
              # values by name, and define for it a list including the
              # range of periods we want to change it, and the new value for
              # each period
              law = list(
                         # We want to simulate over periods 170 to 192,
                         # to see what would happen if the seat beat law
                         # hadn't been implemented
                         time = seq(from=170, to=192, by=1),
                         # ask setx to set law to 0 for these periods
                         value = rep(0,192-170+1)
                         ),

              # Need to set the q4 variable to logically required values
              #q4 = list(
              #          time = seq(from=170, to - 192, by = 1),
              #          value = rep( c(rep(0,9), rep(1,3)), 5)
              #          )
              )

# Ask Zelig to simulate the expected and predicted deaths under no seat belt law
ypre <- sim(zarima.res1, x = xpre)

# See the simulations
print(summary(ypre))

# Plot the sims:  no seat belts
pdf("WithinSampleNoBelt.pdf",width=6,height=3.25)
plot(ypre, lty.set=2, pred.se=FALSE)
dev.off()

# Now we simulate the factual (seat belt law implemented)
xpost <- setx(zarima.res1,
              law = list(time = seq(from=170, to=192, by=1),
                                    value = rep(1,192-170+1)
                         )
              )

# Ask Zelig to simulate the expected and predicted deaths under the seat belt law
ypost <- sim(zarima.res1, x = xpost)

# See the simulations
print(summary(ypost))

# Plot the sims:  yes seat belts
pdf("WithinSampleBelt.pdf",width=6,height=3.25)
plot(ypost, pred.se=FALSE)
dev.off()

# Add the real data to the plot
pdf("WithinSampleBeltObserved.pdf",width=6,height=3.25)
plot(ypost, pred.se=FALSE)
lines(y=death[170:192],x=170:192)
dev.off()

# Ask Zelig to simulate the expected change between these two scenarios
yfirstdiff <- sim(zarima.res1, x=xpre, x1 = xpost)
print(summary(ypost))
pdf("WithinSampleFirstDiff.pdf",width=6,height=7)
plot(yfirstdiff,pred.se=FALSE)
dev.off()
