# -------------------------------------------------------------------
# Lecture 3: Example of an ARIMA model, based on Exercise 7, Enders (2004)

# Required libraries: tseries, moments, FinTS
  rm(list=ls())
  setwd("I:/")
  ls()
# -------------------------------------------------------------------


 data <- read.csv("http://thiloklein.de/R/Lent/ARIMA_class_exercise.csv")
 str(data)
 names(data) <- "series"
 attach(data)

 plot(series, type="l")

 # Is the series stationary?
 # Plot autocorrelation and partial autocorrelation
 par(mfrow=c(2,1))
 acf(series); pacf(series)

 # ADF test
 source("http://thiloklein.de/R/myfunctions.R")

 library(tseries)
 adf.test(series, k=0)
 adf.test.1(x=series, int=T, trend=T, k=0)
    # x    = a numeric vector or time series
    # k    = the lag order to calculate the test statistic with default: trunc((length(x)- 1)^(1/3))
    # In addition, we have
    # int   = logical, a constant is included if int=T
    # trend = logical, a trend variable is included if trend=T

 summary( adf.test.2(series, int=T, trend=T, k=0) )
 # We reject the null that y1 has a unit root. Stationary process.


 # The PACF  indicate a possible AR(1) process, estimate it.
 # (always include a constant)
 ar1 <- arma(series, order=c(1,0))
 summary(ar1)


 ## DIAGNOSTIC TEST

 # 1. Are the inverted AR roots <1? Yes. OK

 # 2. Verify the there is no autocorrelation of partial autocorrelation left (ACF and PACF)
 par(mfrow=c(2,1))
 acf(ar1$resid, na.action=na.pass); pacf(ar1$resid, na.action=na.pass)
 # Yes, ok

 # 3. Are the residuals normal distributed?
 par(mfrow=c(1,1))
 hist(ar1$resid, freq=F)
 grid <- seq(-3,3,.01)
 lines(dnorm(grid, sd=sd(ar1$resid, na.rm=T)) ~ grid, col="blue")
 #install.packages("moments")
 library(moments)
 jarque.test(c(na.omit(ar1$resid)))
 # Probability is greater than 0.05. Note that the null hypothesis is: residuals are normal. We cannot reject the null.
 # OK

 # 4. Is there evidence of conditional heteroskedasticity?
 install.packages("FinTS")
 library(FinTS)
 ArchTest(c(ar1$res), lags=2)
 # No. OK


 ## Let us compare this model with an ARMA(1,1)

 arma11 <- arma(series, order=c(1,1))
 summary(arma11)

 str(summary(arma11))
 summary(arma11)$aic
 summary(ar1)$aic

 # Both criteria indicate that the AR(1) model is better
 # (remember the smaller the criterion the better the model)
