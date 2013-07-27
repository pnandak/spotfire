# -------------------------------------------------------------------
# Lecture 2: Stationarity testing, PPP example from Enders, 2004

# Libraries: tseries, zoo
  rm(list=ls())
  setwd("~/Desktop/Cam/MFin Lectures Lent/Lecture2/Example")
  ls()
# -------------------------------------------------------------------

 ppp <- read.csv("http://thiloklein.de/R/Lent/RealExchangeRates_US_UK.csv")
 names(ppp) <- c("Date","RER")
 str(ppp)
 library(zoo)
 ppp$Date <- as.yearmon(ppp$Date, format="%YM%m")
 head(ppp$Date)

 # In other words if the PPP holds we should find out that the real exchange rate is stationary.
 # Let’s test this theory employing the Augmented Dickey-Fuller test.
 # Additionally note that the theory implies that our test can include only a constant (and not a trend).

 # This is the real exchange rate for UK, defined as US prices in £ the numerator and
 # UK prices in the denominator
 plot(RER ~ Date, type="l", data=ppp)

 # R's default adf:
 # does not allow specification of constant/trend in the test!
 library(tseries)
 adf.test(ppp$RER)
 ?adf.test

 # Alternatives:
 source("http://thiloklein.de/R/myfunctions.R")

 # Augmented Dickey Fuller test (correct p-value)
 adf.test.1(x=ppp$RER, int=T, trend=F)
    # The arguments of the function are as in the original adf.test function, i.e.
    # x    = a numeric vector or time series.
    # k    = the lag order to calculate the test statistic
    #        defaults to (n-1)^(1/3)
    # In addition, we have
    # int   = logical, a constant is included if int=T
    # trend = logical, a trend variable is included if trend=T

 # Augmented Dickey Fuller test (summary table)
 summary( adf.test.2(x=ppp$RER, int=T, trend=F) )
    # The arguments of the function are: 
    # x     = time series vector
    # k     = number of lags to be included in the test
    # int   = logical, a constant is included if int=T
    # trend = logical, a trend variable is included if trend=T

 # The null hypothesis that the RER series has a unit root cannot be rejected
 # (p-value 0.46)


 ## --- A procedure to test for unit roots ---
 # Walter Enders (2004) Applied Econometric time series, page 213 ff

# Step 1: Start with trend and drift model (least restrictive)
 adf.test.1(x=ppp$RER, int=T, trend=T, k=0)

# Step 2: If null is NOT rejected, check
 # where to many deterministic regressors included in step 1?
 # test for significance of the of the trend term by joint hypothesis

 lm1 <- adf.test.2(x=ppp$RER, int=T, trend=T, k=0)
 summary(lm1)

 # wrong p-value from F-distribution:
 linearHypothesis(lm1, c("xD.lag(x, -1)", "xtime(x)"))

 # correct p-value from DF-table
 linearHypothesis.adf(lm1, c("xD.lag(x, -1)", "xtime(x)"), int=T, trend=T)
 # We can not reject H0. -> Coeffient of time-trend is zero
 
# Step 3: If null is not rejected, estimate model without trend
 adf.test.1(x=ppp$RER, int=T, trend=F, k=0) 
 # null hypothesis of unit root is not rejected

 # -> test for joint significance of constant and regressor
 lm2 <- adf.test.2(x=ppp$RER, int=T, trend=F, k=0)
 summary(lm2)
 linearHypothesis.adf(lm2, c("(Intercept)", "x"), int=T, trend=F) 
 # drift is not significant

# Step 4: If null is not rejected, estimate a model without drift and trend
 adf.test.1(x=ppp$RER, int=F, trend=F, k=0)
 # -> null is not rejected
 # -> series contains a unit root
