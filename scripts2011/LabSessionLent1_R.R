# -------------------------------------------------------------------
# MPO1-A Advanced Quantitative Research Methods
# Thilo Klein
# Lab Session 1: Unit Root Test and ADL Models

# Libraries: tseries, dynlm, sandwich, car, AER
  rm(list=ls())
  setwd("~/Desktop/LabSessions_MPO1A_LT/Data")
  source("http://thiloklein.de/R/myfunctions.R")
  ls()
# -------------------------------------------------------------------




## --- Digression: Spurious regression ---
# 1000 regressions of iid x against iid y
# p-value of slope coefficient is below 5% in around 5% of cases
 set.seed <- 123
 count <- 0
 for(i in 1:1000){
   x <- rnorm(50)
   y <- rnorm(50)
   if( summary(lm(y~x))$coef[2,4] < 0.05 ){
     count <- count + 1
   }
 }
 count/1000

# 1000 regressions of random walk x against random walk y
# p-value of slope coefficient is below 5% in more than 60% of cases
 set.seed <- 123
 count <- 0
 for(i in 1:1000){
   x <- cumsum(rnorm(50))
   y <- cumsum(rnorm(50))
   if( summary(lm(y~x))$coef[2,4] < 0.05 ){
     count <- count + 1
   }
 }
 count/1000




# --- Ex 1: Unit Roots -----------------------------

 real <- read.csv("http://thiloklein.de/R/Lent/real.csv", header=T)
 str(real)

# --- Ex 1: a) ---
# For each sequence plot the data and find the ACF and PACF of i) the level of the 
# real exchange rate; and ii) the first difference of the real exchange rate.

 ?ts
 RCAN <- ts(real$RCAN, start=c(1973, 1), frequency=12)
 RCAN
 plot(RCAN)

 par(mfrow=c(2,2))
 acf(RCAN); acf(RCAN, plot=F)
 pacf(RCAN); pacf(RCAN, plot=F)

 RCAN.d1 <- diff(RCAN, lag=1)
 acf(RCAN.d1); acf(RCAN.d1, plot=F)
 pacf(RCAN.d1); pacf(RCAN.d1, plot=F)


# --- Ex 1: c) ---
# Including a constant, use ADF (with and without trend, automatic selection of lags, 
# Swartz criterion) tests to test whether the series are unit root processes.

 library(tseries)
 ?adf.test
 ## ADF, H0: series has a unit root
 # The general regression equation which incorporates a constant and a linear trend is 
 # used and the t-statistic for a first order autoregressive coefficient equals one is 
 # computed.

 # R's default adf:
 adf.test(RCAN)

 # Augmented Dickey Fuller test (correct p-value)
 adf.test.1(RCAN, int=T, trend=F)
    # The arguments of the function are as in the original adf.test function, i.e.
    # x    = a numeric vector or time series.
    # k    = the lag order to calculate the test statistic
    #        defaults to (n-1)^(1/3)
    # In addition, we have
    # int   = logical, a constant is included if int=T
    # trend = logical, a trend variable is included if trend=T

 # Augmented Dickey-Fuller test equation:
 adf <- adf.test.2(RCAN, int=T, trend=F) 
 summary(adf)




## --- Digression: A procedure to test for unit roots ---
# based on Walter Enders (2004) Applied Econometric time series, page 213 ff

# Step 1: Start with trend and drift model (least restrictive)
 adf.test.1(x=RCAN, int=T, trend=T, k=0)

# Step 2: If null is NOT rejected, check
 # where to many deterministic regressors included in step 1?
 # test for significance of the of the trend term by joint hypothesis

 lm1 <- adf.test.2(x=RCAN, int=T, trend=T, k=0)
 summary(lm1)

 # wrong p-value from F-distribution:
 linearHypothesis(lm1, c("xD.lag(x, -1)", "xtime(x)"))

 # correct p-value from DF-table
 linearHypothesis.adf(lm1, c("xD.lag(x, -1)", "xtime(x)"))
 # We can not reject H0. -> Coeffient of time-trend is zero
 
# Step 3: If null is not rejected, estimate model without trend
 adf.test.1(x=RCAN, int=T, trend=F, k=0) 
 # null hypothesis of unit root is not rejected

 # -> test for joint significance of constant and regressor
 lm2 <- adf.test.2(x=RCAN, int=T, trend=F, k=0)
 summary(lm2)
 linearHypothesis.adf(lm2, c("(Intercept)", "x")) 
 # drift is not significant

# Step 4: If null is not rejected, estimate a model without drift and trend
 adf.test.1(x=RCAN, int=F, trend=F, k=0)
 # -> null is not rejected
 # -> series contains a unit root




# --- Ex 1: d) ---
# Use the Japanese data to show that you can reject the null of two unit roots.

 RJAP <- ts(real$RJAP, start=c(1973, 1), freq=12)
 RJAP.d1 <- diff(RJAP, lag=1)

 # for DF-test set k to 0:
 adf.test.1(RJAP, int=T, trend=F, k=0)
 adf.test.1(RJAP.d1, int=T, trend=F, k=0)

 ## Dickey-Fuller test equation: 
 summary( adf.test.2(RJAP.d1, int=T, trend=F, k=0) )




# --- Ex 2: DL Models. Orange juice price and freezing degree days ----------------------------- 
 
 oj <- read.csv("http://thiloklein.de/R/Lent/oj.csv") 
 str(oj) 

 # OR:
 library(AER)
 ?FrozenJuice
 data("FrozenJuice")
 str( data.frame(FrozenJuice) )
 
 
# --- Ex 2: a) --- 
# Generate a time series of the % of change in the ojfro, call it cojfro. Plot the data.  
# Test for unit roots. 

 # create time series object with ts()
 ojfro <- ts(oj$ojfro) 
 ojfro
 cojfro <- ( (ojfro/lag(ojfro,-1)) -1 )*100 
 plot(cojfro) 
 
 ## Dickey Fuller test: 
 # Ho: series has a unit root / is non-stationary
 adf.test.1(na.omit(cojfro), int=F, trend=F, k=0)
 # We reject the hypothesis of a unit root in this time series 
 

# --- Ex 2: b) --- 
# Estimate a static model, with cojfro as dependant variable and frz as independent  
# variable. 
 
 frz <- ts(oj$frz) 
 # create time series union object with ts.union()
 data2b <- ts.union(frz, cojfro) 
 lm2b <- lm(cojfro ~ frz, data=data2b) 
 summary(lm2b) 

 # address potential problems of autocorrelated errors
 library(sandwich)
 ?vcovHC
 sqrt(diag(vcovHC(lm2b, type="HC0")))
 shccm(lm2b)

 ?vcovHAC 
 # sqrt(diag(vcovHAC(lm2b, weights=weights(lm2b)))) #NeweyWest(lm2b, lag=NULL)
 sqrt(diag(vcovHAC(lm2b)))
 library(car)
 shaccm(lm2b) # convenience function for HAC errors


# --- Ex 2: c) --- 
# Estimate a DL model for this data with cojfro as dependant variable and frz as  
# independent variable. 
 
 ## Distributed Lag model 
 library(dynlm)  
 dl2c.18 <- dynlm(cojfro ~ L(frz, 0:18)) 
 shaccm(dl2c.18)
 summary(dl2c.18) 

 # Plot the dynamic multiplier ...
 plot(0:18, dl2c.18$coef[2:20], type="l", col="blue", ylim=c(-.3,.8), xlab="Lag", ylab="Dynamic Multiplier")
 abline(h=0)
 # ... and 95% confidence interval with standard type errors.
 con <- summary(dl2c.18)$coef[2:20,2]*qnorm(p=0.975)
 lines(0:18, dl2c.18$coef[2:20]+con, col="green")
 lines(0:18, dl2c.18$coef[2:20]-con, col="green")
 # ...and 95% confidence interval with HAC errors
 con2 <- sqrt(diag(vcovHAC(dl2c.18)))*qnorm(p=0.975)
 lines(0:18, dl2c.18$coef[2:20]+con2[2:20], col="red")
 lines(0:18, dl2c.18$coef[2:20]-con2[2:20], col="red")
 legend("topright",legend=c("Dynamic multiplier","95% confidence band","95% confidence band with HAC errors"),fill=c("blue","green","red"),bty="n")

 # There are many insignificant coefficients, let's simplify the model. 
 # Sequentially eliminate the insignificant terms. 
 dl2c.12 <- dynlm(cojfro ~ L(frz, c(0,1,12))) 
 summary(dl2c.12) 
 
 # Begin now with 7 terms. 
 dl2c.7 <- dynlm(cojfro ~ L(frz, 0:7)) 
 summary(dl2c.7) 
 
 # Simplify the model. 
 dl2c.1 <- dynlm(cojfro ~ L(frz, 0:1)) 
 summary(dl2c.1) 
 
 
# --- Ex 2: d) --- 
# Compute the impact and long-run multipliers 
 
 # The impact multiplier is 0.55 in the DL(12) and 0.51 in the DL(1). The long-run  
 # multipliers are 0.56 (0.55+0.15-0.14) and 0.64 (0.51+0.13) respectively. 
 
 
# --- Ex 2: e) --- 
# Compute the cumulative multipliers (plot them including confidence intervals). 

 # Take only the DL(18) model, assume that this is the "right model". 
 # Plot the cumulative dynamic multiplier based on the DL(18) model dl2c.18
 mul <- cumsum(dl2c.18$coef[2:20])
 plot(0:18, mul, type="l", col="red", ylim=c(-.4,1.6), xlab="Lag", ylab="Cumulative dynamic multiplier")
 abline(h=0)

 # OR use the following equivalent model (see appendix handout 1)
 dl2e <- dynlm(cojfro ~ L(d(frz, 1), 0:17) + L(frz, 18)) 
 summary(dl2e) 
 # Again plot the cumulative dynamic multiplier...
 mul <- dl2e$coef[c(2:20)]
 lines(0:18, mul, col="blue")
 # ... and 95% confidence interval.
 # con <- summary(dl2e)$coef[2:20,2]*qnorm(p=0.975)   
 con <- sqrt(diag(vcov(dl2e)))[2:20]*qnorm(p=0.975)
 lines(0:18, mul+con, col="green")
 lines(0:18, mul-con, col="green")
 con2 <- sqrt(diag(vcovHAC(dl2e)))[2:20]*qnorm(p=0.975)   
 lines(0:18, mul+con2, col="red")
 lines(0:18, mul-con2, col="red")
 legend("topright",legend=c("Cumulative dynamic multiplier","95% confidence band","95% confidence band with HAC errors"),fill=c("blue","green","red"),bty="n")




# --- Ex 3: ADL model. El niño and fish population -----------------------------

 fish <- read.csv("http://thiloklein.de/R/Lent/soi.csv", h=T)
 str(fish)

# --- Ex 3: a) ---
# Plot the data and test for unit roots.

 soi <- ts(fish$soi, start=c(1950, 1), freq=12)
 rec <- ts(fish$rec, start=c(1950, 1), freq=12)
 par(mfrow=c(2,1))
 plot(soi); plot(rec) 

 ## Both series are stationary:
 adf.test(soi)
 adf.test(rec)


# --- Ex 3: b) ---
# Estimate an Autoregressive Distributed Lag model.

 adl3b.12.12 <- dynlm(rec ~ L(rec, 0:12) + L(soi, 0:12))
 summary(adl3b.12.12)
 adl3b.12.8 <- dynlm(rec ~ L(rec, c(1,2,4:5,7,12)) + L(soi, c(5,6,8)))
 summary(adl3b.12.8)


# --- Ex 3: c) ---
# Compute the short- (or impact) and long-run multipliers

 # Note that there is no impact multiplier, in other words is equivalent to 0. The 
 # long-run multiplier is -83=(-21.5+8.1-3.3)/(1-1.22+0.37+0.12-0.19+0.09+0.03).
 # This means that an increase in 1 point in the SOI reduces the recruitment by 82 in the 
 # long-run.





# -------------------------------------------------------------------
# --- End of Session ------------------------------------------------

 save.image("EndOfSession.RData")
 q("yes")
