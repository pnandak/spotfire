# -------------------------------------------------------------------
# Lecture 1: DL models, orange juice example from Stock and Watson, 2007

# Required libraries: tseries, dynlm
  rm(list=ls())
  source("http://thiloklein.de/R/myfunctions.R")
  setwd("~/Desktop/Cam/MFin Lectures Lent")
  ls()
# -------------------------------------------------------------------


# --- Ex 2: DL Models. Orange juice price and freezing degree days ----------------------------- 
 
 oj <- read.csv("http://thiloklein.de/R/Lent/oj.csv") 
 names(oj) <- c("frz","ojfin","ojfro")
 str(oj) 
 
 
# --- Ex 2: a) --- 
# Generate a time series of the % of change in the ojfro, call it cojfro. Plot the data.  
# Test for unit roots. 

 # create time series object with ts()
 ojfro <- ts(oj$ojfro) 
 cojfro <- ( (ojfro/lag(ojfro,-1)) -1 )*100 
 plot(cojfro) 
 
 ## Dickey Fuller test: 
 # Ho: series has a unit root
 install.packages("tseries")
 library(tseries)
 adf.test(na.omit(cojfro), k=0) 
 # We reject the hypothesis of a unit root in this time series 
 
 
# --- Ex 2: b) --- 
# Estimate a static model, with cojfro as dependant variable and frz as independent  
# variable. 
 
 frz <- ts(oj$frz) 
 # create time series union object with ts.union()
 data2b <- ts.union(frz, cojfro) 
 lm2b <- lm(cojfro ~ frz, data=data2b) 
 summary(lm2b) 

 library(sandwich)
 ?vcovHC
 sqrt(diag(vcovHC(lm2b, type="HC0")))
 shccm(lm2b)

 ?vcovHAC 
 sqrt(diag(vcovHAC(lm2b, weights=weights(lm2b)))) #NeweyWest(lm2b, lag=NULL)
 sqrt(diag(vcovHAC(lm2b)))
 shaccm(lm2b)


# --- Ex 2: c) --- 
# Estimate a DL model for this data with cojfro as dependant variable and frz as  
# independent variable. 
 
 ## Distributed Lag model 
 library(dynlm)  
 dl2c.18 <- dynlm(cojfro ~ L(frz, 0:18)) 
 summary(dl2c.18) 

 con2 <- sqrt(diag(vcovHAC(dl2c.18)))*qnorm(p=0.975)
 names(con2)[2:20] <- paste("lag", 0:18, "")

 # Plot the dynamic multiplier ...
 plot(0:18, dl2c.18$coef[2:20], type="l", col="blue", ylim=c(-.3,.8), xlab="Lag", ylab="Dynamic Multiplier")
 abline(h=0)
 # ... and 95% confidence interval.
 con <- summary(dl2c.18)$coef[2:20,2]*qnorm(p=0.975)
 lines(0:18, dl2c.18$coef[2:20]+con, col="green")
 lines(0:18, dl2c.18$coef[2:20]-con, col="green")
 # ...and 95% confidence interval with HAC errors
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

 # OR use the following equivalent model
 dl2e <- dynlm(cojfro ~ L(frz, 18) + L(d(frz, 1), 0:17)) 
 summary(dl2e) 
 # to plot the cumulative dynamic multiplier...
 mul <- dl2e$coef[c(3:20,2)]
 lines(0:18, mul, col="blue")
 # ... and 95% confidence interval.
 con <- summary(dl2e)$coef[c(3:20,2),2]*qnorm(p=0.975)   
 lines(0:18, mul+con, col="green")
 lines(0:18, mul-con, col="green")
 con2 <- sqrt(diag(vcovHAC(dl2e)))[c(3:20,2)]*qnorm(p=0.975)   
 lines(0:18, mul+con2, col="red")
 lines(0:18, mul-con2, col="red")
 legend("topright",legend=c("Cumulative dynamic multiplier","95% confidence band","95% confidence band with HAC errors"),fill=c("blue","green","red"),bty="n")


 # Now take only the DL(1) model, assume that this is the "right model". 
 # The cumulative multipliers are: 0.51 ("impact") and 0.64 ("long-run"). 
 # To be able to estimate the standard deviations corresponding to each of these  
 # cumulative multipliers, let's estimate the following equivalent model: 
 
 dl2e <- dynlm(cojfro ~ L(frz, 1) + d(frz, 1)) 
 summary(dl2e) 
 
 # Let's plot the cumulative coefficients and coefficients +/- 2 std. error (95% confidence  
 # interval). 
 
 L.frz <- dl2e$coef[2] 
 d.frz <- dl2e$coef[3] 
 plot(c(d.frz, L.frz) ~ c(0,1), type="l", col="blue", ylim=c(.4,.8))  
 
 se.L.frz <- summary(dl2e)$coef[2,2] 
 se.d.frz <- summary(dl2e)$coef[3,2] 
 lines(c(d.frz + 2*se.d.frz, L.frz + 2*se.L.frz) ~ c(0,1), col="green") 
 lines(c(d.frz - 2*se.d.frz, L.frz - 2*se.L.frz) ~ c(0,1), col="green") 


