setwd("I:/")
ls()
source("http://www.thiloklein.de/R/myfunctions.R")

#Generate White Noise, MA1,  AR1 and Random Walk series

set.seed <- 154
wn  = rnorm(500,0,1)  # 500 N(0,1) variates

rw  = cumsum(wn)    

ar1 = arima.sim(list(order=c(1,0,0), ar=.9), n=500) 
ma  = arima.sim(list(order=c(0,0,1), ma=.8), n=500)

wd  = wn +.2
rwd = cumsum(wd)      

#Plot them
par(mfrow=c(4,1))
plot.ts(wn, main="white noise")
plot.ts(rw, main="random walk")
plot.ts(ar1, main=(expression(AR(1)~~~phi==+.9)))
plot.ts(ma, main=(expression(MA(1)~~~theta==.8)))
#plot.ts(rwd, main="random walk with drift")


#White noise
par(mfrow=c(3,1))
plot.ts(wn, main="white noise")
acf(wn); pacf(wn)

#install.packages("ccgarch")
library(ccgarch)
ljung.box.test.1(x = wn, lags = seq(from=1, to=15, by=1) )

#AR1
ar1 = arima.sim(list(order=c(1,0,0), ar=.9), n=500) 
par(mfrow=c(3,1))
plot.ts(ar1, main=(expression(AR(1)~~~phi==+.9)))
acf(ar1); pacf(ar1)
#ljung.box.test.1(x = c(ar1), lags = seq(from=1, to=15, by=1) )


#AR2
ar2 = arima.sim(list(order=c(2,0,0), ar=c(.6,.2)), n=500) 
par(mfrow=c(3,1))
plot.ts(ar2, main=(expression(AR(2)~~~phi[1]==.9~~~phi[2]==.8)))
acf(ar2); pacf(ar2)
#ljung.box.test.1(x = c(ar2), lags = seq(from=1, to=15, by=1) )

#MA1
ma1  = arima.sim(list(order=c(0,0,1), ma=.8), n=500)
par(mfrow=c(3,1))
plot.ts(ma1, main=(expression(MA(1)~~~theta==.8)))
acf(ma1); pacf(ma1)
#ljung.box.test.1(x = c(ma1), lags = seq(from=1, to=15, by=1) )


#MA2
ma2  = arima.sim(list(order=c(0,0,2), ma=c(.9,.8)), n=500)
par(mfrow=c(3,1))
plot.ts(ma2, main=(expression(MA(1)~~~theta[1]==.9~~~theta[2]==.8)))
acf(ma2); pacf(ma2)
#ljung.box.test.1(x = c(ma2), lags = seq(from=1, to=15, by=1) )

#RW
plot.ts(rw, main="random walk")
par(mfrow=c(3,1))
plot.ts(rw, main=(expression(RW)))
acf(rw); pacf(rw)
#ljung.box.test.1(x = c(rw), lags = seq(from=1, to=15, by=1) )

rw.d1 <- diff(rw, lag=1, differences=1)
plot.ts(rw.d1, main=(expression(RW)))
acf(rw.d1); pacf(rw.d1)
#ljung.box.test.1(x = c(rw.d1), lags = seq(from=1, to=15, by=1) )


#ARMA11
arma11 = arima.sim(list(order=c(1,0,1), ar=.9, ma=-.5), n=500)
par(mfcol=c(3,1))
plot.ts(arma11, main=(expression(ARIMA(1,1,1)~~~phi==.9~~~theta==-.5)))
acf(arma11); pacf(arma11)

# ADF test
source("http://thiloklein.de/R/myfunctions.R")
library(tseries)

w   = rnorm(500,0,1)
rw  = cumsum(w)    
adf.test.1(x=rw, int=T, trend=T, k=0)
    # x    = a numeric vector or time series
    # k    = the lag order to calculate the test statistic with default: trunc((length(x)- 1)^(1/3))
    # In addition, we have
    # int   = logical, a constant is included if int=T
    # trend = logical, a trend variable is included if trend=T
#summary( adf.test.2(rw, int=T, trend=T, k=0) )

ar1 = arima.sim(list(order=c(1,0,0), ar=.9), n=500) 
adf.test.1(x=ar1, int=T, trend=T, k=0)
#summary( adf.test.2(ar1, int=T, trend=T, k=0) )

ar1 = arima.sim(list(order=c(1,0,0), ar=.9), n=500) 
ar1mod<-arima(ar1, order = c(1, 0, 0))
#??coeftest
library(lmtest)
coeftest(ar1mod)
#install.packages("ccgarch")
#library(ccgarch)
ljung.box.test.1(x = c(ar1mod$res), lags = seq(from=1, to=15, by=1) )
acf(ar1mod$res); pacf(ar1mod$res)

ar2 = arima.sim(list(order=c(2,0,0), ar=c(.9,-.8)), n=500) 
arima(ar2, order = c(2, 0, 0))

ma1  = arima.sim(list(order=c(0,0,1), ma=.8), n=500)
arima(ma1, order = c(0, 0, 1))

ma2  = arima.sim(list(order=c(0,0,2), ma=c(.9,.8)), n=500)
arima(ma2, order = c(0, 0, 2))

arma11 = arima.sim(list(order=c(1,0,1), ar=.9, ma=-.5), n=500)
arima(arma11, order = c(1, 0, 1))


