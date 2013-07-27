library(foreign)
library(tseries)

sim.ar <- arima.sim(list(ar=c(.6)),n=1000)
sim2.ar <- arima.sim(list(ar=c(.6,-.4)),n=1000)

par(mfrow=c(2,2))
acf(sim.ar,main="ACF of AR(1) Process")
pacf(sim.ar,main="PACF of AR(1) Process")
acf(sim2.ar,main="ACF of AR(2) Process")
pacf(sim2.ar,main="PACF of AR(2) Process")



sim.ma <- arima.sim(list(ma=c(.6)),n=1000)
sim2.ma <- arima.sim(list(ma=c(.6,-.4)),n=1000)

par(mfrow=c(2,2))
acf(sim.ma,main="ACF of MA(1) Process")
pacf(sim.ma,main="PACF of MA(1) Process")
acf(sim2.ma,main="ACF of MA(2) Process")
pacf(sim2.ma,main="PACF of MA(2) Process")



sim.arma <- arima.sim(list(ar=c(.6),ma=c(.6)),n=1000)
sim2.arma <- arima.sim(list(ar=c(.6,-.6),ma=c(.6,.4)),n=1000)

par(mfrow=c(2,2))
acf(sim.arma,main="ACF of ARMA(1,1) Process")
pacf(sim.arma,main="PACF of ARMA(1,1) Process")
acf(sim2.arma,main="ACF of ARMA(2,2) Process")
pacf(sim2.arma,main="PACF of ARMA(2,2) Process")



sim.ar.int <- arima.sim(list(order = c(1,1,0), ar = 0.7), n = 1000)
sim.dar.int <- diff(sim.ar.int)

par(mfrow=c(2,1))
acf(sim.ar.int,main="ACF of an Integrated Process of Order 1")
acf(sim.dar.int,main="ACF of an Integrated Process of Order 1: First Difference")

mod.ar1 <- arima(sim.dar.int,order=c(1,0,0))
mod.ar1
tsdiag(mod.ar1)

mod2.ar1 <- arima(sim.ar.int,order=c(1,1,0))
mod2.ar1
tsdiag(mod2.ar1)

mod3.ma1 <- arima(sim.ar.int,order=c(0,1,1))
mod3.ma1
tsdiag(mod3.ma1)
