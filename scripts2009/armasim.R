allplots <- function(x,y) 
{
ts.plot(ts,main=y)
acf(ts,main="ACF")
pacf(ts,main="PACF")
}

par(mfrow=c(3,1))

ts <- rnorm(1000) 
allplots(ts,"White Noise")

ts <- arima.sim(1000,model=list(ar=.9)) 
allplots(ts,"AR(1) phi=.9")

ts <- arima.sim(1000,model=list(ar=c(1,-.24))) 
allplots(ts,"AR(2) phi_1=1 phi_2=-0.24")

ts <- arima.sim(1000,model=list(ma=.9)) 
allplots(ts,"MA(1) theta=.9")

ts <- arima.sim(1000,model=list(ar=.9,ma=.7)) 
allplots(ts,"ARMA(1,1) phi=.9 theta=.7")

fit <- arima(ts,order = c(1,0,1))
fit

par(mfrow=c(1,1))
tsdiag(fit)
