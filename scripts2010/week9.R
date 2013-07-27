
beer <- read.csv("beer.csv")
weather <- read.csv("weather.csv")

plot(beer$prod, xlab="month", ylab="beer", type="l", col=4, lwd=2)
plot(weather$temp, xlab="day", ylab="temp", type="l", col=2, lwd=2)
plot(rnorm(200), xlab="t", ylab="Y_t", type="l", col=6, lwd=2)

plot(weather$temp[1:58], weather$temp[2:59], pch=20, col=4,
     main="Daily Temp at O'Hare", xlab="temp(t-1)", ylab = "temp(t)")
text(x=5,y=45, col=2, cex=1.5,
     labels=paste("Corr =", round(cor(weather$temp[1:58],weather$temp[2:59]),2)))
par(mfrow=c(1,2))
plot(weather$temp[1:57], weather$temp[3:59], pch=20, col=4,
     main="", xlab="temp(t-2)", ylab = "temp(t)")
text(x=5,y=45, col=2, 
     labels=paste("Lag 2 Corr =", round(cor(weather$temp[1:57],weather$temp[3:59]),2)))
plot(weather$temp[1:56], weather$temp[4:59], pch=20, col=4,
     main="", xlab="temp(t-3)", ylab = "temp(t)")
text(x=5,y=45, col=2, 
     labels=paste("Lag 3 Corr =", round(cor(weather$temp[1:56],weather$temp[4:59]),2)))

print(acf(weather$temp))
acf(beer$prod, lag.max=30)
acf(rnorm(40), lag.max=40)

summary( tempreg <- lm( weather$temp[2:59] ~ weather$temp[1:58] ) )
par(mfrow=c(1,1))
acf(tempreg$residuals)
summary( beerreg <- lm( beer$prod[2:72] ~  beer$prod[1:71] ) )
acf(beerreg$residuals, lag.max=30)
summary( beerreg12 <- lm( beer$prod[12:72] ~  beer$prod[1:61] ) )
acf(beerreg12$residuals, lag.max=30)

random.walk <- rnorm(1)
for(i in 2:200){ random.walk <- c(random.walk, random.walk[i-1]+rnorm(1)) }
plot(random.walk, pch=20, col=2)
lines(random.walk, col=4)
acf(random.walk, lwd=2)

dja <- read.csv("dja.csv")$DJ
n <- 1979
plot(dja, type="l", col=4, xlab="day", ylab="DJA")
summary(ARdj <- lm(dja[2:n] ~ dja[1:(n-1)]))
returns <- (dja[2:n]-dja[1:(n-1)])/dja[1:(n-1)]
plot(returns, type="l", col=3, xlab="day", ylab="DJA Return")
summary( lm(returns[2:n] ~ returns[1:(n-1)]) )

exploding.series <- rnorm(1)
for(i in 2:200){ exploding.series <- c(exploding.series, 1.02*exploding.series[i-1]+rnorm(1)) }
plot(exploding.series, pch=20, col=2)
lines(exploding.series, col=4)

stationary.series <- rnorm(1)
for(i in 2:200){ stationary.series <- c(stationary.series, 0.8*stationary.series[i-1]+rnorm(1)) }
plot(stationary.series, pch=20, col=2)
lines(stationary.series, col=4)
abline(h=0, lty=2, col=8)
acf(stationary.series, lwd=2)

negcor.series <- rnorm(1)
for(i in 2:100){ negcor.series <- c(negcor.series, -0.8*negcor.series[i-1]+rnorm(1)) }
plot(negcor.series, pch=20, col=2)
lines(negcor.series, col=4)
abline(h=0, lty=2, col=8)


###### Airline Passenger Numbers ######

airline <- read.csv("airline.csv")
plot(airline$Passengers, xlab="year", ylab="monthly passengers", type="l", col=3, lwd=2, xaxt="n")
axis(1, at=(0:12)*12, labels=1949:1961)
acf(airline$Passengers, lag.max=100)
plot(log(airline$Passengers), xlab="year", ylab="log monthly passengers", type="l", col=4, lwd=2, xaxt="n")
axis(1, at=(0:12)*12, labels=1949:1961)

Y <- airline$Passengers
t <- 2:144
cos12 <- cos(2*pi*t/12)
sin12 <- sin(2*pi*t/12)
logYlast <- log(Y[t-1])
summary(airlm <- lm(log(Y[t]) ~ t +  sin12 + cos12 + logYlast))
# plot predictions
plot(log(airline$Passengers), xlab="year", ylab="log monthly passengers", type="l", col=4, lty=2, xaxt="n", lwd=2)
axis(1, at=(0:12)*12, labels=1949:1961)
pred <- predict(airlm, data.frame(t=t, cos=cos12, sin=sin12, logYlast=logYlast))
lines(t, pred, col=2, lwd=2)
legend("topleft", legend=c("data", "fitted"), lty=c(2,1), col=c(4,2))
# look at the residuals
par(mfrow=c(1,2))
plot(airlm$resid, xlab="year", ylab="residual", type="l", col=4, main="residuals in time",  xaxt="n", lwd=2)
axis(1, at=(0:12)*12, labels=1949:1961)
acf(airlm$resid, lwd=2)
# We find a month effect!
par(mfrow=c(1,1))
boxplot(airlm$resid ~ airline$Month[t], xlab="month", ylab="residuals", col=7)

dec <- airline$Month[t]==12
mar <- airline$Month[t]==3
jan <- airline$Month[t]==1
jun <- airline$Month[t]==6
jul <- airline$Month[t]==7
aug <- airline$Month[t]==8
nov <- airline$Month[t]==11
holidays <- jun|jul|aug|dec|mar

summary(airlm2 <- lm(log(Y[t]) ~ t + logYlast + sin12 + cos12 + holidays + nov + jan + jul))
# plot predictions
plot(log(airline$Passengers), xlab="year", ylab="log monthly passengers", type="l", col=4, lty=2, xaxt="n", lwd=2)
axis(1, at=(0:12)*12, labels=1949:1961)
pred <- predict(airlm2, data.frame(t=t,  cos12=cos12, sin12=sin12, logYlast=logYlast,
                                  jan=jan, jul=jul, nov=nov, holidays=holidays))
lines(t, pred, col=2, lwd=2)
legend("topleft", legend=c("data", "fitted"), lty=c(2,1), col=c(4,2))
# look at the residuals
par(mfrow=c(1,2))
plot(airlm2$resid, xlab="year", ylab="residual", type="l", col=4, main="residuals in time",  xaxt="n", lwd=2)
axis(1, at=(0:12)*12, labels=1949:1961)
acf(airlm2$resid, lwd=2)
par(mfrow=c(1,1))
boxplot(airlm2$resid ~ airline$Month[t], xlab="month", ylab="residuals", col=7)
