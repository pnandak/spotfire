# DescriptiveStatisticsDaily.r
#
# author: Eric Zivot
# created: October 11, 2006
# updated: October 14, 2008
#
# R functions used
#
# aggregate
# abline
# align
# diff
# log
# plot 
#
# load packages
library("TSA")
library("tseries")
library("zoo")

# get monthly adjusted closing price data on MSFT and SP500 from Yahoo
# using the tseries function get.hist.quote. Set sample to Jan 1998 through
# Dec 2007. Note: if you are not careful with the start and end dates
# or if you set the retclass to "ts" then results might look weird

msftDailyPrices = get.hist.quote(instrument="msft", start="1998-01-01",
                             end="2007-12-31", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="d", retclass="zoo")
class(msftDailyPrices)
colnames(msftDailyPrices)
start(msftDailyPrices)
end(msftDailyPrices)
colnames(msftDailyPrices) = "MSFT"


sp500DailyPrices = get.hist.quote(instrument="^gspc", start="1998-01-01",
                             end="2007-12-31", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="d", retclass="zoo")
colnames(sp500DailyPrices) = "SP500"

# plot daily prices
par(mfrow=c(2,1))
plot(msftDailyPrices, main="MSFT daily closing price", ylab="price", col="blue")      
plot(sp500DailyPrices, main="SP500 daily closing price", ylab="price", col="blue")
par(mfrow=c(1,1))


#
# compute daily cc returns
#

msftDailyReturns = diff(log(msftDailyPrices))
sp500DailyReturns = diff(log(sp500DailyPrices))
DailyReturns = merge(msftDailyReturns, sp500DailyReturns)
# create matrix data 
msftDailyReturns.mat = coredata(msftDailyReturns)
sp500DailyReturns.mat = coredata(sp500DailyReturns)
DailyReturns.mat = coredata(DailyReturns)

# plot daily returns
plot(DailyReturns)
plot(msftDailyReturns, col="blue", main="Daily cc returns on MSFT")
abline(h=0)
plot(sp500DailyReturns, col="blue", main="Daily cc returns on SP500")
abline(h=0)

apply(DailyReturns.mat, 2, mean)
apply(DailyReturns.mat, 2, sd)
apply(DailyReturns.mat, 2, skewness)
apply(DailyReturns.mat, 2, kurtosis)


#
# histograms with normal density overlayed
#

hist(msftDailyReturns.mat, main="MSFT daily returns with Normal curve",
     probability=T, col="slateblue1")
x.vals = seq(-0.15, 0.15, length=100)
lines(x.vals, dnorm(x.vals, mean=mean(msftDailyReturns.mat), 
                    sd=sd(msftDailyReturns.mat)), col="orange", lwd=2)
hist(sp500DailyReturns.mat, main="Histogram of SP500 daily returns",
     probability=T, col="slateblue1")
x.vals = seq(-0.08, 0.08, length=100)
lines(x.vals, dnorm(x.vals, mean=mean(sp500DailyReturns.mat), 
                    sd=sd(sp500DailyReturns.mat)), col="orange", lwd=2)
     
# put histograms on same graph with same scaling
MSFT.breaks = hist(msftDailyReturns.mat, plot=F)
par(mfrow=c(2,1))
	hist(msftDailyReturns.mat, main="MSFT daily returns", 
           probability=T, col="slateblue1")
	x.vals = seq(-0.15, 0.15, length=100)
	lines(x.vals, dnorm(x.vals, mean=mean(msftDailyReturns.mat), 
      	              sd=sd(msftDailyReturns.mat)), col="orange", lwd=2)
	hist(sp500DailyReturns.mat, main="SP500 daily returns", col="slateblue1",
	     breaks=MSFT.breaks$breaks, probability=T, ylim=c(0,35))
	lines(x.vals, dnorm(x.vals, mean=mean(sp500DailyReturns.mat), 
                    sd=sd(sp500DailyReturns.mat)), col="orange", lwd=2)

par(mfrow=c(1,1))

#
# qq-plots
#

par(mfrow=c(2,1))
qqnorm(msftDailyReturns.mat, sub="MSFT", col="blue")
qqline(msftDailyReturns.mat)

qqnorm(sp500DailyReturns.mat, sub="SP500", col="blue")
qqline(sp500DailyReturns.mat)
par(mfrow=c(1,1))

#
# smoothed density with normal density superimposed
#

# bug in density if timeSeries object is used
msft.density = density(msftDailyReturns.mat, n=100, from=-0.17, to=0.17)
plot(msft.density, type="l", lwd=2, ylab="Density estimates", 
     xlab="Daily returns on Microsoft", main="MSFT")
points(msft.density$x, dnorm(msft.density$x, mean=mean(msftDailyReturns.mat),
       sd=sd(msftDailyReturns.mat)), type="l", lwd=2, col=5)
legend(x="topleft", legend=c("Smoothed density", "Normal density"), lty=c(1,1), 
       col=c(1,5), lwd=c(2,2))       

sp500.density = density(sp500DailyReturns.mat, n=100, from=-0.07, to=0.07)
plot(sp500.density, type="l", lwd=2, ylab="Density estimates", 
     xlab="Daily returns on S&P 500 Index", main="SP500")
points(sp500.density$x, dnorm(sp500.density$x, mean=mean(sp500DailyReturns.mat),
       sd=sd(sp500DailyReturns.mat)), type="l", lwd=2, col=5)
legend(-0.06, 40, legend=c("Smoothed density", "Normal density"), lty=c(1,1), 
       col=c(1,5), lwd=c(2,2))       

#
# time series descriptive statistics
#


set.seed(123)
gwn = rnorm(length(msftDailyReturns.mat), mean=mean(msftDailyReturns.mat),
            sd=sd(msftDailyReturns.mat))
par(mfrow=c(2,1))
	ts.plot(msftDailyReturns.mat, ylab="MSFT", main="MSFT daily returns", 
              col="blue")
	abline(h=0)
	ts.plot(gwn, main="Gaussian data calibrated to MSFT", col="blue") 
	abline(h=0)
par(mfrow=c(1,1))

par(mfrow=c(2,1))                   
tmp = acf(msftDailyReturns.mat, type="correlation")
tmp = acf(sp500DailyReturns.mat, type="correlation")
par(mfrow=c(1,1))

par(mfrow=c(2,1))
	ts.plot(abs(gwn), main="Absolute values of GWN")
	ts.plot(gwn^2, main="Squared values of GWN")
par(mfrow=c(1,1))
par(mfrow=c(2,1))
tmp = acf(abs(gwn), type="correlation")
tmp = acf(gwn^2, type="correlation")
par(mfrow=c(1,1))

par(mfrow=c(2,1))
	plot(abs(msftDailyReturns), main="Daily absolute returns on Microsoft",
           col="blue")
	plot(msftDailyReturns^2, main="Daily squared returns on Microsoft",
           col="blue")
par(mfrow=c(1,1))

par(mfrow=c(2,1))
tmp = acf(abs(msftDailyReturns.mat), type="correlation", main="MSFT absolute returns")
tmp = acf(msftDailyReturns.mat^2, type="correlation", main="MSFT squared returns")
par(mfrow=c(1,1))

par(mfrow=c(2,1))
	plot(abs(sp500DailyReturns), main="Daily absolute returns on S&P 500",
           col="blue")
	plot(sp500DailyReturns^2, main="Daily squared returns on S&P 500",
           col="blue")
par(mfrow=c(1,1))

par(mfrow=c(2,1))
tmp = acf(abs(sp500DailyReturns.mat), type="correlation", main="SP500 absolute returns")
tmp = acf(sp500DailyReturns.mat^2, type="correlation", main="SP500 squared returns")
par(mfrow=c(1,1))
