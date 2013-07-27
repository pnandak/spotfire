# dl.price  -- For one ticker, downloads from Yahoo! a single time-series of monthly returns
# 		   spanning a given number of years.

dl.price = function (ticker = "IBM",start_year = 2001,end_year = 2005, frequency="d") {
query = sprintf("s=%s&a=00&b=1&c=%d&d=11&e=31&f=%d&g=%s&ignore=.csv",ticker,start_year,end_year,frequency)
download.file(paste("http://ichart.finance.yahoo.com/table.csv?",query,sep=""),"tempfile")
raw = read.table("tempfile",sep=",",skip=1)
unlink("tempfile")

dates = as.Date(raw[,1])#
close = as.numeric(as.vector(raw[,7]))

data.frame(row.names = rev(dates),Close = rev(close))
}

dateplot = function (x) {
plot(as.Date(row.names(x)),x[,1],type="l",ylab=names(x))
}

library(tseries)

garchex = function (n = 1000, the=.2, phi=.5, sig = 0.1) {
n = n+100
e = rnorm(n)  
x = double(n)
s = double(n)
x[1] = rnorm(1, sd = sig/sqrt(1.0-the-phi)) 
for(i in 2:n) { 
  s[i] = sqrt(sig^2+the*x[i-1]^2+phi*s[i-1]^2)
  x[i] = e[i]*s[i]
  }
x[101:n]
}

lre = garchex(10000)

spx = dl.price("^GSPC",start=1988,end=2006,freq="d")
lre = diff(log(spx[,1]))*100

par(mfrow=c(2,1))
acf(lre)
acf(lre^2)

fit = garch(lre,order=c(1,1))
summary(fit)
plot(fit)

pred = predict(fit)[,1]
rna = as.Date(row.names(spx)[2:(length(lre)+1)])

par(mfrow=c(2,1))
plot(rna,pred,type="l")
plot(rna,lre,type="l")

par(mfrow=c(1,1))
plot(rna,2*pred,type="l")
lines(rna,lre,type="l",col=2)


