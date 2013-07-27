# dl.price  -- For one ticker, downloads from Yahoo! a single time-series of monthly returns
# 		   spanning a given number of years.

dl.price = function (ticker = "IBM",start_year = 2001,
	end_year = 2005, frequency="m") {
query = sprintf("s=%s&a=00&b=1&c=%d&d=11&e=31&f=%d&g=%s&ignore=.csv",
	ticker,start_year,end_year,frequency)
url = "http://ichart.finance.yahoo.com/table.csv?";
download.file(paste(url,query,sep=""),"tempfile")
raw = read.table("tempfile",sep=",",skip=1)
unlink("tempfile")
dates = as.Date(raw[,1])
close = as.numeric(as.vector(raw[,7]))
data.frame(row.names = rev(dates),Close = rev(close))
}

dateplot = function (x) {
plot(as.Date(row.names(x)),x[,1],type="l",ylab=names(x))
}

# Jarque-Bera Test of normality

jb.test = function(x) {
x = x-mean(x)
n = length(x)
x = x/sqrt(sum(x^2)/n)
sk = sum(x^3)/n
ku = sum(x^4)/n
stat = n/6*(sk^2+(ku-3)^2/4)
1-pchisq(stat,df=2)
}

# Downloads data

spx = dl.price("mmm",1990,2000,"m")
dateplot(spx)

# Calculates returns and simulates sample of same size

ret = diff(log(spx[,1]))
#ret = sort(ret)[3:length(ret)]
mu = mean(ret)
sigma = sd(ret)
retnor = (ret-mu)/sigma
sim = rnorm(length(ret))

# plots returns of real and simulated samples

par(mfrow=c(2,1))
ts.plot(retnor,ylim=c(-6,6))
ts.plot(sim,ylim=c(-6,6))
title("IID normal sample")

# Q-Q plots for real and simulated samples

par(mfrow=c(1,2))
qqnorm(ret,main="log returns")
qqnorm(sim,main="IID normal sample")

# Plots empirical and hypothetical distribution functions

par(mfrow = c(1,1))
plot(ecdf(retnor),do.points=F)
xval=seq(-4,4,.02)
lines(xval,pnorm(xval))

# Performs tests of normality

shapiro.test(ret)
jb.test(ret)
ks.test(ret,"pnorm")
  
# Plots kernel density estimate

den = density(retnor)
par(mfrow = c(1,1))
plot(den,xlim = c(-5,5))
lines(xval,dnorm(xval),lty=2)

# Performs a small-sample Monte-Carlo study of a test

x = rnorm(10^5)
dim(x) = c(1000,100)
tes = apply(x,1,function (x) shapiro.test(x)$p.value)
plot(ecdf(tes),do.points=F)

# MLE estimation with MASS

require(MASS)

fit = fitdistr(ret,"normal")
fit

# MLE estimation with stats4

require(stats4)

mlogl = function (mu=0, sigma=1, nu=30) 
-sum(dt((ret-mu)/sigma,df=nu,log=T)-log(sigma))

fit = mle(mlogl)
summary(fit)

mlogl = function (mu=0, sigma=1) -sum(dnorm(ret,mean=mu,sd=sigma,log=T))
fit = mle(mlogl)
summary(fit)

