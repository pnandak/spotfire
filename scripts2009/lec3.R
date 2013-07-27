# timser  -- For one ticker, downloads from Yahoo! a single time-series of monthly returns
# 		 spanning a given number of years.

timser = function (ticker = "IBM",start_year = 1970,end_year = 1999, returns = TRUE) {
query = sprintf("s=%s&a=00&b=1&c=%d&d=00&e=10&f=%d&g=m&ignore=.csv",ticker,start_year,end_year+1)
download.file(paste("http://ichart.finance.yahoo.com/table.csv?",query,sep=""),"tempfile")
raw = read.table("tempfile",sep=",",skip=1)
unlink("tempfile")

dat = as.Date(raw[,1])
stdat = dat[length(dat)]
endat = dat[2]
stmon = as.numeric(format(stdat,"%m"))
styea = as.numeric(format(stdat,"%Y"))
enmon = as.numeric(format(endat,"%m"))
enyea = as.numeric(format(endat,"%Y"))

close = as.numeric(rev(as.vector(raw[,7])))
if (returns) {
	rets = (exp(diff(log(close)))-1)*100;
	} else {
	rets = close
	}
tso = ts(rets,start=c(styea,stmon),end=c(enyea,enmon),freq=12,names=ticker);
return(tso)
}

# crossec -- For a set of tickers, downloads from Yahoo! a multivariate time-series of monthly returns
# 		 spanning a given number of years.

crossec = function (tickers = c("IBM","GE"),start_year = 1970,end_year = 1999, returns = TRUE) {
lre = lapply(tickers,function (tick) timser(tick,start_year,end_year,returns));
tsm = lre[[1]]
for (i in 2:length(lre)) tsm = ts.intersect(tsm,lre[[i]])
colnames(tsm)=tickers
return(tsm)
}

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

rets = crossec(c("XOM","^GSPC","^XOI"),1985,2005)
cln = colnames(rets)
rfree = timser("^IRX",1985,2005,ret=F)
rets = rets-rfree/12
colnames(rets)=cln

pairs(rets)

xomspc = lsfit(rets[,2],rets[,1])
ls.print(xomspc)

lims = c(-20,20)
plot(rets[,"^GSPC"],rets[,"XOM"],xlim=lims,ylim=lims)
abline(xomspc)

xomxoi = lsfit(rets[,3],rets[,1])
ls.print(xomxoi)

plot(rets[,"^XOI"],rets[,"XOM"],xlim=lims,ylim=lims)
abline(xomxoi)

all2fit = lsfit(rets[,2:3],rets[,1])
ls.print(all2fit)

res = all2fit$residuals
qqnorm(res)

xomspc = lm(rets[,1] ~ rets[,2])
summary(xomspc)
all2fit = lm(rets[,1] ~ rets[,2] + rets[,3])
summary(all2fit)
anova(xomspc,all2fit)
