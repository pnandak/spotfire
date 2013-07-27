# timser  -- For one ticker, downloads from Yahoo! a single time-series of monthly returns
# 		 spanning a given number of years.

timser = function (ticker = "IBM",start_year = 1970,end_year = 1999, returns = TRUE) {
query = sprintf("s=%s&a=00&b=1&c=%d&d=00&e=10&f=%d&g=m&ignore=.csv",ticker,start_year,end_year+1)
download.file(paste("http://ichart.finance.yahoo.com/table.csv?",query,sep=""),"tempfile")
raw = read.table("tempfile",sep=",",skip=1)
unlink("tempfile")

dat = as.Date(raw[,1]) #,format = "%d-%b-%y")
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
ts(rets,start=c(styea,stmon),end=c(enyea,enmon),freq=12,names=ticker)
}

# crossec -- For a set of tickers, downloads from Yahoo! a multivariate time-series of monthly returns
# 		 spanning a given number of years.

crossec = function (tickers = c("IBM","GE"),start_year = 1970,end_year = 1999, returns = TRUE) {
lre = lapply(tickers,function (tick) timser(tick,start_year,end_year,returns));
tsm = lre[[1]]
for (i in 2:length(lre)) tsm = ts.intersect(tsm,lre[[i]])
colnames(tsm)=tickers
tsm
}

index_components = function (ticker = "DJI") {
query = sprintf("s=@^%s&f=sl1d1t1c1ohgv&e=.csv",ticker);
download.file(paste("http://finance.yahoo.com/d/quotes.csv?",query,sep=""),destfile="tempfile");
dj = as.vector(read.table("tempfile",sep=",")[,1]);
}

styear = 1980
enyear = 2005
#tickers= casefold(c("^gspc","c","ge","gm","ibm","ko","pg"),upper=T)
tickers = c("^GSPC",index_components("DJI"))
spx    = "^GSPC"
rets   = crossec(tickers,styear,enyear,ret=T)
rates  = timser("^IRX",styear,enyear,ret=F)
exrets = ((rets+100)/(100+rates/12)-1)*100
colnames(exrets) = colnames(rets)

nass   = length(tickers)-1
assets = tickers[2:(nass+1)]
len    = length(exrets[,1])

regs   = lapply(assets,function(name) lm(exrets[,name] ~ exrets[,spx]))
coereg = sapply(1:nass, function(i) coef(regs[[i]]))
alphas = coereg[1,]
betas  = coereg[2,]
expret = colMeans(exrets[,2:(nass+1)])
plot(betas,expret)
#beeret = rbind(betas,expret)
crei = lm(expret ~ betas)
abline(crei)
cren = lm(expret ~ betas - 1)
abline(cren)

nobs  = length(exrets[,1])
creg  = lapply(1:nobs, function(i) lm(exrets[i,2:(nass+1)] ~ betas - 1))
lambda= sapply(1:nobs, function(i) coef(creg[[i]])[1])
mean(lambda)
sd(lambda)/nobs
