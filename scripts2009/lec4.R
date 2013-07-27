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

styear = 1980
enyear = 2000
tickers = casefold(c("^gspc","c","ge","gm","ibm","ko","pg"),upper=T)
spx = "^GSPC"
rets = crossec(tickers,styear,enyear,ret=T)
rates = timser("^IRX",styear,enyear,ret=F)
exrets = ((rets+100)/(100+rates/12)-1)*100
colnames(exrets) = colnames(rets)

pairs(exrets)

#ran = matrix(rnorm(length(tickers)*12*(enyear-styear+1)),c(12*(enyear-styear+1)),length(tickers))
#exrets = ts(ran,start=c(styear,1),end=c(enyear,12),freq=12,names=tickers)

nass = length(tickers)-1
assets = tickers[2:(nass+1)]
len = length(exrets[,1])

regs   = lapply(assets,function(name) lm(exrets[,name] ~ exrets[,spx]))
coereg = sapply(1:nass, function(i) coef(regs[[i]]))
alphas = coereg[1,]
betas  = coereg[2,]
resid  = sapply(1:nass, function(i) resid(regs[[i]]))
resumm = sapply(1:nass, function(i) summary(regs[[i]]))
rsqrd  = sapply(1:nass, function(i) summary(regs[[i]])$r.squared)
stderr = sapply(1:nass, function(i) sqrt(vcov(regs[[i]])[1,1]))
pvals  = as.vector((1-pt(abs(alphas)/stderr,len-1))*2)

colnames(resid) <- assets
pairs(resid)
round(cor(resid),2)

adj   = 1/(1+(mean(exrets[,spx])/sd(exrets[,spx]))^2)
fstat = adj*(len-nass)*len/(nass*(len-1))*alphas %*% solve(var(resid),alphas)
as.numeric(fstat)
1-pf(fstat,nass,len-nass)