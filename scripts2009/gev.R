library(evir)
z.vals = seq(-5, 5, length=200)
cdf.f = ifelse((z.vals > -2), pgev(z.vals,xi=0.5), 0)
cdf.w = ifelse((z.vals < 2), pgev(z.vals,xi=-0.5), 1)
cdf.g = exp(-exp(-z.vals))
pdf.f = ifelse((z.vals > -2), dgev(z.vals,xi=0.5), 0)
pdf.w = ifelse((z.vals < 2), dgev(z.vals,xi=-0.5), 0)
pdf.g = exp(-exp(-z.vals))*exp(-z.vals)
plot(z.vals, pdf.f, type="l", xlab="z", ylab="h(z)")
lines(z.vals, pdf.g, type="l", lty=2)
lines(z.vals, pdf.w, type="l", lty=3)
legend(-5.25, 0.4, legend=c("Frechet (xi=0.5)","Gumbel (xi=0)","Weibull (xi=-0.5)"), lty=1:3)

spd <- dl.price("^gspc",1960,2005,"d")
rets = data.frame(row.names = row.names(spd)[2:length(spd[,1])], Ret =(exp(diff(log(spd[,1])))-1)*100)
dateplot(rets)
months = format(as.Date(rownames(rets)),"%Y-%m")
monmax = aggregate(-rets[,1],list(mon=months),FUN=max)
tsmm = ts(monmax[,2],start=c(1960,1),freq=12)
plot(tsmm,main="Worst Daily Return")
fit <- gev(tsmm)
fit
#plot(fit)
pars <- fit$par.ests
qgev(c(1-1/10/12,1-1/20/12,1-1/100/12), xi = pars["xi"], sigma = pars["sigma"], mu = pars["mu"])
rlev <- rlevel.gev(fit,12*100)
rlev


setwd("C:\\Documents and Settings\\Paolo\\My Documents\\web\\teach\\ma593fal07\\r")
shil = ts(data=read.table("shiller.txt",header=T),freq=12,start=c(1871,1))
rets = (exp(diff(log(shil[,"P"])))-1)*100
rets = window(rets,c(1872,1),c(1986,12))
plot(rets)
monmax = aggregate(-rets,nf=1,FUN=max)
tsmm = ts(monmax,start=1872)
plot(tsmm,main="Worst Annual Return")
fit <- gev(tsmm)
fit
#plot(fit)
pars <- fit$par.ests
qgev(c(1-1/10,1-1/20,1-1/100), xi = pars["xi"], sigma = pars["sigma"], mu = pars["mu"])
rlev <- rlevel.gev(fit,100)
rlev


sim <- rnorm(20000)
meplot(sim)
sim <- rexp(20000)
meplot(sim)
sim <- abs(rcauchy(20000))
meplot(sim)

dret <- rets
length(dret)
meplot(dret)
meplot(-dret)

shape(-dret,end=100)

fit <- gpd(-dret,threshold=9)
fit
#plot(fit)

riskmeasures(fit,c(1-1/(12*10),1-1/(12*20),1-1/(12*100)))

fit <- hill(-as.vector(dret), option="xi",start=15,end=1000,ci=.99)
fit
