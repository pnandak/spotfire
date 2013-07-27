setwd("C:/Documents and Settings/Paolo/My Documents/web/teach/ma593fal07/r")
len = 5
stper = c(1871,1)
shil = ts(data=read.table("shiller.txt",header=T),freq=12,start=stper)
loret = ts(data=exp(diff(log(shil[,"P"]),12*len)),freq=12,start=stper)
dpret = ts.intersect((loret-1)*100,shil[,"D"]/shil[,"P"]*100)
colnames(dpret) = c("RET","D/P")
dpret = window(dpret,1947,1986)
dpy = window(dpret,freq=1)
dp = dpret[,"D/P"]
ret = dpret[,"RET"]
par(mfrow=c(1,2))
plot(dpret[,"D/P"],dpret[,"RET"])
plot(dpy[,"D/P"],dpy[,"RET"])
fit = lm(ret ~ dp)
summary(fit)

sigma = diag(length(dp))
#sigma = diag(15)
sigma = sapply((1-abs(row(sigma)-col(sigma))/(len*12)),function (x) max(x,0))
dim(sigma)= c(length(dp),length(dp))
sma = t(chol(sigma))
git = lm(solve(sma,ret) ~ solve(sma,dp))
summary(git)

#git = gls(ret ~ dp, cor = corAR1())
#summary(git)
