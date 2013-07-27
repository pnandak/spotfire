setwd("C:/Documents and Settings/Paolo/My Documents/web/teach/ma593fal07/r")
shil = ts(data=read.table("shiller.txt",header=T),freq=12,start=c(1871,1))
dprat = shil[,"D"]/shil[,"P"]*100
eprat = shil[,"E"]/shil[,"P"]*100
ts.plot(dprat)
subsam = window(dprat,1950,1980)
ts.plot(subsam)
fit = arima(subsam,order=c(1,1,1))
fit
tsdiag(fit)
pre = predict(fit,12*20)
#rbind(pre$pred,pre$se)
ts.plot(dprat,pre$pred,pre$pred-2*pre$se,pre$pred+2*pre$se)

library(tseries)
adf.test(rnorm(100))
adf.test(cumsum(rnorm(100)))
adf.test(dprat)
adf.test(subsam)

