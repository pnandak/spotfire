############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  11-23-02, 1-15-07                                                 #
# UPDATE:                                                                  #
# PURPOSE: ARCH example from p. 285 of Shumway and Stoffer                 #
#                                                                          #
# NOTES:                                                                   #
#                                                                          #
############################################################################


gnp<-read.table("C:\\chris\\UNL\\STAT_time_series\\Shumway_Stoffer_web_info\\Data\\GNP96.dat",
               header = FALSE, col.names = c("time", "x"), sep = "")
head(gnp)
#x<-ts(data=gnp$x, start=1947, deltat=1/4)
win.graph(width = 8, height = 6, pointsize = 10) 
plot(x = x, ylab = expression(x[t]), xlab = "t", type = "l", col = "red",  main = "GNP data")
grid(col = "gray", lty = "dotted") #For unknown reason, grid() inside of plot() does not match up lines with tick marks
#points(x = gnp$x, pch = 20, col = "blue")  #Does not work here


############################################################################
# ARIMA model

  #Shumway and Stoffer did the log transformation and differencing all outside of arima()
  #  In order to try to reproduce their analysis, I did the same.  
  gnpgr<-diff(x = log(gnp$x), lag = 1, differences = 1) 
  mod.fit.ar<-arima(x = gnpgr, order = c(1, 0, 0), include.mean = TRUE)  
  mod.fit.ar
  examine.mod(mod.fit.obj = mod.fit.ar, mod.name = "ARIMA(1,1,0)")
  #Notice ACF and PACF look like white noise mainly, fat tails on Q-Q plot
 
  #Examine ACF and PACF of the squared residuals
  y<-mod.fit.ar$residuals
  par(mfrow = c(1,2))
  acf(x = y^2, type = "correlation", lag.max = 20, xlim = c(1,20), ylim = c(-1,1), xlab = "h",
     main = expression(paste("Estimated ACF for ", y[t]^2)))
  pacf(x = y^2, lag.max = 20, ylim = c(-1,1), xlim = c(1,20), xlab = "h", main = expression(paste("Estimated PACF for ", y[t]^2)))
  par(mfrow = c(1,1))
 
 
 
###########################################################################
# ARCH model

  library(tseries)

  mean(y) #Since mean is so small, I will leave it out

  mod.fit.garch<-garch(x = y, order = c(0,1)) 
  summary(mod.fit.garch)                      
  plot(mod.fit.garch)

  #PACF of residuals
  pacf(x = mod.fit.garch$residuals[-1], lag.max = 20, ylim = c(-1,1), xlim = c(1,20), xlab = "h", main = "Estimated PACF for residuals")
  
  #Q-Q plot - still problems with residuals?
  qqnorm(y = mod.fit.garch$residuals[-1], ylab = "Residuals", panel.first = grid(col = "gray", lty = "dotted"))
  qqline(y = mod.fit.garch$residuals[-1], col = "red")



###########################################################################
# Fit both parts of the model simultaneously - does not work!

  library(fSeries)
  #mod.fit2<-garchFit(formula.mean = ~ arma(1,0), formula.var = ~ aparch(1,0), data = gnpgr)
  #mod.fit2<-garchFit(formula = ~ arma(1,0) + ~aparch(1,0), data = gnpgr)
  #mod.fit2<-garchFit(formula = ~ arma(0,0) + ~aparch(1,0), data = y)
  #mod.fit2<-garchFit(series = y)
  #summary(mod.fit2)
 
 









#
