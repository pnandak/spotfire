############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  1-15-07                                                           #
# UPDATE:                                                                  #
# PURPOSE: GARCH example from p. 256 of Pena, Tiao, and Tsay               #
#                                                                          #
# NOTES: See p. 7 of book for data description                             #
#        Monthly returns from 1926-1991                                    #
############################################################################


sp500<-read.table(file = "C:\\chris\\UNL\\STAT_time_series\\chapter5\\sp500.dat",
        header = FALSE, col.names = "x", sep = "")
head(sp500)
#x<-ts(sp500$x, start = 1926, deltat=1/12)
#x
x<-sp500$x


#############################################################################
# Figures 9.3 and 9.4 with additional plots

  win.graph(width = 8, height = 6, pointsize = 10)  
  plot(x = x, ylab = expression(x[t]), xlab = "t", type = "l", col = "red",  
       main = "S&P 500 data series")
  points(x = x, pch = 20, col = "blue")
  grid()

  par(mfrow = c(2,2))
  acf(x = x, type = "correlation", lag.max = 20, xlab = "h", main = expression(paste("Estimated ACF for ", x[t])))
  pacf(x = x, lag.max = 20, xlab = "h", main = expression(paste("Estimated PACF for ", x[t])))
  acf(x = x^2, type = "correlation", lag.max = 20, xlab = "h", main = expression(paste("Estimated ACF for ", x[t]^2)))
  pacf(x = x^2, lag.max = 20, xlab = "h", main = expression(paste("Estimated PACF for ", x[t]^2)))
  par(mfrow = c(1,1))


#############################################################################
#Page 256 of PTT say that an ARCH(9) model is needed.
#  Notice - book does not say anything about a model is needed for x first

  library(tseries)
  
  mod.fit<-garch(x = x, order = c(0,9))   
  summary(mod.fit)                     
  plot(mod.fit)     
  
  #PACF of residuals
  pacf(x = mod.fit$residuals[-(1:9)], lag.max = 20, ylim = c(-1,1), xlim = c(1,20), xlab = "h", main = "Estimated PACF for residuals")
  

#############################################################################
# AR(3) model and plots

  mod.fit.ar3<-arima(x = x, order = c(3, 0, 0), include.mean = TRUE)
  mod.fit.ar3
  y<-mod.fit.ar3$residuals 
  examine.mod(mod.fit.obj = mod.fit.ar3, mod.name = "ARIMA(3,0,0)")

  par(mfrow = c(2,2))
  acf(x = y, type = "correlation", lag.max = 20, xlab = "h", main = expression(paste("Estimated ACF for ", y[t])))
  pacf(x = y, lag.max = 20, xlab = "h", main = expression(paste("Estimated PACF for ", y[t])))
  acf(x = y^2, type = "correlation", lag.max = 20, xlab = "h", main = expression(paste("Estimated ACF for ", y[t]^2)))
  pacf(x = y^2, lag.max = 20, xlab = "h", main = expression(paste("Estimated PACF for ", y[t]^2)))
  par(mfrow = c(1,1))


#############################################################################
# Fit GARCH (1,1) to ARMA(3,0) residuals

  mod.fit.garch<-garch(x = y, order = c(1,1))   
  summary(mod.fit.garch)                     
  plot(mod.fit.garch)                        
  pacf(x = mod.fit.garch$residuals[-1], lag.max = 20, ylim = c(-1,1), xlim = c(1,20), xlab = "h", main = "Estimated PACF for GARCH(1,1) residuals")


#############################################################################
# Fit GARCH (1,1) to observed data

  mean(x)
  y.adj<-x - mean(x)
  mod.fit.garch2<-garch(x = y.adj, order = c(1,1))   
  summary(mod.fit.garch2)                     
  plot(mod.fit.garch2)                        
  pacf(x = mod.fit.garch2$residuals[-1], lag.max = 20, ylim = c(-1,1), xlim = c(1,20), xlab = "h", main = "Estimated PACF for GARCH(1,1) residuals")


#############################################################################
# Here is one way R can fit all parts of the model simultaneously using the garchFit()
#  function.  Unfortunately, I can not get this code to work!  See p. 33 of the Wurtz, 
#  Chalabi, and Luksan (2007) paper at http://www.itp.phys.ethz.ch/econophysics/R/pdf/garch.pdf 
#  for where it did work.  

  library(fSeries)
  #garchFit(~arma(0,1)+~aparch(1,1), data = x)
  #garchFit(formula.mean = ~ arma(0,1), formula.var = ~ aparch(1,1), data = x)
   
   









#
