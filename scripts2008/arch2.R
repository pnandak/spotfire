############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  1-15-07                                                           #
# UPDATE:                                                                  #
# PURPOSE: Introduction ARCH example from garch function documentation     #
#                                                                          #
# NOTES:                                                                   #
#                                                                          #
############################################################################

#Need to install package first before doing the below
library(fSeries)
set.seed(8111)
y<-garchSim(model = list(omega = 0.1, alpha = c(0.4, 0.2)), n = 10000)

win.graph(width = 8, height = 6, pointsize = 10)  
plot(x = y, ylab = expression(y[t]), xlab = "t", type = "l", col = "red",  
     main = "ARCH(2) model simulated data")
#points(x = y, pch = 20, col = "blue")


par(mfrow = c(2,2))
acf(x = y, type = "correlation", lag.max = 20, xlim = c(1,20), ylim = c(-1,1), xlab = "h",
     main = expression(paste("Estimated ACF for ", y[t])))
pacf(x = y, lag.max = 20, ylim = c(-1,1), xlim = c(1,20), xlab = "h", main = expression(paste("Estimated PACF for ", y[t])))
acf(x = y^2, type = "correlation", lag.max = 20, xlim = c(1,20), ylim = c(-1,1), xlab = "h",
     main = expression(paste("Estimated ACF for ", y[t]^2)))
pacf(x = y^2, lag.max = 20, ylim = c(-1,1), xlim = c(1,20), xlab = "h", main = expression(paste("Estimated PACF for ", y[t]^2)))
par(mfrow = c(1,1))
 
library(tseries)
garch(x = y, order = c(0,2))
