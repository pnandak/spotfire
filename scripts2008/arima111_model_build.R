######################################################################
# NAME:  Chris Bilder                                                #
# DATE:  12-28-06                                                    #
# PURPOSE: Go through the model building process with ARIMA(1,1,1)   #
#          data in AR1.0.7.txt                                       #
#                                                                    #
# NOTES:                                                             #
######################################################################

  library(RODBC)
  z<-odbcConnectExcel("C:\\chris\\UNL\\STAT_time_series\\chapter3\\arima111.xls")
  arima111<-sqlFetch(z, "Sheet1")
  close(z)

  head(arima111)
  x<-arima111$x

#####################################################################
# Step #1 and #2

  #Plot of the data
  win.graph(width = 8, height = 6, pointsize = 10)  
  par(mfrow = c(1,1))
  plot(x = x, ylab = expression(x[t]), xlab = "t", type = "l", col = c("red"),  
        main = "Plot of the arima111.xls data", panel.first=grid(col = "gray", lty = "dotted"))
  points(x = x, pch = 20, col = "blue")

  #ACF and PACF of x_t
  par(mfrow = c(1,2))
  acf(x = x, type = "correlation", lag.max = 20, xlim = c(1,20), ylim = c(-1,1), main = "Estimated ACF of the arima111.xls data")
  pacf(x = x, lag.max = 20, xlim = c(1,20), ylim = c(-1,1), xlab = "h", main = "Estimated PACF of the arima111.xls data")
  par(mfrow = c(1,1))

  #Examine the first differences
  plot(x = diff(x = x, lag = 1, differences = 1), ylab = expression(x[t]-x[t-1]), xlab = "t", type = "l", col = c("red"),  
        main = "Plot of the 1st differences for arima111.xls data", panel.first=grid(col = "gray", lty = "dotted"))
  points(x = diff(x = x, lag = 1, differences = 1), pch = 20, col = "blue")

  #ACF and PACF of x_t - x_t-1
  par(mfrow = c(1,2))
  acf(x = diff(x = x, lag = 1, differences = 1), type = "correlation", lag.max = 20, xlim = c(1,20), 
      ylim = c(-1,1), main = "Est. ACF 1st diff. for arima111.xls data")
  pacf(x = diff(x = x, lag = 1, differences = 1), lag.max = 20, xlim = c(1,20), ylim = c(-1,1), xlab = "h", 
       main = "Est. PACF 1st diff. for arima111.xls data")
  par(mfrow = c(1,1))


  
#####################################################################
# Step #3 and #4

 #ARIMA(1,1,1)
  mod.fit111<-arima(x = x, order = c(1, 1, 1))
  mod.fit111
  #Need to run function in examine.mod.R file first
  save.it<-examine.mod(mod.fit.obj = mod.fit111, mod.name = "ARIMA(1,1,1)", max.lag = 20)
  save.it

 
 #ARIMA(2,1,0)
  mod.fit210<-arima(x = x, order = c(2, 1, 0))
  mod.fit210
  save.it<-examine.mod(mod.fit.obj = mod.fit210, mod.name = "ARIMA(2,1,0)", max.lag = 20)
  save.it

 #ARIMA(3,1,0)
  mod.fit310<-arima(x = x, order = c(3, 1, 0))
  mod.fit310
  examine.mod(mod.fit.obj = mod.fit310, mod.name = "ARIMA(3,1,0)", max.lag = 20)
  

 #ARIMA(1,1,0)
  mod.fit110<-arima(x = x, order = c(1, 1, 0))
  mod.fit110
  examine.mod(mod.fit.obj = mod.fit110, mod.name = "ARIMA(1,1,0)", max.lag = 20)
  
 
 #ARIMA(0,1,1)
  mod.fit011<-arima(x = x, order = c(0, 1, 1))
  mod.fit011
  examine.mod(mod.fit.obj = mod.fit011, mod.name = "ARIMA(0,1,1)", max.lag = 20)

 
 
 #ARIMA(2,1,1)
  mod.fit211<-arima(x = x, order = c(2, 1, 1))
  mod.fit211
  examine.mod(mod.fit.obj = mod.fit211, mod.name = "ARIMA(2,1,1)", max.lag = 20)

 
 
###############################################################################
# Step #5

  data.frame(mod.name = c("ARIMA(1,1,1)", "ARIMA(2,1,0)", "ARIMA(3,1,0)", "ARIMA(1,1,0)", "ARIMA(0,1,1)", "ARIMA(2,1,1)"),
             AIC = c(mod.fit111$aic, mod.fit210$aic, mod.fit310$aic, mod.fit110$aic, mod.fit011$aic, mod.fit211$aic))
           
           
###############################################################################
# What if included the delta term in the model?

 
 #ARIMA(1,1,1)
  mod.fit111.delta<-arima(x = x, order = c(1, 1, 1), xreg = 1:length(x))
  mod.fit111.delta
  examine.mod(mod.fit.obj = mod.fit111.delta, mod.name = "ARIMA(1,1,1) with delta", max.lag = 20)
  
 #ARIMA(2,1,1)
  mod.fit211.delta<-arima(x = x, order = c(2, 1, 1), xreg = 1:length(x))
  mod.fit211.delta
  examine.mod(mod.fit.obj = mod.fit211.delta, mod.name = "ARIMA(2,1,1) with delta", max.lag = 20)
  
 #ARIMA(3,1,0)
  mod.fit310.delta<-arima(x = x, order = c(3, 1, 0), xreg = 1:length(x))
  mod.fit310.delta
  examine.mod(mod.fit.obj = mod.fit310.delta, mod.name = "ARIMA(3,1,0) with delta", max.lag = 20)
  












#
