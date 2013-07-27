######################################################################
# NAME:  Chris Bilder                                                #
# DATE:  12-29-06                                                    #
# PURPOSE: Go through the model building process with                #
#          ARIMA(1,1,1)x(0,1,1)_12 data in sarima.011.011.12.xls     #
#                                                                    #
# NOTES:                                                             #
######################################################################

#####################################################################
# Actual ACF and PACF for ARIMA(1,0,1)x(0,0,1)_12 with theta1 = -0.4
#   and Theta1 = -0.6.

  win.graph(width = 8, height = 6, pointsize = 10)  
  par(mfrow = c(1,2))
  plot(y = ARMAacf(ma = c(-0.4, rep(x = 0, times = 10), -0.6, 0.24), lag.max = 50, pacf = FALSE), x = 0:50, type = "h", ylim = c(-1,1), 
       xlab = "h", ylab = expression(rho(h)), main = "True ACF for ARIMA(1,0,1)x(0,0,1)_12")
  abline(h = 0)
  plot(x = ARMAacf(ma = c(-0.4, rep(x = 0, times = 10), -0.6, 0.24), lag.max = 50, pacf = TRUE), type = "h", ylim = c(-1,1), xlab = "h", ylab = expression(phi1[hh]),
       main = "True PACF for ARIMA(1,0,1)x(0,0,1)_12")
  abline(h = 0)
  

#####################################################################
# Read in data

  library(RODBC)
  z<-odbcConnectExcel("C:\\chris\\UNL\\STAT_time_series\\chapter3\\sarima011.011.12.xls")
  sarima.data<-sqlFetch(z, "Sheet1")
  close(z)

  head(sarima.data)
  x<-sarima.data$x[1:200]

#####################################################################
# Plots of data

  #Plot of the data
  win.graph(width = 8, height = 6, pointsize = 10)  
  par(mfrow = c(1,1))
  plot(x = x, ylab = expression(x[t]), xlab = "t", type = "l", col = "red",  
        main =  "Data simulated from ARIMA(0,1,1)x(0,1,1)_12",
        panel.first=grid(col = "gray", lty = "dotted"))
  points(x = x, pch = 20, col = "blue")


  #Put gridlines at every 12th time point on the x-axis  
  #  The xaxt = "n" option prevents the x-axis tick marks from being drawn 
  plot(x = x, ylab = expression(x[t]), xlab = "t", type = "l", col = "red", xaxt = "n", yaxt = "n",
        main =  "Data simulated from ARIMA(0,1,1)x(0,1,1)_12")
  points(x = x, pch = 20, col = "blue")
  #x-axis 
  #  Major and minor tick marks - see intro. to R examples
  axis(side = 1, at = seq(from = 0, to = 192, by = 12)) 
  axis(side = 1, at = seq(from = 0, to = 200, by = 2), tck = 0.01, labels = FALSE) 
  abline(v = seq(from = 0, to = 192, by = 12), lty = "dotted", col = "lightgray")
  #y-axis 
  axis(side = 2, at = seq(from = -150, to = -350, by = -50)) 
  abline(h = seq(from = -350, to = -50, by = 50), lty = "dotted", col = "lightgray")


#####################################################################
# ACF and PACF plots

  #ACF and PACF of x_t
  win.graph(width = 8, height = 6, pointsize = 10)
  par(mfcol = c(1,2))
  acf(x = x, type = "correlation", lag.max = 50, ylim = c(-1,1), main = expression(paste("Estimated ACF plot for ", x[t])))
  pacf(x = x, lag.max = 50, ylim = c(-1,1), xlab = "h", main = expression(paste("Estimated PACF plot for ", x[t])))


  #ACF and PACF of (1-B)*x_t
  par(mfrow = c(1,2))
  acf(x = diff(x = x, lag = 1, differences = 1), type = "correlation", lag.max = 50, xlim = c(1,50), 
      ylim = c(-1,1), main = expression(paste("Est. ACF for ", (1-B)*x[t], " data")))
  pacf(x = diff(x = x, lag = 1, differences = 1), lag.max = 50, xlim = c(1,50), ylim = c(-1,1), xlab = "h", 
       main = expression(paste("Est. PACF for ", (1-B)*x[t], " data")))
  par(mfrow = c(1,1))


  #ACF and PACF of (1-B)(1-B^12)*x_t
  par(mfrow = c(1,2))
  x2<-diff(x = diff(x = x, lag = 12, differences = 1), lag = 1, differences = 1)
  acf(x = x2, type = "correlation", lag.max = 50, xlim = c(1,50), 
      ylim = c(-1,1), main = expression(paste("Est. ACF for ", (1-B)*(1-B^12)*x[t], " data")),
      panel.first=grid(col = "gray", lty = "dotted"))
  pacf(x = x2, lag.max = 50, xlim = c(1,50), ylim = c(-1,1), xlab = "h", 
       main = expression(paste("Est. PACF for ", (1-B)*(1-B^12)*x[t], " data")), panel.first=grid(col = "gray", lty = "dotted"))
  par(mfrow = c(1,1))
  
  
  #Plot of the differenced data  
  plot(x = x2, ylab = expression((1-B)*(1-B^12)*x[t]), xlab = "t", type = "l", col = "red", xaxt = "n", 
        main =  expression(paste("Plot of ", (1-B)*(1-B^12)*x[t])))
  points(x = x2, pch = 20, col = "blue")
  axis(side = 1, at = seq(from = 0, to = 192, by = 12)) 
  axis(side = 1, at = seq(from = 0, to = 200, by = 2), tck = 0.01, labels = FALSE) 
  abline(v = seq(from = 0, to = 192, by = 12), lty = "dotted", col = "lightgray")
  abline(h = seq(from = -20, to = 10, by = 10), lty = "dotted", col = "lightgray")


  #ACF and PACF of (1-B^12)*x_t
  par(mfrow = c(1,2))
  acf(x = diff(x = x, lag = 12, differences = 1), type = "correlation", lag.max = 50, xlim = c(1,50), 
      ylim = c(-1,1), main = expression(paste("Est. ACF for ", (1-B^12)*x[t], " data")))
  pacf(x = diff(x = x, lag = 12, differences = 1), lag.max = 50, xlim = c(1,50), ylim = c(-1,1), xlab = "h", 
       main = expression(paste("Est. PACF for ", (1-B^12)*x[t], " data")))
  par(mfrow = c(1,1))

  
#####################################################################
# Fit models and examine diagnostics

  #ARIMA(0,1,0)x(0,1,1)_12
  mod.fit.010.011<-arima(x = x, order = c(0, 1, 0), seasonal = list(order = c(0, 1, 1), period = 12))
  mod.fit.010.011
  #Need to run function in examine.mod.R file first
  examine.mod(mod.fit.obj = mod.fit.010.011, mod.name = "ARIMA(0,1,0)x(0,1,1)_12", max.lag = 50)


  #ARIMA(0,1,1)x(0,1,1)_12
  mod.fit.011.011<-arima(x = x, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))
  mod.fit.011.011
  examine.mod(mod.fit.obj = mod.fit.011.011, mod.name = "ARIMA(0,1,1)x(0,1,1)_12", max.lag = 50)
 
  #ARIMA(0,1,1)x(0,1,1)_12 with constant term
  mod.fit.011.011.delta<-arima(x = x, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12),
    xreg = 1:length(x))
  mod.fit.011.011.delta 
  examine.mod(mod.fit.obj = mod.fit.011.011.delta, mod.name = "ARIMA(0,1,1)x(0,1,1)_12 with delta", max.lag = 50)

#####################################################################
# Forecasting

  #Forecasts 24 time periods into the future
  fore.mod<-predict(object = mod.fit.011.011, n.ahead = 24, se.fit = TRUE) 
  fore.mod
  
  pred.mod<-x - mod.fit.011.011$residuals 

  #Calculate 95% C.I.s 
  low<-fore.mod$pred - qnorm(p = 0.975, mean = 0, sd = 1)*fore.mod$se
  up<-fore.mod$pred + qnorm(p = 0.975, mean = 0, sd = 1)*fore.mod$se
  data.frame(low, up)
  
  #When I originally simulated the data, 224 observations were actually simulated.  
  #  I did this so that I could examine how well the confidence intervals captured the true future values
  x.extra<-sarima.data$x[201:224]
  
  #Plot of forecasts, C.I.s, and true future values.
  win.graph(width = 8, height = 6, pointsize = 10)  
  plot(x = x, ylab = expression(x[t]), xlab = "t", type = "o", col = "red", xaxt = "n", ylim = c(-400, -150), pch = 20,
       xlim = c(0, 224), main =  "Forecast plot")
  axis(side = 1, at = seq(from = 0, to = 224, by = 12), las = 2) 
  axis(side = 1, at = seq(from = 0, to = 224, by = 2), tck = 0.01, labels = FALSE) 
  abline(v = seq(from = 0, to = 224, by = 12), lty = "dotted", col = "lightgray")
  abline(h = seq(from = -400, to = -150, by = 50), lty = "dotted", col = "lightgray")
   
  #Predicted values for t = 1, ..., 200 and forecasts for t = 201, ..., 224
  lines(x = c(pred.mod, fore.mod$pred), lwd = 1, col = "black", type = "o", pch = 17) 
  
  #Forecasts and C.I.s
  lines(y = low, x = 201:224, lwd = 1, col = "darkgreen", lty = "dashed") 
  lines(y = up, x = 201:224, lwd = 1, col = "darkgreen", lty = "dashed") 
  
  #Actual future values
  lines(y = x.extra, x = 201:224, lwd = 1, col = "blue", lty = "solid", type = "o", pch = 20) 
  
  #Legend
  legend(locator(1), legend = c("Observed", "Forecast", "95% C.I.", "Actual future values"), 
         lty = c("solid", "solid", "dashed", "solid"),
         col = c("red", "black", "darkgreen", "blue"), pch = c(20, 17, NA, 20), bty = "n")

 #################
 #Zoom in
   
  #Plot of forecasts, C.I.s, and true future values.
  win.graph(width = 8, height = 6, pointsize = 10)  
  plot(x = x, ylab = expression(x[t]), xlab = "t", type = "o", col = "red", xaxt = "n", pch = 20, ylim = c(-400, -250),
       xlim = c(190, 224), main =  "Forecast plot")
  #These are the same axis() and abline() functions as done in the previous plot
  #  R will still line up everything correctly with what is given in the current plot()
  axis(side = 1, at = seq(from = 0, to = 224, by = 12)) 
  axis(side = 1, at = seq(from = 0, to = 224, by = 2), tck = 0.01, labels = FALSE) 
  abline(v = seq(from = 0, to = 224, by = 12), lty = "dotted", col = "lightgray")
  abline(h = seq(from = -400, to = -150, by = 50), lty = "dotted", col = "lightgray")
   
  #Predicted values for t = 1, ..., 200 and forecasts for t = 201, ..., 224
  lines(x = c(pred.mod, fore.mod$pred), lwd = 1, col = "black", type = "o", pch = 17) 
  
  #Forecasts and C.I.s
  lines(y = low, x = 201:224, lwd = 1, col = "darkgreen", lty = "dashed") 
  lines(y = up, x = 201:224, lwd = 1, col = "darkgreen", lty = "dashed") 
  
  #Actual future values
  lines(y = x.extra, x = 201:224, lwd = 1, col = "blue", lty = "solid", type = "o", pch = 20) 
  
  #Legend
  legend(locator(1), legend = c("Observed", "Forecast", "95% C.I.", "Actual future values"), 
         lty = c("solid", "solid", "dashed", "solid"),
         col = c("red", "black", "darkgreen", "blue"), pch = c(20, 17, NA, 20), bty = "n")
 



########################################################################
# Example showing how NOT to simulate SARIMA data

  #Code simulates some observations, but what model??? 
  set.seed(1890)
  x<-arima.sim(model = list(list(order = c(0,1,1), ma = -0.4), list(order = c(0,1,1), ma = -0.6, period = 12)),
               n = 200, rand.gen = rnorm, sd = 5)
  x[1:10] #Notice that x_1 is missing

  #Plot of the data
  win.graph(width = 8, height = 6, pointsize = 10)  
  par(mfrow = c(1,1))
  plot(x = x, ylab = expression(x[t]), xlab = "t", type = "l", col = c("red"),  
        main =  "Data simulated from ARIMA(0,1,1)x(0,1,1)_12",
        panel.first=grid(col = "gray", lty = "dotted"))
  points(x = x, pch = 20, col = "blue")

  #Fit model to data - notice how far off the estimates are from the parameters.  
  arima(x = x, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))
  #arima(x = x, order = c(0, 1, 1))
