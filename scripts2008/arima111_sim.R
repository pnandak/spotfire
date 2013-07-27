#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-22-06                                                   #
# PURPOSE: Simulate samples from an ARIMA(1,1,1) process            #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#####################################################################
# Get data

  #Data could be simulated using the following code - notice the use of the order option.  
  #set.seed(6632)
  #x<-arima.sim(model = list(order = c(1,1,1), ar = 0.7, ma = 0.4), n = 200, rand.gen = rnorm, sd = 3)

  #Instead, here data that I had simulated earlier using the same model.  
  library(RODBC)
  z<-odbcConnectExcel("C:\\chris\\UNL\\STAT_time_series\\chapter3\\arima111.xls")
  arima111<-sqlFetch(z, "Sheet1")
  close(z)

  head(arima111)
  tail(arima111)
  x<-arima111$x
  

#####################################################################
# Plots

  #Plot of the data
  win.graph(width = 8, height = 6, pointsize = 10)  
  par(mfrow = c(1,1))
  plot(x = x, ylab = expression(x[t]), xlab = "t", type = "l", col = c("red"),  
        main =  expression(paste("ARIMA model: ", (1 - 0.7*B)*(1-B)*x[t] == (1 + 0.4*B)*w[t])), 
        panel.first=grid(col = "gray", lty = "dotted"))
  points(x = x, pch = 20, col = "blue")

  #ACF and PACF of x_t
  win.graph(width = 8, height = 6, pointsize = 10)
  par(mfcol = c(2,3))
  acf(x = x, type = "correlation", lag.max = 20, ylim = c(-1,1), main = expression(paste("Estimated ACF plot for ", x[t])))
  pacf(x = x, lag.max = 20, ylim = c(-1,1), xlab = "h", main = expression(paste("Estimated PACF plot for ", x[t])))

  #ACF and PACF of first differences
  acf(x = diff(x = x, lag = 1, differences = 1), type = "correlation", lag.max = 20, ylim = c(-1,1), main = expression(paste("Estimated ACF plot for ", x[t] - x[t-1])))
  pacf(x = diff(x = x, lag = 1, differences = 1), lag.max = 20, ylim = c(-1,1), xlab = "h", main = expression(paste("Estimated PACF plot for ", x[t] - x[t-1])))

  #True ACF and PACF for ARIMA(1,0,1) (without differences)
  plot(y = ARMAacf(ar = 0.7, ma = 0.4, lag.max = 20), x = 0:20, type = "h", ylim = c(-1,1), xlab = "h", ylab = expression(rho(h)),
       main = "True ACF for ARIMA(1,0,1)")
  abline(h = 0)
  plot(x = ARMAacf(ar = 0.7, ma = 0.4, lag.max = 20, pacf = TRUE), type = "h", ylim = c(-1,1), xlab = "h", ylab = expression(phi1[hh]),
       main = "True ACF for ARIMA(1,0,1)")
  abline(h = 0)
  
  #Plot of the first differences
  win.graph(width = 8, height = 6, pointsize = 10)  
  par(mfrow = c(1,1))
  plot(x = diff(x = x, lag = 1, differences = 1), ylab = expression(x[t] - x[t-1]), xlab = "t", type = "l", col = c("red"),  
        main =  "Plot of data after first differences", 
        panel.first=grid(col = "gray", lty = "dotted"))
  points(x = diff(x = x, lag = 1, differences = 1), pch = 20, col = "blue")


#########################################################################
#Fit model

  mod.fit<-arima(x = x, order = c(1, 1, 1))
  mod.fit
  
  #Covariance matrix
  mod.fit$var.coef
    
  #Test statistic for Ho: phi1 = 0 vs. Ha: phi1 <> 0
  z<-mod.fit$coef[1]/sqrt(mod.fit$var.coef[1,1])
  z
  2*(1-pnorm(q = z, mean = 0, sd = 1))

  #Test statistic for Ho: theta1 = 0 vs. Ha: theta1 <> 0
  z<-mod.fit$coef[2]/sqrt(mod.fit$var.coef[2,2])
  z
  2*(1-pnorm(q = z, mean = 0, sd = 1))


 

#########################################################################
#Shows how to use the xreg option
arima(x = x, order = c(1, 1, 1), xreg = rep(x = 1, times = length(x)))




#########################################################################
# Forecasts

  #Forecasts 5 time periods into the future
  fore.mod<-predict(object = mod.fit, n.ahead = 5, se.fit = TRUE) 
  fore.mod

  #Calculate 95% C.I.s 
  low<-fore.mod$pred - qnorm(p = 0.975, mean = 0, sd = 1)*fore.mod$se
  up<-fore.mod$pred + qnorm(p = 0.975, mean = 0, sd = 1)*fore.mod$se
  data.frame(low, up)
  
  #x_100
  x[199:200]
  mod.fit$residuals[199:200]


  #Forecasts with C.I.s
  win.graph(width = 8, height = 6, pointsize = 10)  
  plot(x = x, ylab = expression(x[t]), xlab = "t", type = "o", col = "red", lwd = 1, pch = 20,
       main = expression(paste("ARIMA model: ", (1 - 0.7*B)*(1-B)*x[t] == (1 + 0.4*B)*w[t])) , 
       panel.first=grid(col = "gray", lty = "dotted"), xlim = c(1, 205))
  lines(x = c(x - mod.fit$residuals, fore.mod$pred), lwd = 1, col = "black", type = "o", pch = 17) 
  lines(y = low, x = 201:205, lwd = 1, col = "darkgreen", lty = "dashed") 
  lines(y = up, x = 201:205, lwd = 1, col = "darkgreen", lty = "dashed") 
  legend(locator(1), legend = c("Observed", "Forecast", "95% C.I."), lty = c("solid", "solid", "dashed"),
         col = c("red", "black", "darkgreen"), pch = c(20, 17, NA), bty = "n")

  #Zoom in
  win.graph(width = 8, height = 6, pointsize = 10)  
  plot(x = x, ylab = expression(x[t]), xlab = "t", type = "o", col = "red", lwd = 1, pch = 20,
       main = expression(paste("ARIMA model: ", (1 - 0.7*B)*(1-B)*x[t] == (1 + 0.4*B)*w[t])), 
       panel.first=grid(col = "gray", lty = "dotted"), xlim = c(196, 205), ylim = c(-540, -440))
  lines(x = c(x - mod.fit$residuals, fore.mod$pred), lwd = 1, col = "black", type = "o", pch = 17) 
  lines(y = low, x = 201:205, lwd = 1, col = "darkgreen", lty = "dashed") 
  lines(y = up, x = 201:205, lwd = 1, col = "darkgreen", lty = "dashed") 
  legend(locator(1), legend = c("Observed", "Forecast", "95% C.I."), lty = c("solid", "solid", "dashed"),
         col = c("red", "black", "darkgreen"), pch = c(20, 17, NA), bty = "n")

##########################################################################
# m = 10

  fore.mod<-predict(object = mod.fit, n.ahead = 10, se.fit = TRUE) 
  fore.mod

  #Calculate 95% C.I.s 
  low<-fore.mod$pred - qnorm(p = 0.975, mean = 0, sd = 1)*fore.mod$se
  up<-fore.mod$pred + qnorm(p = 0.975, mean = 0, sd = 1)*fore.mod$se
  data.frame(low, up)

  #When I originally simulated the data, 210 observations were actually simulated.  
  #  I did this so that I could examine how well the confidence intervals captured the true future values
  x.extra<-c(-494.85, -506.44, -517.70, -526.64, -529.10, -530.94, -532.52, -537.46, -540.39, -541.43)
  
  #Plot of forecasts, C.I.s, and true future values.
  win.graph(width = 8, height = 6, pointsize = 10)  
  plot(x = x, ylab = expression(x[t]), xlab = "t", type = "o", col = "red", lwd = 1, pch = 20,
       main = expression(paste("ARIMA model: ", (1 - 0.7*B)*(1-B)*x[t] == (1 + 0.4*B)*w[t])), 
       panel.first=grid(col = "gray", lty = "dotted"), xlim = c(196, 210), ylim = c(-580, -400))
  lines(x = c(x - mod.fit$residuals, fore.mod$pred), lwd = 1, col = "black", type = "o", pch = 17) 
  lines(y = low, x = 201:210, lwd = 1, col = "darkgreen", lty = "dashed") 
  lines(y = up, x = 201:210, lwd = 1, col = "darkgreen", lty = "dashed") 
  lines(y = x.extra, x = 201:210, lwd = 1, col = "blue", lty = "solid", type = "o", pch = 20) 
  legend(locator(1), legend = c("Observed", "Forecast", "95% C.I.", "Actual future values"), 
         lty = c("solid", "solid", "dashed", "solid"),
         col = c("red", "black", "darkgreen", "blue"), pch = c(20, 17, NA, 20), bty = "n")


############################################################################
# Investigate Shumway and Stoffer's issues with R at http://www.stat.pitt.edu/stoffer/tsa2/Rissues.htm 

  #Use Shumway and Stoffer's (2006) sarima() and sarima.for() functions
  #  NOTE: You must run their functions first before running the code below.  
  par(mfrow = c(1,2))
  mod.fit.SS<-sarima(data = x, p = 1, d = 1, q = 1)
  mod.fit.SS
  names(mod.fit.SS)
  mod.fit.SS$fit$coef
  mod.fit.SS$fit$var.coef
  mod.fit.SS$fit$coef[3]/sqrt(mod.fit.SS$fit$var.coef[3,3])
  save.for<-sarima.for(xdata = x, nahead = 5, p = 1 , d = 1, q = 1, main = "temp")   
  save.for


  #Using arima() with xreg option
  mod.fit2<-arima(x = x, order = c(1, 1, 1), xreg = 1:length(x))
  mod.fit2

  fore.mod2<-predict(object = mod.fit2, n.ahead = 5, se.fit = TRUE, newxreg = (length(x)+1):(length(x)+5))
  fore.mod2
  
  
  #Do all differencing needed BEFORE invoking arima().  Use d = 0 in arima() and include.mean = TRUE.  
  x.diff<-diff(x = x, lag = 1, differences = 1)
  mod.fit.diff<-arima(x = x.diff, order = c(1, 0, 1))
  mod.fit.diff
  #Model is a little different.  Probably due to R not knowing about the undifferenced series.  
  #  Would need to see the exact likelihood function used in its code to figure it out.  

  fore.mod.diff<-predict(object = mod.fit.diff, n.ahead = 5, se.fit = TRUE)
  fore.mod.diff
  #Problem with doing the above method is the need to get the undifferenced data back
  #  Would need to do "integrating" as given in Section 3.7
  

  

#
