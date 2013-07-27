#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-31-06                                                   #
# PURPOSE: Chapter 3 OSU data                                       #
#                                                                   #
# NOTES:                                                            #
#####################################################################


#####################################################################
# Read in data

  library(RODBC)
  z<-odbcConnectExcel("C:\\chris\\UNL\\STAT_time_series\\chapter1\\osu_enroll.xls")
  osu.enroll<-sqlFetch(z, "Sheet1")
  close(z)

  head(osu.enroll)
  tail(osu.enroll)

  #Suppose it was early spring 2002 and you wanted to forecast fall 2002
  #  so use data only up to spring 2002
  x<-osu.enroll$Enrollment[1:38]  
  
#######################################################################
# ACF and PACF plots

  #ACF and PACF of x_t
  win.graph(width = 8, height = 6, pointsize = 10)
  par(mfcol = c(1,2))
  acf(x = x, type = "correlation", ylim = c(-1,1), lag.max = 20, xlim = c(1,20), main = expression(paste("Estimated ACF plot for ", x[t])))
  pacf(x = x, lag.max = 20, ylim = c(-1,1), xlab = "h", main = expression(paste("Estimated PACF plot for ", x[t])))

  #ACF and PACF of (1-B^3)*x_t
  par(mfcol = c(1,2))
  acf(x = diff(x = x, lag = 3, differences = 1), type = "correlation", lag.max = 20, xlim = c(1,20),
      ylim = c(-1,1), main = expression(paste("Est. ACF for ", (1-B^3)*x[t], " data")))
  pacf(x = diff(x = x, lag = 3, differences = 1), lag.max = 20, ylim = c(-1,1), xlab = "h", 
       main = expression(paste("Est. PACF for ", (1-B^3)*x[t], " data")))

  #Plot of (1-B^3)*x_t 
  win.graph(width = 8, height = 6, pointsize = 10)  
  par(mfrow = c(1,1))
  plot(x = diff(x = x, lag = 3, differences = 1), ylab = expression((1-B^3)*x[t]), xlab = "t", type = "l", col = "red",  
        main =  expression(paste("Plot of ", (1-B^3)*x[t])), panel.first=grid(col = "gray", lty = "dotted"))
  points(x = diff(x = x, lag = 3, differences = 1), pch = 20, col = "blue")


  #ACF and PACF of (1-B)*(1-B^3)*x_t
  par(mfrow = c(1,2))
  x2<-diff(x = diff(x = x, lag = 3, differences = 1), lag = 1, differences = 1)
  acf(x = x2, type = "correlation", lag.max = 20, xlim = c(1,20),
      ylim = c(-1,1), main = expression(paste("Est. ACF for ", (1-B)*(1-B^3)*x[t], " data")))
  pacf(x = x2, lag.max = 20, ylim = c(-1,1), xlab = "h", 
       main = expression(paste("Est. PACF for ", (1-B)*(1-B^3)*x[t], " data")))


  #Plot of (1-B)*(1-B^3)*x_t 
  win.graph(width = 8, height = 6, pointsize = 10)  
  par(mfrow = c(1,1))
  plot(x = x2, xlab = "t", type = "l", col = "red", main = expression(paste("Plot of ", (1-B)*(1-B^3)*x[t])), 
      ylab = expression((1-B)*(1-B^3)*x[t]), panel.first=grid(col = "gray", lty = "dotted"))
  points(x = x2, pch = 20, col = "blue")



  #Plot of (1-B^3)^2*x_t 
  win.graph(width = 8, height = 6, pointsize = 10)  
  par(mfrow = c(1,1))
  plot(x = diff(x = x, lag = 3, differences = 2), ylab = expression((1-B^3)^2*x[t]), xlab = "t", type = "l", col = "red",  
        main =  expression(paste("Plot of ", (1-B^3)^2*x[t])), panel.first=grid(col = "gray", lty = "dotted"))
  points(x = diff(x = x, lag = 3, differences = 2), pch = 20, col = "blue")

  #ACF and PACF of (1-B^3)^2*x_t
  par(mfrow = c(1,2))
  x2<-diff(x = diff(x = x, lag = 3, differences = 2), lag = 1, differences = 1)
  acf(x = x2, type = "correlation", lag.max = 20, xlim = c(1,20),
      ylim = c(-1,1), main = expression(paste("Est. ACF for ", (1-B^3)^2*x[t], " data")))
  pacf(x = x2, lag.max = 20, ylim = c(-1,1), xlab = "h", 
       main = expression(paste("Est. PACF for ", (1-B^3)^2*x[t], " data")))



#####################################################################
# Fit models and examine diagnostics - Start with ARIMA(1,0,0)x(0,1,0)_3

  mod.fit.100.010<-arima(x = x, order = c(1, 0, 0), seasonal = list(order = c(0, 1, 0), period = 3))
  mod.fit.100.010
  #Need to run function in examine.mod.R file first
  examine.mod(mod.fit.obj = mod.fit.100.010, mod.name = "ARIMA(1,0,0)x(0,1,0)_3", max.lag = 20)




#####################################################################
# Other models invesigated

  mod.fit<-arima(x = x, order = c(0, 0, 0), seasonal = list(order = c(0, 1, 1), period = 3))
  mod.fit
  examine.mod(mod.fit.obj = mod.fit, mod.name = "ARIMA(0,0,0)x(0,1,1)_3", max.lag = 20)

  mod.fit<-arima(x = x, order = c(1, 0, 0), seasonal = list(order = c(0, 1, 1), period = 3))
  mod.fit
  examine.mod(mod.fit.obj = mod.fit, mod.name = "ARIMA(1,0,0)x(0,1,1)_3", max.lag = 20)

  mod.fit<-arima(x = x, order = c(0, 0, 1), seasonal = list(order = c(0, 1, 1), period = 3))
  mod.fit
  examine.mod(mod.fit.obj = mod.fit, mod.name = "ARIMA(0,0,1)x(0,1,1)_3", max.lag = 20)



  mod.fit<-arima(x = x, order = c(0, 0, 1), seasonal = list(order = c(0, 1, 0), period = 3))
  mod.fit
  examine.mod(mod.fit.obj = mod.fit, mod.name = "ARIMA(0,0,1)x(0,1,0)_3", max.lag = 20)

  mod.fit<-arima(x = x, order = c(1, 0, 1), seasonal = list(order = c(0, 1, 0), period = 3))
  mod.fit
  examine.mod(mod.fit.obj = mod.fit, mod.name = "ARIMA(1,0,1)x(0,1,0)_3", max.lag = 20)



  mod.fit<-arima(x = x, order = c(0, 0, 0), seasonal = list(order = c(1, 1, 0), period = 3))
  mod.fit
  examine.mod(mod.fit.obj = mod.fit, mod.name = "ARIMA(0,0,0)x(1,1,0)_3", max.lag = 20)

  mod.fit<-arima(x = x, order = c(1, 0, 0), seasonal = list(order = c(1, 1, 0), period = 3))
  mod.fit
  examine.mod(mod.fit.obj = mod.fit, mod.name = "ARIMA(1,0,0)x(1,1,0)_3", max.lag = 20)



  mod.fit<-arima(x = x, order = c(0, 0, 0), seasonal = list(order = c(0, 1, 1), period = 3))
  mod.fit
  examine.mod(mod.fit.obj = mod.fit, mod.name = "ARIMA(0,0,0)x(0,1,1)_3", max.lag = 20)

  #Could use fixed = ___ option to avoid the 1st and 2nd seasonal term? http://finzi.psych.upenn.edu/R/Rhelp02a/archive/33891.html
  mod.fit<-arima(x = x, order = c(0, 0, 0), seasonal = list(order = c(0, 1, 3), period = 3))
  mod.fit
  examine.mod(mod.fit.obj = mod.fit, mod.name = "ARIMA(0,0,0)x(0,1,3)_3", max.lag = 20)

  mod.fit<-arima(x = x, order = c(1, 0, 0), seasonal = list(order = c(0, 1, 3), period = 3))
  mod.fit
  examine.mod(mod.fit.obj = mod.fit, mod.name = "ARIMA(1,0,0)x(0,1,3)_3", max.lag = 20)

  mod.fit<-arima(x = x, order = c(0, 0, 1), seasonal = list(order = c(0, 1, 3), period = 3))
  mod.fit
  examine.mod(mod.fit.obj = mod.fit, mod.name = "ARIMA(0,0,1)x(0,1,3)_3", max.lag = 20)





  mod.fit<-arima(x = x, order = c(0, 0, 0), seasonal = list(order = c(1, 2, 0), period = 3))
  mod.fit
  examine.mod(mod.fit.obj = mod.fit, mod.name = "ARIMA(0,0,0)x(1,2,0)_3", max.lag = 20)

  mod.fit<-arima(x = x, order = c(0, 0, 0), seasonal = list(order = c(0, 2, 1), period = 3))
  mod.fit
  examine.mod(mod.fit.obj = mod.fit, mod.name = "ARIMA(0,0,0)x(0,2,1)_3", max.lag = 20)

  mod.fit<-arima(x = x, order = c(1, 0, 0), seasonal = list(order = c(0, 2, 0), period = 3))
  mod.fit
  examine.mod(mod.fit.obj = mod.fit, mod.name = "ARIMA(1,0,0)x(0,2,0)_3", max.lag = 20)



####################################################################
# Start with (1-B)*(1-B^3)*x_t

  mod.fit<-arima(x = x, order = c(0, 1, 0), seasonal = list(order = c(0, 1, 0), period = 3))
  mod.fit
  examine.mod(mod.fit.obj = mod.fit, mod.name = "ARIMA(0,1,0)x(0,1,0)_3", max.lag = 20)

  mod.fit<-arima(x = x, order = c(0, 1, 0), seasonal = list(order = c(1, 1, 0), period = 3))
  mod.fit
  examine.mod(mod.fit.obj = mod.fit, mod.name = "ARIMA(0,1,0)x(1,1,0)_3", max.lag = 20)


  mod.fit<-arima(x = x, order = c(0, 1, 0), seasonal = list(order = c(0, 1, 1), period = 3))
  mod.fit
  examine.mod(mod.fit.obj = mod.fit, mod.name = "ARIMA(0,1,0)x(0,1,1)_3", max.lag = 20)


  mod.fit<-arima(x = x, order = c(1, 1, 0), seasonal = list(order = c(0, 1, 0), period = 3))
  mod.fit
  examine.mod(mod.fit.obj = mod.fit, mod.name = "ARIMA(1,1,0)x(0,1,0)_3", max.lag = 20)


  mod.fit<-arima(x = x, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 0), period = 3))
  mod.fit
  examine.mod(mod.fit.obj = mod.fit, mod.name = "ARIMA(0,1,1)x(0,1,0)_3", max.lag = 20)



################################################################################
#  Forecasts using ARIMA(1,0,0)x(0,1,0)_3

  #Forecasts 6 time periods into the future
  fore.mod<-predict(object = mod.fit.100.010, n.ahead = 6, se.fit = TRUE) 
  fore.mod
  
  pred.mod<-x - mod.fit.100.010$residuals 

  #Calculate 95% C.I.s 
  low<-fore.mod$pred - qnorm(p = 0.975, mean = 0, sd = 1)*fore.mod$se
  up<-fore.mod$pred + qnorm(p = 0.975, mean = 0, sd = 1)*fore.mod$se
  data.frame(low, up)
  


  win.graph(width = 8, height = 8, pointsize = 10)
  #par(mar =c(BELOW,LEFT,ABOVE,RIGHT)) #Space in margin
  par(mfrow = c(3,1), mar=c(0,5,1.5,1.5))
  plot(y = osu.enroll[osu.enroll$Semester == "Fall" & osu.enroll$t < 39,]$Enrollment, 
      x = osu.enroll[osu.enroll$Semester == "Fall" & osu.enroll$t < 39,]$t, 
      xlab = "t (time)", col = "red", main = "OSU Enrollment", xlim = c(1,44), ylab = "", las = 2, 
      pch = 1, type = "o", ylim = c(17000,24000), xaxt = "n")
  lines(y = c(pred.mod[seq(from = 1, to = 37, by = 3)], fore.mod$pred[c(2,5)]), x = seq(from = 1, to = 44, by = 3),   
        lwd = 1, col = "black", type = "o", pch = 17) 
  lines(y = low[c(2,5)], x = c(40,43), lwd = 1, col = "darkgreen", lty = "dashed") 
  lines(y = up[c(2,5)], x = c(40,43), lwd = 1, col = "darkgreen", lty = "dashed") 
  points(y = osu.enroll$Enrollment[40], x = 40, col = "blue", pch = 2) 
  abline(v = seq(from = 1, to = 44, by = 3), lty = "dotted", col = "lightgray")
  abline(h = seq(from = 17000, to = 23000, by = 2000), lty = "dotted", col = "lightgray")
  text(x = 1, y = 17500, labels = "Fall Semester", pos = 4, cex = 2)
  legend(x = 1, y = 23000, legend = c("Observed", "Forecast", "95% C.I.", "Actual future values"), 
         lty = c("solid", "solid", "dashed", NA), col = c("red", "black", "darkgreen", "blue"), pch = c(20, 17, NA, 2), bty = "n")

  par(mar=c(0,5,0.5,1.5))
  plot(y = osu.enroll[osu.enroll$Semester == "Spring" & osu.enroll$t < 39,]$Enrollment, 
       x = osu.enroll[osu.enroll$Semester == "Spring" & osu.enroll$t < 39,]$t, 
       xlab = "t (time)", col = "red", xlim = c(1,44), las = 2, ylab = "",
       pch = 1, type = "o", ylim = c(17000,24000), xaxt = "n")
  lines(y = c(pred.mod[seq(from = 2, to = 38, by = 3)], fore.mod$pred[c(3,6)]), x = seq(from = 2, to = 45, by = 3),   
        lwd = 1, col = "black", type = "o", pch = 17) 
  #axis(side = 1, at = seq(from = 1, to = 42, by = 3)) 
  lines(y = low[c(3,6)], x = c(41,44), lwd = 1, col = "darkgreen", lty = "dashed") 
  lines(y = up[c(3,6)], x = c(41,44), lwd = 1, col = "darkgreen", lty = "dashed") 
  abline(v = seq(from = 1, to = 44, by = 3), lty = "dotted", col = "lightgray")
  abline(h = seq(from = 17000, to = 23000, by = 2000), lty = "dotted", col = "lightgray")
  text(x = 1, y = 17500, labels = "Spring Semester", pos = 4, cex = 2)
  legend(x = 1, y = 23000, legend = c("Observed", "Forecast", "95% C.I.", "Actual future values"), 
         lty = c("solid", "solid", "dashed", NA), col = c("red", "black", "darkgreen", "blue"), pch = c(20, 17, NA, 2), bty = "n")

  par(mar=c(4,5,0.5,1.5))
  plot(y = osu.enroll[osu.enroll$Semester == "Summer" & osu.enroll$t < 39,]$Enrollment, 
       x = osu.enroll[osu.enroll$Semester == "Summer" & osu.enroll$t < 39,]$t, 
       xlab = "t (time)", col = "red", xlim = c(1,44), pch = 1, type = "o", ylab = "", 
       ylim = c(6000,10000), xaxt = "n", las = 2)
  lines(y = c(pred.mod[seq(from = 3, to = 36, by = 3)], fore.mod$pred[c(1,4)]), x = seq(from = 3, to = 42, by = 3),   
        lwd = 1, col = "black", type = "o", pch = 17) 
  axis(side = 1, at = seq(from = 1, to = 45, by = 3)) 
  lines(y = low[c(1,4)], x = c(39,42), lwd = 1, col = "darkgreen", lty = "dashed") 
  lines(y = up[c(1,4)], x = c(39,42), lwd = 1, col = "darkgreen", lty = "dashed") 
  points(y = osu.enroll$Enrollment[39], x = 39, col = "blue", pch = 2) 
  abline(v = seq(from = 1, to = 44, by = 3), lty = "dotted", col = "lightgray")
  abline(h = seq(from = 6000, to = 10000, by = 1000), lty = "dotted", col = "lightgray")
  text(x = 1, y = 6200, labels = "Summer Semester", pos = 4, cex = 2)
  legend(x = 1, y = 10000, legend = c("Observed", "Forecast", "95% C.I.", "Actual future values"), 
         lty = c("solid", "solid", "dashed", NA), col = c("red", "black", "darkgreen", "blue"), pch = c(20, 17, NA, 2), bty = "n")
   
   
   
##################################################################################
# Forecasts for fall 1998, fall 1999, fall 2000, fall 2001 using the 
#  ARIMA(1,0,0)x(0,1,0)_3 model with data only up to the previous spring semester

  mod.fit.100.010.1998<-arima(x = x[1:26], order = c(1, 0, 0), seasonal = list(order = c(0, 1, 0), period = 3))
  mod.fit.100.010.1998
  fore.mod<-predict(object = mod.fit.100.010.1998, n.ahead = 2, se.fit = TRUE) 
  low<-fore.mod$pred - qnorm(p = 0.975, mean = 0, sd = 1)*fore.mod$se
  up<-fore.mod$pred + qnorm(p = 0.975, mean = 0, sd = 1)*fore.mod$se
  data.frame(low, up)

  mod.fit.100.010.1999<-arima(x = x[1:29], order = c(1, 0, 0), seasonal = list(order = c(0, 1, 0), period = 3))
  mod.fit.100.010.1999
  fore.mod<-predict(object = mod.fit.100.010.1999, n.ahead = 2, se.fit = TRUE) 
  low<-fore.mod$pred - qnorm(p = 0.975, mean = 0, sd = 1)*fore.mod$se
  up<-fore.mod$pred + qnorm(p = 0.975, mean = 0, sd = 1)*fore.mod$se
  data.frame(low, up)

  mod.fit.100.010.2000<-arima(x = x[1:32], order = c(1, 0, 0), seasonal = list(order = c(0, 1, 0), period = 3))
  mod.fit.100.010.2000
  fore.mod<-predict(object = mod.fit.100.010.2000, n.ahead = 2, se.fit = TRUE) 
  low<-fore.mod$pred - qnorm(p = 0.975, mean = 0, sd = 1)*fore.mod$se
  up<-fore.mod$pred + qnorm(p = 0.975, mean = 0, sd = 1)*fore.mod$se
  data.frame(low, up)

  mod.fit.100.010.2001<-arima(x = x[1:35], order = c(1, 0, 0), seasonal = list(order = c(0, 1, 0), period = 3))
  mod.fit.100.010.2001
  fore.mod<-predict(object = mod.fit.100.010.2001, n.ahead = 2, se.fit = TRUE) 
  low<-fore.mod$pred - qnorm(p = 0.975, mean = 0, sd = 1)*fore.mod$se
  up<-fore.mod$pred + qnorm(p = 0.975, mean = 0, sd = 1)*fore.mod$se
  data.frame(low, up)
