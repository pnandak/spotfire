#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-22-06                                                   #
# PURPOSE: Simulate samples from an ARMA(p,q) process               #
#                                                                   #
# NOTES:                                                            #
#####################################################################


  list.ar<-c(0.8);  list.ma<-c(0.5); plot.title<-"AR par = 0.8, MA par 0.5"
  list.ar<-c(0.8);  list.ma<-c(0); plot.title<-"AR par = 0.8"
  list.ar<-c(0);  list.ma<-c(0.5); plot.title<-"MA par 0.5"
  list.ar<-c(0.7, -0.5); list.ma<-c(-0.5); plot.title<-"AR par = 0.7 -0.5, MA par -0.5"
  list.ar<-c(0.7, -0.5, 0.5, -0.8); list.ma<-c(-0.6); plot.title<-"AR par = 0.7 -0.5 0.5 -0.8, MA par -0.6"


  #Simulate the data
  set.seed(1788)
  x<-arima.sim(model = list(ar = list.ar, ma = list.ma), n = 1000, rand.gen = rnorm, sd = 1)
  
  
  #Plot of the data
  win.graph(width = 8, height = 6, pointsize = 10)  
  plot(x = x, ylab = expression(x[t]), xlab = "t", type = "l", col = c("red"),  
        main =  paste("ARMA model:", plot.title), 
        panel.first=grid(col = "gray", lty = "dotted"))
  points(x = x, pch = 20, col = "blue")


 #Estimated ACF and PACF
  win.graph(width = 8, height = 6, pointsize = 10)  
  par(mfcol = c(2,2))
  acf(x = x, type = "correlation", lag.max = 20, ylim = c(-1,1), xlab = "h",
      main = paste("ARMA model:", plot.title))
  abline(h = c(-0.5, 0.5), lty = "dotted", col = "gray")
  pacf(x = x, lag.max = 20, ylim = c(-1,1), xlab = "h",
       main = paste("ARMA model:", plot.title))
  abline(h = c(-0.5, 0.5), lty = "dotted", col = "gray")
  #Note: acf(x = x, type = "partial") also gives the PACF plot
  
  
  #True ACF and PACF
  plot(y = ARMAacf(ar = list.ar, ma = list.ma, lag.max = 20), x = 0:20, type = "h", ylim = c(-1,1), xlab = "h", ylab = expression(rho(h)),
       main = paste("ARMA model:", plot.title))
  abline(h = 0)
  abline(h = c(-0.5, 0.5), lty = "dotted", col = "gray")
  plot(x = ARMAacf(ar = list.ar, ma = list.ma, lag.max = 20, pacf = TRUE), type = "h", ylim = c(-1,1), xlab = "h", ylab = expression(phi1[hh]),
       main = paste("ARMA model:", plot.title))
  abline(h = 0)
  abline(h = c(-0.5, 0.5), lty = "dotted", col = "gray")

#, col = "red", lwd = 2

#
