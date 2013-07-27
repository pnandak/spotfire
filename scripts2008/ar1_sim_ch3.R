#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-21-06                                                   #
# PURPOSE: Simulate an AR(1) series                                 #
#                                                                   #
# NOTES:                                                            #
#####################################################################


#####################################################################
# Simulate AR(1) using arima.sim()

  set.seed(7181)
  x<-arima.sim(model = list(ar = c(0.7)), n = 100, rand.gen = rnorm, sd = 10)
  
  win.graph(width = 8, height = 6, pointsize = 10)  #Opens up wider plot window than the default (good for time series plots)
  plot(x = x, ylab = expression(x[t]), xlab = "t", type = "l", col = c("red"),  
        main = expression(paste(x[t] == 0.7*x[t-1] + w[t], " where ", w[t], "~ ind. N(0,100)")) , 
        panel.first=grid(col = "gray", lty = "dotted"))
  points(x = x, pch = 20, col = "blue")


#####################################################################
# Using ARMAacf() and ARMAtoMA() functions.

  round(ARMAacf(ar = c(0.7), lag.max = 20),4)
  plot(y = ARMAacf(ar = c(0.7), lag.max = 20), x = 0:20, type = "h", ylim = c(-1,1), xlab = "h", ylab = expression(rho(h)),
       main = expression(paste("ACF for AR(1) with ", phi1[1] == 0.7)))
  abline(h = 0)

  round(ARMAacf(ar = c(-0.7), lag.max = 20),4) 
  plot(y = ARMAacf(ar = c(-0.7), lag.max = 20), x = 0:20, type = "h", ylim = c(-1,1), xlab = "h", ylab = expression(rho(h)),
       main = expression(paste("ACF for AR(1) with ", phi1[1] == -0.7)))
  abline(h = 0)

  round(ARMAtoMA(ar = c(0.7), lag.max = 20),4)

  #Example for AR(2)
  round(ARMAtoMA(ar = c(0.7, -0.4), lag.max = 20),4)

###################################################################
# PACF

  round(ARMAacf(ar = c(0.7), lag.max = 20, pacf = TRUE),4)
  plot(x = ARMAacf(ar = c(0.7), lag.max = 20, pacf = TRUE), type = "h", ylim = c(-1,1), xlab = "h", ylab = expression(phi1[hh]),
       main = expression(paste("PACF for AR(1) with ", phi1[1] == 0.7)))
  abline(h = 0)
