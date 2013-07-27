#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-21-06                                                   #
# PURPOSE: Simulate an MA(1) series                                 #
#                                                                   #
# NOTES:                                                            #
#####################################################################


#####################################################################
# Simulate MA(1) using arima.sim()

  set.seed(8199)
  x<-arima.sim(model = list(ma = c(0.7)), n = 100, rand.gen = rnorm, sd = 10)
  
  win.graph(width = 8, height = 6, pointsize = 10)  #Opens up wider plot window than the default (good for time series plots)
  plot(x = x, ylab = expression(x[t]), xlab = "t", type = "l", col = c("red"), 
        main = expression(paste(x[t] == w[t] + 0.7*w[t-1], " where ", w[t], " ~ ind. N(0,100)")) , 
        panel.first=grid(col = "gray", lty = "dotted"))
  points(x = x, pch = 20, col = "blue")


  par(mfrow = c(1,2))
  round(ARMAacf(ma = c(0.7), lag.max = 20),4)
  plot(y = ARMAacf(ma = c(0.7), lag.max = 20), x = 0:20, type = "h", ylim = c(-1,1), xlab = "h", ylab = expression(rho(h)),
       main = expression(paste("ACF for MA(1) with ", theta[1] == 0.7)))
  abline(h = 0)

  round(ARMAacf(ma = c(-0.7), lag.max = 20),4) 
  plot(y = ARMAacf(ma = c(-0.7), lag.max = 20), x = 0:20, type = "h", ylim = c(-1,1), xlab = "h", ylab = expression(rho(h)),
       main = expression(paste("ACF for MA(1) with ", theta[1] == -0.7)))
  abline(h = 0)

  round(ARMAtoMA(ma = c(0.7), lag.max = 5),4)

  #Example for MA(2)
  round(ARMAtoMA(ma = c(0.7, -0.4), lag.max = 5),4)


###################################################################
# PACF

  par(mfrow = c(1,2))
  round(ARMAacf(ma = c(0.7), lag.max = 20, pacf = TRUE),4)
  plot(x = ARMAacf(ma = c(0.7), lag.max = 20, pacf = TRUE), type = "h", ylim = c(-1,1), xlab = "h", ylab = expression(phi[hh]),
       main = expression(paste("PACF for MA(1) with ", theta[1] == 0.7)))
  abline(h = 0)

  round(ARMAacf(ma = c(-0.7), lag.max = 20, pacf = TRUE),4) 
  plot(x = ARMAacf(ma = c(-0.7), lag.max = 20, pacf = TRUE), type = "h", ylim = c(-1,1), xlab = "h", ylab = expression(phi[hh]),
       main = expression(paste("PACF for MA(1) with ", theta[1] == -0.7)))
  abline(h = 0)













#
