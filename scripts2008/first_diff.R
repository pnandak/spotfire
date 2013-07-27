#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-20-06                                                   #
# PURPOSE: First differences and ARIMA(1,1,0) model                 #
#                                                                   #
# NOTES:                                                            #
#####################################################################


#Two ways to simulate observations from this model

####################################################################
# Using for loop

  set.seed(7328)  
  w<-rnorm(n = 200, mean = 0, sd = 1)

  x<-numeric(length = 200)
  x.1<-0
  x.2<-0
  for (i in 1:length(x)) {
    x[i]<-1.7*x.1 - 0.7*x.2 + w[i] 
    x.2<-x.1
    x.1<-x[i] 
  }

  #Do not use first 100
  x<-x[101:200]
  
  win.graph(width = 8, height = 6, pointsize = 10)  #Opens up wider plot window than the default (good for time series plots)
  plot(x = x, ylab = expression(x[t]), xlab = "t", type = "l", col = c("red"), lwd = 1 , 
        main = expression(paste("Data simulated from ", (1-0.7*B)*(1-B)*x[t] == w[t], " where ", w[t], "~N(0,1)")) , 
        panel.first=grid(col = "gray", lty = "dotted"))
  points(x = x, pch = 20, col = "blue")
  
  #Find first differences
  plot(x = diff(x = x, lag = 1, differences = 1), ylab = expression(x[t]-x[t-1]), xlab = "t (time)", type = "l", 
        col = "red", main = expression(paste("1st diff. for data simulated from ", (1-0.7*B)*(1-B)*x[t] == w[t], " where ", w[t], "~N(0,1)")), panel.first=grid(col = "gray", lty = "dotted"))
  points(x = diff(x = x, lag = 1, differences = 1), pch = 20, col = "blue")

                            
####################################################################
# Using for arima.sim() - more on this function in Chapter 3

  #More
  set.seed(4782)
  x<-arima.sim(model = list(order = c(1,1,0), ar = c(0.7)), n = 100, rand.gen = rnorm, sd = 1)
  plot(x = x, ylab = expression(x[t]), xlab = "t", type = "l", col = c("red"), lwd = 1 , 
        main = expression(paste("Data simulated from ", (1-0.7*B)*(1-B)*x[t] == w[t], " where ", w[t], "~N(0,1)")) , 
        panel.first=grid(col = "gray", lty = "dotted"))
  points(x = x, pch = 20, col = "blue")

  #Find first differences
  plot(x = diff(x = x, lag = 1, differences = 1), ylab = expression(x[t]-x[t-1]), xlab = "t (time)", type = "l", 
        col = "red", main = expression(paste("1st diff. for data simulated from ", (1-0.7*B)*(1-B)*x[t] == w[t], " where ", w[t], "~N(0,1)")), panel.first=grid(col = "gray", lty = "dotted"))
  points(x = diff(x = x, lag = 1, differences = 1), pch = 20, col = "blue")


#
