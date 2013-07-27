#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-18-06                                                   #
# PURPOSE: Simulate a white noise series                            #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#####################################################################
# Simulate a white noise series 

  set.seed(8128)
  w<-rnorm(n = 100, mean = 0, sd = 1)
  head(w)

##################################################################################################################
# moving average

  m<-filter(x = w, filter = rep(x = 1/3, times = 3), method = "convolution", sides = 1)
  head(m)
  tail(m)
  (w[1]+w[2]+w[3])/3
  (w[98]+w[99]+w[100])/3
  
  #This is what the book does
  #m<-filter(x = w, filter = rep(x = 1/3, times = 3), method = "convolution", sides = 2)

  win.graph(width = 8, height = 6, pointsize = 10)  #Opens up wider plot window than the default (good for time series plots)
  par(mfrow = c(1,1))
  plot(x = m, ylab = expression(m[t]), xlab = "t", type = "l", col = c("brown"), lwd = 1, 
        main = expression(paste("Moving average where ", m[t] == (w[t] + w[t-1] + w[t-2])/3)), 
        panel.first=grid(col = "gray", lty = "dotted"))
  points(x = m, pch = 20, col = "orange")

  plot(x = m, ylab = expression(paste(m[t], " or ", w[t])), xlab = "t", type = "l", col = c("brown"), lwd = 4, ylim = c(max(w), min(w)), 
        main = expression(paste("Moving average where ", m[t] == (w[t] + w[t-1] + w[t-2])/3)), panel.first=grid(col = "gray", lty = "dotted"))
  points(x = m, pch = 20, col = "orange")
  lines(x = w, col = "red", lty = "dotted")
  points(x = w, pch = 20,col = "blue")
  legend(x = locator(1), legend=c("Moving average", "White noise"), lty=c("solid", "dotted"), col=c("brown", "red"), 
         lwd = c(4,1), bty="n")

  #7-point moving average
  m7<-filter(x = w, filter = rep(x = 1/7, times = 7), method = "convolution", sides = 1)
  plot(x = m, ylab = expression(paste(m[t], " or ", w[t])), xlab = "t", type = "l", col = c("brown"), lwd = 2, ylim = c(max(w), min(w)), 
        main = "Compare moving averages", panel.first=grid(col = "gray", lty = "dotted"))
  points(x = m, pch = 20, col = "orange")
  lines(x = w, col = "red", lty = "dotted")
  points(x = w, pch = 20,col = "blue")
  lines(x = m7, col = "lightgreen", lty = "solid", lwd = 4)
  points(x = m7, pch = 20, col = "darkgreen")
  legend(x = locator(1), legend=c("3-pt Moving average", "White noise", "7-pt Moving average"), 
         lty=c("solid", "dotted", "solid"), col=c("brown", "red", "lightgreen"), 
         lwd = c(2,1,4), bty="n")









#
