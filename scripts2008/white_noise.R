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

  #Using plot.ts() which is set up for time series plots
  win.graph(width = 8, height = 6, pointsize = 10) #Open a new plot window
  plot.ts(x = w, ylab = expression(w[t]), xlab = "t", type = "o", col = "red", 
          main = expression(paste("White noise where ", w[t], " ~ ind. N(0, 1)")), panel.first=grid(col = "gray", lty = "dotted"))

  #Advantage of second plot is separate control over color of points
  win.graph(width = 8, height = 6, pointsize = 10)
  plot(x = w, ylab = expression(w[t]), xlab = "t", type = "l", col = c("red"), 
        main = expression(paste("White noise where ", w[t], " ~ ind. N(0, 1)")), panel.first=grid(col = "gray", lty = "dotted"))
  points(x = w, pch = 20, col = "blue")


###############################################################################
# Simulate a second series

  set.seed(1298)
  w.new<-rnorm(n = 100, mean = 0, sd = 1)
  head(w.new)

  par(mfrow = c(1,1))
  plot(x = w, ylab = expression(w[t]), xlab = "t", type = "l", col = "red", 
        main = expression(paste("White noise where ", w[t], " ~ ind. N(0, 1)")), panel.first=grid(col = "gray", lty = "dotted"))
  points(x = w, pch = 20, col = "blue")
  lines(x = w.new, col = "green")
  points(x = w.new, pch = 20,col = "orange")
  legend(x = locator(1),legend=c("Time series 1", "Time Series 2"), lty=c(1,1), col=c("red", "green"), bty="n")


  win.graph(width = 8, height = 6, pointsize = 10) #Open a new plot window
  par(mfrow = c(2,1))
  plot(x = w, ylab = expression(w[t]), xlab = "t", type = "l", col = c("red"), 
        main = expression(paste("White noise where ", w[t], " ~ ind. N(0, 1)")), panel.first=grid(col = "gray", lty = "dotted"))
  points(x = w, pch = 20, col = "blue")
  plot(x = w.new, ylab = expression(w.new[t]), xlab = "t", type = "l", col = c("green"), 
        main = expression(paste("White noise where ", w[t], " ~ ind. N(0, 1)")), panel.first=grid(col = "gray", lty = "dotted"))
  points(x = w.new, pch = 20, col = "orange")

  ########################
  # What if used plot.ts()?
  win.graph(width = 8, height = 6, pointsize = 10) #Open a new plot window
  plot.ts(x = cbind(w, w.new), ylab = expression(w[t]), xlab = "t", type = "o", col = "red", 
          main = expression(paste("White noise where ", w[t], " ~ ind. N(0, 1)")), panel.first=grid(col = "gray", lty = "dotted"))
  #Problem: gridlines do not extend to second plot
  
  plot.ts(x = cbind(w, w.new), ylab = expression(w[t]), xlab = "t", type = "o", col = "red", 
          main = expression(paste("White noise where ", w[t], " ~ ind. N(0, 1)")))
  grid(col = "gray", lty = "dotted")
  #Problem: gridlines do not appear correctly on plots (could fix by specifying where to draw them using abline)
