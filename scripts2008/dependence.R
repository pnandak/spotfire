#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-19-06                                                   #
# PURPOSE: Show plots with different amounts of dependence          #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#####################################################################
# Simulate a white noise series 

  set.seed(4599) 
  w<-rnorm(n = 100, mean = 0, sd = 1)
  head(w)

##################################################################################################################
# moving average

  win.graph(width = 8, height = 6, pointsize = 10)  #Opens up wider plot window than the default (good for time series plots)

  #rho = 0.4972376 (see Section 3.2 (p. 91) for why)
  x<-filter(x = w, filter = c(1,0.9), method = "convolution", sides = 1)
  plot(x = x, ylab = expression(x[t]), xlab = "t", type = "l", col = c("red"), lwd = 1 , 
        main = expression(paste(x[t] == w[t] + 0.9*w[t-1], "where w[t] ~ ind. N(0,1)")) , 
        panel.first=grid(col = "gray", lty = "dotted"))
  points(x = x, pch = 20, col = "blue")
  #Pearson correlation between x_t and x_t-1 
  cor(x = x[3:100], y = x[2:99]) 

  
  #rho = 0 (see Section 3.2 (p. 91) for why)
  x<-filter(x = w, filter = c(1,0), method = "convolution", sides = 1)
  plot(x = x, ylab = expression(x[t]), xlab = "t", type = "l", col = c("red"), lwd = 1 , 
        main = expression(paste(x[t] == w[t] + 0*w[t-1], "where w[t] ~ ind. N(0,1)")) , 
        panel.first=grid(col = "gray", lty = "dotted"))
  points(x = x, pch = 20, col = "blue")
  cor(x = x[3:100], y = x[2:99]) 
  cor.test(x = x[3:100], y = x[2:99]) #Not significant 

  
  #rho = -0.4972376 (see Section 3.2 (p. 91) for why)
  x<-filter(x = w, filter = c(1,-0.9), method = "convolution", sides = 1)
  plot(x = x, ylab = expression(x[t]), xlab = "t", type = "l", col = c("red"), lwd = 1 , 
        main = expression(paste(x[t] == w[t] + 0.9*w[t-1], "where w[t] ~ ind. N(0,1)")) , 
        panel.first=grid(col = "gray", lty = "dotted"))
  points(x = x, pch = 20, col = "blue")
  cor(x = x[3:100], y = x[2:99]) 


#
