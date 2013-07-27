#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  1-8-02, 12-23-05, 12-6-06                                  #
# Purpose: Examine the logistic distribution                        #
#                                                                   #
# NOTES:                                                            #
#####################################################################

mu<--2
sigma<-2

curve(expr = 1/sigma * exp(-(x-mu)/sigma) /(1+exp(-(x-mu)/sigma))^2, ylab = "f(x)", xlab = "x", from = -15, to = 15, lwd = 2, 
      main = expression(paste("Logistic PDF with ", mu==-2, " and ",sigma==2)), col = "red", panel.first = grid(col = "gray", lty = "dotted"))
#Note that expr = dlogis(x, location=mu, scale=sigma) could also be used 
abline(h = 0)

curve(expr = 1/(1+exp(-(x-mu)/sigma)), ylab = "F(x)", xlab = "x", from = -15, to = 15, lwd = 2, 
      main = expression(paste("Logistic CDF with ", mu==-2, " and ",sigma==2)), col = "red", 
      panel.first = grid(col = "gray", lty = "dotted"))
#Note that expr = plogis(x, location=mu, scale=sigma) could also be used 
