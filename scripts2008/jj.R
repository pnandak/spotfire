#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  12-20-06                                                   #
# PURPOSE: Example 1.1 data on p. 4 (Johnson and Johnson quarterly  #
#          earnings per share)                                      #
#                                                                   #
# NOTES:                                                            #
#####################################################################

jj<-read.table(file = "C:\\chris\\UNL\\STAT_time_series\\Shumway_Stoffer_web_info\\Data\\jj.dat", 
               header=FALSE, col.names = "x")
head(jj)
x<-jj$x


win.graph(width = 8, height = 6, pointsize = 10)  #Opens up wider plot window than the default (good for time series plots)
plot(x = x, ylab = expression(x[t]), xlab = "t", type = "l", col = "red", lwd = 1 , 
     main = "Johnson and Johnson quarterly earnings per share", 
     panel.first=grid(col = "gray", lty = "dotted"))
points(x = x, pch = 20, col = "blue")


plot(x = log(x), ylab = expression(log(x[t])), xlab = "t", type = "l", col = "red", lwd = 1 , 
     main = "Johnson and Johnson quarterly earnings per share - log transformed", 
     panel.first=grid(col = "gray", lty = "dotted"))
points(x = log(x), pch = 20, col = "blue")


#Still some variance issues???
plot(x = diff(x = log(x), lag = 1, differences = 1), ylab = expression(log(x[t]) - log(x[t-1])), xlab = "t", type = "l", col = "red", lwd = 1 , 
     main = "Johnson and Johnson quarterly earnings per share - log transformed and 1st diff.", 
     panel.first=grid(col = "gray", lty = "dotted"))
points(x = diff(x = log(x), lag = 1, differences = 1), pch = 20, col = "blue")


#############################################################
#How could one do a Box-Cox transformation here?

library(MASS) #boxcox() function is in this package

#Optimally, one would like to use boxcox() with arima() function fit, but boxcox() will not work with it. 
mod.fit<-arima(x = x, order = c(1, 0, 0), include.mean = TRUE)
save.bc<-boxcox(object = mod.fit, lambda = seq(from = -2, to = 2, by = 0.01))
names(save.bc)
lambda.hat<-save.bc$x[save.bc$y == max(save.bc$y)] 
lambda.hat


#Could try using with a regression model.  Probably would want to use "t" as a explanatory variable
#  that removes any nonstationary in the mean.  
#mod.fit<-lm(formula = x ~ 1)
jj.t<-1:length(x)
mod.fit<-lm(formula = x ~ jj.t)

save.bc<-boxcox(object = mod.fit, lambda = seq(from = -2, to = 2, by = 0.01))
title(main = "Box-Cox transformation plot")  
names(save.bc)
lambda.hat<-save.bc$x[save.bc$y == max(save.bc$y)] 
lambda.hat


#Could write your own code using a profile likelihood method (this is what boxcox() does with a regression model fit)
#  Thus, maximumize the likelihood with an additional lambda parameter and new y.t (after fitting ARIMA model 
#  of interest - treat ARIMA model estimates as constants).











#
