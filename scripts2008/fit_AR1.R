######################################################################
# NAME:  Chris Bilder                                                #
# DATE:  12-24-06                                                    #
# PURPOSE: Fit AR(1) model to data AR1.0.7.txt                       #
#                                                                    #
# NOTES:                                                             #
######################################################################

ar1<-read.table(file = "C:\\chris\\UNL\\STAT_time_series\\chapter1\\AR1.0.7.txt", header=TRUE, sep = "")
head(ar1)
x<-ar1$x

win.graph(width = 8, height = 6, pointsize = 10)  #Opens up wider plot window than the default (good for time series plots)
plot(x = x, ylab = expression(x[t]), xlab = "t", type = "l", col = "red", lwd = 1 , 
     main = expression(paste("Data simulated from AR(1): ", x[t] == 0.7*x[t-1] + w[t], " where ", w[t], "~N(0,1)")) , 
      panel.first=grid(col = "gray", lty = "dotted"))
points(x = x, pch = 20, col = "blue")


######################################################################
# Fit model to data

  mod.fit<-arima(x = x, order = c(1, 0, 0), method = "CSS-ML", include.mean = TRUE)
  #summary(mod.fit)  #Not helpful
  mod.fit  #This is the best summary
  names(mod.fit)

  #Estimated phi1 and mu
  mod.fit$coef
  
  #Estimated sigma^2
  mod.fit$sigma
 
  #Covariance matrix
  mod.fit$var.coef
    
  #Test statistic for Ho: phi1 = 0 vs. Ha: phi1 <> 0
  z<-mod.fit$coef[1]/sqrt(mod.fit$var.coef[1,1])
  z
  
  #p-value
  2*(1-pnorm(q = z, mean = 0, sd = 1))


#########################################################################################

#Shows how to use xreg option
arima(x = x, order = c(1, 0, 0), method = "CSS-ML", include.mean = FALSE, xreg = rep(x = 1, times = length(x)))
  

#########################################################################################
# Forecasting
  
  #Notice class of mod.fit is "Arima".  Therefore, generic functions, like predict, will actually
  #  call predict.Arima().  
  class(mod.fit)

  #Forecasts 5 time periods into the future
  fore.mod<-predict(object = mod.fit, n.ahead = 5, se.fit = TRUE) 
  fore.mod

  #Calculate 95% C.I.s 
  low<-fore.mod$pred - qnorm(p = 0.975, mean = 0, sd = 1)*fore.mod$se
  up<-fore.mod$pred + qnorm(p = 0.975, mean = 0, sd = 1)*fore.mod$se
  data.frame(low, up)
  
  #x_100
  x[100]
  
  
#Finite-history prediction is used, via KalmanForecast. This is only statistically efficient if the MA part of the fit is invertible, so predict.Arima will give a warning for non-invertible MA models. 
#The standard errors of prediction exclude the uncertainty in the estimation of the ARMA model and the regression coefficients. According to Harvey (1993, pp. 58–9) the effect is small. 


##################################################################################
# Plots

  #Residuals
  names(mod.fit)
  mod.fit$residuals

  #Predicted values for t = 1, ..., 100 - remember that residual = observed - predicted
  x - mod.fit$residuals 
  
  
  #Plot of observed and predicted
  win.graph(width = 8, height = 6, pointsize = 10)  
  plot(x = x, ylab = expression(x[t]), xlab = "t", type = "o", col = "red", lwd = 1, pch = 20,
       main = expression(paste("Data simulated from AR(1): ", x[t] == 0.7*x[t-1] + w[t], " where ", w[t], "~N(0,1)")) , 
       panel.first=grid(col = "gray", lty = "dotted"))
  lines(x = x - mod.fit$residuals, lwd = 1, col = "black", type = "o", pch = 17) 
  legend(locator(1), legend = c("Observed", "Forecast"), lty = c("solid", "solid"), col = c("red", "black"), 
         pch = c(20, 17), bty = "n")


  #Plot of observed and predicted - another plot with different colors for points (not sure how to get this on the legend)
  #  Perhaps the previous plot is better due to less colors.  
  plot(x = x, ylab = expression(x[t]), xlab = "t", type = "l", col = "red", lwd = 1 , 
       main = expression(paste("Data simulated from AR(1): ", x[t] == 0.7*x[t-1] + w[t], " where ", w[t], "~N(0,1)")) , 
       panel.first=grid(col = "gray", lty = "dotted"))
  points(x = x, pch = 20, col = "blue")
  lines(x = x - mod.fit$residuals, lwd = 1, col = "black") 
  points(x = x - mod.fit$residuals, pch = 17, col = "green")
  legend(locator(1), legend = c("Observed", "Forecast"), lty = c("solid", "solid"), col = c("red", "black"), 
         pch = c(20, 17))


  #Add the forecasts into the first plot with C.I.s
  win.graph(width = 8, height = 6, pointsize = 10)  
  plot(x = x, ylab = expression(x[t]), xlab = "t", type = "o", col = "red", lwd = 1, pch = 20,
       main = expression(paste("Data simulated from AR(1): ", x[t] == 0.7*x[t-1] + w[t], " where ", w[t], "~N(0,1)")) , 
       panel.first=grid(col = "gray", lty = "dotted"), xlim = c(1, 105))
  lines(x = c(x - mod.fit$residuals, fore.mod$pred), lwd = 1, col = "black", type = "o", pch = 17) 
  lines(y = low, x = 101:105, lwd = 1, col = "darkgreen", lty = "dashed") 
  lines(y = up, x = 101:105, lwd = 1, col = "darkgreen", lty = "dashed") 
  legend(locator(1), legend = c("Observed", "Forecast", "95% C.I."), lty = c("solid", "solid", "dashed"),
         col = c("red", "black", "darkgreen"), pch = c(20, 17, NA), bty = "n")

  #Zoom in
  win.graph(width = 8, height = 6, pointsize = 10)  
  plot(x = x, ylab = expression(x[t]), xlab = "t", type = "o", col = "red", lwd = 1, pch = 20,
       main = expression(paste("Data simulated from AR(1): ", x[t] == 0.7*x[t-1] + w[t], " where ", w[t], "~N(0,1)")) , 
       panel.first=grid(col = "gray", lty = "dotted"), xlim = c(96, 105))
  lines(x = c(x - mod.fit$residuals, fore.mod$pred), lwd = 1, col = "black", type = "o", pch = 17) 
  lines(y = low, x = 101:105, lwd = 1, col = "darkgreen", lty = "dashed") 
  lines(y = up, x = 101:105, lwd = 1, col = "darkgreen", lty = "dashed") 
  legend(locator(1), legend = c("Observed", "Forecast", "95% C.I."), lty = c("solid", "solid", "dashed"),
         col = c("red", "black", "darkgreen"), pch = c(20, 17, NA), bty = "n")













#
