######################################################################
# NAME:  Chris Bilder                                                #
# DATE:  12-28-06                                                    #
# PURPOSE: Go through the model building process with AR(1) data in  #
#          AR1.0.7.txt                                               #
#                                                                    #
# NOTES:                                                             #
######################################################################

ar1<-read.table(file = "C:\\chris\\UNL\\STAT_time_series\\chapter1\\AR1.0.7.txt", header=TRUE, sep = "")
head(ar1)
x<-ar1$x

#####################################################################
# Step #1 and #2
 
  win.graph(width = 8, height = 6, pointsize = 10)  
  plot(x = x, ylab = expression(x[t]), xlab = "t", type = "l", col = "red", lwd = 1 , 
       main = "Plot of AR1.0.7.txt data", panel.first=grid(col = "gray", lty = "dotted"))
  points(x = x, pch = 20, col = "blue")

  par(mfrow = c(1,2))
  acf(x = x, type = "correlation", main = "Estimated ACF for AR1.0.7.txt data", xlim = c(1,20), ylim = c(-1,1))
  pacf(x = x, lag.max = 20, main = "Estimated PACF for AR1.0.7.txt data", xlim = c(1,20), ylim = c(-1,1))
  par(mfrow = c(1,1))
  
  
#####################################################################
# Step #3

  #ARIMA(1,0,0)
  mod.fit1<-arima(x = x, order = c(1, 0, 0), include.mean = TRUE)
  mod.fit1
  z<-mod.fit1$coef/sqrt(diag(mod.fit1$var.coef))
  p.value<-2*(1-pnorm(q = abs(z), mean = 0, sd = 1))
  data.frame(z, p.value)


  #ARIMA(0,0,1)
  mod.fit2<-arima(x = x, order = c(0, 0, 1), include.mean = TRUE)
  mod.fit2
  z<-mod.fit2$coef/sqrt(diag(mod.fit2$var.coef))
  p.value<-2*(1-pnorm(q = abs(z), mean = 0, sd = 1))
  data.frame(z, p.value)


#####################################################################
# Step #4

 #AR(1)
  tsdiag(object = mod.fit1, gof.lag = 20)
  
  #Could also get the Ljung-Box-Pierce test this way.  
  #Box.test(mod.fit1$residuals, lag = 20, type = "Ljung-Box")
  #Box-Pierce test  
  #Box.test(mod.fit1$residuals, lag = 20) 

  par(mfrow = c(2,2)) 
  pacf(x = mod.fit1$residuals, lag.max = 20, main = "Estimated PACF for residuals using ARIMA(1,0,0) fit", 
       xlim = c(1,20), ylim = c(-1,1))
       
  hist(x = mod.fit1$residuals, main = "Histogram of residuals", xlab = "Residuals", freq = FALSE)
  curve(expr = dnorm(x, mean = mean(mod.fit1$residuals), sd = sd(mod.fit1$residuals)), col = "red", add = TRUE)

  qqnorm(y = mod.fit1$residuals, ylab = "Residuals", panel.first = grid(col = "gray", lty = "dotted"))
  qqline(y = mod.fit1$residuals, col = "red")
  par(mfrow = c(1,1)) 


 #MA(1)
  tsdiag(object = mod.fit2, gof.lag = 20)
  
  par(mfrow = c(2,2)) 
  pacf(x = mod.fit2$residuals, lag.max = 20, main = "Estimated PACF for residuals using ARIMA(0,0,1) fit", 
       xlim = c(1,20), ylim = c(-1,1))
       
  hist(x = mod.fit2$residuals, main = "Histogram of residuals", xlab = "Residuals", freq = FALSE)
  curve(expr = dnorm(x, mean = mean(mod.fit2$residuals), sd = sd(mod.fit2$residuals)), col = "red", add = TRUE)

  qqnorm(y = mod.fit2$residuals, ylab = "Residuals", panel.first = grid(col = "gray", lty = "dotted"))
  qqline(y = mod.fit2$residuals, col = "red")
  par(mfrow = c(1,1)) 

#####################################################################
# Step #5

  #The AIC values were already given in the arima() output, but here is another way to get them.
  mod.fit1$aic
  -2*mod.fit1$loglik+2*3

  mod.fit2$aic
  







#
