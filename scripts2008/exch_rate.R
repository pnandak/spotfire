############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  11-23-02, 1-16-07                                                 #
# UPDATE:                                                                  #
# PURPOSE: GARCH example from p. 255 of Pena, Tiao, and Tsay               #
#                                                                          #
# NOTES:                                                                   #
#                                                                          #
############################################################################


library(tseries)

exch<-read.table(file = "C:\\chris\\UNL\\STAT_time_series\\chapter5\\exchrate.dat", 
                  header = FALSE, col.names = "x", sep = "")
head(exch)
x<-exch$x


###########################################################################
# Initial plots of the data

  win.graph(width = 8, height = 6, pointsize = 10)  
  plot(x = x, ylab = expression(x[t]), xlab = "t", type = "l", col = "red",  
       main = "Exchange rate data", panel.first=grid(col = "gray", lty = "dotted"))
  #points(x = x, pch = 20, col = "blue")

  par(mfrow = c(1,2))
  acf(x = x, type = "correlation", main = "Estimated ACF for exchange rate data", xlim = c(1,20), ylim = c(-1,1))
  pacf(x = x, lag.max = 20, main = "Estimated PACF for exchange rate data", xlim = c(1,20), ylim = c(-1,1))
  par(mfrow = c(1,1))
 

###########################################################################
# Below is a duplication of Figure 9.1 on p. 251

  #Convert to ts object to make sure everything lines up correctly for (x_t - x_t-1)/x_t-1
  x.ts<-ts(data = x)
  y<-diff(x = x.ts, lag = 1, differences = 1)/lag(x.ts, k = -1)
  #check (x[2]-x[1])/x[1]
  (x[2]-x[1])/x[1]
  y[1]  #This is actually y at t = 2 due to y starting at time = 2 in the ts object
  #check (x[2488]-x[2487])/x[2487]
  (x[2488]-x[2487])/x[2487]
  y[2487] #This is actually y at t = 2488 due to y starting at time = 2 in the ts object
 

  par(mfrow = c(2,1))
  plot(x = y, ylab = expression(y[t]), xlab = "t", type = "l", col = "red",  
       main = expression(paste("Plot of ", y[t] == frac(x[t] - x[t-1],x[t]))))
  grid() #I had problems getting lines to match up with tick marks using panel.first
  plot(x = y^2, ylab = expression(y[t]^2), xlab = "t", type = "l", col = "red",  
       main = expression(paste("Plot of ", y[t]^2 == bgroup("(",frac(x[t] - x[t-1],x[t]),")")^2)))
  grid()
  par(mfrow = c(1,1))

   
###########################################################################
# Below is a duplication of Figure 9.2 on p. 252

  #I took out my normal ylim = c(-1,1) so that you can see the PACF values similar to the figure in the book.  
  par(mfrow = c(2,1))
  acf(x = y, type = "correlation", main = expression(paste("Estimated ACF for ", y[t])), xlim = c(1,20))
  pacf(x = y^2, lag.max = 20, main = expression(paste("Estimated PACF for ", y[t]^2)), xlim = c(1,20))
  par(mfrow = c(1,1))


  #ACF plot for y^2_t
  acf(x = y^2, lag.max = 20, main = expression(paste("Estimated ACF for ", y[t]^2)), xlim = c(1,20))

   
###########################################################################
# ARCH(3) model

  #Since mean of y is very close to 0, I am not going to use a mean adjustment
  mean(y)
 
  #Fit and examine ARCH(3) model 
  mod.fit<-garch(x = y, order = c(0,3))
  summary(mod.fit)                      
  plot(mod.fit)                        
  
  #PACF of the residuals^2 - note that first three values are NA
  pacf(x = mod.fit$residuals[-(1:3)]^2, lag.max = 20, main = "Estimated PACF for residuals from ARCH(3)", xlim = c(1,20))

  #Print the large standardized residuals
  index<-1:length(mod.fit$residuals)
  data.frame(index=index[abs(mod.fit$residuals)>3], y=y[abs(mod.fit$residuals)>3], 
             std.resid=mod.fit$residuals[abs(mod.fit$residuals)>3])


####################################################################################
#Examine an ARCH(1) model

  #Fit and examine ARCH(1) model 
  mod.fit<-garch(x = y, order = c(0,1))
  summary(mod.fit)                      
  plot(mod.fit)                        

  #PACF of the residuals^2 - note that first three values are NA
  pacf(x = mod.fit$residuals[-1]^2, lag.max = 20, main = "Estimated PACF for residuals from ARCH(1)", xlim = c(1,20))


####################################################################################
#Examine an ARCH(10) model

  #Fit and examine ARCH(10) model 
  mod.fit<-garch(x = y, order = c(0,10))
  summary(mod.fit)                      
  plot(mod.fit)                        

  #PACF of the residuals^2 - note that first three values are NA
  pacf(x = mod.fit$residuals[-(1:10)]^2, lag.max = 20, main = "Estimated PACF for residuals from ARCH(10)", xlim = c(1,20))










#
