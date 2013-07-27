############################################################################
# NAME:  Chris Bilder                                                      #
# DATE:  11-24-02, 1-15-07                                                 #
# UPDATE:                                                                  #
# PURPOSE: ARCH example from garch function documentation                  #
#                                                                          #
# NOTES:                                                                   #
#                                                                          #
############################################################################

############################################################################
# Simulate data 

  library(tseries)
  set.seed(1532)
  n<-1100
  a<-c(0.1, 0.4)  #ARCH(1) coefficients - alpha0 and alpha1
  e<-rnorm(n = n, mean = 0, sd = 1)  
  y<-numeric(n)   #intializes a vector of x's to be n long
  y[1]<-rnorm(n =1, mean = 0, sd = sqrt(a[1]/(1.0-a[2]))) #start value

  for(i in 2:n)     #Simulate ARCH(1) process
   {
    y[i]<-e[i]*sqrt(a[1]+a[2]*y[i-1]^2)
   }
  y<-y[101:1100]    #Drop the first 100 and just call it y again


  win.graph(width = 8, height = 6, pointsize = 10)  
  plot(x = y, ylab = expression(y[t]), xlab = "t", type = "l", col = "red",  
       main = "ARCH(1) simulaed data", panel.first=grid(col = "gray", lty = "dotted"))
  points(x = y, pch = 20, col = "blue")
  


##################################################################
# Fit model and examine diagnostics 

  mean(y)
  mod.fit<-garch(x = y - mean(y), order = c(0,1)) #Fit ARCH(1) 
  summary(mod.fit)                      #Summarizes fit
  names(mod.fit)
  mod.fit$coef
  plot(mod.fit)                         #Various plots


  #Also can get the same test as in summary() this way
  Box.test(x = mod.fit$residuals^2, type = "Ljung-Box", lag = 1)

  #Would have done at the beginning 
  Box.test(x = y^2, type = "Ljung-Box", lag = 1)
  Box.test(x = y^2, type = "Ljung-Box", lag = 20)

  #PACF of y^2 and residuals
  par(mfrow = c(2,1))
  pacf(x = y^2, lag.max = 20, ylim = c(-1,1), xlim = c(1,20), xlab = "h", main = expression(paste("Estimated PACF for ", tilde(y)[t]^2)))
  #First residual is NA due to model being fit
  pacf(x = mod.fit$residuals[2:1000], lag.max = 20, ylim = c(-1,1), xlim = c(1,20), xlab = "h", main = "Estimated PACF for residuals")
  par(mfrow = c(1,1))

##################################################################
# Predicted values and residuals 

  #Show predicted sigma_t
  predict(object = mod.fit)[1:10]  #The [1:10] says to just show the first 10
  mod.fit$fitted.values[1:10]
 
  #For example: sigma_t=2 is
  sqrt(0.099410+0.352664*( -0.1629078-mean(y))^2)
  #y[1] = -0.1629078


  #Show residuals
  residuals(mod.fit)[1:10]
  mod.fit$residuals[1:10]
  (y[1:10]-mean(y))/predict(mod.fit)[1:10]
   
  


##################################################################
#Example in documentation for ARCH(2)
  n <- 1100
  a <- c(0.1, 0.5, 0.2)  # ARCH(2) coefficients
  e <- rnorm(n)  
  x <- double(n) #intializes a vector of x's to be n long
  x[1:2] <- rnorm(2, sd = a[1]/(1.0-a[2]-a[3])) #start values
  for(i in 3:n)  # Generate ARCH(2) process
   {
    x[i] <- e[i]*sqrt(a[1]+a[2]*x[i-1]^2+a[3]*x[i-2]^2)
   }
  x <- ts(x[101:1100])
  x.arch <- garch(x, order = c(0,2))  # Fit ARCH(2) 
  summary(x.arch)                     # Diagnostic tests
  plot(x.arch)       
