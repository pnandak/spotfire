######################################################################
# NAME:  Chris Bilder                                                #
# DATE:  1-5-06                                                      #
# PURPOSE: Duplicate example 5.6 on p. 294 of Shumway and Stoffer    #
#          using gls in the nlme library                             #
# NOTES:                                                             #
#                                                                    #
######################################################################


#Read in data
mort<-read.table(file = "C:\\chris\\UNL\\STAT_time_series\\Shumway_Stoffer_web_info\\Data\\cmort.dat", 
                  header = FALSE, col.names = "mort", sep = "")
temp<-read.table(file = "C:\\chris\\UNL\\STAT_time_series\\Shumway_Stoffer_web_info\\Data\\temp.dat", 
                  header = FALSE, col.names = "temp", sep = "") 
part<-read.table(file = "C:\\chris\\UNL\\STAT_time_series\\Shumway_Stoffer_web_info\\Data\\part.dat", 
                  header = FALSE, col.names = "part", sep = "") 
ex2.43<-data.frame(mort, temp, part, t = 1:nrow(mort))
head(ex2.43)

#Plot of data
win.graph(width = 8, height = 6, pointsize = 10)  
par(mfrow = c(3,1))
plot(x = ex2.43$mort, ylab = expression(y[t]), xlab = "t", type = "l", col = "red", lwd = 1 , 
       main = "Plot of mortality data", panel.first=grid(col = "gray", lty = "dotted"))
points(x = ex2.43$mort, pch = 20, col = "blue")

plot(x = ex2.43$temp, ylab = expression(T[t]), xlab = "t", type = "l", col = "red", lwd = 1 , 
       main = "Plot of temperature data", panel.first=grid(col = "gray", lty = "dotted"))
points(x = ex2.43$temp, pch = 20, col = "blue")

plot(x = ex2.43$part, ylab = expression(P[t]), xlab = "t", type = "l", col = "red", lwd = 1 , 
       main = "Plot of Particulate data", panel.first=grid(col = "gray", lty = "dotted"))
points(x = ex2.43$part, pch = 20, col = "blue")



######################################################################
# Fit the model

  library(nlme) #gls function is in this package

  start.time<-proc.time()    #Find start time

  mod.fit<-gls(model = mort ~ t + I(temp - mean(temp)) + I((temp - mean(temp))^2) + part, data = ex2.43, 
               correlation = corARMA(form = ~t, p = 2))  #see p. 8.6 of STAT 870 notes for more on I() function
  summary(mod.fit)

  #Find end time and total time elapsed
  end.time<-proc.time()
  save.time<-end.time-start.time
  cat("\n Number of minutes running:", save.time[3]/60, "\n \n")

  names(mod.fit)
  
  #C.I.s for model parameters
  save.int<-intervals(object = mod.fit, level = 0.95)
  names(save.int)
  save.int$corStruct[,2]


#########################################################################
# Residuals

  #Residuals without ARMA part
  head(mod.fit$residuals)

  par(mfrow = c(1,2))
  acf(x = mod.fit$residuals, lag.max = 20, type = "correlation", main = "Estimated ACF for x residuals", xlim = c(1,20), ylim = c(-1,1))
  pacf(x = mod.fit$residuals, lag.max = 20, main = "Estimated PACF for x residuals", xlim = c(1,20), ylim = c(-1,1))
  par(mfrow = c(1,1))
  
  
  #Show an example of finding w^
  mod.fit$residuals[1:5]
  filter(x = mod.fit$residuals[1:5], filter = c(1, -save.int$corStruct[,2]), sides = 1) 

  #Check using x.3 - should be (1-0.3939042B-0.4381177B^2)x.3
  -3.8537334 - 0.3939043*8.5525256 - 0.4381177*(-0.9735835)

  #Use all
  w.resid<-filter(x = mod.fit$residuals, filter = c(1, -save.int$corStruct[,2]), sides = 1) 
  par(mfrow = c(1,2))
  acf(x = w.resid[-(1:2)], lag.max = 20, type = "correlation", main = "Estimated ACF for w residuals", xlim = c(1,20), ylim = c(-1,1))
  pacf(x = w.resid[-(1:2)], lag.max = 20, main = "Estimated PACF for w residuals", xlim = c(1,20), ylim = c(-1,1))
  par(mfrow = c(1,1))
 
  #Easier way to get w^
  #  R calls these the "normalized" residuals.  They are standardized residuals pre-multiplied by the 
  #  inverse square-root factor of the estimated error correlation matrix.  These are not exactly the same as 
  #  w.resid since they are standardized, but notice the resulting ACF and PACF will be the same.  
  w.resid2<-residuals(object = mod.fit, type = "normalized")
  head(w.resid2)
  par(mfrow = c(1,2))
  acf(x = w.resid2, lag.max = 20, type = "correlation", main = "Estimated ACF for normalized residuals", xlim = c(1,20), ylim = c(-1,1))
  pacf(x = w.resid2, lag.max = 20, main = "Estimated PACF for normalized residuals", xlim = c(1,20), ylim = c(-1,1))
  par(mfrow = c(1,1))


######################################################################
# Predictions

  #Predictions
  pred.mod<-predict(object = mod.fit)
  head(pred.mod)
   
  mod.fit$residuals[1:5]
  
  #3rd predicted value
  98.21373 + 0.3939*8.5525256 + 0.4381*(-0.9735835)

  #4th predicted value
  96.09230 + 0.3939*(-3.8537335) + 0.4381*8.5525256

  #All predicted using ARMA errors
  x.tilde<-filter(x = mod.fit$residuals, filter = save.int$corStruct[,2], sides = 1)
  head(x.tilde) 
  
  beta.z<-ts(data = pred.mod[-(1:2)], start = 3) 
  x.tilde2<-ts(data = x.tilde[-1], start = 3)
  pred2<-beta.z+x.tilde2
  head(pred2)
  
 

######################################################################
# C.I.s for pred. values 
  
  round(mod.fit$varBeta,4)
  
  #Notice the order of the explanatory variables needs to match those in the covariance matrix
  pred.data<-as.matrix(data.frame(int = 1, t = 1:nrow(mort), temp - mean(temp), (temp-mean(temp))^2, part))
  obs.numb<-2
  se<-sqrt(pred.data[obs.numb,]%*%mod.fit$varBeta%*%pred.data[obs.numb,]) #One observation
  low<-pred.mod[obs.numb] - qnorm(p = 0.975, mean = 0, sd = 1)*se
  up<-pred.mod[obs.numb] + qnorm(p = 0.975, mean = 0, sd = 1)*se
  
  cat("The 95% C.I. without ARMA error is:", round(low,2), ",", round(up,2), "\n")
  #The above produces (93.58, 104.07) for observation #1
  #                   (90.97, 101.21) for observation #2

  #SAS obtains (94.056935918, 103.81263709) for observation #1
  #            (91.405237528, 100.88551804) for observation #2


  
#  mod.fit$sigma
  
  
  
  
  
#   












#
