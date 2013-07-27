######################################################################
# NAME:  Chris Bilder                                                #
# DATE:  12-19-06                                                    #
# PURPOSE: AR(1) data originally generated for OSU time series course#
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
# Examine ACF

  rho.x<-acf(x = x, type = "correlation", main = 
             expression(paste("Data simulated from AR(1): ", x[t] == 0.7*x[t-1] + w[t], " where ", w[t], "~N(0,1)")))
  rho.x
  names(rho.x)
  rho.x$acf
  rho.x$acf[1:2]
  
  acf(x = x, type = "covariance", main = 
             expression(paste("Data simulated from AR(1): ", x[t] == 0.7*x[t-1] + w[t], " where ", w[t], "~N(0,1)")))
  

  x.ts<-ts(x)
  set1<-ts.intersect(x.ts, x.ts1 = lag(x = x.ts, k = -1), x.ts2 = lag(x = x.ts, k = -2), x.ts3 = lag(x = x.ts, k = -3))
  cor(set1)
  set1
  #pairs(formula = ~x.ts+x.ts1+x.ts2+x.ts3, data=set1)

  library(car) #scatterplot.matrix is in this package - may need to install first
  scatterplot.matrix(formula = ~x.ts+x.ts1+x.ts2+x.ts3, data=set1, 
                     reg.line=lm, smooth=TRUE, span=0.5, diagonal = 'histogram')

  set2<-ts.intersect(x.ts, x.ts1 = lag(x = x.ts, k = 1), x.ts2 = lag(x = x.ts, k = 2), x.ts3 = lag(x = x.ts, k = 3))
  set2
  cor(set2)

  #Another way to see dependence
  lag.plot(x = x, lags = 4, layout = c(2,2), main = "x vs. lagged x", do.lines = FALSE)

#####################################################################
#Used in Section 3.6

  gamma.x<-acf(x = x, type = "covariance", main = 
             expression(paste("Data simulated from AR(1): ", x[t] == 0.7*x[t-1] + w[t], " where ", w[t], "~N(0,1)")))
  gamma.x
  mean(x)



#
