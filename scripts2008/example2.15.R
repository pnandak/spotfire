#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  1-15-06                                                    #
# UPDATE:                                                           #
# PURPOSE: Example on p. 81 of Efron and Tibshirani (1993) and      #
#          example 2.15 in BMA                                      #
# NOTES:                                                            #
#####################################################################

###################################################################################
# Simulate the data 

  set.seed(1203)
  y<-runif(n = 50, min = 0, max = 1)
  t<-max(y)
  n<-length(y)
  #cat("The sample is, \n", sort(y), "\n and t is", t, "\n")
  cat("t is", t, "\n")


###################################################################################
# Nonparametric bootsrap

  R<-2000
  set.seed(2311)
  y.star.npar<-matrix(data = sample(x = y, size = n*R, replace = TRUE), nrow = 2000, ncol = n)
  t.star.npar<-apply(X = y.star.npar, MARGIN = 1, FUN = max)

  par(pty = "s", mfrow = c(1,2))

  #Default margins for y and x-axis is 4% larger than specified
  #  This turns that off - turn back on again with par(xaxs = "r") where r stands for "regular"
  par(xaxs = "i") 
  hist(x = t.star.npar, main = "Histogram for t*", xlab = "t*", xlim = c(0.9,1))

  hist(x = n*(t-t.star.npar)/t, main = "Histogram for q*", xlab = "q*", freq=FALSE, 
       xlim = c(0,3))
  curve(expr = dexp(x = x, rate = 1), col = 2, add = TRUE)

  table(t.star.npar)

#Use with n = 5000
#par(xaxs = "i") 
#hist(x = t.star.npar, main = "Histogram for t*", xlab = "t*")
#hist(x = n*(t-t.star.npar)/t, main = "Histogram for q*", xlab = "q*", freq=FALSE)
#curve(dexp(x, rate = 1), col = 2, add = TRUE)

  par(mfrow=c(1,1))
  plot.ecdf(x = n*(t-t.star.npar)/t, verticals = TRUE, do.p = FALSE, main = "Boot. estimate of G", lwd = 2, xlab = "q*",
            panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"), ylab = "EDF or asymp app. of CDF")
  curve(expr = pexp(q = x, rate = 1), col = 2, add = TRUE)

###################################################################################
# Another way to do the nonparametric bootstrap with the boot() function here

  library(boot)
  calc.t<-function(data,i) {
    d<-data[i]
    max(d)  
  }
  calc.t(data = y, i = 1:length(y))
  
  set.seed(1213) 
  boot.res<-boot(data = y, statistic = calc.t, R = 2000, sim = "ordinary")

  par(pty = "s", mfrow = c(1,2))
  hist(x = boot.res$t, main = "Histogram for t*", xlab = "t*", xlim = c(0.9,1))

  hist(x = n*(t-boot.res$t)/t, main = "Histogram for q*", xlab = "q*", freq=FALSE, 
       xlim = c(0,3))
  curve(expr = dexp(x, rate = 1), col = 2, add = TRUE)

  table(boot.res$t)

  par(mfrow=c(1,1))
  plot.ecdf(x = n*(t-boot.res$t)/t, verticals = TRUE, do.p = FALSE, main = "Boot. estimate of G", lwd = 2, xlab = "q*",
            panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"), ylab = "EDF or asymp app. of CDF")
  curve(expr = pexp(q = x, rate = 1), col = 2, add = TRUE)



###################################################################################
#Parametric bootstrap

  set.seed(8102)
  y.star.par<-matrix(data = runif(n = n*R, min = 0, max = t), nrow = 2000, ncol = n)  
  t.star.par<-apply(X = y.star.par, MARGIN = 1, FUN = max)


  win.graph(width = 7, height = 7, pointsize = 12)
  par(mfrow = c(1,2), xaxs = "i") 
  hist(x = t.star.par, main = "Histogram for t*", xlab = "t*", xlim = c(0.9,1), nclass = 15)

  hist(x = n*(t-t.star.par)/t, main = "Histogram for q*", xlab = "q*", freq=FALSE, 
       xlim = c(0,3))
  curve(expr = dexp(x, rate = 1), col = 2, add = TRUE)

  par(mfrow = c(1,1), xaxs = "i")
  plot.ecdf(x = n*(t-t.star.par)/t, verticals = TRUE, do.p = FALSE, main = "Boot. estimate of G", lwd = 2, xlab = "q*",
            panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"), ylab = "EDF or asymp app. of CDF")
  curve(expr = pexp(x, rate = 1), col = 2, add = TRUE)













 
