#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  1-30-06                                                    #
# UPDATE:                                                           #
# PURPOSE: Example 3.4 additional work                              #
#                                                                   #
# NOTES:                                                            #
#####################################################################


#####################################################################
# Simulate data 
  
  set.seed(5310)

  n1<-20
  mu1<-14

  #Pop #1 is Exp(14) so that variance is 14^2 = 196
  y<-rexp(n = n1, rate = 1/mu1)
  
  #Symmetric distribution 
  y.sym<-c(y, 2*median(y)-y)
  
####################################################################
# Investigate the new symmetric distribution

  par(pty = "s", mfrow=c(1,2))
  hist(x = y.sym, main = "Symmetric distribution", xlab = "y.sym", freq=FALSE)
  curve(dnorm(x, mean = mean(y.sym), sd = sd(y.sym)), col = "red", add = TRUE, lty = "dotted") #Normal distribution
  lines(density(x = y.sym), col = "blue", lty = "solid") #Use default settings - density estimation

  #EDF
  plot.ecdf(y.sym, verticals = TRUE, do.p = FALSE, main = "EDF for symmetric distribution", lwd = 2,
            panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"), ylab = "F.sym^", xlab = "y.sym")
  abline(h = 0.5, col = "blue", lty = "dashed")  #Reason: Symmetric distribution should be mirror image
  
  
#####################################################################
# Resampling - show with sample mean
  
  library(boot)
  
  #No particular reason why I chose the mean for t
  #  The important part of this example is to understand how the resampling is done
  calc.t<-function(data, i, n) {
     d<-data[i]
     mean(d[1:n]) 
  }


  #Try the statistic function - see note by the boot() function for reason of specifying n  
  calc.t(data = y.sym, i = 1:length(y.sym), n = length(y))


  #Bootstrap it!
  #  Notice how I use just a sample of size n in the resamples even though
  #  there are 2n different values in y.sym
  set.seed(2134)
  boot.res<-boot(data = y.sym, statistic = calc.t, R = 999, sim = "ordinary", n = length(y))
  boot.res
  plot(boot.res)

 
  
# 
  
  
  
  
