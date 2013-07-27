#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  1-20-06, updated 10-18-07                                  #
# UPDATE:                                                           #
# PURPOSE: Double boot for Section 2.7.5                            #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#####################################################################
# Read in data

library(boot)

  #AC data
  y<-c(3,5,7,18,43,85,91,98,100,130,230,487)
  t<-mean(y)
  n<-length(y)
  cat("My sample is", sort(y), "\n which produces an observed statistic of", t, "\n")

  calc.t<-function(data, i) {
     d<-data[i]
     mean(d) 
  }
  
  set.seed(9182)
  boot.res<-boot(data = y, statistic = calc.t, R = 999, sim="ordinary") 

#######################################################################
# Double bootstrap implementation with an approximate pivotal

  #Find start time
  start.time<-proc.time()

  M<-100 #BMA recommend 50-200, see Booth and Sarkar (1998) for a larger recommendation
  R<-999

  calc.t2<-function(data, i) {
     d2<-data[i]
     mean(d2) 
  }

  calc.t<-function(data, i, M) {
     d<-data[i]
     boot.res.M<-boot(data = d, statistic = calc.t2, R = M, sim="ordinary")
     
     #Testing
     #cat("Show d: ", d, "\n")
     #cat("Show v:", var(boot.res.M$t), "\n")  
     
     v.boot<-var(boot.res.M$t)     
     c(mean(d), v.boot)
  }
  
  #Testing
  calc.t(data = y, i = c(1:length(y)), M = M) 
  
  set.seed(9182)
  boot.res<-boot(data = y, statistic = calc.t, R = R, sim="ordinary", M = M) 
  #plot(boot.res)  #boot.res$t[,1] is plotted
  z.star<-(boot.res$t[,1]-t)/sqrt(boot.res$t[,2])
  
  #Summary of z.star distribution 
  summary(z.star)
  sort(z.star)[c(1:10,995:999)]
  hist(x = z.star, main = "Histogram for z*, R=999, M=800", freq=FALSE, xlab = "z*", breaks = seq(from = -14, to = 3, by = 0.5))
  curve(dnorm(x, mean = mean(z.star), sd = sd(z.star)), col = 2, add = TRUE)

  
  #Examine the standard deviations produced by the double boot
  summary(sqrt(boot.res$t[,2]))
  #hist(x = sqrt(boot.res$t[,2]), main = "S.D. produced by double bootstrap", xlab = "(v_r ^*)^0.5" )
  hist(x = sqrt(boot.res$t[,2]), main = "S.D. produced by double bootstrap", xlab = expression(sqrt(v[r]^"*")))
  abline(h=0)
  segments(x0 = sqrt(var(y)/n), y0 = -5, x1 = sqrt(var(y)/n), y1 = 10, col = "red", lwd = 2)
    
  #Actual standard deviation of y_bar using usual formula of s/sqrt(n)
  sqrt(var(y)/n)
  
  #Find end time and total time elapsed
  end.time<-proc.time()
  save.time<-end.time-start.time
  cat("\n Number of minutes running:", save.time[3]/60, "\n \n")

  













#
