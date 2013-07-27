#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  1-16-06, updated 10-18-07                                  #
# UPDATE:                                                           #
# PURPOSE: Investigate empirical influence values                   #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#####################################################################
# Do nonparametric bootstrap here to prepare for future calculations

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
    
  #Do bootstrap
  set.seed(9182)
  boot.res<-boot(data = y, statistic = calc.t, R = 999, sim="ordinary") 

  linear.app<-linear.approx(boot.out = boot.res) #BMA p. 47, equation 2.35 right side
  head(linear.app) 
  head(boot.res$t) #Same since t(F) linear function of y's
  

################################################################################
# Show how to work with functions to get the empirical influence function values
  
  #Write the statistic of interest in terms of weights (default here is 1/n)
  calc.tw<-function(data, w = rep(x = 1, times = length(data))/length(data)) {
     d<-data
     sum(d*w) 
  }
  calc.tw(y)
  t
  
  #This function takes numerical derivatives to find the l_j values and NEEDS
  #  the statistic's function to be written in terms of weights
  l.j<-empinf(data = y, statistic = calc.tw, stype = "w") 
  l.j
  
  y - mean(y) #Verify above result is correct
  
  var.linear(l.j)
  sum(l.j^2)/n^2 


#############################################################################
# Variance calcuations 
  
  #Usual estimated variance
  var(y)/n             #usual unbiased estimator
  (n-1)*(1/n^2)*var(y) #usual biased estimator
  
  #jackknife - Equation 2.42
  l.jack<-empinf(data = y, statistic = calc.t, stype = "i", type = "jack") 

  #regression estimates - Equation 2.46
  l.reg<-empinf(boot.out = boot.res)
  
  data.frame(l.reg, l.jack)
  
  #Finding variance using var.linear() or simply equation 2.36 with app. empirical influence values
  var.linear(l.jack)
  var.linear(l.reg)
  sum(l.jack^2)/n^2 
  sum(l.reg^2)/n^2  
  
 
  
  
  
#
