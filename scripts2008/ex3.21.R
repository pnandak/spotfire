#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  1-28-06, update 10-22-07                                   #
# UPDATE:                                                           #
# PURPOSE: Example 3.21                                             #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#####################################################################
# Simulate data - BMA does not provide any actual data so I will 
#   simulate it using their observed correlation.  No mean vector or 
#   or variances were given in the example
 
  library(mvtnorm)

  set.seed(8719)
  set1<-rmvnorm(n = 20, mean = c(0, 0), sigma = matrix(data = c(1, 0.74, 0.74, 1), ncol = 2, nrow = 2))
  set1
  n<-nrow(set1)
  
  #Check observed
  apply(X = set1, FUN = mean, MARGIN = 2)
  var(set1)
  cor(set1)
  
#######################################################################
# Do bootstrap
 
  library(boot)
  
  calc.t2<-function(data2, i2) {
    d2<-data2[i2,]
    r<-cor(d2)[1,2] 
    r
  }
  
  calc.t<-function(data, i, M = 50) { 
    d<-data[i,]
    n<-nrow(d)
    r<-cor(d)[1,2]
    boot.res.M<-boot(data = d, statistic = calc.t2, R = M, sim="ordinary")
    
    #Calculate variance - See example 2.18
    u.s<-(d[,1]-mean(d[,1]))/sqrt(mean(d[,1]^2) - mean(d[,1])^2) 
    x.s<-(d[,2]-mean(d[,2]))/sqrt(mean(d[,2]^2) - mean(d[,2])^2) 
    v.L<-1/n^2 * sum( (u.s*x.s - 0.5*r*(u.s^2 + x.s^2))^2 )  
    v.L.tran<-1/n^2 * sum( (1/(1-r^2) * (u.s*x.s - 0.5*r*(u.s^2 + x.s^2)))^2 ) 
    
    #Variance from double boot
    v.boot<-var(boot.res.M$t)
    v.boot.tran<-var(0.5*log((1+boot.res.M$t)/(1-boot.res.M$t)) )
        
    c(r, v.L, v.boot, v.L.tran, v.boot.tran) 
  }
  
  #Try it
  calc.t(data = set1, i = 1:20)
 
  #Remember: Could have used other ways to get the variance in addition to v.L and v.boot
  l.jack<-empinf(data = set1, statistic = calc.t, stype = "i", type = "jack", index = 1) 
  var.linear(l.jack) 

  #Do bootstrap
  set.seed(1652)
    
  #Find start time
  start.time<-proc.time()

  #Could (and generally should?) use larger values for M
  boot.res<-boot(data = set1, statistic = calc.t, R = 999, sim="ordinary", M = 800) 
  plot(boot.res)   

  #Find end time and total time elapsed
  end.time<-proc.time()
  save.time<-end.time-start.time
  cat("\n Number of minutes running:", save.time[3]/60, "\n \n")

#######################################################################
# Figure 3.8
 
  par(mfrow = c(1,2))
  
  #Figure 3.8 left
  plot(x = boot.res$t[,1], y = boot.res$t[,2], main = "vL* vs. t*", xlab = "t*", ylab = "vL*", 
       col = "red") 
  curve(expr = 1/n * (1 - x^2)^2, col = "blue", add = TRUE, lwd = 1)
  lines(supsmu(x = boot.res$t[,1], y = boot.res$t[,2]), col = "darkgreen", lwd = 2) #using defaults
  legend(x = 0.7, y = 0.04, legend = c("Asymptotic var", "Smoother"), col=c("blue", "darkgreen"),
         lty = c(1, 1), lwd = c(1,2), bty="n", cex=0.75)

  #Other smoothers - see p. 228-231 of Venables and Ripley (2002) for a list of a few
  #  These show alternative smoothers to use when duplicating Figure 3.7
  #lines(loess.smooth(x = boot.res$t[,1], y = boot.res$t[,2]), col = "green", lwd = 2)
  #lines(ksmooth(x = boot.res$t[,1], y = boot.res$t[,2]), col = "black", lwd = 2)

  #Figure 3.8 right
  plot(x = 0.5*log((1+boot.res$t[,1])/(1-boot.res$t[,1])), y = boot.res$t[,4], 
       main = "vL.tran* vs. t*", xlab = "t.tran*", ylab = "vL.tran*", col = "red") 
  abline(h = 1/n, col = "blue", lwd = 1)  #asymptotic variance approximation
  lines(supsmu(x = 0.5*log((1+boot.res$t[,1])/(1-boot.res$t[,1])), y = boot.res$t[,4]), 
        col = "darkgreen", lwd = 2) #using defaults
  legend(x = 1.4, y = 0.12, legend = c("Asymptotic var", "Smoother"), col=c("blue", "darkgreen"),
         lty = c(1, 1), lwd = c(1,2), bty="n", cex=0.75)
  mean(boot.res$t[,4])  
 
 
 
#######################################################################
# Reproduce Figure 3.8 using the double bootstrap variance measures instead of the 
#  equation 2.36 with the empirical influence values (nonparametric delta method)

  par(mfrow = c(1,2))

  ymax<-max(boot.res$t[,2], boot.res$t[,3])  #Get plots on same y-axis scale
  
  #Figure 3.8 left with v.boot
  plot(x = boot.res$t[,1], y = boot.res$t[,3], main = "v.boot* vs. t*", xlab = "t*", ylab = "v.boot*", 
       col = "red", ylim = c(0, ymax)) 
  curve(expr = 1/n * (1 - x^2)^2, col = "blue", add = TRUE, lwd = 1)
  lines(supsmu(x = boot.res$t[,1], y = boot.res$t[,3]), col = "darkgreen", lwd = 2) #using defaults
  legend(x = 0.7, y = 0.1, legend = c("Asymptotic var", "Smoother"), col=c("blue", "darkgreen"),
         lty = c(1, 1), lwd = c(1,2), bty="n", cex=0.75)

  #Allows for a direct comparison to Figure 3.8 left plot
  plot(x = boot.res$t[,1], y = boot.res$t[,2], main = "vL* vs. t*", xlab = "t*", ylab = "vL*", 
       col = "red", ylim = c(0, ymax)) 
  curve(expr = 1/n * (1 - x^2)^2, col = "blue", add = TRUE, lwd = 1)
  lines(supsmu(x = boot.res$t[,1], y = boot.res$t[,2]), col = "darkgreen", lwd = 2) #using defaults
  legend(x = 0.7, y = 0.1, legend = c("Asymptotic var", "Smoother"), col=c("blue", "darkgreen"),
         lty = c(1, 1), lwd = c(1,2), bty="n", cex=0.75)




  par(mfrow = c(1,2)) 
  
  ymax<-max(boot.res$t[,4], boot.res$t[,5])  #Get plots on same y-axis scale

  #Figure 3.8 left with v.boot.tran
  plot(x = 0.5*log((1+boot.res$t[,1])/(1-boot.res$t[,1])), y = boot.res$t[,5], 
       main = "v.boot.tran* vs. t*", xlab = "t.tran*", ylab = "v.boot.tran*", col = "red",
       ylim = c(0, ymax)) 
  abline(h = 1/n, col = "blue")  #asymptotic variance approximation
  lines(supsmu(x = 0.5*log((1+boot.res$t[,1])/(1-boot.res$t[,1])), y = boot.res$t[,5]), 
        col = "darkgreen", lwd = 2) #using defaults
  legend(x = 1.4, y = 0.17, legend = c("Asymptotic var", "Smoother"), col=c("blue", "darkgreen"),
         lty = c(1, 1), bty="n", cex=0.75)
  mean(boot.res$t[,5])  

  #Allows for a direct comparison to Figure 3.8 right plot
  plot(x = 0.5*log((1+boot.res$t[,1])/(1-boot.res$t[,1])), y = boot.res$t[,4], 
       main = "vL.tran* vs. t*", xlab = "t.tran*", ylab = "vL.tran*", col = "red",
       ylim = c(0, ymax)) 
  abline(h = 1/n, col = "blue")  #asymptotic variance approximation
  lines(supsmu(x = 0.5*log((1+boot.res$t[,1])/(1-boot.res$t[,1])), y = boot.res$t[,4]), 
        col = "darkgreen", lwd = 2) #using defaults
  legend(x = 1.4, y = 0.14, legend = c("Asymptotic var", "Smoother"), col=c("blue", "darkgreen"),
         lty = c(1, 1), bty="n", cex=0.75)
  mean(boot.res$t[,4])  
   
  
  
  #
