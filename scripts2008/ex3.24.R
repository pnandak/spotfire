#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  1-29-06                                                    #
# UPDATE:                                                           #
# PURPOSE: Example 3.24                                             #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#####################################################################
#  Initial calculations

  library(boot)
  
  n<-nrow(frets)

  #Measurements in millimeters
  frets
  
  #Table 3.11 - They work on the log scale with no reasoning given
  cor.mat<-cor(log(frets))  
  cor.mat

  #Calculate the pairwise partial correlations using the cor2pcor function 
  #  Works with covariance or correlation matrix
  library(corpcor)
  cor2pcor(m = cor.mat)
  #cov(frets); cor2pcor(m = cov.mat)
 
  #Scatter plot matrix 
  pairs(log(frets))


#####################################################################
# Bootstrap calculations and top of Figure 3.12
 
  calc.t<-function(data, i) { 
    d<-data[i,]
    cor.mat<-cor(d) 
    part.cor<-cor2pcor(m = cor.mat)
    t<-part.cor[2,3]
    t 
  }
  
  #Test it
  calc.t(data = log(frets), i = 1:n)
 
  set.seed(8918)
  boot.res<-boot(data = log(frets), statistic = calc.t, R = 999, sim = "ordinary")
  boot.res
  
  jack.after.boot(boot.out = boot.res, index = 1, stinf = FALSE, main = "Figure 3.12")
  plot(x = boot.res, jack = TRUE, stinf = FALSE) #Gives same plot plus usual plots from plot(boot.res)

  #Why doesn't the jack.after.boot() function actually use the jackknife estimated empirical influence function values?
  #  Not sure - see the "J" element in the function for what is actually being computed.  If the jackknife values
  #  were calculated, this is what they would be:
  l.jack<-empinf(data = log(frets), statistic = calc.t, stype = "i", type = "jack")
  l.jack[order(l.jack)]
  order(l.jack)
  
  #Uses regression empirical influence values
  #jack.after.boot(boot.out = boot.res, index = 1, stinf = FALSE, main = "Figure 3.12", useJ = FALSE)

  #The empirical influence values can be supplied as well by the user - see the help for how 
  #  Figure 3.12 does say that the "infinitesimal jackknife" values are used - these are the
  #  empirical influence values




#####################################################################
# Bottom of Figure 3.12
  
  par(mfrow = c(1,2))
  
  #Left plot
  plot(x = log(frets$b1), y = log(frets$l2), type = "n", xlab = "Log b1", ylab = "Log l2", main = "Figure 3.12 lower left")
  text(x = log(frets$b1), y = log(frets$l2), labels = 1:n, cex = 0.75, col = c("red", "blue", rep("red", n-2)))
  abline(lm(formula = log(l2) ~ log(b1), data = frets), col = "darkgreen", lwd = 1)
  abline(lm(formula = log(l2) ~ log(b1), data = frets[-2,]), col = "purple", lwd = 2)
  legend(x = 4.91, y = 5.25, legend = c("all obs.", "w/o #2"), col=c("darkgreen", "purple"),
         lty = c(1, 1), lwd = c(1,2), bty="n", cex=0.75)

  #Right plot
  resid.log.b1<-lm(formula = log(b1) ~ log(b2) + log(l1), data = frets)$residuals
  resid.log.l2<-lm(formula = log(l2) ~ log(b2) + log(l1), data = frets)$residuals
  plot(x = resid.log.b1, y = resid.log.l2, type = "n", xlab = "Residual for log b1", ylab = "Residual for log l2", 
       main = "Figure 3.12 lower right")
  text(x = resid.log.b1, y = resid.log.l2, labels = 1:n, cex = 0.75, col = c("red", "blue", rep("red", n-2)))
  abline(lm(formula = resid.log.l2 ~ resid.log.b1), col = "darkgreen", lwd = 1)
  abline(lm(formula = resid.log.l2[-2] ~ resid.log.b1[-2]), col = "purple", lwd = 2)
  legend(x = -0.02, y = 0.05, legend = c("all obs.", "w/o #2"), col=c("darkgreen", "purple"),
         lty = c(1, 1), lwd = c(1,2), bty="n", cex=0.75)
  
  cor(resid.log.b1, resid.log.l2)  #Another way to think of the partial correlation
  
  
  
#################################################################################
#Examine what happens to the partial correlation and the bootstrap variance when #2 is removed 
#  P. 116 discusses these values so I reproduced them here

  set.seed(6871)
  boot.res.wo2<-boot(data = log(frets[-2,]), statistic = calc.t, R = 999, sim = "ordinary")
  boot.res.wo2
  par(mfrow = c(1,1))
  jack.after.boot(boot.out = boot.res.wo2, index = 1, stinf = FALSE, main = "Figure 3.12")
 


#################################################################################
# Plot of equation 3.41 vs. empirical influence values (estimated with jackknife)
#   This could be generalized better into a function for future use, but it works
#   for now with this specific problem.  Be careful with using it for other problems!
#   The code was inspired by examining the code in jack.after.boot().

  t.bar.j<-numeric(n)
  t.star.bar.j<-numeric(n)
  for (j in 1:n) {
    freq<-boot.array(boot.res) 
    values<-boot.res$t[freq[, j] == 0]  #This is the key part
    t.star.bar.j[j]<-mean(values)
    t.bar.j[j]<-calc.t(log(frets[-j,]))
  }

  #I am not including the n constant in equation 3.41
  bj.b<-t.star.bar.j - t.bar.j - (mean(boot.res$t) - boot.res$t0)

  l.jack<-empinf(data = log(frets), statistic = calc.t, stype = "i", type = "jack") 

  plot(x = l.jack, y = bj.b, type = "n", main = "b_-j - b vs. jackknife estimated empirical influence values", 
       xlab = "jackknife estimated empirical influence values", ylab = "b_-j - b", col = "red")
  abline(h=0, col = "black", lty = "dotted")
  text(x = l.jack, y = bj.b, labels = 1:n, cex = 0.75, col = c("red", "blue", rep("red", n-2)))




#####################################################################
#Code from the help on jack.after.boot()

  pcorr <- function( x )
  { 
  #  function to find the correlations and partial correlations between
  #  the four measurements.
     v <- cor(x)
     v.d <- diag(var(x))
     iv <- solve(v)
     iv.d <- sqrt(diag(iv))
     iv <- - diag(1/iv.d) %*% iv %*% diag(1/iv.d)
     q <- NULL
     n <- nrow(v)
     for (i in 1:(n-1)) 
          q <- rbind( q, c(v[i,1:i],iv[i,(i+1):n]) )
     q <- rbind( q, v[n,] )
     diag(q) <- round(diag(q))
     q
  }

  frets.fun <- function( data, i ) {
     d <- data[i,]
     v <- pcorr( d )
     c(v[1,],v[2,],v[3,],v[4,])
  }


  frets.boot <- boot(log(as.matrix(frets)), frets.fun, R=999)
  #  we will concentrate on the partial correlation between head breadth
  #  for the first son and head length for the second.  This is the 7th
  #  element in the output of frets.fun so we set index=7
  jack.after.boot(frets.boot,useJ=FALSE,stinf=FALSE,index=7)
