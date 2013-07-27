#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  2-24-06                                                    #
# UPDATE: 11-2-07                                                   #
# PURPOSE: Example 5.1, 5.2, 5.4, 5.8                               #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#####################################################################
# Get the data

  library(boot)
  
  y<-aircondit$hours 
  n<-length(y)
  t<-mean(y)
  
  #Normal app. (same interval as with the bootstrap bias adjustment and variance estimate)
  low.norm<-t - qnorm(0.975)*sqrt(t^2/n)
  up.norm<-t + qnorm(0.975)*sqrt(t^2/n)
  # Could also use t - qnorm(0.025)*sqrt(t^2/n) for up.norm

  #Remember that R has a different definition of a gamma PDF than BMA
  low.exact<-t/qgamma(1-0.025, shape = n, scale = 1/n)
  up.exact<-t/qgamma(0.025  , shape = n, scale = 1/n)

  #Regular t-distribution interval; remember that t is mean(y) here
  lower.t<-t - qt(0.975, n-1)*sd(y)/sqrt(n) 
  upper.t<-t + qt(0.975, n-1)*sd(y)/sqrt(n)

  data.frame(lower = c(low.norm, low.exact, lower.t), 
             upper = c(up.norm, up.exact, upper.t))
  
  #Variance stabilized, normal app. 
  low.stab.norm<-exp(log(t) - qnorm(0.975)*sqrt(1/n))
  up.stab.norm<-exp(log(t) + qnorm(0.975)*sqrt(1/n))
  data.frame(lower = low.stab.norm, upper = up.stab.norm)
    
  #Bias adjusted, var. stab. normal app.
  #  This is from Maple - Could have also used results from integrate() function as shown later
  E.logT.star<--0.042246+log(t)  
  E.logT.star
  b<-E.logT.star - log(t)
  b
  Var.logT.star<-0.08690 #Can also use psigamma(12, deriv = 1) 
  low.stab.norm.boot<-exp(log(t) - b - qnorm(0.975)*sqrt(Var.logT.star))
  up.stab.norm.boot<-exp(log(t) - b - qnorm(0.025)*sqrt(Var.logT.star))
  data.frame(lower = low.stab.norm.boot, upper = up.stab.norm.boot)

 
  #Basic boot. limits - note that BMA actually take resamples to obtain
  #  t*_((999+1)*0.025) = 53.3 and t*_((999+1)*0.975) = 176.5, but this is not
  #  necessary since T*~Gamma(n, t) using BMA notation
  #  Thus, I just use the gamma distribution to get very similar values
  t.star.quant<-qgamma(p = c(0.025, 0.975), shape = n, scale = t/n)
  low.basic<-2*t - t.star.quant[2]
  up.basic<-2*t - t.star.quant[1]
  data.frame(lower = low.basic, upper = up.basic)

  #Basic boot limits on log-scale - note that the transformation
  #  does not cause a change in the T* quantiles other than log( ) of them
  low.tran.basic<-exp(2*log(t) - log(t.star.quant[2]))
  up.tran.basic<-exp(2*log(t) - log(t.star.quant[1]))
  data.frame(lower = low.tran.basic, upper = up.tran.basic)
  
  

#####################################################################
# Demonstrate how to do some integrations needed for parametric bootstrap
#   Note that T* ~ Gamma(n, t) since T ~ Gamma(n, mu) using BMA notation.
#   Use this information to find E(log(T*)) and Var(log(T*)).
#   In the end, I do similar calculations in Maple and use those results 
#   which provide a little different variance.
  
  f.t.star<-function(t.star, n, mu.star) {
    #mu.star = t
    (n/mu.star)^n / gamma(n) * t.star^(n-1) * exp(-n*t.star/mu.star)
  }
  
  #Integrate() does adaptive quadrature
  #  Check to make sure integrate to 1
  integrate(f.t.star, 0, Inf, subdivisions = 1000, n = n, mu.star = t)

  
  f.t.star1<-function(t.star, n, mu.star) {
    log(t.star) * (n/mu.star)^n / gamma(n) * t.star^(n-1) * exp(-n*t.star/mu.star)
  }
  
  #E*(log(T*))
  e1<-integrate(f.t.star1, 0, Inf, subdivisions = 1000, n = n, mu.star = t)
  e1


  f.t.star2<-function(t.star, n, mu.star) {
    log(t.star)^2 * (n/mu.star)^n / gamma(n) * t.star^(n-1) * exp(-n*t.star/mu.star)
  }
  
  #E*(log(T*)^2)
  e2<-integrate(f.t.star2, 0, Inf, subdivisions = 1000, n = n, mu.star = t)
  e2
  
  
  #Var(log(T*))
  e2$value - e1$value^2
  
 

#####################################################################
#Npar boot for some calculations need later

  calc.t<-function(data, i) {
     d<-data[i]
     n<-length(d)
     v.L<-1/n^2*(n-1)*var(d)
     t<-mean(d)
     c(t, v.L) 
  }
  
  #Try it
  calc.t(data = y, i = 1:length(y))
  
  #Do bootstrap
  set.seed(9182)  #Same as in Chapter 2
  R<-999
  boot.res<-boot(data = y, statistic = calc.t, R = R, sim="ordinary") 
  #plot(boot.res)   




#####################################################################
# Example 5.2

  v.L<-1/n^2*(n-1)*var(y)
  v.L  #Matches top of p. 200
  
  #Normal app.
  low.norm<-t - qnorm(0.975)*sqrt(v.L)
  up.norm<-t + qnorm(0.975)*sqrt(v.L)

  #Quantiles of t*
  quantile(x = boot.res$t[,1], probs = c(0.025, 0.975), type = 1)
  
  #Quantiles used in studentized
  quantile(x = (boot.res$t[,1]-boot.res$t0[1])/sqrt(boot.res$t[,2]), probs = c(0.025, 0.975), type = 1)

  #Basic and studentized
  save.ci<-boot.ci(boot.out = boot.res, conf = 0.95, type = c("norm", "basic", "stud"), 
                   var.t0 = boot.res$t0[2], var.t = boot.res$t[,2]) 
  save.ci


  #BMA mention a 99% basic interval - see "-27.3" value in book
  boot.ci(boot.out = boot.res, conf = 0.99, type = c("basic"), var.t0 = boot.res$t0[2], var.t = boot.res$t[,2]) 


  #Basic and studentized using log transformation
  hdot.func<-function(u) {
    1/u
  }
  save.tran.ci<-boot.ci(boot.out = boot.res, conf = 0.95, type = c("norm", "basic", "stud"), 
                   var.t0 = boot.res$t0[2], var.t = boot.res$t[,2],
                   h = log, hinv = exp, hdot = hdot.func) 
  save.tran.ci
  #Basic trans does not match BMA interval of (66.2, 218.8)

  name<-c("normal app.", "Basic", "Studentized", "Basic trans", , "Studentized trans")
  lower<-c(low.norm, save.ci$basic[4], save.ci$stud[4], save.tran.ci$basic[4], save.tran.ci$stud[4])
  upper<-c(up.norm, save.ci$basic[5], save.ci$stud[5], save.tran.ci$basic[5], save.tran.ci$stud[5])
  data.frame(name, lower, upper)


  #Figure 5.1
  par(mfrow = c(1,2))
  plot(x = boot.res$t[,1], y = sqrt(boot.res$t[,2]), xlab = "t*", ylab = "sqrt(V.L*)")
  plot(x = log(boot.res$t[,1]), y = sqrt(boot.res$t[,2])/boot.res$t[,1], xlab = "log(t*)", ylab = "sqrt(v.L*)/t*")
  #Why plot sqrt(v.L*)/t* on the y-axis?  Think of what happens with a variance stabilizing transformation
  #  Derivative of log(mu) us 1/mu.  Thus, the original variance is v, say, gets multiplied by 1/mu^2 (remember this is
  #  is squared in the delta-method).  If you are not comfortable with this, one could use a double boot procedure
  #  to calculate the variance of log(t*) for each re-resample



#####################################################################
# Example 5.4

  #Percentile interval - nonparametric
  boot.ci(boot.out = boot.res, conf = 0.95, type = c("perc")) 
  quantile(x = boot.res$t[,1], probs = c(0.025, 0.975), type = 1)

  #Percentile interval - parametric (Y~Exp), remember that T ~ Gamma(n, mu) and T* ~ Gamma(n, t) using BMA notation
  qgamma(p = c(0.025, 0.975), shape = n, scale = t/n)
  #Does not match with p. 203 - However, notice p. 197 does give 25th and 975th percentiles of 53.3 and 176.4 found through R resamples!



#####################################################################
# Example 5.8

  boot.ci(boot.out = boot.res, conf = 0.95, type = c("bca")) 
  
  #Empirical influence values
  l.j<-y - mean(y)  
  
  #Empirical influence values estimated by the jackknife (of course, these would   
  #  be the same here - see Chapter 2) 
  l.jack<-empinf(data = y, statistic = calc.t, stype = "i", type = "jack") 

  #Acceleration 
  a<-1/6 * sum(l.j^3)/sum(l.j^2)^(3/2)
  a.jack<-1/6 * sum(l.jack^3)/sum(l.jack^2)^(3/2)
  
  data.frame(a, a.jack)

  #Bias correction
  sum(boot.res$t[,1]<=boot.res$t0[1])/(R+1)
  w<-qnorm(p = sum(boot.res$t[,1]<=boot.res$t0[1])/(R+1))
  w
  
  #Note that BMA get a w = 0.0728
  pnorm(q = 0.0728)
  #[1] 0.5290174

  
  #BCa calculations in Table 5.4
  alpha<-c(0.025, 0.975, 0.05, 0.95)
  z.tilde = w + qnorm(p = alpha)
  alpha.tilde<-pnorm(q = w + z.tilde/(1-a*z.tilde))
  r<-(R+1)*alpha.tilde
  r
 
  #Note that r will need to be an integer or the quantile function can be used as below
  #quantile(x = boot.res$t[,1], probs = alpha.tilde, type = 1) #Equivalent to using ceiling function
  limit.ceil<-sort(boot.res$t[,1])[ceiling(r)]
  limit.floor<-sort(boot.res$t[,1])[floor(r)]
  #Alternatively, interpolation could be used as outlined on p. 195 of BMA - see use of norm.inter.mine function below
  
  #This is Table 5.4 - of course, there are differences since I used different resamples
  #Additional reasons for small differences between the limits here and those 
  #  produced by boot.ci() is that I used the ceiling and floor function instead 
  #  of interpolation when finding the quantiles.  One way to have smaller 
  #  differences is to just take a larger number of resamples!
  data.frame(alpha, z.tilde, alpha.tilde, r,  limit.ceil, limit.floor)



#FROM boot package (remember Chapter 2 discussion about hidden functions)
#  I renamed norm.inter, norm.inter.mine

norm.inter.mine <- function(t,alpha)
#  
#  Interpolation on the normal quantile scale.  For a non-integer
#  order statistic this function interpolates between the surrounding
#  order statistics using the normal quantile scale.  See equation
#  5.8 of Davison and Hinkley (1997)
#
{   R <- length(t)
   rk <- (R+1)*alpha
   if (!all(rk>1 & rk<R))
       warning("Extreme Order Statistics used as Endpoints")
   k <- trunc(rk)
   inds <- 1:length(k)
   out <- inds
   kvs <- k[k>0 & k<R]
   tstar <- sort(t,partial=sort(union(c(1,R),c(kvs,kvs+1))))
   ints <- (k == rk)
   if (any(ints)) out[inds[ints]] <- tstar[k[inds[ints]]]
   out[k==0] <- tstar[1]
   out[k==R] <- tstar[R]
   not <- function(v) xor(rep(T,length(v)),v)
   temp <- inds[not(ints) & k!=0 & k!=R]
   temp1 <- qnorm(alpha[temp])
   temp2 <- qnorm(k[temp]/(R+1))
   temp3 <- qnorm((k[temp]+1)/(R+1))
   tk <- tstar[k[temp]]
   tk1 <- tstar[k[temp]+1]
   out[temp] <- tk + (temp1-temp2)/(temp3-temp2)*(tk1 - tk)
   cbind(round(rk,2),out)
}    
        
norm.inter.mine(t = boot.res$t[,1], alpha = r[1]/999)
