#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  2-12-06                                                    #
# UPDATE:                                                           #
# PURPOSE: Example 4.16 with the exponential tilts only             #
#                                                                   #
# NOTES: See p. 535-536 for BMA's code                              #
#####################################################################


#####################################################################
# Read in data and prepare it for the calculations

  library(boot)
  
  #BMA call this "z".  I changed the name to grav.z to avoid confusion with a studentized quantity  
  grav.z<-grav$g  #grav includes only series 7 and 8 
  grav.z
  
  #Whereever you see series = 8, replace with its negative values
  #  Reason: sum(y_1j * p_1j) - sum(y_2j * p_2j) = 0; notice the minus in there
  grav.z[grav$series==8]<--grav.z[grav$series==8]  

  #The help for exp.tilt() says L is the empirical influence function values
  #  BMA in their code on p. 535-6 simply use the data set for L
  #  At the end of this program, I tried putting the empirical influence function values in for 
  #  L, but the function did not produce the correct results
  grav.z.tilt<-exp.tilt(L=grav.z, theta=0, strata=grav$series)
  grav.z.tilt

  #Verify sum to one for each series
  sum(grav.z.tilt$p[grav$series==7])
  sum(grav.z.tilt$p[grav$series==8])
  
  #Verify that Ho is satisfied; subtract the two below to get 0 (remember that we 
  #  already did the minus part for the series = 8)
  sum(grav.z.tilt$p[grav$series==7]*grav.z[grav$series==7])
  sum(grav.z.tilt$p[grav$series==8]*grav.z[grav$series==8])
  
  
  # Figure 4.11 - left plot
  par(mfrow = c(1,2), pty = "s", xaxs = "r")
  plot(x = grav$g, y = grav.z.tilt$p, main = "Exponential tilt probabilities", ylab = "Null probabilities", 
       xlab = "y", type = "n", ylim = c(0,0.2), xlim = c(60,90), panel.first = grid(nx = NULL, ny = NULL, 
       col="gray", lty="dotted"))
  text(x = grav$g, y = grav.z.tilt$p, labels = as.numeric(grav$series) - 6, cex = 0.75)
  points(x = grav$g, y = rep(0, length(grav$g)), pch = 3) 
 
  # Figure 4.11 - right plot - population #2 line is not quite the same as in BMA (they do not give information
  #   about the type of density estimation performed so I may be using a different method)
  plot(density(x = grav$g[grav$series==7], weights = grav.z.tilt$p[grav$series==7]), lty = "dotted", col = "red", 
       main = "Kernel density estimates", xlim = c(60, 90), xlab = "y", ylab = "Null density")
  lines(density(x = grav$g[grav$series==8], weights = grav.z.tilt$p[grav$series==8]), lty = "dashed", col = "blue")
  lines(density(x = grav$g, weights = grav.z.tilt$p/2), lty = "solid", col = "darkgreen")
  legend(x = 60, y = 0.15, legend = c("pop 1", "pop 2", "pooled"), lty = c("dotted", "dashed", "solid"), 
         col = c("red", "blue", "darkgreen"), cex = 0.75)
   
    
    
#####################################################################
# Perform the bootstrap


  grav.test<-function(data, i, p.ij.0) { 
    d<-data[i,]
    
    #BMA's code - Provides a good example of needing to figure out different types of coding (see below)
    t<-diff(tapply(d$g,d$series,mean))[7]  
    
    #My code to find variance and sample size for each group
    n<-tapply(X = d$g, INDEX = d$series, FUN = length)[7:8]   
    var.ybar<-tapply(X = d$g, INDEX = d$series, FUN = var)[7:8] 
    
    #Unbiased variance estimator - could have used npar delta method variance as well
    v<-var.ybar[1]/n[1] + var.ybar[2]/n[2]  
    
    #Variance under Ho - remember that values of y1 and y2 are chosen with respect to p.ij.0 probabilities
    #  already so it may be appropriate to estimate each group mean by mean(y1, y2) and calculate the 
    #  variances the usual way with these probabilities in mind
    y1<-d$g[d$series == 7]
    y2<-d$g[d$series == 8]
    v.o.1<-sum(1/n[1] * sum((y1 - mean(y1, y2))^2)/(n[1] - 1) + 1/n[2] * sum((y2 - mean(y1, y2))^2)/(n[2] - 1)) 

    #Calculate Example 4.20 variance
    mu.hat1.0<-sum(y1*p.ij.0[d$series == 7])
    mu.hat2.0<-sum(y2*p.ij.0[d$series == 8])
    
    v.0.2<-sum( sum((y1 - mu.hat1.0)^2 * p.ij.0[d$series == 7])/n[1] + 
                sum((y2 - mu.hat2.0)^2 * p.ij.0[d$series == 8])/n[2] )
    
    c(t, v, v.o.1, v.0.2)
  }


  #Test part of the code
  grav.test(data = grav, i = 1:nrow(grav), p.ij.0 = grav.z.tilt$p)
  tapply(grav$g, grav$series, mean)        #Finds means for each group
  t<-diff(tapply(grav$g, grav$series, mean))[7]  #diff returns x_t - x_t-1 (first order differences) normally
  n<-tapply(X = grav$g, INDEX = grav$series, FUN = length)[7:8]   
  var.t<-tapply(X = grav$g, INDEX = grav$series, FUN = var)[7:8] 
  v<-var.t[1]/n[1] + var.t[2]/n[2]  #could have used npar delta method as well
  z<-(t-0)/sqrt(v)
  z
  
  
  set.seed(7815)
  grav.boot<-boot(data=grav, statistic=grav.test, R=999, weights=grav.z.tilt$p, strata=grav$series, p.ij.0 = grav.z.tilt$p)
  plot(grav.boot)

  #P-value - using t - remember this is a one-sided test
  (sum(grav.boot$t[,1]>=grav.boot$t0[1])+1)/(grav.boot$R+1)
  
  
  #P-value - using different studentized methods - remember this is a one-sided test
  #  This z.star method here is similar to equations 16.6 and 16.7 in Efron and Tibshirani (1993, p. 224)
  z.star.0<-(grav.boot$t[,1]-0)/sqrt(grav.boot$t[,2])
  z.0<-(grav.boot$t0[1]-0)/sqrt(grav.boot$t0[2])
  (sum(z.star.0>=z.0)+1)/(grav.boot$R+1)
  plot(grav.boot, t0=z.0, t=z.star.0)  #quick way to construct a plot
  
  z.star1.0<-(grav.boot$t[,1]-0)/sqrt(grav.boot$t[,3])
  z1.0<-(grav.boot$t0[1]-0)/sqrt(grav.boot$t0[3])
  (sum(z.star1.0>=z1.0)+1)/(grav.boot$R+1)
  #plot(grav.boot, t0=z1.0, t=z.star1.0)

  z.star2.0<-(grav.boot$t[,1]-0)/sqrt(grav.boot$t[,4])
  z2.0<-(grav.boot$t0[1]-0)/sqrt(grav.boot$t0[4])
  (sum(z.star2.0>=z2.0)+1)/(grav.boot$R+1)
  #plot(grav.boot, t0=z2.0, t=z.star2)

    

  #Additional set of plots
  par(mfrow = c(1,2), pty = "s", xaxs = "i")
  
  #Histogram
  hist(z.star.0, main = expression(paste("Histogram of ", z[0]^{"*"})), xlab=expression(z[0]^{"*"}), freq = FALSE)
  abline(v = z.0, col = "darkgreen", lwd = 5)
  curve(dnorm(x, mean = mean(z.star), sd = sd(z.star)), col = "red", add = TRUE)

  #EDF
  plot.ecdf(z.star.0, verticals = TRUE, do.p = FALSE, main = expression(paste("EDF for ", z[0]^{"*"})), lwd = 2,
            panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"), ylab = "Estimated CDF", xlab = expression(z[0]^{"*"}))
  curve(expr = pnorm(x, mean(z.star.0), sd = sd(z.star.0)), col = "red", add = TRUE)


  #Look at histograms and EDFs of the other Z*_0 as well



############################################################################################
#The documentation in exp.tilt is a little confusing since it says the L option should be the 
#  empirical influence values.  Here's what happens when I do this.  
  l.jack<-empinf(data = grav, statistic = grav.test, stype = "i", type = "jack", strata = grav$series)
  l.jack 

  #Produces all equal p_ij 
  temp.tilt<-exp.tilt(L=l.jack, theta=0, strata=grav$series)
  temp.tilt
  z.tilt<-exp.tilt( L=z, theta=0, strata=grav$series)
  z.tilt


 
    
############################################################################################
# My own try of finding p_ij - Try with log for p_ij

lagrange.expr<-function(par.f, d, p.hat.1, p.hat.2) {
   
    n1<-length(d$g[d$series==7])
    n2<-length(d$g[d$series==8])
    p.1<-exp(par.f[1:n1])
    p.2<-exp(par.f[(n1+1):(n1+n2)])
    lambda<-par.f[n1+n2+1]
    alpha1<-par.f[n1+n2+2]
    alpha2<-par.f[n1+n2+3]
    
    kl.dist<-sum(sum(p.1*log(p.1/p.hat.1)), sum(p.2*log(p.2/p.hat.2)))
    expr<-kl.dist - lambda*(sum(d$g[d$series==7]*p.1) - sum(d$g[d$series==8]*p.2)) 
                  - alpha1*(sum(p.1)-1) - alpha2*(sum(p.2)-1)
    expr 
    
  }  
    
  #Initial estimators - used as starting points
  par.init<-c(rep(x = log(1/length(grav$g[grav$series==7])), times = length(grav$g[grav$series==7])), 
              rep(x = log(1/length(grav$g[grav$series==8])), times = length(grav$g[grav$series==8])), 
              0, 0, 0)
  p.hat.1<-rep(x = 1/length(grav$g[grav$series==7]), times = length(grav$g[grav$series==7]))
  p.hat.2<-rep(x = 1/length(grav$g[grav$series==8]), times = length(grav$g[grav$series==8]))


  #Test function - should be 0 since set p_ij = p^_ij
  lagrange.expr(par.f = par.init, d = grav, p.hat.1 = p.hat.1, p.hat.2 = p.hat.2)

  save.opt<-optim(par = par.init, fn = lagrange.expr, d = grav, p.hat.1 = p.hat.1, p.hat.2 = p.hat.2,
                  control=list(trace = 0, maxit=10000), method = "BFGS", hessian = FALSE)
  save.opt
  exp(save.opt$par)[-(27:29)]
    



############################################################################################
# My own try of finding p_ij - w/o log fix

lagrange.expr<-function(par.f, d, p.hat.1, p.hat.2) {
   
    n1<-length(d$g[d$series==7])
    n2<-length(d$g[d$series==8])
    #cat("\n", n1, "\n", n2)
    p.1<-par.f[1:n1]
    p.2<-par.f[(n1+1):(n1+n2)]
    lambda<-par.f[n1+n2+1]
    alpha1<-par.f[n1+n2+2]
    alpha2<-par.f[n1+n2+3]
    
    kl.dist<-sum(sum(p.1*log(p.1/p.hat.1)), sum(p.2*log(p.2/p.hat.2)))
    expr<-kl.dist - lambda*(sum(d$g[d$series==7]*p.1) - sum(d$g[d$series==8]*p.2)) 
                  - alpha1*(sum(p.1)-1) - alpha2*(sum(p.2)-1)
    expr 
    
  }  
    
  #Initial estimators - used as starting points
  par.init<-c(rep(x = 1/length(grav$g[grav$series==7]), times = length(grav$g[grav$series==7])), 
              rep(x = 1/length(grav$g[grav$series==8]), times = length(grav$g[grav$series==8])), 
              0, 0, 0)
  p.hat.1<-rep(x = 1/length(grav$g[grav$series==7]), times = length(grav$g[grav$series==7]))
  p.hat.2<-rep(x = 1/length(grav$g[grav$series==8]), times = length(grav$g[grav$series==8]))


  #Test function
  lagrange.expr(par.f = par.init, d = grav, p.hat.1 = p.hat.1, p.hat.2 = p.hat.2)

  save.opt<-optim(par = par.init, fn = lagrange.expr, d = grav, p.hat.1 = p.hat.1, p.hat.2 = p.hat.2,
                  control=list(trace = 0, maxit=10000), method = "Nelder-Mead", hessian = FALSE)
  save.opt
  #Changed method from BFGS 


#
