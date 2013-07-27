################################################################
#  NAME: Chris Bilder                                          #
#  DATE: 2-4-06,                                               #
#  UPDATE: 10-24-07                                            #
#  PURPOSE: Example 4.5                                        #
#  NOTES:                                                      #
################################################################


################################################################
# The data

  library(boot)
  
  aircondit #Could also simply type in the data
  n<-nrow(aircondit)
  
  #MLEs for lognormal 
  alpha.hat<-mean(log(aircondit$hours))
  beta.hat<-sqrt((n-1)*var(log(aircondit$hours))/n)
  data.frame(alpha.hat, beta.hat)
  
 
  #This was my original way to find MLEs in Chapter 2.  
  #  See later code for a better way which forces kappa.hat and mu.hat > 0.  
  #  Note the function returns -log likelihood function for a gamma(alpha, beta)
   #-log likelihood function for a gamma(kappa, mu)
    logL<-function(par.gam, data) {

      kappa<-par.gam[1]
      mu<-par.gam[2]
      n<-length(data)
  
      #optim() finds a minimum so I need to put a negative here since want a max
      -(-n*lgamma(kappa) + n*kappa*log(kappa) - n*kappa*log(mu) - kappa/mu * sum(data) + (kappa-1)*sum(log(data)))

    }

    #MOM estimators - used as starting points
    par.gam<-c(mean(aircondit$hours)^2/var(aircondit$hours), mean(aircondit$hours))
    #Find MLEs (default method did not produce convergence)
    save.opt<-optim(par = par.gam, fn = logL, data = aircondit$hours, control=list(trace = 0, maxit=10000), method = "BFGS",
                    hessian = TRUE)
    save.opt
    kappa.hat<-save.opt$par[1]
    mu.hat<-save.opt$par[2]
    
    
 
##############################################################################
#  Take resamples and find mle* under H_o
#    I modified the gammaLoglik code from http://finzi.psych.upenn.edu/R/Rhelp02a/archive/23795.html
#    When I tried to use my own function (see above and example2.5_2.6... .R), I was having problems with
#    obtaining negative estimates of alpha for the resamples.  The gammaLoglik code solves the problem!  
#    As you can see, the key was to work with the log parameters and use exp( ) to get them back on the correct scale.

  gammaLoglik <- function(par.gam, data, negative=TRUE){ 
    logkappa <- par.gam[1]
    logmu <- par.gam[2]
    lglk <- sum(dgamma(data, shape=exp(logkappa), scale=exp(logmu-logkappa), log=TRUE)) 
    if(negative) return(-lglk) else return(lglk) 
  } 
  
  #Test evaluations of the gammaLoglik function 
  tst <- rgamma(n = 10, shape = 1)  
  gammaLoglik(par.gam = c(1, 1), data = tst) 
  gammaLoglik(par.gam = log(par.gam), data = aircondit$hours) 
  gammaLoglik(par.gam = log(c(kappa.hat, mu.hat)), data = aircondit$hours) 

  #Test it out on the observed data
  save.it<-optim(par = log(par.gam), fn = gammaLoglik, data = aircondit$hours,
               control=list(trace = 0, maxit = 10000), method = "BFGS", hessian = TRUE)
  save.it
  exp(save.it$par)
  kappa.hat<-exp(save.it$par)[1]
  mu.hat<-exp(save.it$par)[2]
  data.frame(kappa.hat, mu.hat)
  
  
  #Get estimated covariance matrix for parameter estimates using MLE theory
  #  In other words, find the inverse of the estimated Fisher information matrix
  #  Don't divide by n since already in there - see p. 128 of Ferguson (1996)
  #  since Fisher_n(theta) = n*Fisher_1(theta).  save.it$hessian is a numerical 
  #  evaluation of Fisher_n(theta).  
  cov.log.est.par<-solve(save.it$hessian)
  cov.log.est.par
  #This is using multivariate delta-method
  g.dot<-matrix(c(exp(save.it$par[1]),0,0,exp(save.it$par[2])), nrow = 2, ncol = 2)
  cov.mat<-g.dot %*% cov.log.est.par %*% g.dot
  sqrt(cov.mat[1,1])  #Estimated standard deviation of kappa.hat
  sqrt(cov.mat[2,2])  #Estimated standard deviation of mu.hat


  #Take resamples under H_o
  R<-999
  set.seed(6716)
  y0.star<-matrix(data = rgamma(n = n*R, shape = kappa.hat, scale = mu.hat/kappa.hat), nrow = R, ncol = n)
  y0.star[1,]    #r = 1


  #Function used when trying to find all of the mle* values
  find.est<-function(data, maxiter = 10000) {
     kappa.mom<-mean(data)^2/var(data) 
     mu.mom<-mean(data)
     par.gam<-c(kappa.mom, mu.mom)
    
     save<-optim(par = log(par.gam), fn = gammaLoglik, data = data,
                 control=list(trace = 0, maxit = maxiter), method = "BFGS", hessian = FALSE)

     alpha.hat.1<-mean(log(data))
     beta.hat.1<-sqrt((n-1)*var(log(data))/n)
  
     c(exp(save$par), save$convergence, alpha.hat.1, beta.hat.1)
   }
   
  
   #Find the mle* values
   par.est.star<-apply(X = y0.star, FUN = find.est, MARGIN = 1)
   par.est.star[,1:5]       #check first few - notice how row #1 is alpha.hat.0*, row #2 is beta.hat.0*, ...
   
   sum(par.est.star[1,]<0)  #check for negative estimates from iterative procedure
   sum(par.est.star[2,]<0)  #check for negative estimates from iterative procedure
   sum(par.est.star[3,])    #check for nonconvergence in iterative procedure
   
   par(mfrow = c(1,2))
   hist(par.est.star[1,], main = "Histogram for kappa.mle*", freq=FALSE, xlab = "kappa.mle*") 
   curve(expr = dnorm(x, mean = kappa.hat, sd = sqrt(cov.mat[1,1])), col = 2, add = TRUE) 
   #May be reasonable as well to use the mean and sd of all of the kappa.mle* for the normal approximation above
   #  I am just using results from the asymptotic normality of MLEs

   hist(par.est.star[2,], main = "Histogram for mu.mle*", freq=FALSE, xlab = "mu.mle*")
   curve(expr = dnorm(x, mean = mu.hat, sd = sqrt(cov.mat[2,2])), col = 2, add = TRUE) 

   #Normal approximation put on plots just for fun :)
   #  Why are the normal approximations not good?  Remember that I am using the asymptotic distribution
   #  for MLEs here.  My sample size is only 12 for each parameter estimate* calculated
   
   
   par(mfrow = c(1,2))
   hist(par.est.star[4,], main = "Histogram for alpha.mle*", freq=FALSE, xlab = "alpha.mle*")
   hist(par.est.star[5,], main = "Histogram for beta.mle*", freq=FALSE, xlab = "beta.mle*")
    


###############################################################################
# Find test statistic and p-value
 
  calc.t<-function(all, n) {
    data<-all[1:n]
    kappa<-all[n+1]
    mu<-all[n+2]
    alpha<-all[n+3]
    beta<-all[n+4]
       
    #Note: This is 1/n * sum( log(f_0) - log(f_1)) which has f_0 and f_1 reversed from BMA.
    #t<-1/n * (sum(dgamma(x = data, shape = alpha0, scale = beta0, log = TRUE))  -
    #          sum(dlnorm(x = data, meanlog = alpha1, sdlog = beta1, log = TRUE)))
  
    t<-1/n * (sum(dlnorm(x = data, meanlog = alpha, sdlog = beta, log = TRUE)) -
              sum(dgamma(x = data, shape = kappa, scale = mu/kappa, log = TRUE)))           
    lrt.usual.form<--2*(sum(dgamma(x = data, shape = kappa, scale = mu/kappa, log = TRUE))  -
                        sum(dlnorm(x = data, meanlog = alpha, sdlog = beta, log = TRUE)))
    c(t, lrt.usual.form)
  }
 
    
  #Need to put resamples and parameter estimates into one data set for function
  each0<-cbind(y0.star, t(par.est.star[-3,]))
  head(each0)
  
  #Find t and t.star
  t.star<-apply(X = each0, FUN = calc.t, MARGIN = 1, n = n)
  t<-calc.t(all = c(aircondit$hours, kappa.hat, mu.hat, alpha.hat, beta.hat), n = n)
  
  
  #Histogram in Figure 4.2
  par(mfrow = c(1,1))
  hist(x = t.star[1,], main = "Histogram for t*, R=999", xlab = "t*", freq = FALSE)   
  abline(v = t[1], col = "red", lwd = 5)
  text(x = t[1]+0.01, y = -0.05, labels = "t")

  #P-value 
  (1 + sum(t.star[1,]>t[1]))/(R + 1)


  #P-value 
  (1 + sum(t.star[2,]>t[2]))/(R + 1)



  #Work used to figure out how to calculate f_1  
  #dlnorm(x = aircondit$hours, meanlog = alpha.hat.1, sdlog = beta.hat.1, log = FALSE)
  #1/(beta.hat.1*aircondit$hours) * dnorm(log(aircondit$hours), alpha.hat.1, beta.hat.1)
  #1/(beta.hat.1*aircondit$hours) * dnorm((log(aircondit$hours)- alpha.hat.1)/beta.hat.1, mean = 0, sd = 1)
  
  
  
  
#################################################################################
#  Show how MLEs can be found numerically for f_1
#    This is not used anywhere else in the program and is only presented
#    for illustrative purposes. 

   logL2<-function(par.gam, data) {

      alpha<-par.gam[1]
      beta<-par.gam[2]
      n<-length(data)
  
      #optim() finds a minimum so I need to put a negative here since want a max
      -sum(dlnorm(x = data, meanlog = alpha, sdlog = beta, log = TRUE))

    }

    #Find MLEs 
    save.opt<-optim(par = c(alpha.hat.1, beta.hat.1), fn = logL2, data = aircondit$hours, 
                    control=list(trace = 0, maxit=10000), method = "BFGS",)
    save.opt
 
 
  
######################################################################################
#  Code can be modified below to do the shading of the histogram in Figure 4.2
#    This is not used anywhere else in the program and is only presented
#    for illustrative purposes. 
 
  par(mfrow = c(1,1))
  save.hist<-hist(x = t.star, main = "Histogram for t*, R=999", xlab = "t*", freq = FALSE)   
  
  #Shade under histogram - found code on R listserv for confshade
  confshade <- function(x,ylo,yhi,col=8) {
    n <- length(x)
    for (i in 1:(n-1)) {
      polygon(c(x[i],x[i+1],x[i+1],x[i]),c(ylo[i],ylo[i+1],yhi[i+1],yhi[i]),
        col=col,border=F)
    }
  }
  
  #This does whole histogram
  confshade(x =  c(save.hist$breaks[1], rep(save.hist$breaks[2:(length(save.hist$breaks)-1)], each = 2), 
                   save.hist$breaks[length(save.hist$breaks)]), 
            ylo = rep(0, times = length(save.hist$density), each = 2), 
            yhi = rep(save.hist$density, each = 2), col = "red")
  
  
  
  
##############################################################################
#  BMA's estimation method for kappa using their gamma definition. 
#    This is not used anywhere else in the program and is only presented
#    for illustrative purposes. 

  find.kappa<-function(kappa, data) {
     log(kappa) - digamma(kappa) - log(mean(data)) + mean(log(data))
   }

  save<-uniroot(find.kappa, c(0.00001, 10000), data = aircondit$hours)
  kappa<-save$root   
  save
  






#
