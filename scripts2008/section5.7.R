#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  2-25-06                                                    #
# UPDATE: 11-3-07                                                   #
# PURPOSE: Section 5.7 simulations                                  #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#####################################################################
# Simulate the data and other settings

  theta<-100/50

  set.seed(8910)
  numb<-10000
  y1<-matrix(data = rgamma(n = 25*numb, shape = 0.7,  scale = 100/0.7), nrow = numb, ncol = 25)
  y2<-matrix(data = rgamma(n = 25*numb, shape = 1,  scale = 50), nrow = numb, ncol = 25)
  
  ybar1<-apply(X = y1, FUN = mean, MARGIN = 1)
  ybar2<-apply(X = y2, FUN = mean, MARGIN = 1)
  t<-ybar1/ybar2

  hist(t, xlab = "t")

  R<-999

###################################################################
# Range of error rates expected if method is working correctly

  alpha.all<-c(0.01, 0.025, 0.05, 0.10)
  lower<-round(alpha.all-qnorm(0.995)*sqrt(alpha.all*(1-alpha.all)/numb),4)
  upper<-round(alpha.all+qnorm(0.995)*sqrt(alpha.all*(1-alpha.all)/numb),4)
  data.frame(alpha.all, lower, upper)
  
  

###################################################################
# Normal approximation


  gammaLoglik <- function(par.gam, data, negative=TRUE){ 
    logkappa <- par.gam[1]
    logmu <- par.gam[2]
    lglk <- sum(dgamma(data, shape=exp(logkappa), scale=exp(logmu-logkappa), log=TRUE)) 
    if(negative) return(-lglk) else return(lglk) 
  } 
    
    
  #Function used when trying to find all of the mle values
  find.est<-function(data, maxiter = 10000) {
     kappa.mom<-mean(data)^2/var(data) 
     mu.mom<-mean(data)  
     par.gam<-c(kappa.mom, mu.mom) 
     save<-optim(par = log(par.gam), fn = gammaLoglik, data = data,
                 control=list(trace = 0, maxit = maxiter), method = "BFGS", hessian = FALSE)
 
     c(exp(save$par), save$convergence)
   }
   
  
   #Find the mle values for sample 1
   par.est1<-apply(X = y1, FUN = find.est, MARGIN = 1)
   par.est1[,1:5]  
   kappa1.hat<-par.est1[1,]           
   mu1.hat<-par.est1[2,] 
   sum(par.est1[3,])    #check for nonconvergence in iterative procedure

   #Find the mle values for sample 2
   par.est2<-apply(X = y2, FUN = find.est, MARGIN = 1)
   par.est2[,1:5]  
   kappa2.hat<-par.est2[1,]           
   mu2.hat<-par.est2[2,] 
   sum(par.est2[3,])    #check for nonconvergence in iterative procedure


   #C.I. - Compare to lower and upper limit 5% columns on p. 231 of BMA
   n.eq<-25 #equal sample sizes here
   v.asym<-mu1.hat^2/mu2.hat^2 * (1/kappa1.hat + 1/kappa2.hat)*1/n.eq
   lower.norm<-t - qnorm(1-0.05)*sqrt(v.asym)
   upper.norm<-t - qnorm(0.05)*sqrt(v.asym)
  
   #Miss lower
   miss.norm.lower<-lower.norm>theta
   mean(miss.norm.lower)
   #Miss upper
   miss.norm.upper<-upper.norm<theta
   mean(miss.norm.upper)
   #Length - use for Figure 5.7 later
   length.norm<-upper.norm-lower.norm
   mean(length.norm)

  
  
  #Just checking some calculations here:
  t[1]
  mu1.hat[1]
  mu2.hat[1]
  kappa1.hat[1]
  kappa2.hat[1]
  
  #95% C.I. for first simulated data set
  1.95997-1.96*108.4162/55.31646*sqrt(1/1.528326+1/1.207891)*sqrt(1/25)
  1.95997+1.96*108.4162/55.31646*sqrt(1/1.528326+1/1.207891)*sqrt(1/25)

  
###################################################################
# Basic boot.

  library(boot)

  calc.t<-function(data, i) {
    d<-data[i,]
    y.mean<-tapply(X = d$y, INDEX = d$pop, FUN = mean)
    t<-y.mean[1]/y.mean[2]
    t 
  }

  #FIRST, TRY FOR FIRST SIMULATED SET OF DATA SETS
  
  #Organize data
  set1<-rbind(data.frame(y = y1[1,], pop = 1), data.frame(y = y2[1,], pop = 2))
  head(set1)
  tail(set1)
  
  #Try it
  calc.t(data = set1, i = 1:nrow(set1))
 
  set.seed(1891)
  alpha<-0.05  
  boot.res<-boot(data = set1, statistic = calc.t, R = R, sim="ordinary", strata = set1$pop) 
  boot.ci(boot.out = boot.res, conf = 1-alpha, type = "basic")$basic[4:5]
  
 
  #NEXT, TRY FOR 1,000 SIMULATED DATA SETS (used only 1,000 because 10,000 would take 4.2 hours on my laptop)
  
  set.seed(1891)
  save.basic<-matrix(data = NA, nrow = 1000, ncol = 2)
  start.time<-proc.time()    #Find start time
  #Do for all simulated data sets - probably could reprogram it using apply() function
  for (i in 1:1000) { 
 
    set1<-rbind(data.frame(y = y1[i,], pop = 1), data.frame(y = y2[i,], pop = 2))
    boot.res<-boot(data = set1, statistic = calc.t, R = R, sim="ordinary", strata = set1$pop) 
    save.basic[i,]<-boot.ci(boot.out = boot.res, conf = 1-0.10, type = "basic")$basic[4:5]
  
  }

  #Find end time and total time elapsed
  end.time<-proc.time()
  save.time<-end.time-start.time
  cat("\n Number of minutes running:", save.time[3]/60, "\n \n")


  #Miss lower
  miss.basic.lower<-save.basic[,1]>theta
  mean(miss.basic.lower)
  #Miss upper
  miss.basic.upper<-save.basic[,2]<theta
  mean(miss.basic.upper)
  #Length - use for Figure 5.7 later
  length.basic<-save.basic[,2] - save.basic[,1]
  mean(length.basic)
  
  #Projected time for all 10,000 is 4.2 hours on old Acer Tablet PC
  #  Do 1,000 instead and get done in 0.42 hours - took 25.22333 minutes



######################################################################################
# Part of Figure 5.7

  ci.length<-rbind(data.frame(length = length.norm, name = "normal"),
                   data.frame(length = length.basic, name = "basic"))
  par(mfrow = c(1,2))
  boxplot(formula = length ~ name, data = ci.length, col = "lightblue", main = "Box plot",
          ylab = "length", xlab = "Method")
  
  #Type of syntax not same as boxplot()
  stripchart(ci.length$length ~ ci.length$name, method = "jitter", vertical = TRUE, pch = 1, main = "Dot plot",
             ylab = "length", xlab = "Method")  
  
  
   
   
   
#
