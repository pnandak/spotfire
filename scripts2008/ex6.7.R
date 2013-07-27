########################################################################
# NAME:  Chris Bilder                                                  #
# DATE:  3-12-06                                                       #
# UPDATE:                                                              #
# PURPOSE: Example 6.7                                                 #
#                                                                      #
# NOTES:                                                               #
########################################################################

########################################################################
# Get the data and do some initial examinations

  library(boot)

  head(rock)
  n<-nrow(rock)
  
  mod.fit<-lm(log(perm) ~ area + peri + shape, data = rock)
  sum.fit<-summary(mod.fit)
  sum.fit
  
  influence.stat<-lm.influence(mod.fit)
  h.j<-influence.stat$hat
  # stand.resid<-rstandard(model = mod.fit) can also be used too
  stand.resid<-mod.fit$residuals/(sum.fit$sigma * sqrt(1 - h.j))

  
  #Figure 6.8 - notice how I got the "core number"
  par(mfrow=c(1,1), xaxs = "r")
  plot(x = ceiling((1:n)/4), y = stand.resid, main = "Stand. resid. vs. core number", 
       xlab = "Core number", ylab = "Standardized residual", lwd = 2,
       panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"))


#################################################################################
#BMA do not do this part
#  What if created another variable for core number?  This is just another way to remove the problem

  core<-ceiling((1:n)/4)
  rock2<-data.frame(rock, core)
  #Notice use of factor below - treats it as a categorical variable like a treatment number in ANOVA
  mod.fit<-lm(log(perm) ~ area + peri + shape + factor(core), data = rock2)
  sum.fit<-summary(mod.fit)
  sum.fit

  influence.stat<-lm.influence(mod.fit)
  h.j<-influence.stat$hat
  # stand.resid<-rstandard(model = mod.fit) can be used too
  stand.resid<-mod.fit$residuals/(sum.fit$sigma * sqrt(1 - h.j))

  
  #Figure 6.8 new version - this is much better than the previous plot! 
  plot(x = core, y = stand.resid, main = "Stand. resid. vs. core number", 
       xlab = "Core number", ylab = "Standardized residual", lwd = 2,
       panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"))



#################################################################################
# Find the mean for each x by core and fit model

  library(nlme)
  
  core<-ceiling((1:n)/4)
  rock2<-data.frame(rock, core)
  head(rock2)
  
  #Find the means for each explanatory variable by core group
  #  Note that perm was already the same for each core group
  rock.small<-gsummary(object = rock2, FUN = mean, groups = core)
  rock.small
  
  mod.fit<-lm(log(perm) ~ area + peri + shape, data = rock.small)
  sum.fit<-summary(mod.fit)
  sum.fit  #Produces Table 6.6 on p. 283
  
  z.0<-(sum.fit$coefficients[4,1] - 0)/sum.fit$coefficients[4,2]
  z.0

  mod.fit.reduced<-lm(log(perm) ~ area + peri, data = rock.small)
  anova(mod.fit.reduced, mod.fit, test = "F")
  
#################################################################################
# Bootstrap - model-based resampling

  calc.t.modbased<-function(data, i, mu.hat, x) {
    epsilon<-data[i]
    log.perm<-mu.hat + epsilon  
    rock.Ho<-data.frame(log.perm, x)
    mod.fit.modbased<-lm(log.perm ~ area + peri + shape, data = rock.Ho)
    sum.fit<-summary(mod.fit.modbased)
    c(as.numeric(mod.fit.modbased$coefficients), sum.fit$coefficients[4,2])  
  }
  
  #Work with Ho model
  mod.fit.Ho<-lm(log(perm) ~ area + peri, data = rock.small)  
  mu.hat.Ho<-mod.fit.Ho$fitted.values
  h.j.Ho<-lm.influence(mod.fit.Ho)$hat
  
  #Modified residuals
  r.j<-(log(rock.small$perm)-mu.hat.Ho)/sqrt(1-h.j.Ho)
  
  #Try it 
  calc.t.modbased(data = r.j - mean(r.j), i = 1:length(r.j), mu.hat = mu.hat.Ho, x = rock.small)
  
  
  set.seed(1912)
  boot.res.modbased<-boot(data = r.j - mean(r.j), statistic = calc.t.modbased, R = 999, sim = "ordinary", 
                          mu.hat = mu.hat.Ho, x = rock.small)
  boot.res.modbased                    


  #Find p-value without a studentized quantity - for Ho: Beta3 = 0 vs. Ha: Beta3>0
  #  Notice that mod.fit$coefficients[4] was used instead of boot.res.modbased$t0[4]
  (sum(boot.res.modbased$t[,4]>=mod.fit$coefficients[4])+1)/(boot.res.modbased$R+1)
  #Two-sided p-value could be calculated as 2*(above quantity) since want measure of how 
  #  extreme the test statistic is relative to the distribution.  Notice the above quantity is < 0.5; this
  #  is why we would use 2*P(T^* > t) = 2*min{P(T* > t), P(T*<t)}.  In other words, the observed value of t is closer to the right
  #  side end of the distribution
  

  #Find p-value using a studentized quantity - Ho: Beta3 = 0 vs. Ha: Beta3<>0
  z.0<-(sum.fit$coefficients[4,1] - 0)/sum.fit$coefficients[4,2]  #Found previously as well
  z.star.0<-(boot.res.modbased$t[,4] - 0)/boot.res.modbased$t[,5]
  (sum(abs(z.star.0)>=abs(z.0))+1)/(boot.res.modbased$R+1)
  #Alternative two-tail p-value 
  2*min((sum(z.star.0>=z.0)+1)/(boot.res.modbased$R+1), (sum(z.star.0<=z.0)+1)/(boot.res.modbased$R+1))
  
  
  #Find p-value using a studentized quantity - Ho: Beta3 = 0 vs. Ha: Beta3>0 as done in BMA
  (sum(z.star.0>=z.0)+1)/(boot.res.modbased$R+1)



  #Plots of resampling distribution under Ho
  par(mfrow = c(1,2), pty = "s", xaxs = "i")
  
  #Histogram - compare to the left plot in Figure 6.9
  hist(z.star.0, main = expression(paste("Histogram of ", z[0]^{"*"})), xlab=expression(paste(z[0]^{"*"})), freq = FALSE,
       xlim = c(min(z.star.0,z.0), max(z.star.0,z.0))) 
  abline(v = z.0, col = "darkgreen", lwd = 5)
  curve(expr = dnorm(x, mean = mean(z.star.0), sd = sd(z.star.0)), col = "red", add = TRUE)
  curve(expr = dt(x, df = mod.fit$df.residual), col = "darkblue", add = TRUE, lty = "dashed") #t with df = 8 which is what we would normally use
  legend(locator(1), legend = c(expression(paste("N(mean(", z[0]^{"*"},"), var(", z[0]^{"*"}, "))")), "t(8)"), lty = c("solid", "dashed"), 
         col = c("red", "darkblue"), cex = 0.6, bty = "n")

  #EDF
  plot.ecdf(z.star.0, verticals = TRUE, do.p = FALSE, main = expression(paste("EDF for ", z[0]^{"*"})), lwd = 2,
            panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"), 
            ylab = expression(paste(hat(G), " under ", H[o])), xlab = expression(paste(z[0]^{"*"})))
  curve(expr = pnorm(x, mean(z.star.0), sd = sd(z.star.0)), col = "red", add = TRUE)
  curve(expr = pt(x, df = mod.fit$df.residual), col = "darkblue", add = TRUE, lty = "dashed") #t with df = 8 which is what we would normally use





#################################################################################
# Bootstrap - case resampling
#   To help out, I have repeated some code from earlier in the program

  #Want to subtract out effects of X_0 from y
  #  Fit model under Ho
  mod.fit.Ho<-lm(log(perm) ~ area + peri, data = rock.small)  
  e.0<-mod.fit.Ho$residual 
  X.0<-as.matrix(data.frame(one = 1, area = rock.small$area, peri = rock.small$peri))
  H.0<-X.0%*%solve(t(X.0)%*%X.0)%*%t(X.0)  #Hat matrix
  #Check diagonal elements with lm.influence(mod.fit.Ho)$hat - yes, they check out
  
  #Want to subtract out effects of X_0 from X_1
  I.0<-diag(rep(x = 1, times = nrow(X.0))) #Identity matrix
  X.1<-as.matrix(rock.small$shape)
  X.1.0<-(I.0 - H.0)%*%X.1                 #Residuals
  X.1.0
  gamma.hat<-solve(t(X.1.0)%*%X.1.0)%*%t(X.1.0)%*%e.0 #Bottom p. 279 in BMA

  #Check X.1.0 with regression of X_1 as response and X_0 as explanatory variables
  mod.fit.ck<-lm(shape ~ area + peri, data = rock.small)  
  mod.fit.ck$residual #yes, they check out
  #When X.1 contains more than one variable in it, the above will still work as a check provided
  #  X.1 is a matrix (see documentation for lm() ).  In this setting, there will be more than one "response" 
  #  variable in X_1. 
  #  This corresponds to a "multiple" regression setting with more than one response variable
  #  STAT 873 (Chapter 12 from Dallas Johnson's 1998 multivariate book) explains that one can do a separate regression
  #  on each response variable.  This is what lm() will do with multiple response variables
  #  In the end,  X.1.0<-(I.0 - H.0)%*%X.1 is just providing these residuals as described in BMA on p. 279 
  #  Example of using more than one response variable (THIS IS NOT X.1!):    
  #mod.fit.ck<-lm(as.matrix(data.frame(rock.small$shape, rock.small$perm)) ~ area + peri, data = rock.small)  
  #mod.fit.ck

  
  
  #The same sampling method used in ex6.3.R can be used here.  This means to put the e.0 and X.1.0
  #  into one column of a data.frame (e.0 first and X.1.0 below it) with a second column denoting
  #  if it is e.0 or X.1.0.  The boot() function can then be implemented with the strata option to 
  #  resample within the e.0 or X.1.0 designation.  The calc.t() function can then reform the 
  #  data set in its correct form.  
  #The general problem with this idea is when X.1.0 is not a vector but is a matrix instead.
  #  This will happen when testing more than one explanatory variable at a time.  Therefore, I have wrote
  #  my calc.t function a little different below.  In place of X.1.0, I have created a dummy vector
  #  containing 1's (could have contained something else).  A data.frame is created with one column containing e.0
  #  first and then the dummy vector.  Again, a second column is created as an identifier.  I also pass into 
  #  calc.t() the X.1.0 matrix separately.  When boot() is run with the strata option, I will use the resampled
  #  indices it created to get the correct X.1.0* matrix.  
  #When X.1.0 is not a vector, a different test statistic needs to be used.  It can be the usual full model
  #  vs. reduced model sum of squares comparison statistic that has a F-distribution provided all model 
  #  assumptions are satisfied.  
  #Perhaps, it is easier to just do this all separately without the boot() function???? 
  
  #Create data set
  set1<-rbind(data.frame(value = e.0, variable = "e.0"), 
              data.frame(value = rep(x = 1, times = length(e.0)), variable = "dummy"))
  head(set1)
  tail(set1)
  
  calc.t.cases<-function(data, i, X.1.0.mat, n) {
    d<-data[i,]
    e.0<-d[d$variable == "e.0",1] #just get e.0
    X.1.0<-X.1.0.mat[i[(n+1):(2*n)]-n,]  
    gamma.hat<-solve(t(X.1.0)%*%X.1.0)%*%t(X.1.0)%*%e.0
    residual<-e.0 - X.1.0%*%gamma.hat
     
    #Note: ncol(X.1.0) does not work for some reason in this function???
    s.sq<-sum(residual^2) / (n - NCOL(X.1.0)) #Denominator is sample size - number of parameters estimating 
    var.gamma.hat<-s.sq * solve(t(X.1.0)%*%X.1.0)
    
    #(1,1) element (only one here) is the variance for shape's parameter estimate
    c(gamma.hat, var.gamma.hat) 
  }
  
  #Try it
  n<-length(e.0) 
  calc.t.cases(data = set1, i = 1:(2*n), X.1.0.mat = X.1.0, n = n)
  
  set.seed(3209)
  boot.res.cases<-boot(data = set1, statistic = calc.t.cases, R = 999, sim = "ordinary", strata = set1$variable,
                       X.1.0.mat = X.1.0, n = n)
  boot.res.cases


  #P-value without studentized quantity - Ho: Beta3 = 0 vs. Ha: Beta3>0 as done in BMA
  (sum(boot.res.cases$t[,1]>boot.res.cases$t0[1])+1)/(boot.res.cases$R+1)
  #(sum(boot.res.cases$t[,1]>gamma.hat)+1)/(boot.res.cases$R+1)  #Of course, same with this gamma.hat
  #Two-sided p-value could be calculated as 2*(above quantity)
  
  #Find p-value using a studentized quantity - Ho: Beta3 = 0 vs. Ha: Beta3<>0
  z.0<-(boot.res.cases$t0[1] - 0)/boot.res.cases$t0[2]  #Found previously as well
  z.star.0<-(boot.res.cases$t[,1] - 0)/boot.res.cases$t[,2]
  (sum(abs(z.star.0)>abs(z.0))+1)/(boot.res.cases$R+1)
 
  #Find p-value using a studentized quantity - Ho: Beta3 = 0 vs. Ha: Beta3>0 as done in BMA
  (sum(z.star.0>z.0)+1)/(boot.res.cases$R+1)

   
   
   
   
   
   
   
   
   
   
#
