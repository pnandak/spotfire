########################################################################
# NAME:  Chris Bilder                                                  #
# DATE:  3-6-06                                                        #
# UPDATE: 11-5-07                                                      #
# PURPOSE: Example 6.1 and 6.2                                         #
#                                                                      #
# NOTES: Mammals data set is not in the boot package!  I had to        #
#        download it form http://statwww.epfl.ch/davison/BMA/Data4BMA/ #
########################################################################

########################################################################
# Get the data and do some initial examinations

  library(boot)

  mammals<-read.table(file = "C:\\chris\\UNL\\STAT_boot\\chapter6\\mammals.dat", header = TRUE)
  head(mammals)

  #Figure 6.1
  par(mfrow = c(1,2))
  plot(x = mammals$body, y = mammals$brain, main = "Brain weight vs. body weight", xlab = "Body weight", 
       ylab = "Brain weight", panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"))
  plot(x = mammals$body, y = mammals$brain, log = "xy", main = "Brain weight vs. body weight (log scale)", 
       xlab = "Body weight", ylab = "Brain weight", panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"))

  #log transformation applied
  plot(x = log(mammals$body), y = log(mammals$brain), main = "log(Brain weight) vs. log(body weight)", 
       xlab = "log(Body weight)", ylab = "log(Brain weight)", 
       panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"))
  
  
  
########################################################################
# Example 6.1: Figure 6.2 work

  mod.fit<-lm(log(brain) ~ log(body), data = mammals)
  summary(mod.fit)
  names(mod.fit)
  anova(mod.fit)
  
  sum.fit<-summary(mod.fit)
  names(sum.fit)
  sum.fit$sigma #sqrt(MSE)
  
  #Get h_j
  influence.stat<-lm.influence(mod.fit)
  h.j<-influence.stat$hat
  
  #Modified residuals
  r.j<-mod.fit$residuals/sqrt(1-h.j)
  mean(r.j)
  
  #Using my examine.mod.multiple.final() function from STAT 870 - get from R web page on STAT 870 website
  #examine.mod.multiple.final(mod.fit.obj = mod.fit, first.order = 1)
  
  par(mfrow = c(1,2))
  n<-nrow(mammals)
  norm.quant<-qnorm(p = seq(from = 1/(n+1), to = 1-1/(n+1), by = 1/(n+1)), mean = mean(r.j), sd = sd(r.j))
  plot(y = sort(r.j), x = norm.quant, main = expression(paste("QQ-Plot for ", r[j])), ylab = "Modified residual", 
       xlab = "Quantiles of Standard Normal", panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"))
  abline(a = 0, b = 1, col = "red")
 
  plot(y = r.j, x = h.j,  main = expression(paste(r[j], " vs. ", h[j])), ylab = "Modified residual", 
       xlab = "Leverage h", panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"))

  
  #Additional set of plots
  #Histogram
  par(mfrow = c(1,2), pty = "s") 
  hist(r.j, main = expression(paste("Histogram of ", r[j])), xlab = expression(r[j]), freq = FALSE)
  curve(dnorm(x, mean = mean(r.j), sd = sd(r.j)), col = "red", add = TRUE)

  #EDF 
  # ^ does not appear correct on G?  Not sure how to fix
  plot.ecdf(r.j, verticals = TRUE, do.p = FALSE, main = expression(paste("EDF for ", r[j])), lwd = 2,
            panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"), 
            ylab = expression(paste(hat(G), " for ", r[j])),  xlab = expression(r[j]))
  curve(expr = pnorm(x, mean = mean(r.j), sd = sd(r.j)), col = "red", add = TRUE)

  
  #Plots produced using an object of class "lm"
  par(mfrow = c(1,2))
  plot(mod.fit, which = 1:2)  #Just do the first 2 of 6 possible plots (see help for plot.lm)
                              #  Can not copy and paste these two plots without doing this
  plot(mod.fit)  #Do the default plots                 



########################################################################
# Example 6.1: Bootstrap - model-based resampling 


  calc.t.modbased<-function(data, i, mu.hat, x) {
    epsilon<-data[i]
    y<-mu.hat + epsilon
    mod.fit.modbased<-lm(y ~ x)
    sum.fit.modbased<-summary(mod.fit.modbased)
    #Replaced below with sum.fit.modbased$sigma which is sqrt(MSE)
    #s.sq<-sum(mod.fit.modbased$residuals^2)/(mod.fit.modbased$df.residual) #MSE
    c(as.numeric(mod.fit.modbased$coefficients), sum.fit.modbased$sigma)  #as.numeric() is just used to remove some not needed labels
  }
  
  #Try it 
  calc.t.modbased(data = r.j - mean(r.j), i = 1:n, mu.hat = mod.fit$fitted.values, x = log(mammals$body))
  
  set.seed(8719)
  boot.res.modbased<-boot(data = r.j - mean(r.j), statistic = calc.t.modbased, R = 999, sim = "ordinary", 
                 mu.hat = mod.fit$fitted.values, x = log(mammals$body))
  boot.res.modbased
  plot(boot.res.modbased)  #This is for beta_hat0
  plot(boot.res.modbased, index = 2) 
  plot(boot.res.modbased, index = 3) 
  
  SSx<-var(log(mammals$body))*(n-1)  #SUM( (x_i - x_bar)^2 )
  z.star.beta1<-(boot.res.modbased$t[,2] -  boot.res.modbased$t0[2])/sqrt(boot.res.modbased$t[,3]^2/SSx)  #Note: Var(beta_hat1*) = s^2* / SSx (see equation 6.6 and p. 263)
  quantile(x = z.star.beta1, probs = c(0.05, 0.95), type = 1)  #Estimated quantiles for z* on p. 263
  qnorm(p = c(0.05, 0.95))  #Expected quantiles for epsilon~N(0, sigma^2)
  
  
  
########################################################################
# Example 6.2: Case based resampling 
  
  calc.t.cases<-function(data, i) {
    d<-data[i,]
    mod.fit.cases<-lm(log(d$brain) ~ log(d$body), data = d)
    sum.fit.cases<-summary(mod.fit.cases)
    #s.sq<-sum(mod.fit.cases$residuals^2)/(mod.fit.cases$df.residual) #MSE
    c(as.numeric(mod.fit.cases$coefficients), sum.fit.cases$sigma)
  }
  
  #Try it
  calc.t.cases(data = mammals, i = 1:n)
  
  
  set.seed(4121)
  boot.res.cases<-boot(data = mammals, statistic = calc.t.cases, R = 999, sim = "ordinary")
  boot.res.cases

 
  #The purpose of this part it to come close to what BMA calls "robust theoretical" in Table 6.1
  l.reg.beta0<-empinf(boot.out = boot.res.cases, index = 1)
  l.reg.beta1<-empinf(boot.out = boot.res.cases, index = 2)
  
  l.jack.beta0<-empinf(data = mammals, statistic = calc.t.cases, stype = "i", type = "jack", index = 1) 
  l.jack.beta1<-empinf(data = mammals, statistic = calc.t.cases, stype = "i", type = "jack", index = 2) 

  data.frame(var.beta0.jack = var.linear(l.jack.beta0), var.beta1.jack = var.linear(l.jack.beta1),
             var.beta0.reg = var.linear(l.reg.beta0), var.beta1.reg = var.linear(l.reg.beta1))
  sqrt(data.frame(var.beta0.jack = var.linear(l.jack.beta0), var.beta1.jack = var.linear(l.jack.beta1),
             var.beta0.reg = var.linear(l.reg.beta0), var.beta1.reg = var.linear(l.reg.beta1)))


  #Figure 6.3 - sort of
  #  Remember that BMA construct QQ-plots a little different from me 
  #  I used the mean and standard deviation from the beta_hat*'s to examine normality
  par(mfrow = c(1,2))
  numb<-length(boot.res.cases$t[,1])
  norm.quant<-qnorm(p = seq(from = 1/(numb+1), to = 1-1/(numb+1), by = 1/(numb+1)), mean = mean(boot.res.cases$t[,1]), 
                    sd = sd(boot.res.cases$t[,1]))
  plot(y = sort(boot.res.cases$t[,1]), x = norm.quant, main = expression(paste("QQ-Plot for ", hat(beta)[0]^{"*"})), 
       ylab = "Intercept", xlab = "Quantiles of Standard Normal", 
       panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"))
  abline(a = 0, b = 1, col = "red")
 
  norm.quant<-qnorm(p = seq(from = 1/(numb+1), to = 1-1/(numb+1), by = 1/(numb+1)), mean = mean(boot.res.cases$t[,2]), 
                    sd = sd(boot.res.cases$t[,2]))
  plot(y = sort(boot.res.cases$t[,2]), x = norm.quant, main = expression(paste("QQ-Plot for ", hat(beta)[1]^{"*"})), 
       ylab = "Slope", xlab = "Quantiles of Standard Normal", 
       panel.first = grid(nx = NULL, ny = NULL, col="gray", lty="dotted"))
  abline(a = 0, b = 1, col = "red")


  #Figure 6.3 - This is close to how BMA did the plots
  sum.fit<-summary(mod.fit)
  names(sum.fit)
  qqnorm(boot.res.cases$t[,1], pch=1, main = expression(paste("QQ-Plot for ", hat(beta)[0]^{"*"})), 
       ylab = "Intercept", xlab = "Quantiles of Standard Normal")
  abline(a = sum.fit$coefficients[1,1], b = sum.fit$coefficients[1,2], col = "red", lty = "dotted")

  qqnorm(boot.res.cases$t[,2], pch=1, main = expression(paste("QQ-Plot for ", hat(beta)[1]^{"*"})), 
         ylab = "Slope", xlab = "Quantiles of Standard Normal")
  abline(a = sum.fit$coefficients[2,1], b = sum.fit$coefficients[2,2], col = "red", lty = "dotted")
  abline(a = sum.fit$coefficients[2,1], b = sqrt(var.linear(l.jack.beta1)), col = "blue", lty = "dashed")
  #I used the jackknife estimated empirical influence values in the nonparametric delta-method variance above
  legend(locator(1), legend = c("Use eq. 6.6 for variance", "use similar variance to eq. 6.17"), 
         lty = c("dotted", "dashed"), col = c("red", "blue"), cex = 0.75)











#
