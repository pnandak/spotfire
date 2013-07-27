#####################################################################
# NAME:  Chris Bilder                                               #
# DATE:  4-13-08                                                    #
# UPDATE:                                                           #
# Purpose: Examine placekicking data for bootstrap course           #
#                                                                   #
# NOTES:                                                            #
#####################################################################

#Read in data
  library(xlsReadWrite)
  placekick<-read.xls(file = "C:\\chris\\unl\\STAT950\\Chapter7\\placekick.xls", colNames = TRUE) 
  head(placekick)

#Fit model
  mod.fit<-glm(formula = good/total ~ change+distance+pat+wind+distance:wind, data = placekick, 
        weight=total, family = binomial(link = logit), na.action = na.exclude, 
        control = list(epsilon = 0.0001, maxit = 50, trace = T))
  summary(mod.fit)
  sum.fit<-summary(mod.fit)
  sum.fit$cov.unscaled  #Estimated covariance matrix for beta_hats
  sum.fit$cov.unscaled[2,2]

#C.I. for Beta_change
  wald.ci<-mod.fit$coefficients[2]-qnorm(p = c(0.95, 0.05), mean = 0, sd = 1)*sqrt(sum.fit$cov.unscaled[2,2])
  wald.ci
#C.I. for OR_change
  wald.ci.theta<-exp(mod.fit$coefficients[2]-qnorm(p = c(0.95, 0.05), mean = 0, sd = 1)*sqrt(sum.fit$cov.unscaled[2,2]))
  wald.ci.theta


#Estimate probability of success for field goal described in paper
  alpha<-0.1
  elliott<-data.frame(change=1, distance=42, pat=0, wind=0)
  save.pi.hat<-predict(object = mod.fit, newdata = elliott, type = "response", se = TRUE)
  save.pi.hat

  save.lp.hat<-predict(object = mod.fit, newdata = elliott, type = "link", se = TRUE)
  lower.lp<-save.lp.hat$fit-qnorm(1-alpha/2)*save.lp.hat$se
  upper.lp<-save.lp.hat$fit+qnorm(1-alpha/2)*save.lp.hat$se
  lower<-exp(lower.lp)/(1+exp(lower.lp))
  upper<-exp(upper.lp)/(1+exp(upper.lp))
  data.frame(elliott, lp.hat = round(save.lp.hat$fit,4), 
           lower.lp = round(lower.lp,4), upper.lp = round(upper.lp,4),
           lower = round(lower,4), upper = round(upper,4))



##################################################################
# Plot

  plot(placekick$distance, placekick$good/placekick$total,
     xlab="Distance in Yards", ylab="Estimated Probability of Success", type="n", 
     panel.first=grid(col = "gray", lty = "dotted"), main = 
     "Estimated probability of success of a field goal (PAT=0)")

  #Put estimated logistic regression model on the plot - change=0, wind=0
  #  This maybe could have been incorporated in the plot statement above.  
  curve(expr = plogis(mod.fit$coefficients[1]+mod.fit$coefficients[3]*x), lwd=2, col = "red", add = TRUE)

  #Put estimated logistic regression model on the plot - change=1, wind=0
  curve(expr = plogis(mod.fit$coefficients[1]+mod.fit$coefficients[3]*x+mod.fit$coefficients[2]), lty=3, 
      lwd=2, col = "green", add = TRUE)

  #Put estimated logistic regression model on the plot - change=0, wind=1
  curve(expr = plogis(mod.fit$coefficients[1]+mod.fit$coefficients[3]*x+mod.fit$coefficients[5]+mod.fit$coefficients[6]*x),
      lty=4, lwd=2, col = "blue", add = TRUE)

  #Put estimated logistic regression model on the plot - change=1, wind=1
  curve(expr = plogis(mod.fit$coefficients[1]+mod.fit$coefficients[3]*x+mod.fit$coefficients[2]+mod.fit$coefficients[5]+mod.fit$coefficients[6]*x),
      lty=2, lwd=2, col = "purple", add = TRUE)

  names1<-c("Change=0, Wind=0", "Change=1, Wind=0", "Change=0, Wind=1", "Change=1, Wind=1")
  legend(locator(1), legend=names1, lty=c(1,3,4,2), col=c("red","green","blue","purple"), bty="n", cex=0.75, lwd=2)



#####################################################################
# Bootstrap part - case based resampling using binomial form of data

  library(boot)

  calc.t.cases<-function(data, i, newdata) {
    d<-data[i,]
    mod.fit.cases<-glm(formula = good/total ~ change+distance+pat+wind+distance:wind, data = d, 
        weight=total, family = binomial(link = logit), na.action = na.exclude, 
        control = list(epsilon = 0.0001, maxit = 50, trace = F))
    sum.fit.cases<-summary(mod.fit.cases)
    
    save.pi.hat<-predict(object = mod.fit.cases, newdata = newdata, se = TRUE, type = "response")

    #Returns beta_0, beta_change, beta_distance, beta_pat, beta_wind, beta_distance:wind, var(beta_change), 
    #  pi_hat(x), sqrt(Var(pi.hat = Elliott)), OR change, converged?, residual deviance
    c(as.numeric(mod.fit.cases$coefficients), sum.fit.cases$cov.unscaled[2,2], save.pi.hat$fit, save.pi.hat$se.fit, 
      as.numeric(exp(mod.fit.cases$coefficients[2])), mod.fit.cases$converged, mod.fit.cases$deviance)
  }
  
  #Try it
    calc.t.cases(data = placekick, i = 1:nrow(placekick), newdata = elliott)
  
  #Do bootstrap
    set.seed(9198)
    boot.res.cases1<-boot(data = placekick, statistic = calc.t.cases, R = 1999, sim = "ordinary", newdata = elliott)
    boot.res.cases1
    plot(boot.res.cases1, index = 12) #check to see if get any really small residual deviance values

  #See all plots
    for (i in 1:12) {
      par(ask=T) #R will not go to the next plot until you give it the o.k.
      plot(boot.res.cases1, index = i)  #Look into in future: Why beta_hat_wind has some extreme values (could be due to app. only 10% of Bernoulli observations are wind  = 1)
    }
    par(ask=F) 

  #C.I. for beta_change
    save.ci.cases1<-boot.ci(boot.out = boot.res.cases1, conf = 0.90, type = c("norm", "basic", "bca", "perc"), index = 2)
    save.ci.cases1
  
  #Two ways for the beta_change C.I. with the studentized interval
    save.ci.cases2<-boot.ci(boot.out = boot.res.cases1, conf = 0.90, type = "stud", index = c(2,7))
    save.ci.cases2
    boot.ci(boot.out = boot.res.cases1, conf = 0.90, type = "stud", t = boot.res.cases1$t[,2], t0 = boot.res.cases1$t0[2], 
      var.t = boot.res.cases1$t[,7], var.t0 = boot.res.cases1$t0[7])
 
  #All C.I.s
    data.frame(name = c("Wald", "Normal BMA", "Basic", "BCa", "Percentile", "Studentized"),
             lower = c(wald.ci[1], save.ci.cases1$normal[2], save.ci.cases1$basic[4], save.ci.cases1$bca[4], save.ci.cases1$perc[4], save.ci.cases2$stud[4]),
             upper = c(wald.ci[2], save.ci.cases1$normal[3], save.ci.cases1$basic[5], save.ci.cases1$bca[5], save.ci.cases1$perc[5], save.ci.cases2$stud[5]),
             length = c(wald.ci[2] - wald.ci[1], save.ci.cases1$normal[3] - save.ci.cases1$normal[2],
                        save.ci.cases1$basic[5] - save.ci.cases1$basic[4],  save.ci.cases1$bca[5] - save.ci.cases1$bca[4], 
                        save.ci.cases1$perc[5] - save.ci.cases1$perc[4], save.ci.cases2$stud[5] - save.ci.cases2$stud[4]))
  
  #C.I. for OR of theta = exp(beta_change)
    save.ci.cases.or1<-boot.ci(boot.out = boot.res.cases1, conf = 0.90, type = c("norm", "basic", "bca", "perc"), index = 10)
    save.ci.cases.or1
    save.ci.cases.or2<-boot.ci(boot.out = boot.res.cases1, conf = 0.90, type = "stud", t = exp(boot.res.cases1$t[,2]), t0 = exp(boot.res.cases1$t0[2]), 
      var.t = exp(boot.res.cases1$t[,2])^2*boot.res.cases1$t[,7], var.t0 = exp(boot.res.cases1$t0[2])^2*boot.res.cases1$t0[7])
    #The variance used above comes about through using the delta-method;  Var(exp(beta^)) = exp(beta^)^2*Var(beta^)

    data.frame(name = c("Wald", "Basic", "BCa", "Percentile", "Studentized"), 
             lower = c(wald.ci.theta[1], save.ci.cases.or1$basic[4], save.ci.cases.or1$bca[4], save.ci.cases.or1$perc[4], 
                       save.ci.cases.or2$stud[4]), 
             upper = c(wald.ci.theta[2], save.ci.cases.or1$basic[5], save.ci.cases.or1$bca[5], save.ci.cases.or1$perc[5], 
                       save.ci.cases.or2$stud[5]))
  
  #BCa and Percentile just using C.I. for beta_change
  #  NOTE: BCa can be slightly different due to the numerical approximations used with the calculation of a - see p. 187 of Efron and Tibshirani (1993)
    c(1/exp(save.ci.cases1$bca[5]), 1/exp(save.ci.cases1$bca[4]))
    c(1/exp(save.ci.cases1$perc[5]), 1/exp(save.ci.cases1$perc[4]))

  
  #C.I. for pi(x = Elliott)
    save.ci.cases.pi<-boot.ci(boot.out = boot.res.cases1, conf = 0.90, type = c("norm", "basic", "bca", "perc"), index = 8)
    save.ci.cases.pi
    save.ci.cases.pi.stud<-boot.ci(boot.out = boot.res.cases1, conf = 0.90, type = "stud", t = boot.res.cases1$t[,8], t0 = boot.res.cases1$t0[8],
     var.t = boot.res.cases1$t[,9], var.t0 = boot.res.cases1$t0[9])

  #Using calculations from earlier 
    wald1<-save.pi.hat$fit-qnorm(c(1-alpha/2, alpha/2))*save.pi.hat$se

    data.frame(name = c("Wald1", "Wald2", "Basic", "BCa", "Percentile", "Studentized"), 
             lower = c(wald1[1], lower, save.ci.cases.pi$basic[4], save.ci.cases.pi$bca[4], save.ci.cases.pi$perc[4], 
                       save.ci.cases.pi.stud$stud[4]), 
             upper = c(wald1[2], upper, save.ci.cases.pi$basic[5], save.ci.cases.pi$bca[5], save.ci.cases.pi$perc[5], 
                       save.ci.cases.pi.stud$stud[5]))
 

 
###################################################################
# Bernoulli form of the data

  #Convert data to Bernoulli form
  placekick.bernoulli<-matrix(data = NA, nrow = 0, ncol = 5)

  for  (i in 1:nrow(placekick)) {
    if (placekick$good[i] > 0) {
      placekick.bernoulli<-rbind(placekick.bernoulli, matrix(data = data.frame(y = 1, placekick[i,2:5]), nrow = placekick$good[i], ncol = 5, byrow = TRUE))  
    } 
  
    if (placekick$total[i] - placekick$good[i]> 0) {
      placekick.bernoulli<-rbind(placekick.bernoulli, matrix(data = data.frame(y = 0, placekick[i,2:5]), nrow = placekick$total[i] - placekick$good[i], ncol = 5, byrow = TRUE))
    }
  }

  placekick2<-data.frame(y = as.numeric(placekick.bernoulli[,1]), change = as.numeric(placekick.bernoulli[,2]),
                       distance = as.numeric(placekick.bernoulli[,3]), pat = as.numeric(placekick.bernoulli[,4]),
                       wind = as.numeric(placekick.bernoulli[,5]))
  head(placekick2)


  #Check get same estimates (small differences will occur due to convergence criterion
  mod.fit2<-glm(formula = y ~ change+distance+pat+wind+distance:wind, data = placekick2, 
        family = binomial(link = logit), na.action = na.exclude, 
        control = list(epsilon = 0.0001, maxit = 50, trace = T))
  summary(mod.fit2)
  save.pi.hat2<-predict(object = mod.fit2, newdata = elliott, type = "response", se = TRUE)
  save.pi.hat2




##################################################################
# Bootstrap part - case based resampling using Bernoulli form of data


  calc.t.cases2<-function(data, i, newdata) {
    d<-data[i,]
    mod.fit.cases<-glm(formula = y ~ change+distance+pat+wind+distance:wind, data = d, 
        family = binomial(link = logit), na.action = na.exclude, 
        control = list(epsilon = 0.0001, maxit = 50, trace = F))
    sum.fit.cases<-summary(mod.fit.cases)
    
    save.pi.hat<-predict(object = mod.fit.cases, newdata = newdata, se = TRUE, type = "response")

    #Returns beta_0, beta_change, beta_distance, beta_pat, beta_wind, beta_distance:wind, var(beta_change), 
    #  pi_hat(x = Elliott), sqrt(Var(pi.hat = Elliott)), OR change, converged?, residual deviance
    c(as.numeric(mod.fit.cases$coefficients), sum.fit.cases$cov.unscaled[2,2], save.pi.hat$fit, save.pi.hat$se.fit, 
      as.numeric(exp(mod.fit.cases$coefficients[2])), mod.fit.cases$converged, mod.fit.cases$deviance)
  }

  #Try it
    calc.t.cases2(data = placekick2, i = 1:nrow(placekick2), newdata = elliott)
  
  #Do bootstrap
    set.seed(9198) #Use same seed number as before - This is just done to emphasize that DIFFERENT resamples result
    boot.res.cases2<-boot(data = placekick2, statistic = calc.t.cases2, R = 1999, sim = "ordinary", newdata = elliott)
    boot.res.cases2
    plot(boot.res.cases2, index = 12) #check to see if get any really small residual deviance values (would be sign of complete separation)

    #Examine all plots
    for (i in 1:12) {
      par(ask=T)
      plot(boot.res.cases2, index = i)
    }
    par(ask=F)


  #C.I. for beta_change
    save.ci.cases1<-boot.ci(boot.out = boot.res.cases2, conf = 0.90, type = c("norm", "basic", "bca", "perc"), index = 2)
    save.ci.cases1
  
  #beta_change C.I. with the studentized interval - could have done in the function call above
    save.ci.cases2<-boot.ci(boot.out = boot.res.cases2, conf = 0.90, type = "stud", index = c(2,7))
    save.ci.cases2

  #All C.I.s
    data.frame(name = c("Wald", "Normal BMA", "Basic", "BCa", "Percentile", "Studentized"),
             lower = c(wald.ci[1], save.ci.cases1$normal[2], save.ci.cases1$basic[4], save.ci.cases1$bca[4], save.ci.cases1$perc[4], save.ci.cases2$stud[4]),
             upper = c(wald.ci[2], save.ci.cases1$normal[3], save.ci.cases1$basic[5], save.ci.cases1$bca[5], save.ci.cases1$perc[5], save.ci.cases2$stud[5]),
             length = c(wald.ci[2] - wald.ci[1], save.ci.cases1$normal[3] - save.ci.cases1$normal[2],
                        save.ci.cases1$basic[5] - save.ci.cases1$basic[4],  save.ci.cases1$bca[5] - save.ci.cases1$bca[4],
                        save.ci.cases1$perc[5] - save.ci.cases1$perc[4], save.ci.cases2$stud[5] - save.ci.cases2$stud[4]))
  
  #C.I. for pi(x = Elliott)
    save.ci.cases.pi<-boot.ci(boot.out = boot.res.cases2, conf = 0.90, type = c("norm", "basic", "bca", "perc"), index = 8)
    save.ci.cases.pi



##################################################################
# Bootstrap part - parametric resampling

    #These are from a previous fit of the model.
    pi.hat<-mod.fit2$fitted.values

    save.par<-matrix(data = NA, nrow = 1999, ncol = 11)

    for (i in 1:1999) {

      y.star<-rbinom(n = nrow(placekick2), size = 1, prob = pi.hat)
      par.data<-data.frame(y.star, placekick2)
      mod.fit.par<-glm(formula = y.star ~ change+distance+pat+wind+distance:wind, data = par.data,
         family = binomial(link = logit), na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = F))
      sum.fit.par<-summary(mod.fit.par)
      save.pi.hat<-predict(object = mod.fit.par, newdata = elliott, se = TRUE, type = "response")

      save.par[i,]<-c(as.numeric(mod.fit.par$coefficients), sum.fit.par$cov.unscaled[2,2], save.pi.hat$fit, save.pi.hat$se.fit,
        mod.fit.par$converged, mod.fit.par$deviance)

    }
  
    #Intervals for Beta_change
    #Percentile interval
    perc<-as.numeric(quantile(x = save.par[,2], probs = c(0.05, 0.95), type = 1))
    #Basic interval
    low.basic<-2*mod.fit2$coefficients[2] - perc[2]
    up.basic<-2*mod.fit2$coefficients[2] - perc[1]

    data.frame(name = c("Percentile", "Basic"), lower = c(perc[1], low.basic), upper = c(perc[2], up.basic))
  
    #Intervals for pi
    #Percentile interval
    perc<-as.numeric(quantile(x = save.par[,8], probs = c(0.05, 0.95), type = 1))
    #Basic interval
    save.pi.hat<-predict(object = mod.fit, newdata = elliott, type = "response", se = TRUE)
    low.basic<-2*save.pi.hat$fit - perc[2]
    up.basic<-2*save.pi.hat$fit - perc[1]

    data.frame(name = c("Percentile", "Basic"), lower = c(perc[1], low.basic), upper = c(perc[2], up.basic))




    #Now, use the boot() function to do the parametric bootstrap
    
    #Probably should pass in the elliott data here instead of the way I have it.
    calc.t.par<-function(data) {
       mod.fit.par<-glm(formula = y ~ change+distance+pat+wind+distance:wind, data = data,
           family = binomial(link = logit), na.action = na.exclude, control = list(epsilon = 0.0001, maxit = 50, trace = F))
       sum.fit.par<-summary(mod.fit.par)
       save.pi.hat<-predict(object = mod.fit.par, newdata = elliott, se = TRUE, type = "response")
       c(as.numeric(mod.fit.par$coefficients), sum.fit.par$cov.unscaled[2,2], save.pi.hat$fit, save.pi.hat$se.fit,
         mod.fit.par$converged, mod.fit.par$deviance)
    }

    sim.data<-function(data, mle) {
      y.star<-rbinom(n = nrow(data), size = 1, prob = mle)
      #data.frame(y, placekick2[,-1])
      data$y<-y.star #Replace observed y with y.star
      data
    }
    
    #Testing functions above
    #temp<-sim.data(data = placekick2, mle = mod.fit2$fitted.values)
    # head(temp)
    #calc.t.par(temp)
    #head(placekick2)
  
    boot.res.par<-boot(data = placekick2, statistic = calc.t.par, R = 1999, sim = "parametric",
                     ran.gen = sim.data, mle = mod.fit2$fitted.values)
    boot.res.par
  

    save.ci.par1<-boot.ci(boot.out = boot.res.par, conf = 0.90, type = c("norm", "basic", "perc"), index = 2)
    save.ci.par1
    #Note: boot.ci() will not calculate the empirical influence values for a parametric bootstrap so bca can not be done
    #      exactly the same way as usual.  One can derive these values or possibly calculate them all outside of boot.ci()
    #      and then pass them into the function
    

  #studentized interval
    save.ci.par2<-boot.ci(boot.out = boot.res.par, conf = 0.90, type = "stud", index = c(2,7))
    save.ci.par2

  #All C.I.s
    data.frame(name = c("Wald", "Normal BMA", "Basic", "Percentile", "Studentized"),
             lower = c(wald.ci[1], save.ci.par1$normal[2], save.ci.par1$basic[4], save.ci.par1$perc[4], save.ci.par2$stud[4]),
             upper = c(wald.ci[2], save.ci.par1$normal[3], save.ci.par1$basic[5], save.ci.par1$perc[5], save.ci.par2$stud[5]),
             length = c(wald.ci[2] - wald.ci[1], save.ci.par1$normal[3] - save.ci.par1$normal[2],
                        save.ci.par1$basic[5] - save.ci.par1$basic[4],
                        save.ci.par1$perc[5] - save.ci.par1$perc[4], save.ci.par2$stud[5] - save.ci.par2$stud[4]))
 
   #C.I. for pi
    save.ci.par.pi<-boot.ci(boot.out = boot.res.par, conf = 0.90, type = c("norm", "basic", "perc"), index = 8)
    save.ci.par.pi
    boot.ci(boot.out = boot.res.par, conf = 0.90, type = "stud", t = boot.res.par$t[,8], t0 = boot.res.par$t0[8],
      var.t = boot.res.par$t[,9]^2, var.t0 = boot.res.par$t0[9]^2) 
  
 


#


#
